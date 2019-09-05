module ChannelInitializationTests

open FSharp.Control.Reactive
open System.Reactive.Threading.Tasks
open DotNetLightning.Utils.Primitives
open NBitcoin
open CustomEventAggregator
open DotNetLightning.LN
open DotNetLightning.Infrastructure

open System
open System.Net
open DotNetLightning.Chain
open DotNetLightning.Utils.Primitives
open Expecto
open Foq
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options

type internal ActorCreator =
    static member hex = NBitcoin.DataEncoders.HexEncoder()
    
    
    static member getAlice(?keyRepo: IKeysRepository, ?nodeParams, ?chainWatcher, ?broadCaster) =
        let aliceParam = TestConstants.getAliceParam()
        let keyRepo = defaultArg keyRepo (aliceParam.KeyRepo)
        let peerLogger = TestLogger.create<PeerManager>()
        let channelLogger = TestLogger.create<ChannelManager>()
        let nodeParams = defaultArg nodeParams (Options.Create<NodeParams>(aliceParam.NodeParams))
        let chainWatcher = defaultArg chainWatcher (Mock<IChainWatcher>().Create())
        let broadCaster = defaultArg broadCaster (Mock<IBroadCaster>().Create())
        let eventAggregator = ReactiveEventAggregator() :> IEventAggregator
        {
            PeerManagerEntity.Id = IPEndPoint.Parse("127.0.0.2") :> EndPoint |> PeerId
            PM = PeerManager(keyRepo,
                             peerLogger,
                             nodeParams,
                             eventAggregator,
                             chainWatcher,
                             broadCaster
                             )
            CM =
                let channelEventRepo = Mock<IChannelEventRepository>().Create()
                let chainListener = Mock<IChainListener>().Create()
                let feeEstimator =
                    Mock<IFeeEstimator>.Method(fun x -> <@ x.GetEstSatPer1000Weight @>).Returns(1000u |> FeeRatePerKw)
                let fundingTxProvider = Mock<IFundingTxProvider>().Create()
                ChannelManager(nodeParams,
                               channelLogger,
                               eventAggregator,
                               channelEventRepo,
                               chainListener,
                               keyRepo,
                               feeEstimator,
                               fundingTxProvider
                               )
            EventAggregator = eventAggregator
        }
    
    static member getBob(?keyRepo: IKeysRepository, ?nodeParams, ?chainWatcher, ?broadCaster) =
        let bobParam = TestConstants.getBobParam()
        let keyRepo = defaultArg keyRepo (bobParam.KeyRepo)
        let peerLogger = TestLogger.create<PeerManager>()
        let channelLogger = TestLogger.create<ChannelManager>()
        let nodeParams = defaultArg nodeParams (Options.Create<NodeParams>(bobParam.NodeParams))
        let chainWatcher = defaultArg chainWatcher (Mock<IChainWatcher>().Create())
        let broadCaster = defaultArg broadCaster (Mock<IBroadCaster>().Create())
        let eventAggregator = ReactiveEventAggregator() :> IEventAggregator
        {
            PeerManagerEntity.Id = IPEndPoint.Parse("127.0.0.2") :> EndPoint |> PeerId
            PM = PeerManager(keyRepo,
                             peerLogger,
                             nodeParams,
                             eventAggregator,
                             chainWatcher,
                             broadCaster
                             )
            CM =
                let channelEventRepo = Mock<IChannelEventRepository>().Create()
                let chainListener = Mock<IChainListener>().Create()
                let feeEstimator =
                    Mock<IFeeEstimator>.Method(fun x -> <@ x.GetEstSatPer1000Weight @>).Returns(1000u |> FeeRatePerKw)
                let fundingTxProvider = Mock<IFundingTxProvider>().Create()
                ChannelManager(nodeParams,
                               channelLogger,
                               eventAggregator,
                               channelEventRepo,
                               chainListener,
                               keyRepo,
                               feeEstimator,
                               fundingTxProvider
                               )
            EventAggregator = eventAggregator
        }
    
    static member initiateActor(alice, bob) = async {
        let actors = new PeerActors(alice, bob)
        let t = actors.Initiator.PM.EventAggregator.GetObservable<PeerEvent>()
                     |> Observable.awaitFirst(function PeerEvent.Connected _ -> Some () | _ -> None)
        let bobNodeId = bob.CM.KeysRepository.GetNodeSecret().PubKey |> NodeId
        do! actors.Launch(bobNodeId) |> Async.AwaitTask
        let! _ = t
        return actors
    }
    
[<Tests>]
let tests =
    testList "Basic Channel handling between 2 peers" [
        ftestAsync "Channel Initialization" {
            let alice = ActorCreator.getAlice()
            let bob = ActorCreator.getBob()
            let mutable bobInit = None
            let bobInitTask = alice.EventAggregator.GetObservable<PeerEvent>()
                              |> Observable.choose(function | ReceivedInit(nodeId, init) -> Some init | _ -> None)
                              |> Observable.subscribe(fun i -> bobInit <- Some i)
            let! actors = ActorCreator.initiateActor(alice, bob)
            Console.WriteLine "actors started"
            
            do! Async.Sleep 100
            let initFunder = { InputInitFunder.PushMSat = TestConstants.pushMsat
                               TemporaryChannelId = Key().ToBytes() |> uint256 |> ChannelId
                               FundingSatoshis = TestConstants.fundingSatoshis
                               InitFeeRatePerKw = TestConstants.feeratePerKw
                               FundingTxFeeRatePerKw = TestConstants.feeratePerKw
                               LocalParams =
                                   let defaultFinalScriptPubKey = Key().PubKey.WitHash.ScriptPubKey
                                   alice.PM.MakeLocalParams(defaultFinalScriptPubKey, true, TestConstants.fundingSatoshis)
                               RemoteInit = bobInit.Value
                               ChannelFlags = 0x00uy
                               ChannelKeys =  alice.CM.KeysRepository.GetChannelKeys(false) }
            Console.WriteLine "Creating outbound channel ..."
            let aliceChannelEventFuture =
                alice.EventAggregator.GetObservable<ChannelEventWithContext>()
                |> Observable.map(fun cc -> cc.ChannelEvent)
                |> Observable.awaitFirst(fun e -> Some e)
            let bobChannelEventFuture =
                bob.EventAggregator.GetObservable<ChannelEventWithContext>()
                |> Observable.map(fun cc -> cc.ChannelEvent)
                |> Observable.awaitFirst(Some)
            
            let bobNodeId = bob.CM.KeysRepository.GetNodeSecret().PubKey |> NodeId
            alice.CM.AcceptCommand(bobNodeId, ChannelCommand.CreateOutbound(initFunder))
            
            match! aliceChannelEventFuture  with
            | ChannelEvent.NewOutboundChannelStarted _ -> ()
            | e -> failwithf "%A" e
            
            match! bobChannelEventFuture with
            | NewInboundChannelStarted _ -> ()
            | e -> failwith "%A" e
            
            do! Async.Sleep 100
            return ()
        }
    ]

