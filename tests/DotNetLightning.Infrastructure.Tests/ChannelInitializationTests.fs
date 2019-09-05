module ChannelInitializationTests

open FSharp.Control.Reactive
open System.Reactive.Threading.Tasks
open DotNetLightning.Utils.Primitives
open NBitcoin
open CustomEventAggregator
open DotNetLightning.LN
open DotNetLightning.Infrastructure

open System.Net
open DotNetLightning.Chain
open Expecto
open Foq
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options

type internal ActorCreator =
    static member hex = NBitcoin.DataEncoders.HexEncoder()
    
    static member aliceNodeSecret = 
        Key(ActorCreator.hex.DecodeData("1111111111111111111111111111111111111111111111111111111111111111"))
        
    static member aliceNodeId = ActorCreator.aliceNodeSecret.PubKey |> NodeId
      
    static member bobNodeSecret =
        Key(ActorCreator.hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202"))
    static member bobNodeId = ActorCreator.bobNodeSecret.PubKey |> NodeId
    
    
    static member getAlice(?keyRepo: IKeysRepository, ?nodeParams, ?chainWatcher, ?broadCaster) =
        let keyRepo = defaultArg keyRepo (Mock<IKeysRepository>.Method(fun repo -> <@ repo.GetNodeSecret @>).Returns(ActorCreator.aliceNodeSecret))
        let peerLogger = TestLogger.create<PeerManager>()
        let channelLogger = TestLogger.create<ChannelManager>()
        let nodeParams = defaultArg nodeParams (Options.Create<NodeParams>(TestConstants.getAliceParam().NodeParams))
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
                let feeEstimator = Mock<IFeeEstimator>().Create()
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
        let keyRepo = defaultArg keyRepo (Mock<IKeysRepository>.Method(fun repo -> <@ repo.GetNodeSecret @>).Returns(ActorCreator.bobNodeSecret))
        let peerLogger = TestLogger.create<PeerManager>()
        let channelLogger = TestLogger.create<ChannelManager>()
        let nodeParams = defaultArg nodeParams (Options.Create<NodeParams>(TestConstants.getBobParam().NodeParams))
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
                let feeEstimator = Mock<IFeeEstimator>().Create()
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
        do! actors.Launch(ActorCreator.bobNodeId) |> Async.AwaitTask
        let! _ = alice.EventAggregator.GetObservable<PeerEvent>()
                     |> Observable.filter(function PeerEvent.Connected _ -> true | _ -> false)
                     |> Observable.first
                     |> fun o -> o.ToTask() |> Async.AwaitTask
        return actors
    }
    
[<Tests>]
let tests =
    testList "Basic Channel handling between 2 peers" [
        ptestAsync "Channel Initialization" {
            let alice = ActorCreator.getAlice()
            let bob = ActorCreator.getBob()
            let! actors = ActorCreator.initiateActor(alice, bob)
            let! bobInit = alice.EventAggregator.GetObservable<PeerEvent>()
                            |> Observable.choose(function | ReceivedInit(nodeId, init) -> Some init | _ -> None)
                            |> fun o -> o.ToTask()
                            |> Async.AwaitTask
            let initFunder = { InputInitFunder.PushMSat = TestConstants.pushMsat
                               TemporaryChannelId = Key().ToBytes() |> uint256 |> ChannelId
                               FundingSatoshis = TestConstants.fundingSatoshis
                               InitFeeRatePerKw = TestConstants.feeratePerKw
                               FundingTxFeeRatePerKw = TestConstants.feeratePerKw
                               LocalParams =
                                   let defaultFinalScriptPubKey = Key().PubKey.WitHash.ScriptPubKey
                                   alice.PM.MakeLocalParams(defaultFinalScriptPubKey, true, TestConstants.fundingSatoshis)
                               RemoteInit = bobInit
                               ChannelFlags = 0x00uy
                               ChannelKeys =  alice.CM.KeysRepository.GetChannelKeys(false) }
            alice.CM.AcceptCommand(ActorCreator.bobNodeId, ChannelCommand.CreateOutbound(initFunder))
            let! e = alice.EventAggregator.GetObservable<ChannelEvent>().ToTask() |> Async.AwaitTask
            match e with
            | ChannelEvent.NewOutboundChannelStarted _ -> ()
            | e -> failwithf "%A" e
            let! e = bob.EventAggregator.GetObservable<ChannelEvent>().ToTask() |> Async.AwaitTask
            match e with
            | ChannelEvent.WeAcceptedOpenChannel _ -> ()
            | e -> failwithf "%A" e
            return ()
        }
    ]

