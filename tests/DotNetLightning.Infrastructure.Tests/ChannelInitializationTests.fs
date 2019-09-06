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
open System.Threading.Tasks
open DotNetLightning.Chain
open DotNetLightning.Utils.Primitives
open Expecto
open Foq
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options
open TestConstants

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
                let channelEventRepo =
                    Mock<IChannelEventRepository>
                        .Method(fun x -> <@ x.SetEventsAsync @>).Returns(Task.CompletedTask)
                let chainListener = Mock<IChainListener>().Create()
                let feeEstimator =
                    Mock<IFeeEstimator>.Method(fun x -> <@ x.GetEstSatPer1000Weight @>).Returns(5000u |> FeeRatePerKw)
                let fundingTxProvider = DummyFundingTxProvider(aliceParam.NodeParams.ChainNetwork)
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
        let peerLogger =
            // Mock<ILogger<PeerManager>>().Create()
            TestLogger.create<PeerManager>()
        let channelLogger =
            // Mock<ILogger<ChannelManager>>().Create()
            TestLogger.create<ChannelManager>()
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
                let channelEventRepo =
                    Mock<IChannelEventRepository>
                        .Method(fun x -> <@ x.SetEventsAsync @>).Returns(Task.CompletedTask)
                let chainListener = Mock<IChainListener>().Create()
                let feeEstimator =
                    Mock<IFeeEstimator>.Method(fun x -> <@ x.GetEstSatPer1000Weight @>).Returns(5000u |> FeeRatePerKw)
                let fundingTxProvider = DummyFundingTxProvider(bobParam.NodeParams.ChainNetwork)
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
        testAsync "Channel Initialization" {
            let alice = ActorCreator.getAlice()
            let bob = ActorCreator.getBob()
            let bobInitTask = alice.EventAggregator.GetObservable<PeerEvent>()
                              |> Observable.awaitFirst(function | ReceivedInit(_nodeId, init) -> Some init | _ -> None)
            let! _actors = ActorCreator.initiateActor(alice, bob)
            
            let! bobInit = bobInitTask
            
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
            let aliceChannelEventFuture =
                alice.EventAggregator.AwaitChannelEvent()
            let bobChannelEventFuture1 =
                bob.EventAggregator.AwaitChannelEvent(function NewInboundChannelStarted msg -> Some msg | _ -> None)
            let bobChannelEventFuture2 =
                bob.EventAggregator.AwaitChannelEvent(function WeAcceptedOpenChannel(acceptChannel, _state) -> Some acceptChannel | _ -> None)
                
            let aliceReceivedAcceptChannel =
                alice.EventAggregator.AwaitChannelEvent(function WeAcceptedAcceptChannel(i, _) -> Some i | _ -> None)
            
            let bobNodeId = bob.CM.KeysRepository.GetNodeSecret().PubKey |> NodeId
            do! alice.CM.AcceptCommandAsync(bobNodeId, ChannelCommand.CreateOutbound(initFunder)) |> Async.AwaitTask
            
            let! r = aliceChannelEventFuture
            Expect.isSome r "timeout"
            let! r = bobChannelEventFuture1
            Expect.isSome (r) "timeout"
            let! r = bobChannelEventFuture2
            Expect.isSome (r) "timeout"
            
            let! r = aliceReceivedAcceptChannel
            Expect.isSome r "timeout"
            
            return ()
        }
    ]

