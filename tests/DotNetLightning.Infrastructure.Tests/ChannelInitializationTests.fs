module ChannelInitializationTests

open FSharp.Control.Reactive
open DotNetLightning.Utils.Primitives
open NBitcoin
open CustomEventAggregator
open DotNetLightning.LN
open DotNetLightning.Infrastructure

open System
open System.Net
open System.Threading.Tasks
open DotNetLightning.Chain
open Expecto
open Foq
open Microsoft.Extensions.Options
open TestConstants

type internal ActorCreator =
    static member hex = NBitcoin.DataEncoders.HexEncoder()
    
    
    static member getAlice(?keyRepo: IKeysRepository, ?nodeParams, ?chainWatcher, ?broadCaster) =
        let aliceParam = TestConstants.getAliceParam()
        let keyRepo = defaultArg keyRepo (aliceParam.KeyRepo)
        let peerLogger = TestLogger.create<PeerManager>(ConsoleColor.Red)
        let channelLogger = TestLogger.create<ChannelManager>(ConsoleColor.Magenta)
        let nodeParams = defaultArg nodeParams (Options.Create<NodeParams>(aliceParam.NodeParams))
        let chainWatcher = defaultArg chainWatcher (Mock<IChainWatcher>().Create())
        let broadCaster = defaultArg broadCaster (Mock<IBroadCaster>().Create())
        let eventAggregator = ReactiveEventAggregator() :> IEventAggregator
        {
            PeerManagerEntity.Id = IPEndPoint.Parse("127.1.1.1") :> EndPoint |> PeerId
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
            TestLogger.create<PeerManager>(ConsoleColor.Blue)
        let channelLogger =
            // Mock<ILogger<ChannelManager>>().Create()
            TestLogger.create<ChannelManager>(ConsoleColor.Green)
        let nodeParams = defaultArg nodeParams (Options.Create<NodeParams>(bobParam.NodeParams))
        let chainWatcher = defaultArg chainWatcher (Mock<IChainWatcher>().Create())
        let broadCaster = defaultArg broadCaster (Mock<IBroadCaster>().Create())
        let eventAggregator = ReactiveEventAggregator() :> IEventAggregator
        {
            PeerManagerEntity.Id = IPEndPoint.Parse("127.1.1.2") :> EndPoint |> PeerId
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
    
let temporaryChannelId =
    "5555555555555555555555555555555555555555555555555555555555555555"
    |> hex.DecodeData
    |> uint256
    |> ChannelId
    
let defaultFinalScriptPubKey =
    "5555555555555555555555555555555555555555555555555555555555555555"
    |> hex.DecodeData
    |> Key
    |> fun k -> k.PubKey.WitHash.ScriptPubKey
[<Tests>]
let tests =
    testList "Basic Channel handling between 2 peers" [
        testAsync "Channel Initialization" {

            let alice = ActorCreator.getAlice()
            let bob = ActorCreator.getBob()
            let bobInitTask = alice.EventAggregator.GetObservable<PeerEvent>()
                              |> Observable.map(fun e -> printfn "alice has raised peer event %A" e; e)
                              |> Observable.awaitFirst(function | ReceivedInit(_nodeId, init) -> Some init | _ -> None)
            let! _actors = ActorCreator.initiateActor(alice, bob)
            
            let! bobInit = bobInitTask
            
            let channelKeys = alice.CM.KeysRepository.GetChannelKeys(false)
            let initFunder = { InputInitFunder.PushMSat = TestConstants.pushMsat
                               TemporaryChannelId = temporaryChannelId
                               FundingSatoshis = TestConstants.fundingSatoshis
                               InitFeeRatePerKw = TestConstants.feeratePerKw
                               FundingTxFeeRatePerKw = TestConstants.feeratePerKw
                               LocalParams =
                                   alice.PM.MakeLocalParams(channelKeys.ToChannelPubKeys() ,defaultFinalScriptPubKey, true, TestConstants.fundingSatoshis)
                               RemoteInit = if (bobInit.IsSome) then bobInit.Value else failwith "alice did not receive init from bob"
                               ChannelFlags = 0x00uy
                               ChannelKeys = channelKeys }
            let aliceChannelEventFuture =
                alice.EventAggregator.AwaitChannelEvent()
            let bobStartedNewInboundChannelTask =
                bob.EventAggregator.AwaitChannelEvent(function NewInboundChannelStarted msg -> Some msg | _ -> None)
            let bobAcceptedOpenChannelTask =
                bob.EventAggregator.AwaitChannelEvent(function WeAcceptedOpenChannel(acceptChannel, _state) -> Some acceptChannel | _ -> None)
                
            let aliceReceivedAcceptChannelTask =
                alice.EventAggregator.AwaitChannelEvent(function WeAcceptedAcceptChannel(i, _) -> Some i | _ -> None)
            
            let bobAcceptedFundingCreatedTask =
                bob.EventAggregator.AwaitChannelEvent(function WeAcceptedFundingCreated(i, _) -> Some i | _ -> None)
                
            let aliceAcceptedFundingSignedTask =
                alice.EventAggregator.AwaitChannelEvent(function WeAcceptedFundingSigned(i, _) -> Some i | _ -> None)
            
            let bobNodeId = bob.CM.KeysRepository.GetNodeSecret().PubKey |> NodeId
            do! alice.CM.AcceptCommandAsync(bobNodeId, ChannelCommand.CreateOutbound(initFunder)) |> Async.AwaitTask
            
            let! r = aliceChannelEventFuture
            Expect.isSome r "timeout"
            let! r = bobStartedNewInboundChannelTask
            Expect.isSome (r) "timeout"
            let! r = bobAcceptedOpenChannelTask
            Expect.isSome (r) "timeout"
            
            let! r = aliceReceivedAcceptChannelTask
            Expect.isSome r "timeout"
            
            let! r = bobAcceptedFundingCreatedTask
            Expect.isSome r "timeout"
            
            let! r = aliceAcceptedFundingSignedTask
            Expect.isSome r "timeout"
            
            return ()
        }
    ]

