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


let dummyBlockChainInstanceId = 
    "8888888888888888888888888888888888888888888888888888888888888888"
    |> BlockChainInstanceId
let n = Network.RegTest
type internal ActorCreator =
    static member hex = NBitcoin.DataEncoders.HexEncoder()
    
    
    static member getAlice(?keyRepo: IKeysRepository, ?nodeParams) =
        let aliceParam = TestConstants.getAliceParam()
        let keyRepo = defaultArg keyRepo (aliceParam.KeyRepo)
        let peerLogger = TestLogger.create<PeerManager>(ConsoleColor.Red)
        let channelLogger = TestLogger.create<ChannelManager>(ConsoleColor.Magenta)
        let nodeParams = defaultArg nodeParams (Options.Create<NodeParams>(aliceParam.NodeParams))
        let chainWatcher =
            Mock<IChainWatcher>()
                .Setup(fun x -> <@ x.InstallWatchTx(any(), any()) @>).Returns(true)
                .Setup(fun x -> <@ x.InstallWatchOutPoint(any(), any()) @>).Returns(true)
                .Setup(fun x -> <@ x.WatchAllTxn() @>).Returns(true)
                .Create()
        let broadCaster = DummyBroadCaster()
        let eventAggregator = ReactiveEventAggregator() :> IEventAggregator
        let fundingTxProvider = DummyFundingTxProvider(aliceParam.NodeParams.ChainNetwork)
        {
            NodeParams = aliceParam.NodeParams
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
            CurrentHeight = 100
            FundingTxProvider = fundingTxProvider
        }
    
    static member getBob(?keyRepo: IKeysRepository, ?nodeParams) =
        let bobParam = TestConstants.getBobParam()
        let keyRepo = defaultArg keyRepo (bobParam.KeyRepo)
        let peerLogger =
            // Mock<ILogger<PeerManager>>().Create()
            TestLogger.create<PeerManager>(ConsoleColor.Blue)
        let channelLogger =
            // Mock<ILogger<ChannelManager>>().Create()
            TestLogger.create<ChannelManager>(ConsoleColor.Green)
        let nodeParams = defaultArg nodeParams (Options.Create<NodeParams>(bobParam.NodeParams))
        let chainWatcher =
            Mock<IChainWatcher>()
                .Setup(fun x -> <@ x.InstallWatchTx(any(), any()) @>).Returns(true)
                .Setup(fun x -> <@ x.InstallWatchOutPoint(any(), any()) @>).Returns(true)
                .Setup(fun x -> <@ x.WatchAllTxn() @>).Returns(true)
                .Create()
        let broadCaster = DummyBroadCaster()
        let eventAggregator = ReactiveEventAggregator() :> IEventAggregator
        let fundingTxProvider = DummyFundingTxProvider(bobParam.NodeParams.ChainNetwork)
        {
            NodeParams = bobParam.NodeParams
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
            CurrentHeight = 100
            FundingTxProvider = fundingTxProvider
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
               
            let aliceFundingConfirmedTask =
                alice.EventAggregator.AwaitChannelEvent(function FundingConfirmed _ -> Some () | _ -> None)
                
            let aliceSentFundingLockedTask =
                alice.EventAggregator.AwaitChannelEvent(function WeSentFundingLocked _ -> Some () | _ -> None)
                
            let bobReceivedFundingLockedTask =
                bob.EventAggregator.AwaitChannelEvent(function TheySentFundingLocked _ -> Some () | _ -> None)
                
            let aliceBothFundingLockedTask =
                alice.EventAggregator.AwaitChannelEvent(function BothFundingLocked _ -> Some () | _ -> None)
            let bobBothFundingLockedTask =
                bob.EventAggregator.AwaitChannelEvent(function BothFundingLocked _ -> Some () | _ -> None)
            
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
            
            let fundingTx = (alice.FundingTxProvider :?> DummyFundingTxProvider).DummyTx
            
            // We need three confirmation before we consider funding is locked
            // (3 is default for NodeParams)
            alice.PublishDummyBlockWith([fundingTx])
            alice.PublishDummyBlockWith([])
            alice.PublishDummyBlockWith([])
            let! r = aliceFundingConfirmedTask
            Expect.isSome r "timeout"
            let! r = aliceSentFundingLockedTask
            Expect.isSome r "timeout"
            
            let! r = bobReceivedFundingLockedTask
            Expect.isSome r "timeout"
            
            bob.PublishDummyBlockWith([fundingTx])
            bob.PublishDummyBlockWith([])
            bob.PublishDummyBlockWith([])
            
            let! r = aliceBothFundingLockedTask
            Expect.isSome r "timeout"
            
            let! r = bobBothFundingLockedTask
            Expect.isSome r "timeout"
            
            return ()
        }
        
        testAsync "funding_confirmed should be sent only when it is really confirmed" {
            return ()
        }
    ]

