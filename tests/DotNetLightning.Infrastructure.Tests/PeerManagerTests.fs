module PeerManagerTests

open DotNetLightning.Serialization.Msgs
open DotNetLightning.Chain
open DotNetLightning.Infrastructure
open DotNetLightning.Utils
open DotNetLightning.Peer
open CustomEventAggregator

open System.IO.Pipelines
open System.Net
open Microsoft.Extensions.Options

open DotNetLightning.Infrastructure.ActorManagers
open DotNetLightning.Infrastructure.Interfaces
open System
open System.Threading.Tasks
open DotNetLightning.Utils.Aether
open Foq
open NBitcoin
open Expecto
open Expecto.Logging
open Expecto.Logging.Message

let hex = NBitcoin.DataEncoders.HexEncoder()

let logger = Log.create "PeerManager tests"
let log (_logLevel) =
    let logCore = eventX >> logger.info
    logCore
    //fun s -> () //printfn "%s"

let eventAggregatorMock = ReactiveEventAggregator()

let ourNodeSecret = Key(hex.DecodeData("1111111111111111111111111111111111111111111111111111111111111111"))
let keysRepoMock = Mock<IKeysRepository>.Method(fun repo -> <@ repo.GetNodeSecret @>).Returns(ourNodeSecret)

let broadCasterMock = Mock<IBroadCaster>().Create()

let chainWatcherMock = Mock<IChainWatcher>().Create()

let aliceNodeParams = Options.Create<ChainConfig>(TestConstants.getAliceParam().NodeParams)
let bobNodeParams = Options.Create<ChainConfig>(TestConstants.getBobParam().NodeParams)


let createPipe() =
    let pipe = System.IO.Pipelines.Pipe()
    { new IDuplexPipe with
        member x.Output = pipe.Writer;
        member x.Input = pipe.Reader }

let ie = "1212121212121212121212121212121212121212121212121212121212121212" |> hex.DecodeData |> Key
let updateIEForTestVector (peerMan: PeerManager, peerId: PeerId) =
    match peerMan.KnownPeers.TryGetValue peerId with
    | false, _ -> failwith "Fail: peerMan.KnownPeers.[peerId] not found"
    | true, actor ->
        actor.State <- { actor.State with ChannelEncryptor = (Optic.set PeerChannelEncryptor.OutBoundIE_ ie actor.State.ChannelEncryptor) }
        peerMan.KnownPeers.[peerId] <- actor

[<Tests>]
let tests = testList "PeerManagerTests" [
    
    testTask "PeerManager can initiate outbound connection just from their nodeid and network address" {
      let dPipe = createPipe()
      let theirPeerId = IPEndPoint.Parse("127.0.0.2") :> EndPoint |> PeerId
      let theirNodeId = PubKey("028d7500dd4c12685d1f568b4c2b5048e8534b873319f3a8daa612b469132ec7f7") |> NodeId
      let peerManager = PeerManager (eventAggregatorMock,
                                    TestLogger.create(ConsoleColor.White),
                                    getTestLoggerFactory(),
                                    keysRepoMock,
                                    aliceNodeParams.Value,
                                    chainWatcherMock,
                                    broadCasterMock,
                                    DotNetLightningNetworkProvider.getNetwork (NetworkType.Mainnet) ("BTC"))
      do! peerManager.NewOutBoundConnection(theirNodeId, theirPeerId, dPipe.Output, ie).AsTask()
      updateIEForTestVector (peerManager, theirPeerId)
      let actOneExpected = "0x00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a"
      let! actOneActual = dPipe.Input.ReadExactAsync(50, true).AsTask()
      let actOneActual: byte[] = actOneActual
      Expect.equal actOneExpected (actOneActual.ToHexString()) ""

      // other peer responds with act two
      let actTwo = hex.DecodeData ("0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae")
      let! _ = dPipe.Output.WriteAsync(actTwo).AsTask()

      // complete handshake by processing act two
      let! _ = peerManager.ReadAsync(theirPeerId, dPipe).AsTask()
      
      do! Task.Delay 200

      let actualPeerId = (peerManager.NodeIdToPeerId.TryGetValue(theirNodeId) |> snd)
      Expect.equal (actualPeerId) (theirPeerId) ""
      return ()
    }

    testTask "PeerManager can initiate inbound connection" {
      let dPipe = createPipe()
      let theirPeerId = IPEndPoint.Parse("127.0.0.2") :> EndPoint |> PeerId
      let keyRepoMock =
          let ourNodeSecret = hex.DecodeData ("2121212121212121212121212121212121212121212121212121212121212121") |> Key
          Mock<IKeysRepository>.Method(fun repo -> <@ repo.GetNodeSecret @>).Returns(ourNodeSecret)
      let peerManager = PeerManager (eventAggregatorMock,
                                     TestLogger.create(ConsoleColor.White),
                                     getTestLoggerFactory(),
                                     keyRepoMock,
                                     aliceNodeParams.Value,
                                     chainWatcherMock,
                                     broadCasterMock,
                                     DotNetLightningNetworkProvider.getNetwork (NetworkType.Mainnet) ("BTC"))

      // processing act 1 ...
      let act1 = hex.DecodeData ("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
      let ourEphemeral = hex.DecodeData("2222222222222222222222222222222222222222222222222222222222222222") |> Key
      do! dPipe.Output.WriteAsync(act1).AsTask()
      do! peerManager.ReadAsync(theirPeerId, dPipe, ourEphemeral).AsTask()

      // should send act 2 to the peer
      let! act2 = dPipe.Input.ReadExactAsync(50, true).AsTask()
      let act2: byte[] = act2
      let actTwoExpected = "0x0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae"
      Expect.equal (act2.ToHexString()) (actTwoExpected) ""

      // if the peer responds with correct act 3 ...
      let actThree = hex.DecodeData ("00b9e3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa22355361aa02e55a8fc28fef5bd6d71ad0c38228dc68b1c466263b47fdf31e560e139ba")
      do! dPipe.Output.WriteAsync(actThree).AsTask()
      do! peerManager.ReadAsync(theirPeerId, dPipe).AsTask()

      // their node id should be the one expected
      let theirNodeIdExpected =
          "034f355bdcb7cc0af728ef3cceb9615d90684bb5b2ca5f859ab0f0b704075871aa"
          |> hex.DecodeData |> PubKey |> NodeId
          
      do! Task.Delay 100

      // also, we can query the peer id with node id
      let peerIdRetrieved =
          match peerManager.NodeIdToPeerId.TryGetValue theirNodeIdExpected with
          | true, p -> p
          | false, _ -> failwith "peer id is not set to NodeId -> PeerId Dict"
      Expect.equal (peerIdRetrieved) (theirPeerId) ""

    }

    testTask "2 peers can communicate with each other" {
      let aliceEventAggregator = ReactiveEventAggregator()
      let bobEventAggregator = ReactiveEventAggregator()
      let alice = { PM = PeerManager(aliceEventAggregator,
                                     TestLogger.create(ConsoleColor.Red),
                                     getTestLoggerFactory(),
                                     keysRepoMock,
                                     aliceNodeParams.Value,
                                     chainWatcherMock,
                                     broadCasterMock,
                                     DotNetLightningNetworkProvider.getNetwork (NetworkType.Mainnet) ("BTC"))
                    CM = Mock<IChannelManager>().Create()
                    Id = IPEndPoint.Parse("127.0.0.3") :> EndPoint |> PeerId
                    EventAggregator = aliceEventAggregator
                    NodeParams = aliceNodeParams.Value
                    CurrentHeight = 0
                    FundingTxProvider = Mock<IFundingTxProvider>().Create()
                    }
      let bobNodeSecret =
        Key(hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202"))
      let keyRepoBob = 
        Mock<IKeysRepository>.Method(fun repo -> <@ repo.GetNodeSecret @>).Returns(bobNodeSecret)
        
      let bob = { PM = PeerManager(bobEventAggregator,
                                   TestLogger.create(ConsoleColor.Blue),
                                   getTestLoggerFactory(),
                                   keyRepoBob,
                                   ChainConfig(),
                                   chainWatcherMock,
                                   broadCasterMock,
                                   DotNetLightningNetworkProvider.getNetwork (NetworkType.Mainnet) ("BTC"))
                  CM = Mock<IChannelManager>().Create()
                  Id = IPEndPoint.Parse("127.0.0.2") :> EndPoint |> PeerId
                  EventAggregator = bobEventAggregator
                  NodeParams = aliceNodeParams.Value
                  CurrentHeight = 0
                  FundingTxProvider = Mock<IFundingTxProvider>().Create()
                  }
      let actors = new PeerActors(alice, bob)
      
      // this should trigger All handshake process
      let bobNodeId = bobNodeSecret.PubKey |> NodeId
      let aliceFinishedActTwoTask =
          alice.EventAggregator.AwaitPeerEvent(function | { PeerEvent = PeerEvent.ActTwoProcessed _ } -> Some() | _ -> None)
      let bobFinishedActThreeTask =
          bob.EventAggregator.AwaitPeerEvent(function | { PeerEvent = PeerEvent.ActThreeProcessed _ } -> Some() | _ -> None)
          
      let aliceReceivedInitTask =
          alice.EventAggregator.AwaitPeerEvent(function | { PeerEvent = ReceivedInit(i, _) } -> Some i | _ -> None)
      let bobReceivedInitTask =
          bob.EventAggregator.AwaitPeerEvent(function | { PeerEvent = ReceivedInit(i, _) } -> Some i | _ -> None)
          
          
      do! actors.Launch(bobNodeId)
      
      let! t = aliceFinishedActTwoTask |> Async.StartAsTask
      Expect.isSome t ""
      let! t = bobFinishedActThreeTask |> Async.StartAsTask
      Expect.isSome t ""
      
      let! t = aliceReceivedInitTask |> Async.StartAsTask
      Expect.isSome t "alice did not receive init"
      let! t = bobReceivedInitTask |> Async.StartAsTask
      Expect.isSome t "bob did not receive init"
      
      // --- ping from initiator ---
      let bobReceivedPingTask =
          bob.EventAggregator.AwaitPeerEvent(function | { PeerEvent = ReceivedPing _ } -> Some() | _ -> None)
      let aliceReceivedPongTask =
          alice.EventAggregator.AwaitPeerEvent(function | { PeerEvent = ReceivedPong _ } -> Some() | _ -> None)
      do! actors.Initiator.PM.AcceptCommand({ PeerCommand = PeerCommand.EncodeMsg({ PingMsg.BytesLen = 22us;
                                                                                    PongLen = 32us })
                                              PeerId = bob.Id }).AsTask()
      let! t = bobReceivedPingTask |> Async.StartAsTask
      Expect.isSome t ""
      let! t = aliceReceivedPongTask |> Async.StartAsTask
      Expect.isSome t ""
      

      // --- ping from responder ---
      let aliceReceivedPingTask =
          alice.EventAggregator.AwaitPeerEvent(function | { PeerEvent = ReceivedPing _ } -> Some() | _ -> None)
      let bobReceivedPongTask =
          bob.EventAggregator.AwaitPeerEvent(function | { PeerEvent = ReceivedPong _ } -> Some() | _ -> None)
      do! actors.Responder.PM.AcceptCommand({ PeerCommand = PeerCommand.EncodeMsg({ PingMsg.BytesLen = 22us;
                                                                                    PongLen = 32us })
                                              PeerId = alice.Id }).AsTask()
      let! t = aliceReceivedPingTask |> Async.StartAsTask
      Expect.isSome t ""
      let! t = bobReceivedPongTask |> Async.StartAsTask
      Expect.isSome t ""
      
      return ()
    }
  ]
