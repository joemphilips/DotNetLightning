module PeerManagerTests

open DotNetLightning.Serialize.Msgs
open DotNetLightning.Chain
open DotNetLightning.Infrastructure
open DotNetLightning.Utils
open DotNetLightning.LN
open CustomEventAggregator

open FSharp.Control.Tasks
open FSharp.Control.Reactive
open System.IO.Pipelines
open System.Net
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options

open System.Threading.Tasks
open DotNetLightning.Utils.Aether
open Foq
open NBitcoin
open Expecto
open Expecto.Logging
open Expecto.Logging.Message

let hex = NBitcoin.DataEncoders.HexEncoder()

let logger = Log.create "PeerManager tests"
let log (logLevel) =
    let logCore = eventX >> logger.info
    logCore
    //fun s -> () //printfn "%s"

let eventAggregatorMock = new ReactiveEventAggregator()

let ourNodeSecret = Key(hex.DecodeData("1111111111111111111111111111111111111111111111111111111111111111"))
let keysRepoMock = Mock<IKeysRepository>.Method(fun repo -> <@ repo.GetNodeSecret @>).Returns(ourNodeSecret)

let broadCasterMock = Mock<IBroadCaster>().Create()

let chainWatcherMock = Mock<IChainWatcher>().Create()

let aliceNodeParams = Options.Create<NodeParams>(TestConstants.getAliceParam().NodeParams)
let bobNodeParams = Options.Create<NodeParams>(TestConstants.getBobParam().NodeParams)


let createPipe() =
    let pipe = System.IO.Pipelines.Pipe()
    { new IDuplexPipe with
        member x.Output = pipe.Writer;
        member x.Input = pipe.Reader }

let ie = "1212121212121212121212121212121212121212121212121212121212121212" |> hex.DecodeData |> Key
let updateIEForTestVector (peerMan: PeerManager, peerId: PeerId) =
    match peerMan.KnownPeers.TryGetValue peerId with
    | false, _ -> failwith ""
    | true, p ->
        let newPeer = { p with ChannelEncryptor = (Optic.set PeerChannelEncryptor.OutBoundIE_ ie p.ChannelEncryptor) }
        peerMan.KnownPeers.[peerId] <- newPeer

[<Tests>]
let tests = testList "PeerManagerTests" [
    testCaseAsync "PeerManager can initiate outbound connection just from their nodeid and network address" <| (task {
      let dPipe = createPipe()
      let theirPeerId = IPEndPoint.Parse("127.0.0.2") :> EndPoint |> PeerId
      let theirNodeId = PubKey("028d7500dd4c12685d1f568b4c2b5048e8534b873319f3a8daa612b469132ec7f7") |> NodeId
      let peerManager = PeerManager (keysRepoMock,
                                    TestLogger.create(),
                                    aliceNodeParams,
                                    eventAggregatorMock,
                                    chainWatcherMock,
                                    broadCasterMock)
      let! read = peerManager.NewOutBoundConnection(theirNodeId, theirPeerId, dPipe.Output, ie)
      let _ = read |> function | Ok b -> b | Result.Error e -> failwithf "%A" e
      updateIEForTestVector (peerManager, theirPeerId)
      let actOneExpected = "0x00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a"
      let! actOneActual = dPipe.Input.ReadExactAsync(50, true)
      Expect.equal actOneExpected (actOneActual.ToHexString()) ""

      // other peer responds with act two
      let actTwo = hex.DecodeData ("0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae")
      let! _ = dPipe.Output.WriteAsync(actTwo)

      // complete handshake by processing act two
      let! _ = peerManager.ProcessMessageAsync(theirPeerId, dPipe)

      Expect.isTrue (peerManager.OpenedPeers.ContainsKey(theirPeerId)) ""
      let actualPeerId = (peerManager.NodeIdToPeerId.TryGetValue(theirNodeId) |> snd)
      Expect.equal (actualPeerId) (theirPeerId) ""
      let _ = match peerManager.OpenedPeers.TryGetValue(theirPeerId) with
              | true, p -> p
              | false, _ -> failwith "Peer not opened"
      return ()
    } |> Async.AwaitTask)

    testCaseAsync "PeerManager can initiate inbound connection" <| (task {
      let dPipe = createPipe()
      let theirPeerId = IPEndPoint.Parse("127.0.0.2") :> EndPoint |> PeerId
      let keyRepoMock =
          let ourNodeSecret = hex.DecodeData ("2121212121212121212121212121212121212121212121212121212121212121") |> Key
          Mock<IKeysRepository>.Method(fun repo -> <@ repo.GetNodeSecret @>).Returns(ourNodeSecret)
      let peerManager = PeerManager (keyRepoMock,
                                    TestLogger.create(),
                                    aliceNodeParams,
                                    eventAggregatorMock,
                                    chainWatcherMock,
                                    broadCasterMock)

      // processing act 1 ...
      let act1 = hex.DecodeData ("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
      let ourEphemeral = hex.DecodeData("2222222222222222222222222222222222222222222222222222222222222222") |> Key
      do! dPipe.Output.WriteAsync(act1)
      do! peerManager.ProcessMessageAsync (theirPeerId, dPipe, ourEphemeral)

      // should send act 2 to the peer
      let! act2 = dPipe.Input.ReadExactAsync (50, true)
      let actTwoExpected = "0x0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae"
      Expect.equal (act2.ToHexString()) (actTwoExpected) ""

      // if the peer responds with correct act 3 ...
      let actThree = hex.DecodeData ("00b9e3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa22355361aa02e55a8fc28fef5bd6d71ad0c38228dc68b1c466263b47fdf31e560e139ba")
      do! dPipe.Output.WriteAsync(actThree)
      do! peerManager.ProcessMessageAsync (theirPeerId, dPipe)

      // their node id should be the one expected
      let theirNodeIdExpected =
          "034f355bdcb7cc0af728ef3cceb9615d90684bb5b2ca5f859ab0f0b704075871aa"
          |> hex.DecodeData |> PubKey |> NodeId
      let theirNodeIdActual =
          match peerManager.OpenedPeers.TryGetValue theirPeerId with
          | true, p -> p.TheirNodeId.Value
          | false, _ -> failwith "peer is not set to the peer manager's opened peer dict"
      Expect.equal (theirNodeIdExpected) (theirNodeIdActual) ""

      // also, we can query the peer id with node id
      let peerIdRetrieved =
          match peerManager.NodeIdToPeerId.TryGetValue theirNodeIdActual with
          | true, p -> p
          | false, _ -> failwith "peer id is not set to NodeId -> PeerId Dict"
      Expect.equal (peerIdRetrieved) (theirPeerId) ""

    } |> Async.AwaitTask)

    testCaseAsync "2 peers can communicate with each other" <| (task {
      let aliceEventAggregatorMock =
          Mock<IEventAggregator>.Method(fun ea -> <@ ea.GetObservable<ChannelEventWithContext> @> )
              .Returns(Observable.empty)
      let bobEventAggregatorMock =
          Mock<IEventAggregator>.Method(fun ea -> <@ ea.GetObservable<ChannelEventWithContext> @> )
              .Returns(Observable.empty)
      let alice = { PM = PeerManager(keysRepoMock,
                                     TestLogger.create(),
                                     aliceNodeParams,
                                     aliceEventAggregatorMock,
                                     chainWatcherMock,
                                     broadCasterMock)
                    CM = Mock<IChannelManager>().Create()
                    Id = IPEndPoint.Parse("127.0.0.3") :> EndPoint |> PeerId
                    EventAggregator = aliceEventAggregatorMock }
      let bobNodeSecret =
        Key(hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202"))
      let keyRepoBob = 
        Mock<IKeysRepository>.Method(fun repo -> <@ repo.GetNodeSecret @>).Returns(bobNodeSecret)
        
      let bob = { PM = PeerManager(keyRepoBob,
                                   TestLogger.create(),
                                   Options.Create<NodeParams>(new NodeParams()),
                                   bobEventAggregatorMock,
                                   chainWatcherMock,
                                   broadCasterMock)
                  CM = Mock<IChannelManager>().Create()
                  Id = IPEndPoint.Parse("127.0.0.2") :> EndPoint |> PeerId
                  EventAggregator = bobEventAggregatorMock }
      let actors = new PeerActors(alice, bob)
      
      // this should trigger All handshake process
      let bobNodeId = bobNodeSecret.PubKey |> NodeId
      do! actors.Launch(bobNodeId)
      do! Task.Delay 1000
      
      let _ =
          match actors.Initiator.PM.OpenedPeers.TryGetValue (actors.Responder.Id) with
          | true, p ->
            Expect.equal (p.ChannelEncryptor.GetNoiseStep()) (NextNoiseStep.NoiseComplete) "Noise State should be completed"
          | false, _ -> failwith "bob is not in alice's OpenedPeer"
          match actors.Responder.PM.OpenedPeers.TryGetValue (actors.Initiator.Id) with
          | true, p ->
            Expect.equal (p.ChannelEncryptor.GetNoiseStep()) (NextNoiseStep.NoiseComplete) "Noise State should be completed"
          | false, _ -> failwith "alice is not in bob's OpenedPeer"
          
      Mock.Verify(<@ aliceEventAggregatorMock.Publish<PeerEvent>(It.Is(function | PeerEvent.Connected _ -> true | _ -> false)) @>, once)
      Mock.Verify(<@ bobEventAggregatorMock.Publish<PeerEvent>(It.Is(function | PeerEvent.Connected _ -> true | _ -> false)) @>, once)
      
      // --- ping from initiator ---
      do! actors.Initiator.PM.AcceptCommand(PeerCommand.SendPing(actors.Responder.Id, { Ping.BytesLen = 22us;
                                                                                        PongLen = 32us }))
      do! Task.Delay 200
      Mock.Verify(<@ bobEventAggregatorMock.Publish<PeerEvent>(It.Is(function | ReceivedPing _ -> true | _ -> false)) @>, once)
      Mock.Verify(<@ aliceEventAggregatorMock.Publish<PeerEvent>(It.Is(function | ReceivedPong _ -> true | _ -> false)) @>, once)

      // --- ping from responder ---
      do! actors.Responder.PM.AcceptCommand(PeerCommand.SendPing(actors.Initiator.Id, { Ping.BytesLen = 22us;
                                                                                        PongLen = 32us }))
      do! Task.Delay 200
      Mock.Verify(<@ aliceEventAggregatorMock.Publish<PeerEvent>(It.Is(function | ReceivedPing _ -> true | _ -> false)) @>, once)
      Mock.Verify(<@ bobEventAggregatorMock.Publish<PeerEvent>(It.Is(function | ReceivedPong _ -> true | _ -> false)) @>, once)
      
      // --- ping from initiator again ---
      do! actors.Initiator.PM.AcceptCommand(PeerCommand.SendPing(actors.Responder.Id, { Ping.BytesLen = 22us;
                                                                                        PongLen = 32us }))
      do! Task.Delay 200
      Mock.Verify(<@ bobEventAggregatorMock.Publish<PeerEvent>(It.Is(function | ReceivedPing _ -> true | _ -> false)) @>, exactly(2))
      Mock.Verify(<@ aliceEventAggregatorMock.Publish<PeerEvent>(It.Is(function | ReceivedPong _ -> true | _ -> false)) @>, exactly(2))

      Mock.Verify(<@ bobEventAggregatorMock.Publish<PeerEvent>(It.Is(function | ReceivedError _ -> true | _ -> false)) @>, never)
      return ()
    } |> Async.AwaitTask)
  ]
