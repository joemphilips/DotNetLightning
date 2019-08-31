module PeerManagerTests

open Expecto

open DotNetLightning.Chain
open DotNetLightning.Infrastructure
open DotNetLightning.Utils
open DotNetLightning.LN
open DotNetLightning.Infrastructure
open CustomEventAggregator

open FSharp.Control.Tasks
open System.IO.Pipelines
open System.Net
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options

open System
open System.Threading.Tasks
open DotNetLightning.Utils.Aether
open Moq
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

let channelManagerMock = new Mock<IChannelManager>()
let eventAggregatorMock = new Mock<IEventAggregator>()
let keysRepoMock = new Mock<IKeysRepository>()

let ourNodeSecret = Key(hex.DecodeData("1111111111111111111111111111111111111111111111111111111111111111"))
ignore <| keysRepoMock.Setup(fun repo -> repo.GetNodeSecret()).Returns(ourNodeSecret)

let loggerMock = new Mock<ILogger<PeerManager>>()
let loggerInternal = Logger.fromMicrosoftLogger (loggerMock.Object)
let aliceNodeParams = Options.Create<NodeParams>(TestConstants.getAliceParam().NodeParams)
let bobNodeParams = Options.Create<NodeParams>(TestConstants.getBobParam().NodeParams)

type private PeerManagerEntity = {
    PM: PeerManager
    Id: PeerId
}

type private PeerActors(a, b, pipe) =
    member val Initiator: PeerManagerEntity = a
    member val Responder: PeerManagerEntity = b
    member val Transport: IDuplexPipe = pipe with get
    member val InitiatorTask = null with get, set
    member val ResponderTask = null with get, set
    member this.Launch(nodeIdForResponder: NodeId) = task {
            let! r = this.Initiator.PM.NewOutBoundConnection(nodeIdForResponder, this.Responder.Id, this.Transport.Output)
            match r with Ok r -> () | Result.Error e -> failwithf "%A" e
            this.InitiatorTask <- task {
                while true do
                    do! this.Initiator.PM.ProcessMessageAsync(this.Responder.Id, this.Transport)
            }
            this.ResponderTask <- task {
                while true do
                    do! this.Responder.PM.ProcessMessageAsync(this.Initiator.Id, this.Transport)
            }
        }
    interface IDisposable with
        member this.Dispose() =
            ()
            

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
      let peerManager = PeerManager (keysRepoMock.Object,
                                    loggerMock.Object,
                                    aliceNodeParams,
                                    eventAggregatorMock.Object,
                                    channelManagerMock.Object)
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
      let actualPeerId = (peerManager.NodeIdToDescriptor.TryGetValue(theirNodeId) |> snd)
      Expect.equal (actualPeerId) (theirPeerId) ""
      let p = match peerManager.OpenedPeers.TryGetValue(theirPeerId) with
              | true, p -> p
              | false, _ -> failwith "Peer not opened"
      match p.ChannelEncryptor.NoiseState with
      | Finished { SK = sk; SN = sn; SCK = sck; RK = rk; RN = rn; RCK = rck } ->
          Expect.equal (sk.ToBytes().ToHexString()) ("0x969ab31b4d288cedf6218839b27a3e2140827047f2c0f01bf5c04435d43511a9") ""
          Expect.equal (sn) (0UL) ""
          Expect.equal (sck.ToBytes().ToHexString()) ("0x919219dbb2920afa8db80f9a51787a840bcf111ed8d588caf9ab4be716e42b01") ""
          Expect.equal (rk.ToBytes().ToHexString()) ("0xbb9020b8965f4df047e07f955f3c4b88418984aadc5cdb35096b9ea8fa5c3442") ""
          Expect.equal (rn) (0UL) ""
          Expect.equal (rck.ToBytes().ToHexString()) ("0x919219dbb2920afa8db80f9a51787a840bcf111ed8d588caf9ab4be716e42b01") ""
      | s -> failwithf "not in finished state %A" s
    } |> Async.AwaitTask)

    testCaseAsync "PeerManager can initiate inbound connection" <| (task {
      let dPipe = createPipe()
      let theirPeerId = IPEndPoint.Parse("127.0.0.2") :> EndPoint |> PeerId
      let keyRepoMock =
          let ourNodeSecret = hex.DecodeData ("2121212121212121212121212121212121212121212121212121212121212121") |> Key
          new Mock<IKeysRepository>()
          |> fun m -> m.Setup(fun repo -> repo.GetNodeSecret()).Returns(ourNodeSecret) |> ignore; m
      let peerManager = PeerManager (keyRepoMock.Object,
                                    loggerMock.Object,
                                    aliceNodeParams,
                                    eventAggregatorMock.Object,
                                    channelManagerMock.Object)

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
          match peerManager.NodeIdToDescriptor.TryGetValue theirNodeIdActual with
          | true, p -> p
          | false, _ -> failwith "peer id is not set to NodeId -> PeerId Dict"
      Expect.equal (peerIdRetrieved) (theirPeerId) ""

    } |> Async.AwaitTask)

    ftestCaseAsync "2 peers can communicate with each other" <| (task {
      let alice = { PM = PeerManager(keysRepoMock.Object,
                               loggerMock.Object,
                               aliceNodeParams,
                               eventAggregatorMock.Object,
                               channelManagerMock.Object)
                    Id = IPEndPoint.Parse("127.0.0.3") :> EndPoint |> PeerId }
      let bobNodeSecret =
        Key(hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202"))
      let keyRepoBob = 
        new Mock<IKeysRepository>()
        |> fun m -> m.Setup(fun repo -> repo.GetNodeSecret()).Returns(bobNodeSecret) |> ignore; m
      let bob = { PM = PeerManager(keyRepoBob.Object,
                            loggerMock.Object,
                            Options.Create<NodeParams>(new NodeParams()),
                            eventAggregatorMock.Object,
                            channelManagerMock.Object)
                  Id = IPEndPoint.Parse("127.0.0.2") :> EndPoint |> PeerId }
      let dPipe = createPipe()
      let actors = new PeerActors(alice, bob, dPipe)
      
      // this should trigger All handshake process
      let bobNodeId = bobNodeSecret.PubKey |> NodeId
      do! actors.Launch(bobNodeId)
      do! Task.Delay 2000
      
      let _ =
          match actors.Initiator.PM.OpenedPeers.TryGetValue (actors.Responder.Id) with
          | true, _ -> ()
          | false, _ -> failwith "bob is not in alice's OpenedPeer"
          match actors.Responder.PM.OpenedPeers.TryGetValue (actors.Initiator.Id) with
          | true, _ -> ()
          | false, _ -> failwith "alice is not in bob's OpenedPeer"
          
      return ()
    } |> Async.AwaitTask)
  ]
