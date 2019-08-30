module Tests

open DotNetLightning.Chain
open DotNetLightning.Infrastructure
open DotNetLightning.Utils
open DotNetLightning.LN
open CustomEventAggregator

open Expecto
open Moq

open NBitcoin

open System.Net
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options

let hex = NBitcoin.DataEncoders.HexEncoder()

let channelManagerMock = new Mock<IChannelManager>()
let eventAggregatorMock = new Mock<IEventAggregator>()
let keysRepoMock = new Mock<IKeysRepository>()
let loggerMock = new Mock<ILogger<PeerManager>>()
let loggerInternal = Logger.fromMicrosoftLogger (loggerMock.Object)
let alice = Constants.getAliceParam()
let nodeParams = Options.Create<NodeParams>(alice.NodeParams)

[<Tests>]
let tests =
  testList "PeerManager Handshake Tests" [
    testCase "Noise Initiator" <| fun _ ->
      let ourNodeId = Key(hex.DecodeData("1111111111111111111111111111111111111111111111111111111111111111"))
      let theirPeerId = IPEndPoint.Parse("127.0.0.2") :> EndPoint |> PeerId
      let theirNodeId = PubKey("028d7500dd4c12685d1f568b4c2b5048e8534b873319f3a8daa612b469132ec7f7") |> NodeId
      let peerManager = PeerManager(keysRepoMock.Object,
                                    loggerMock.Object,
                                    nodeParams,
                                    eventAggregatorMock.Object,
                                    channelManagerMock.Object)
      let peer = peerManager.NewOutBoundConnection(theirPeerId, )
      peerManager.ReadAsync()
      eventAggregatorMock.Verify(fun e -> e.Publish(), Times.Exactly(1))
      failwith ""
  ]