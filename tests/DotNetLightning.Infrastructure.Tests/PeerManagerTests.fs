module Tests

open Expecto
open Moq

open DotNetLightning.Chain
open DotNetLightning.Infrastructure
open CustomEventAggregator

open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options

let channelManagerMock = new Mock<IChannelManager>()
let eventAggregatorMock = new Mock<IEventAggregator>()
let keysRepoMock = new Mock<IKeysRepository>()
let loggerMock = new Mock<ILogger<PeerManager>>()
let loggerInternal = Logger.fromMicrosoftLogger (loggerMock.Object)
let alice = Constants.getAliceParam()
let nodeParams = Options.Create<NodeParams>(alice.NodeParams)

[<Tests>]
let tests =
  testList "samples" [
    testCase "PeerManager Tests" <| fun _ ->
      let peerManager = PeerManager(keysRepoMock.Object,
                                    loggerMock.Object,
                                    nodeParams,
                                    eventAggregatorMock.Object,
                                    channelManagerMock.Object)
      failwith ""
  ]