namespace DotNetLightning.Infrastructure.Tests

open DotNetLightning.Chain
open DotNetLightning.Infrastructure
open DotNetLightning.Utils
open DotNetLightning.LN
open DotNetLightning.Infrastructure
open CustomEventAggregator

open FSharp.Control.Tasks
open System
open System.IO.Pipelines
open System.Net
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options

open DotNetLightning.Utils.Aether
open System.Collections
open Moq
open Xunit
open NBitcoin

module PeerManagerTests =
    let hex = NBitcoin.DataEncoders.HexEncoder()

    let channelManagerMock = new Mock<IChannelManager>()
    let eventAggregatorMock = new Mock<IEventAggregator>()
    let keysRepoMock = new Mock<IKeysRepository>()
    
    let ourNodeSecret = Key(hex.DecodeData("1111111111111111111111111111111111111111111111111111111111111111"))
    ignore <| keysRepoMock.Setup(fun repo -> repo.GetNodeSecret()).Returns(ourNodeSecret)
    let loggerMock = new Mock<ILogger<PeerManager>>()
    let loggerInternal = Logger.fromMicrosoftLogger (loggerMock.Object)
    let alice = Constants.getAliceParam()
    let nodeParams = Options.Create<NodeParams>(alice.NodeParams)

    let createPipe() =
        let pipe = System.IO.Pipelines.Pipe()
        { new IDuplexPipe with
            member x.Output = pipe.Writer;
            member x.Input = pipe.Reader }
        
    let ie = "1212121212121212121212121212121212121212121212121212121212121212" |> hex.DecodeData |> Key
    let updateIEForTestVector(peerMan: PeerManager, peerId: PeerId) =
        match peerMan.KnownPeers.TryGetValue peerId with
        | false, _ -> failwith ""
        | true, p ->
            let newPeer = { p with ChannelEncryptor = (Optic.set PeerChannelEncryptor.OutBoundIE_ ie p.ChannelEncryptor) }
            peerMan.KnownPeers.[peerId] <- newPeer

    [<Fact>]
    let ``PeerManagerTest`` () = task {
      let dPipe = createPipe()
      let ourNodeId = Key(hex.DecodeData("1111111111111111111111111111111111111111111111111111111111111111"))
      let theirPeerId = IPEndPoint.Parse("127.0.0.2") :> EndPoint |> PeerId
      let theirNodeId = PubKey("028d7500dd4c12685d1f568b4c2b5048e8534b873319f3a8daa612b469132ec7f7") |> NodeId
      let peerManager = PeerManager(keysRepoMock.Object,
                                    loggerMock.Object,
                                    nodeParams,
                                    eventAggregatorMock.Object,
                                    channelManagerMock.Object)
      let! read = peerManager.NewOutBoundConnection(theirNodeId, theirPeerId, dPipe.Output, ie)
      let _ = read |> function Ok b -> b | Result.Error e -> failwithf "%A" e
      updateIEForTestVector(peerManager, theirPeerId)
      let actOneExpected = "0x00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a"
      let! actOneActual = dPipe.Input.ReadExactAsync(50, true)
      Assert.Equal(actOneExpected, actOneActual.ToHexString())
      
      // other peer responds with act two
      let actTwo = hex.DecodeData("0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae")
      let! _ = dPipe.Output.WriteAsync(actTwo)
      
      // complete handshake by processing act two
      let! _ = peerManager.ReadAsync(theirPeerId, dPipe)
      
      Assert.True(peerManager.OpenedPeers.ContainsKey(theirPeerId))
      let actualPeerId = (peerManager.NodeIdToDescriptor.TryGetValue(theirNodeId) |> snd)
      Assert.Equal<_>(actualPeerId, theirPeerId)
      let p = match peerManager.OpenedPeers.TryGetValue(theirPeerId) with true, p -> p | false,_ -> failwith ""
      match p.ChannelEncryptor.NoiseState with
      | Finished {SK = sk; SN=sn; SCK = sck; RK = rk; RN = rn; RCK = rck } ->
          Assert.Equal(sk.ToBytes().ToHexString(), "0x969ab31b4d288cedf6218839b27a3e2140827047f2c0f01bf5c04435d43511a9")
          Assert.Equal(sn, 0UL)
          Assert.Equal(sck.ToBytes().ToHexString(), "0x919219dbb2920afa8db80f9a51787a840bcf111ed8d588caf9ab4be716e42b01")
          Assert.Equal(rk.ToBytes().ToHexString(), "0xbb9020b8965f4df047e07f955f3c4b88418984aadc5cdb35096b9ea8fa5c3442")
          Assert.Equal(rn, 0UL)
          Assert.Equal(rck.ToBytes().ToHexString(), "0x919219dbb2920afa8db80f9a51787a840bcf111ed8d588caf9ab4be716e42b01")
      | s -> failwithf "not in finished state %A" s
    }
