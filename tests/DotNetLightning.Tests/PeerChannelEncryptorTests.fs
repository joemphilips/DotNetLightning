module PeerChannelEncryptorTests
open Expecto
open NBitcoin
open DotNetLightning.Utils.Aether
open DotNetLightning.Utils.Aether.Operators
open DotNetLightning.Utils
open DotNetLightning.LN

let hex = NBitcoin.DataEncoders.HexEncoder()
let getOutBoundPeerForInitiatorTestVectors () =
    let theirNodeId = PubKey("028d7500dd4c12685d1f568b4c2b5048e8534b873319f3a8daa612b469132ec7f7")

    let outboundPeer =
        let ie = (Key(hex.DecodeData("1212121212121212121212121212121212121212121212121212121212121212")))
        PeerChannelEncryptor.newOutBound(NodeId theirNodeId)
        |> Optic.set PeerChannelEncryptor.OutBoundIE_ ie
        |> fun c ->
                Expect.equal (Optic.get (PeerChannelEncryptor.OutBoundIE_) c) (Some(ie)) ""
                c

    let actual, result = outboundPeer |> PeerChannelEncryptor.getActOne
    let expected = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
    Expect.equal actual expected ""
    result

[<Tests>]
let tests =
    testList "PeerChannelEncryptorTests" [
        ftestCase "noise initiator test vectors" <| fun _ ->
            let ourNodeId = Key(hex.DecodeData("1111111111111111111111111111111111111111111111111111111111111111"))
            let outboundPeer = getOutBoundPeerForInitiatorTestVectors()
            let actTwo = hex.DecodeData("0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae")
            let res = outboundPeer |> PeerChannelEncryptor.processActTwo (actTwo, ourNodeId) |> RResult.rtoResult
            Expect.isOk (res) ""
    ]