module PeerChannelEncryptorTests
open Expecto
open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.LN
open DotNetLightning.Serialize

let hex = NBitcoin.DataEncoders.HexEncoder()

[<Tests>]
let tests =
    testList "PeerChannelEncryptorTests" [
        ptestCase "get outbound peer for initiator test vectors" <| fun _ ->
            let theirNodeId = PubKey("028d7500dd4c12685d1f568b4c2b5048e8534b873319f3a8daa612b469132ec7f7")

            let mutable outboundPeer = PeerChannelEncryptor.NewOutbound(NodeId theirNodeId)
            let newState =
                match outboundPeer.NoiseState with
                | InProgress ns ->
                    let newOS = OutBound { IE = Key(hex.DecodeData("1212121212121212121212121212121212121212121212121212121212121212")) }
                    InProgress { ns with DirectionalState = newOS }
                | _ -> failwith ""

            outboundPeer.NoiseState <- newState

            let actual = outboundPeer.GetActOne()
            let expected = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
            Expect.equal actual expected ""
    ]