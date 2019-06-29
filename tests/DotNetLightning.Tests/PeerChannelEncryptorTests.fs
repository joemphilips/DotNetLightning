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
    ftestList "PeerChannelEncryptorTests" [
        testCase "noise initiator test vectors" <| fun _ ->
            let ourNodeId = Key(hex.DecodeData("1111111111111111111111111111111111111111111111111111111111111111"))

            let testCase1() =
                let outboundPeer = getOutBoundPeerForInitiatorTestVectors()
                let actTwo = hex.DecodeData("0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae")
                let res = outboundPeer |> PeerChannelEncryptor.processActTwo (actTwo, ourNodeId)
                Expect.isOk (res |> RResult.rtoResult) ""

                let (actual, nodeid), nextPCE = res |> RResult.rderef
                let expected = hex.DecodeData("00b9e3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa22355361aa02e55a8fc28fef5bd6d71ad0c38228dc68b1c466263b47fdf31e560e139ba")
                Expect.equal (actual) (expected) ""

                match nextPCE.NoiseState with
                | Finished {SK = sk; SN=sn; SCK = sck; RK = rk; RN = rn; RCK = rck } ->
                    Expect.equal (sk.ToBytes()) (hex.DecodeData("969ab31b4d288cedf6218839b27a3e2140827047f2c0f01bf5c04435d43511a9")) "sk does not match"
                    Expect.equal (sn) (0UL) "sn does not match"
                    Expect.equal (sck.ToBytes()) (hex.DecodeData("919219dbb2920afa8db80f9a51787a840bcf111ed8d588caf9ab4be716e42b01")) "sck does not match"
                    Expect.equal (rk.ToBytes()) (hex.DecodeData("bb9020b8965f4df047e07f955f3c4b88418984aadc5cdb35096b9ea8fa5c3442")) "rk does not match"
                    Expect.equal (rn) (0UL) "rn does not match"
                    Expect.equal (rck.ToBytes()) (hex.DecodeData("919219dbb2920afa8db80f9a51787a840bcf111ed8d588caf9ab4be716e42b01")) ""
                | s -> failwithf "not in finished state %A" s
        
            testCase1()

            /// Transport-initiator act3 short read test
            let testCase2 () =
                let outboundPeer = getOutBoundPeerForInitiatorTestVectors()
                let actTwo = hex.DecodeData("0x0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730")
                Expect.throws (fun _ -> outboundPeer |> PeerChannelEncryptor.processActTwo(actTwo, ourNodeId) |> ignore) ""

            /// Trnsport-initiator act2 bad version test
            let testCase3() =
                let outboundPeer = getOutBoundPeerForInitiatorTestVectors()
                let actTwo = hex.DecodeData("0102466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae")
                Expect.isError (outboundPeer |> PeerChannelEncryptor.processActTwo(actTwo, ourNodeId) |> RResult.rtoResult) ""

            testCase3()

            /// Transport-initiator act2 bad key serialization test
            let testCase4() =
                let outboundPeer = getOutBoundPeerForInitiatorTestVectors()
                let actTwo = hex.DecodeData("0004466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae")
                Expect.isError (outboundPeer |> PeerChannelEncryptor.processActTwo(actTwo, ourNodeId) |> RResult.rtoResult) ""
            testCase4()

            /// transport-initiator act2 bad MAC test
            let testCase5() =
                let outboundPeer = getOutBoundPeerForInitiatorTestVectors()
                let actTwo = hex.DecodeData("0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730af")
                Expect.isError(outboundPeer |> PeerChannelEncryptor.processActTwo(actTwo, ourNodeId) |> RResult.rtoResult) ""
            testCase5()

        testCase "noise responder text vectors" <| fun _ ->
            let ourNodeId = hex.DecodeData("2121212121212121212121212121212121212121212121212121212121212121") |> Key
            let ourEphemeral = hex.DecodeData("2222222222222222222222222222222222222222222222222222222222222222") |> Key

            /// Transport - responder successful handshake
            let testCase1() =
                let inboundPeer1 = ourNodeId |> PeerChannelEncryptor.newInBound
                let actOne = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
                let actOneExpected = hex.DecodeData("0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae")
                let actualRR  = inboundPeer1 |> PeerChannelEncryptor.processActOneWithEphemeralKey (actOne, ourNodeId, ourEphemeral)
                Expect.isOk (actualRR |> RResult.rtoResult) ""
                let actual, inboundPeer2  = actualRR |> RResult.rderef 
                Expect.equal (actual) (actOneExpected) ""

                let actThree = hex.DecodeData("00b9e3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa22355361aa02e55a8fc28fef5bd6d71ad0c38228dc68b1c466263b47fdf31e560e139ba")
                let actThreeExpected = hex.DecodeData("034f355bdcb7cc0af728ef3cceb9615d90684bb5b2ca5f859ab0f0b704075871aa") |> PubKey |> NodeId
                let actual = inboundPeer2 |> PeerChannelEncryptor.processActThree(actThree)
                Expect.isOk (actual |> RResult.rtoResult) ""
                Expect.equal (actual |> RResult.rderef |> fst) (actThreeExpected) ""
            testCase1()
    ]