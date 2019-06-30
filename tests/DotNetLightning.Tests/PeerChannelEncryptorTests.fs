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
            let testCase1 =
                let inboundPeer1 = ourNodeId |> PeerChannelEncryptor.newInBound
                let actOne = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
                let actOneExpected = hex.DecodeData("0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae")
                let actualRR  = inboundPeer1 |> PeerChannelEncryptor.processActOneWithEphemeralKey (actOne, ourNodeId, ourEphemeral)
                Expect.isOk (actualRR |> RResult.rtoResult) ""
                let actual, inboundPeer2  = actualRR |> RResult.rderef 
                Expect.equal (actual) (actOneExpected) ""

                let actThree = hex.DecodeData("00b9e3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa22355361aa02e55a8fc28fef5bd6d71ad0c38228dc68b1c466263b47fdf31e560e139ba")
                let actThreeExpected = hex.DecodeData("034f355bdcb7cc0af728ef3cceb9615d90684bb5b2ca5f859ab0f0b704075871aa") |> PubKey |> NodeId
                let actualRR = inboundPeer2 |> PeerChannelEncryptor.processActThree(actThree)
                Expect.isOk (actualRR |> RResult.rtoResult) ""
                let actual, nextState = actualRR |> RResult.rderef
                Expect.equal (actual) (actThreeExpected) ""
                match nextState.NoiseState with
                | Finished { SK = sk; SN = sn; SCK = sck; RK = rk; RN = rn; RCK = rck } ->
                    Expect.equal (sk) (hex.DecodeData("bb9020b8965f4df047e07f955f3c4b88418984aadc5cdb35096b9ea8fa5c3442") |> uint256) ""
                    Expect.equal sn 0UL ""
                    Expect.equal (sck) (hex.DecodeData("919219dbb2920afa8db80f9a51787a840bcf111ed8d588caf9ab4be716e42b01") |> uint256) ""
                    Expect.equal (rk) (hex.DecodeData("969ab31b4d288cedf6218839b27a3e2140827047f2c0f01bf5c04435d43511a9") |> uint256) ""
                    Expect.equal (rn) (0UL) ""
                    Expect.equal (rck) (hex.DecodeData("919219dbb2920afa8db80f9a51787a840bcf111ed8d588caf9ab4be716e42b01") |> uint256) ""
                | _ -> failwith ""

            /// Trnansport-responder act1 short read test
            let testCase2 =
                let inboundPeer = PeerChannelEncryptor.newInBound(ourNodeId)
                let actOne = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c")
                Expect.throwsT<System.Exception>(fun _ -> PeerChannelEncryptor.processActOneWithKey actOne ourNodeId inboundPeer |> ignore) "did not throw error"

            /// Transport-responder act1 bad version test
            let testCase3 =
                let inboundPeer = PeerChannelEncryptor.newInBound(ourNodeId)
                let actOne = "01036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a" |> hex.DecodeData
                let actualRR = inboundPeer |> PeerChannelEncryptor.processActOneWithEphemeralKey (actOne, ourNodeId, ourEphemeral)
                Expect.isError (actualRR |> RResult.rtoResult) ""

            /// Transport responder act1 babd key serialization test
            let testCase4 =
                let inboundPeer = ourNodeId |> PeerChannelEncryptor.newInBound
                let actOne = hex.DecodeData("00046360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
                let actualRR = inboundPeer |> PeerChannelEncryptor.processActOneWithEphemeralKey (actOne, ourNodeId, ourEphemeral)
                Expect.isError  (actualRR |> RResult.rtoResult) ""

            /// Transport-responder act1 bad MAC test
            let testCase5 =
                let inboundPeer = ourNodeId |> PeerChannelEncryptor.newInBound
                let actOne = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6b")
                let actualRR = inboundPeer |> PeerChannelEncryptor.processActOneWithEphemeralKey (actOne, ourNodeId, ourEphemeral)
                Expect.isError (actualRR |> RResult.rtoResult) ""

            /// Transport responder act3 bad version test
            let testCase6 =
                let inboundPeer = ourNodeId |> PeerChannelEncryptor.newInBound
                let actOne = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
                let actual1, inboundPeer2 = inboundPeer |> PeerChannelEncryptor.processActOneWithEphemeralKey (actOne, ourNodeId, ourEphemeral) |> RResult.rderef

                let actThree = hex.DecodeData("01b9e3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa22355361aa02e55a8fc28fef5bd6d71ad0c38228dc68b1c466263b47fdf31e560e139ba")
                let actualRR = inboundPeer2 |> PeerChannelEncryptor.processActThree actThree
                Expect.isError (actualRR |> RResult.rtoResult) ""

            /// Transport responder act3 short read test
            let testCase7 =
                let inboundPeer = PeerChannelEncryptor.newInBound(ourNodeId)
                let actOne = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
                let _, inboundPeer2 = inboundPeer |> PeerChannelEncryptor.processActOneWithEphemeralKey (actOne, ourNodeId, ourEphemeral) |> RResult.rderef
                let actThree = hex.DecodeData("00b9e3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa22355361aa02e55a8fc28fef5bd6d71ad0c38228dc68b1c466263b47fdf31e560e139")
                Expect.throwsT<System.Exception>(fun _ ->
                         inboundPeer2 |> PeerChannelEncryptor.processActThree actThree |> ignore
                     ) "did not throw error"

            /// Transport-responder act3 bad MAC for ciphertext
            let testCase8 =
                let inboundPeer = PeerChannelEncryptor.newInBound ourNodeId
                let actOne = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
                let expectedActOneResult = hex.DecodeData("0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae")
                let actualActOneResult, inboundPeer2 = inboundPeer |> PeerChannelEncryptor.processActOneWithEphemeralKey(actOne, ourNodeId, ourEphemeral) |> RResult.rderef
                Expect.equal actualActOneResult expectedActOneResult ""
                let actThree = hex.DecodeData("00c9e3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa22355361aa02e55a8fc28fef5bd6d71ad0c38228dc68b1c466263b47fdf31e560e139ba")
                let r = PeerChannelEncryptor.processActThree actThree inboundPeer2 |> RResult.rtoResult
                Expect.isError r ""

            /// transport-responder act3 bad rx
            let testCase9 =
                let inboundPeer = PeerChannelEncryptor.newInBound(ourNodeId)
                let actOne = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
                let actualActOneResult, inboundPeer2 = inboundPeer |> PeerChannelEncryptor.processActOneWithEphemeralKey(actOne, ourNodeId, ourEphemeral) |> RResult.rderef
                let expectedActOneResult = hex.DecodeData("0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae")
                Expect.equal (actualActOneResult) (expectedActOneResult) ""

                let actThree = hex.DecodeData("00bfe3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa2235536ad09a8ee351870c2bb7f78b754a26c6cef79a98d25139c856d7efd252c2ae73c")
                let r = PeerChannelEncryptor.processActThree actThree inboundPeer2 |> RResult.rtoResult
                Expect.isError r ""

            /// transport-responder act3 abd MAC text
            let testCase10 =
                let inboundPeer = PeerChannelEncryptor.newInBound(ourNodeId)
                let actOne = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
                let expectedActOneResult = hex.DecodeData("0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae")
                let actualActOneResult, inboundPeer2 = inboundPeer |> PeerChannelEncryptor.processActOneWithEphemeralKey(actOne, ourNodeId, ourEphemeral) |> RResult.rderef
                Expect.equal (actualActOneResult) (expectedActOneResult) ""

                let actThree = hex.DecodeData("00b9e3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa22355361aa02e55a8fc28fef5bd6d71ad0c38228dc68b1c466263b47fdf31e560e139bb")
                let r = PeerChannelEncryptor.processActThree actThree inboundPeer2 |> RResult.rtoResult
                Expect.isError (r) ""
            ()

        testCase "message encryption decryption test vectors" <| fun _ ->
            let mutable outboundPeer = getOutBoundPeerForInitiatorTestVectors()
            let testCase1 = 
                let ourNodeId = "1111111111111111111111111111111111111111111111111111111111111111" |> hex.DecodeData |> Key
                let actTwo = "0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae" |> hex.DecodeData
                let (actualActTwoResult, _), outboundPeer2 = outboundPeer |> PeerChannelEncryptor.processActTwo (actTwo, ourNodeId) |> RResult.rderef
                let expectedActTwoResult = "00b9e3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa22355361aa02e55a8fc28fef5bd6d71ad0c38228dc68b1c466263b47fdf31e560e139ba" |> hex.DecodeData
                Expect.equal actualActTwoResult expectedActTwoResult ""
                match outboundPeer2.NoiseState with
                | Finished { SK = sk; SN = sn; SCK = sck; RK = rk; RN = rn; RCK = rck } ->
                    Expect.equal sk (hex.DecodeData("969ab31b4d288cedf6218839b27a3e2140827047f2c0f01bf5c04435d43511a9") |> uint256)  "" 
                    Expect.equal sn (0UL) ""
                    Expect.equal sck ("919219dbb2920afa8db80f9a51787a840bcf111ed8d588caf9ab4be716e42b01" |> hex.DecodeData |> uint256) ""
                    Expect.equal rk ("bb9020b8965f4df047e07f955f3c4b88418984aadc5cdb35096b9ea8fa5c3442" |> hex.DecodeData |> uint256) ""
                    Expect.equal rn 0UL ""
                    Expect.equal rck ("919219dbb2920afa8db80f9a51787a840bcf111ed8d588caf9ab4be716e42b01" |> hex.DecodeData |> uint256) ""
                    outboundPeer <- outboundPeer2
                | _ -> failwith ""

            let ourNodeId = hex.DecodeData("2121212121212121212121212121212121212121212121212121212121212121") |> Key
            let mutable inboundPeer = PeerChannelEncryptor.newInBound ourNodeId
            /// transport-responder successful handshake
            let testCase2 = 
                let ourEphemeral = hex.DecodeData("2222222222222222222222222222222222222222222222222222222222222222") |> Key
                let actOne = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
                let expectedActOneResult = "0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae" |> hex.DecodeData
                let actualActOneResult, inboundPeer2 = inboundPeer |> PeerChannelEncryptor.processActOneWithEphemeralKey( actOne, ourNodeId, ourEphemeral) |> RResult.rderef
                Expect.equal (actualActOneResult) (expectedActOneResult) ""
                let actThree = "00b9e3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa22355361aa02e55a8fc28fef5bd6d71ad0c38228dc68b1c466263b47fdf31e560e139ba" |> hex.DecodeData
                let expectedActThreeResult = "034f355bdcb7cc0af728ef3cceb9615d90684bb5b2ca5f859ab0f0b704075871aa" |> hex.DecodeData  |> PubKey |> NodeId
                let actualActThreeResult, inboundPeer3 = PeerChannelEncryptor.processActThree (actThree) inboundPeer2 |> RResult.rderef
                Expect.equal actualActThreeResult expectedActThreeResult ""

                match inboundPeer3.NoiseState with
                | Finished { SK = sk; SN = sn; SCK = sck; RK= rk; RN = rn; RCK = rck} ->
                    Expect.equal sk (hex.DecodeData("bb9020b8965f4df047e07f955f3c4b88418984aadc5cdb35096b9ea8fa5c3442") |> uint256) ""
                    Expect.equal sn 0UL ""
                    Expect.equal sck ("919219dbb2920afa8db80f9a51787a840bcf111ed8d588caf9ab4be716e42b01" |> hex.DecodeData |> uint256) ""
                    Expect.equal rk ("969ab31b4d288cedf6218839b27a3e2140827047f2c0f01bf5c04435d43511a9" |> hex.DecodeData |> uint256)  ""
                    Expect.equal rn 0UL ""
                    Expect.equal rck ("919219dbb2920afa8db80f9a51787a840bcf111ed8d588caf9ab4be716e42b01" |> hex.DecodeData |> uint256 ) ""
                    inboundPeer <- inboundPeer3
                | _ -> failwith ""

            for i in 0..1005 do
                printfn "%d th  iteration ----- \n\n" i
                let msg = [| 0x68uy; 0x65uy; 0x6cuy; 0x6cuy; 0x6fuy |]
                printfn "outbound peer before encryption %A" outboundPeer
                let res = outboundPeer.EncryptMessage msg
                printfn "outbound peer after encryption %A" outboundPeer
                Expect.equal (res.Length) (5 + 2 * 16 + 2) ""
                let lengthHeader = res.[0..2+16 - 1]
                printfn "length header is %A" lengthHeader
                printfn "inbound peer before decryption is %A" inboundPeer
                let actualLengthRR = inboundPeer.DecryptLengthHeader(lengthHeader)
                Expect.isOk (actualLengthRR |> RResult.rtoResult) ""
                let actualLength = actualLengthRR |> RResult.rderef
                let expectedLength = uint16 msg.Length
                Expect.equal actualLength (expectedLength) ""

                let actual = inboundPeer.DecryptMessage(res.[2 + 16..]) |> RResult.rderef
                printfn "inbound peer after decryption is %A" inboundPeer
                Expect.equal (actual) (msg) ""

                if i = 0 then
                    let expected = hex.DecodeData("cf2b30ddf0cf3f80e7c35a6e6730b59fe802473180f396d88a8fb0db8cbcf25d2f214cf9ea1d95")
                    Expect.equal res expected ""
                ()
            ()
    ]