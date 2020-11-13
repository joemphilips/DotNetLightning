module PeerChannelEncryptorTests

open Expecto
open Expecto.Logging
open NBitcoin
open DotNetLightning.Utils.Aether
open DotNetLightning.Utils
open DotNetLightning.Peer

open ResultUtils
open ResultUtils.Portability

let hex = NBitcoin.DataEncoders.HexEncoder()
let logger = Log.create "PeerChannelEncryptor tests"

let getOutBoundPeerForInitiatorTestVectors () =
    let theirNodeId = PubKey("028d7500dd4c12685d1f568b4c2b5048e8534b873319f3a8daa612b469132ec7f7")

    let outboundPeer =
        let ie = (new Key(hex.DecodeData("1212121212121212121212121212121212121212121212121212121212121212")))
        PeerChannelEncryptor.newOutBound(NodeId theirNodeId, ie)
        |> fun c ->
                Expect.equal (Optic.get (PeerChannelEncryptor.OutBoundIE_) c) (Some(ie)) ""
                c

    let actual, result = outboundPeer |> PeerChannelEncryptor.getActOne
    let expected = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
    Expect.equal actual expected ""
    result

[<Tests>]
let peerChannelEncryptorTests =
    testList "PeerChannelEncryptorTests" [
        testCase "noise initiator test vectors" <| fun _ ->
            let ourNodeId = new Key(hex.DecodeData("1111111111111111111111111111111111111111111111111111111111111111"))

            let testCase1() =
                let outboundPeer = getOutBoundPeerForInitiatorTestVectors()
                let actTwo = hex.DecodeData("0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae")
                let res = outboundPeer |> PeerChannelEncryptor.processActTwo actTwo ourNodeId
                Expect.isOk (Result.ToFSharpCoreResult res) ""

                let (actual, _nodeid), nextPCE = res |> Result.deref
                let expected = "0x00b9e3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa22355361aa02e55a8fc28fef5bd6d71ad0c38228dc68b1c466263b47fdf31e560e139ba"
                Expect.equal (actual.ToHexString()) (expected) ""

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
                let actTwo = hex.DecodeData("0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730")
                Expect.throws (fun _ -> outboundPeer |> PeerChannelEncryptor.processActTwo actTwo ourNodeId |> ignore) ""
                
            testCase2()

            /// Trnsport-initiator act2 bad version test
            let testCase3() =
                let outboundPeer = getOutBoundPeerForInitiatorTestVectors()
                let actTwo = hex.DecodeData("0102466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae")
                Expect.isError (Result.ToFSharpCoreResult (outboundPeer |> PeerChannelEncryptor.processActTwo actTwo ourNodeId)) ""

            testCase3()

            /// Transport-initiator act2 bad key serialization test
            let testCase4() =
                let outboundPeer = getOutBoundPeerForInitiatorTestVectors()
                let actTwo = hex.DecodeData("0004466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae")
                Expect.isError (Result.ToFSharpCoreResult (outboundPeer |> PeerChannelEncryptor.processActTwo actTwo ourNodeId)) ""
            testCase4()

            /// transport-initiator act2 bad MAC test
            let testCase5() =
                let outboundPeer = getOutBoundPeerForInitiatorTestVectors()
                let actTwo = hex.DecodeData("0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730af")
                Expect.isError(Result.ToFSharpCoreResult (outboundPeer |> PeerChannelEncryptor.processActTwo actTwo ourNodeId)) ""
            testCase5()

        testCase "noise responder test vectors" <| fun _ ->
            let ourNodeSecret = hex.DecodeData("2121212121212121212121212121212121212121212121212121212121212121") |> fun h -> new Key(h)
            let ourEphemeral = hex.DecodeData("2222222222222222222222222222222222222222222222222222222222222222") |> fun h -> new Key(h)

            /// Transport - responder successful handshake
            let _testCase1 =
                let inboundPeer1 =  ourNodeSecret |> PeerChannelEncryptor.newInBound
                let actOne = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
                let actTwoExpected = hex.DecodeData("0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae")
                let actualR  = inboundPeer1 |> PeerChannelEncryptor.processActOneWithEphemeralKey actOne  ourNodeSecret ourEphemeral
                Expect.isOk (Result.ToFSharpCoreResult actualR) ""
                let actual, inboundPeer2  = actualR |> Result.deref
                Expect.equal (actual) (actTwoExpected) ""

                let actThree = hex.DecodeData("00b9e3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa22355361aa02e55a8fc28fef5bd6d71ad0c38228dc68b1c466263b47fdf31e560e139ba")
                let theirNodeIdExpected = hex.DecodeData("034f355bdcb7cc0af728ef3cceb9615d90684bb5b2ca5f859ab0f0b704075871aa") |> PubKey |> NodeId
                let actualRR = inboundPeer2 |> PeerChannelEncryptor.processActThree(actThree)
                Expect.isOk (Result.ToFSharpCoreResult actualRR) ""
                let actual, nextState = actualRR |> Result.deref
                Expect.equal (actual) (theirNodeIdExpected) ""
                match nextState.NoiseState with
                | Finished { SK = sk; SN = sn; SCK = sck; RK = rk; RN = rn; RCK = rck } ->
                    Expect.equal (sk) (hex.DecodeData("bb9020b8965f4df047e07f955f3c4b88418984aadc5cdb35096b9ea8fa5c3442") |> uint256) ""
                    Expect.equal sn 0UL ""
                    Expect.equal (sck) (hex.DecodeData("919219dbb2920afa8db80f9a51787a840bcf111ed8d588caf9ab4be716e42b01") |> uint256) ""
                    Expect.equal (rk) (hex.DecodeData("969ab31b4d288cedf6218839b27a3e2140827047f2c0f01bf5c04435d43511a9") |> uint256) ""
                    Expect.equal (rn) (0UL) ""
                    Expect.equal (rck) (hex.DecodeData("919219dbb2920afa8db80f9a51787a840bcf111ed8d588caf9ab4be716e42b01") |> uint256) ""
                | _ -> failwith "Fail: nextState.NoiseState not finished"

            /// Transport-responder act1 short read test
            let _testCase2 =
                let inboundPeer = PeerChannelEncryptor.newInBound( ourNodeSecret)
                let actOne = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c")
                Expect.throwsT<System.ArgumentException>(fun _ -> PeerChannelEncryptor.processActOneWithKey actOne  ourNodeSecret inboundPeer |> ignore) "did not throw error"

            /// Transport-responder act1 bad version test
            let _testCase3 =
                let inboundPeer = PeerChannelEncryptor.newInBound( ourNodeSecret)
                let actOne = "01036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a" |> hex.DecodeData
                let actualR = inboundPeer |> PeerChannelEncryptor.processActOneWithEphemeralKey actOne  ourNodeSecret ourEphemeral
                Expect.isError (Result.ToFSharpCoreResult actualR) ""

            /// Transport responder act1 babd key serialization test
            let _testCase4 =
                let inboundPeer =  ourNodeSecret |> PeerChannelEncryptor.newInBound
                let actOne = hex.DecodeData("00046360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
                let actualR = inboundPeer |> PeerChannelEncryptor.processActOneWithEphemeralKey actOne  ourNodeSecret ourEphemeral
                Expect.isError  (Result.ToFSharpCoreResult actualR) ""

            /// Transport-responder act1 bad MAC test
            let _testCase5 =
                let inboundPeer =  ourNodeSecret |> PeerChannelEncryptor.newInBound
                let actOne = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6b")
                let actualRR = inboundPeer |> PeerChannelEncryptor.processActOneWithEphemeralKey actOne  ourNodeSecret ourEphemeral
                Expect.isError (Result.ToFSharpCoreResult actualRR) ""

            /// Transport responder act3 bad version test
            let _testCase6 =
                let inboundPeer =  ourNodeSecret |> PeerChannelEncryptor.newInBound
                let actOne = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
                let _actual1, inboundPeer2 = inboundPeer |> PeerChannelEncryptor.processActOneWithEphemeralKey actOne  ourNodeSecret ourEphemeral |> Result.deref

                let actThree = hex.DecodeData("01b9e3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa22355361aa02e55a8fc28fef5bd6d71ad0c38228dc68b1c466263b47fdf31e560e139ba")
                let actualR = inboundPeer2 |> PeerChannelEncryptor.processActThree actThree
                Expect.isError (Result.ToFSharpCoreResult actualR) ""

            /// Transport responder act3 short read test
            let _testCase7 =
                let inboundPeer = PeerChannelEncryptor.newInBound( ourNodeSecret)
                let actOne = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
                let _, inboundPeer2 = inboundPeer |> PeerChannelEncryptor.processActOneWithEphemeralKey actOne  ourNodeSecret ourEphemeral |> Result.deref
                let actThree = hex.DecodeData("00b9e3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa22355361aa02e55a8fc28fef5bd6d71ad0c38228dc68b1c466263b47fdf31e560e139")
                Expect.throwsT<System.ArgumentException>(fun _ ->
                         inboundPeer2 |> PeerChannelEncryptor.processActThree actThree |> ignore
                     ) "did not throw error"

            /// Transport-responder act3 bad MAC for ciphertext
            let _testCase8 =
                let inboundPeer = PeerChannelEncryptor.newInBound  ourNodeSecret
                let actOne = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
                let expectedActOneResult = hex.DecodeData("0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae")
                let actualActOneResult, inboundPeer2 = inboundPeer |> PeerChannelEncryptor.processActOneWithEphemeralKey actOne  ourNodeSecret ourEphemeral |> Result.deref
                Expect.equal actualActOneResult expectedActOneResult ""
                let actThree = hex.DecodeData("00c9e3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa22355361aa02e55a8fc28fef5bd6d71ad0c38228dc68b1c466263b47fdf31e560e139ba")
                let r = PeerChannelEncryptor.processActThree actThree inboundPeer2
                Expect.isError (Result.ToFSharpCoreResult r) ""

            /// transport-responder act3 bad rx
            let _testCase9 =
                let inboundPeer = PeerChannelEncryptor.newInBound( ourNodeSecret)
                let actOne = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
                let actualActOneResult, inboundPeer2 = inboundPeer |> PeerChannelEncryptor.processActOneWithEphemeralKey actOne  ourNodeSecret ourEphemeral |> Result.deref
                let expectedActOneResult = hex.DecodeData("0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae")
                Expect.equal (actualActOneResult) (expectedActOneResult) ""

                let actThree = hex.DecodeData("00bfe3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa2235536ad09a8ee351870c2bb7f78b754a26c6cef79a98d25139c856d7efd252c2ae73c")
                let r = PeerChannelEncryptor.processActThree actThree inboundPeer2
                Expect.isError (Result.ToFSharpCoreResult r) ""

            /// transport-responder act3 abd MAC text
            let _testCase10 =
                let inboundPeer = PeerChannelEncryptor.newInBound( ourNodeSecret)
                let actOne = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
                let expectedActOneResult = hex.DecodeData("0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae")
                let actualActOneResult, inboundPeer2 = inboundPeer |> PeerChannelEncryptor.processActOneWithEphemeralKey actOne  ourNodeSecret ourEphemeral |> Result.deref
                Expect.equal (actualActOneResult) (expectedActOneResult) ""

                let actThree = hex.DecodeData("00b9e3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa22355361aa02e55a8fc28fef5bd6d71ad0c38228dc68b1c466263b47fdf31e560e139bb")
                let r = PeerChannelEncryptor.processActThree actThree inboundPeer2
                Expect.isError (Result.ToFSharpCoreResult r) ""
            ()

        testCase "message encryption decryption test vectors" <| fun _ ->
            let mutable outboundPeer = getOutBoundPeerForInitiatorTestVectors()
            let _testCase1 = 
                let ourNodeId = "1111111111111111111111111111111111111111111111111111111111111111" |> hex.DecodeData |> fun h -> new Key(h)
                let actTwo = "0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae" |> hex.DecodeData
                let (actualActTwoResult, _), outboundPeer2 = outboundPeer |> PeerChannelEncryptor.processActTwo actTwo ourNodeId |> Result.deref
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
                | _ -> failwith "Fail: outboundPeer2.NoiseState not Finished"

            let ourNodeId = hex.DecodeData("2121212121212121212121212121212121212121212121212121212121212121") |> fun h -> new Key(h)
            let mutable inboundPeer = PeerChannelEncryptor.newInBound ourNodeId
            /// transport-responder successful handshake
            let _testCase2 = 
                let ourEphemeral = hex.DecodeData("2222222222222222222222222222222222222222222222222222222222222222") |> fun h -> new Key(h)
                let actOne = hex.DecodeData("00036360e856310ce5d294e8be33fc807077dc56ac80d95d9cd4ddbd21325eff73f70df6086551151f58b8afe6c195782c6a")
                let expectedActOneResult = "0002466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f276e2470b93aac583c9ef6eafca3f730ae" |> hex.DecodeData
                let actualActOneResult, inboundPeer2 = inboundPeer |> PeerChannelEncryptor.processActOneWithEphemeralKey actOne ourNodeId ourEphemeral |> Result.deref
                Expect.equal (actualActOneResult) (expectedActOneResult) ""
                let actThree = "00b9e3a702e93e3a9948c2ed6e5fd7590a6e1c3a0344cfc9d5b57357049aa22355361aa02e55a8fc28fef5bd6d71ad0c38228dc68b1c466263b47fdf31e560e139ba" |> hex.DecodeData
                let expectedActThreeResult = "034f355bdcb7cc0af728ef3cceb9615d90684bb5b2ca5f859ab0f0b704075871aa" |> hex.DecodeData  |> PubKey |> NodeId
                let actualActThreeResult, inboundPeer3 = PeerChannelEncryptor.processActThree (actThree) inboundPeer2 |> Result.deref
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
                | _ -> failwith "Fail: inboundPeer3.NoiseState not Finished"

            let log = fun _s -> ()
                // eventX >> logger.info 
            let rec loop (i: int) (localOutBound) (localInbound) =
                log (sprintf "%d th iteration ----\n\n" i)
                log (sprintf "outbound is %A" localOutBound)
                log (sprintf "inbound is %A" localInbound)
                log (sprintf "----\n\n")
                if i > 1005 then
                    ()
                else
                    let msg = [| 0x68uy; 0x65uy; 0x6cuy; 0x6cuy; 0x6fuy |]
                    let res, newOutBound =
                        let instruction = noise {
                                return! encryptMessage msg;
                            }
                        runP instruction localOutBound |> Result.deref

                    Expect.equal (res.Length) (5 + 2 * 16 + 2) ""
                    log(sprintf "new outbound is %A" newOutBound)
                    let lengthHeader = res.[0..2+16 - 1]
                    let actualLengthR =
                        let instruction = noise {
                            let! header = decryptLengthHeader (lengthHeader)
                            return header
                        }
                        runP instruction localInbound
                    Expect.isOk (Result.ToFSharpCoreResult actualLengthR) ""
                    let actualLength, inbound2 = actualLengthR |> Result.deref
                    log (sprintf "new inbound is %A" inbound2)
                    let expectedLength = uint16 msg.Length
                    Expect.equal actualLength (expectedLength) ""

                    let actualRR =
                        let instruction = noise {
                            let! msg = decryptMessage (res.[2 + 16..])
                            return msg
                        }
                        runP instruction inbound2
                    let actual, inbound3 = actualRR |> Result.deref
                    Expect.equal (actual) (msg) ""

                    if i = 0 then
                        let expected = hex.DecodeData("cf2b30ddf0cf3f80e7c35a6e6730b59fe802473180f396d88a8fb0db8cbcf25d2f214cf9ea1d95")
                        Expect.equal res expected ""
                    if i = 1 then
                        let expected = hex.DecodeData("72887022101f0b6753e0c7de21657d35a4cb2a1f5cde2650528bbc8f837d0f0d7ad833b1a256a1")
                        Expect.equal res expected ""
                    if i = 500 then
                        let expected = hex.DecodeData("178cb9d7387190fa34db9c2d50027d21793c9bc2d40b1e14dcf30ebeeeb220f48364f7a4c68bf8")
                        Expect.equal res expected ""
                    if i = 501 then
                        let expected = hex.DecodeData("1b186c57d44eb6de4c057c49940d79bb838a145cb528d6e8fd26dbe50a60ca2c104b56b60e45bd")
                        Expect.equal res expected ""
                    if i = 1000 then
                        let expected = hex.DecodeData("4a2f3cc3b5e78ddb83dcb426d9863d9d9a723b0337c89dd0b005d89f8d3c05c52b76b29b740f09")
                        Expect.equal res expected ""
                    if i = 1001 then
                        let expected = hex.DecodeData("2ecd8c8a5629d0d02ab457a0fdd0f7b90a192cd46be5ecb6ca570bfc5e268338b1a16cf4ef2d36")
                        Expect.equal res expected ""

                    loop (i + 1) newOutBound inbound3

            loop 0 outboundPeer inboundPeer
    ]