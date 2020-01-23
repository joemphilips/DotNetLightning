module SphinxTests

open ResultUtils
open Expecto
open Expecto.Logging
open Expecto.Logging.Message
open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Utils.OnionError
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Crypto
open DotNetLightning.Crypto.Sphinx

let hex = NBitcoin.DataEncoders.HexEncoder()

let sessionKey = "4141414141414141414141414141414141414141414141414141414141414141" |> hex.DecodeData |> Key
let privKeys = [ "4141414141414141414141414141414141414141414141414141414141414141"
                 "4242424242424242424242424242424242424242424242424242424242424242"
                 "4343434343434343434343434343434343434343434343434343434343434343"
                 "4444444444444444444444444444444444444444444444444444444444444444"
                 "4545454545454545454545454545454545454545454545454545454545454545" ]
                |> List.map(hex.DecodeData >> Key)
let pubKeys = privKeys |> List.map(fun k -> k.PubKey)

let expectedPubKeys = [ "02eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f283686619"
                        "0324653eac434488002cc06bbfb7f10fe18991e35f9fe4302dbea6d2353dc0ab1c"
                        "027f31ebc5462c1fdce1b737ecff52d37d75dea43ce11c74d25aa297165faa2007"
                        "032c0b7cf95324a07d05398b240174dc0c2be444d96b159aa6c7f7b1e668680991"
                        "02edabbd16b41c8371b92ef2f04c1185b4f03b6dcd52ba9b78d9d7c89c8f221145" ]
                      |> List.map(hex.DecodeData >> PubKey)

let payloads = [ "000000000000000000000000000000000000000000000000000000000000000000"
                 "000101010101010101000000010000000100000000000000000000000000000000"
                 "000202020202020202000000020000000200000000000000000000000000000000"
                 "000303030303030303000000030000000300000000000000000000000000000000"
                 "000404040404040404000000040000000400000000000000000000000000000000" ]
               |> List.map(hex.DecodeData)

let associatedData = "4242424242424242424242424242424242424242424242424242424242424242" |> hex.DecodeData

let logger = Log.create "Sphinx tests"
let logCore = eventX >> logger.info 
let log level = logCore

[<Tests>]
let tests =
    testList "bolt4 tests" [
        testCase "pubkey is as expected" <| fun _ ->
            Expect.equal (pubKeys) (expectedPubKeys) ""

        testCase "generate ephemeral keys and secrets" <| fun _ ->
            let (ephKeys, sharedSecrets) = computeEphemeralPublicKeysAndSharedSecrets sessionKey pubKeys
            let expectedEphKeys = [ "02eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f283686619"
                                    "028f9438bfbf7feac2e108d677e3a82da596be706cc1cf342b75c7b7e22bf4e6e2"
                                    "03bfd8225241ea71cd0843db7709f4c222f62ff2d4516fd38b39914ab6b83e0da0"
                                    "031dde6926381289671300239ea8e57ffaf9bebd05b9a5b95beaf07af05cd43595"
                                    "03a214ebd875aab6ddfd77f22c5e7311d7f77f17a169e599f157bbcdae8bf071f4" ]
                                   |> List.map(hex.DecodeData >> PubKey)
            let expectedSharedSecrets = [ "53eb63ea8a3fec3b3cd433b85cd62a4b145e1dda09391b348c4e1cd36a03ea66"
                                          "a6519e98832a0b179f62123b3567c106db99ee37bef036e783263602f3488fae"
                                          "3a6b412548762f0dbccce5c7ae7bb8147d1caf9b5471c34120b30bc9c04891cc"
                                          "21e13c2d7cfe7e18836df50872466117a295783ab8aab0e7ecc8c725503ad02d"
                                          "b5756b9b542727dbafc6765a49488b023a725d631af688fc031217e90770c328" ]
                                        |>  List.map(hex.DecodeData >> Key)
            Expect.equal ephKeys expectedEphKeys ""
            Expect.equal sharedSecrets expectedSharedSecrets ""

        testCase "generate filler" <| fun _ ->
            let (_, sharedSecrets) = computeEphemeralPublicKeysAndSharedSecrets sessionKey pubKeys
            let filler = generateFiller "rho" sharedSecrets.[0..sharedSecrets.Length - 2] (PayloadLength + MacLength) (Some(20))
            let expectedFiller = "c6b008cf6414ed6e4c42c291eb505e9f22f5fe7d0ecdd15a833f4d016ac974d33adc6ea3293e20859e87ebfb937ba406abd025d14af692b12e9c9c2adbe307a679779259676211c071e614fdb386d1ff02db223a5b2fae03df68d321c7b29f7c7240edd3fa1b7cb6903f89dc01abf41b2eb0b49b6b8d73bb0774b58204c0d0e96d3cce45ad75406be0bc009e327b3e712a4bd178609c00b41da2daf8a4b0e1319f07a492ab4efb056f0f599f75e6dc7e0d10ce1cf59088ab6e873de377343880f7a24f0e36731a0b72092f8d5bc8cd346762e93b2bf203d00264e4bc136fc142de8f7b69154deb05854ea88e2d7506222c95ba1aab065c8a851391377d3406a35a9af3ac" |> hex.DecodeData
            Expect.equal filler expectedFiller ""

        testCase "Create packet (reference test vector)" <| fun _ ->
            let (onion, ss) =
                let p = PacketAndSecrets.Create (sessionKey, pubKeys, payloads, associatedData)
                (p.Packet, p.SharedSecrets)
            let expectedPacket =
                // "0002eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f283686619e5f14350c2a76fc232b5e46d421e9615471ab9e0bc887beff8c95fdb878f7b3a71da571226458c510bbadd1276f045c21c520a07d35da256ef75b4367962437b0dd10f7d61ab590531cf08000178a333a347f8b4072e216400406bdf3bf038659793a86cae5f52d32f3438527b47a1cfc54285a8afec3a4c9f3323db0c946f5d4cb2ce721caad69320c3a469a202f3e468c67eaf7a7cda226d0fd32f7b48084dca885d15222e60826d5d971f64172d98e0760154400958f00e86697aa1aa9d41bee8119a1ec866abe044a9ad635778ba61fc0776dc832b39451bd5d35072d2269cf9b040d6ba38b54ec35f81d7fc67678c3be47274f3c4cc472aff005c3469eb3bc140769ed4c7f0218ff8c6c7dd7221d189c65b3b9aaa71a01484b122846c7c7b57e02e679ea8469b70e14fe4f70fee4d87b910cf144be6fe48eef24da475c0b0bcc6565ae82cd3f4e3b24c76eaa5616c6111343306ab35c1fe5ca4a77c0e314ed7dba39d6f1e0de791719c241a939cc493bea2bae1c1e932679ea94d29084278513c77b899cc98059d06a27d171b0dbdf6bee13ddc4fc17a0c4d2827d488436b57baa167544138ca2e64a11b43ac8a06cd0c2fba2d4d900ed2d9205305e2d7383cc98dacb078133de5f6fb6bed2ef26ba92cea28aafc3b9948dd9ae5559e8bd6920b8cea462aa445ca6a95e0e7ba52961b181c79e73bd581821df2b10173727a810c92b83b5ba4a0403eb710d2ca10689a35bec6c3a708e9e92f7d78ff3c5d9989574b00c6736f84c199256e76e19e78f0c98a9d580b4a658c84fc8f2096c2fbea8f5f8c59d0fdacb3be2802ef802abbecb3aba4acaac69a0e965abd8981e9896b1f6ef9d60f7a164b371af869fd0e48073742825e9434fc54da837e120266d53302954843538ea7c6c3dbfb4ff3b2fdbe244437f2a153ccf7bdb4c92aa08102d4f3cff2ae5ef86fab4653595e6a5837fa2f3e29f27a9cde5966843fb847a4a61f1e76c281fe8bb2b0a181d096100db5a1a5ce7a910238251a43ca556712eaadea167fb4d7d75825e440f3ecd782036d7574df8bceacb397abefc5f5254d2722215c53ff54af8299aaaad642c6d72a14d27882d9bbd539e1cc7a527526ba89b8c037ad09120e98ab042d3e8652b31ae0e478516bfaf88efca9f3676ffe99d2819dcaeb7610a626695f53117665d267d3f7abebd6bbd6733f645c72c389f03855bdf1e4b8075b516569b118233a0f0971d24b83113c0b096f5216a207ca99a7cddc81c130923fe3d91e7508c9ac5f2e914ff5dccab9e558566fa14efb34ac98d878580814b94b73acbfde9072f30b881f7f0fff42d4045d1ace6322d86a97d164aa84d93a60498065cc7c20e636f5862dc81531a88c60305a2e59a985be327a6902e4bed986dbf4a0b50c217af0ea7fdf9ab37f9ea1a1aaa72f54cf40154ea9b269f1a7c09f9f43245109431a175d50e2db0132337baa0ef97eed0fcf20489da36b79a1172faccc2f7ded7c60e00694282d93359c4682135642bc81f433574aa8ef0c97b4ade7ca372c5ffc23c7eddd839bab4e0f14d6df15c9dbeab176bec8b5701cf054eb3072f6dadc98f88819042bf10c407516ee58bce33fbe3b3d86a54255e577db4598e30a135361528c101683a5fcde7e8ba53f3456254be8f45fe3a56120ae96ea3773631fcb3873aa3abd91bcff00bd38bd43697a2e789e00da6077482e7b1b1a677b5afae4c54e6cbdf7377b694eb7d7a5b913476a5be923322d3de06060fd5e819635232a2cf4f0731da13b8546d1d6d4f8d75b9fce6c2341a71b0ea6f780df54bfdb0dd5cd9855179f602f917265f21f9190c70217774a6fbaaa7d63ad64199f4664813b955cff954949076dcf"
                "0002eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f283686619e5f14350c2a76fc232b5e46d421e9615471ab9e0bc887beff8c95fdb878f7b3a716a996c7845c93d90e4ecbb9bde4ece2f69425c99e4bc820e44485455f135edc0d10f7d61ab590531cf08000179a333a347f8b4072f216400406bdf3bf038659793d4a1fd7b246979e3150a0a4cb052c9ec69acf0f48c3d39cd55675fe717cb7d80ce721caad69320c3a469a202f1e468c67eaf7a7cd8226d0fd32f7b48084dca885d56047694762b67021713ca673929c163ec36e04e40ca8e1c6d17569419d3039d9a1ec866abe044a9ad635778b961fc0776dc832b3a451bd5d35072d2269cf9b040f6b7a7dad84fb114ed413b1426cb96ceaf83825665ed5a1d002c1687f92465b49ed4c7f0218ff8c6c7dd7221d589c65b3b9aaa71a41484b122846c7c7b57e02e679ea8469b70e14fe4f70fee4d87b910cf144be6fe48eef24da475c0b0bcc6565ae82cd3f4e3b24c76eaa5616c6111343306ab35c1fe5ca4a77c0e314ed7dba39d6f1e0de791719c241a939cc493bea2bae1c1e932679ea94d29084278513c77b899cc98059d06a27d171b0dbdf6bee13ddc4fc17a0c4d2827d488436b57baa167544138ca2e64a11b43ac8a06cd0c2fba2d4d900ed2d9205305e2d7383cc98dacb078133de5f6fb6bed2ef26ba92cea28aafc3b9948dd9ae5559e8bd6920b8cea462aa445ca6a95e0e7ba52961b181c79e73bd581821df2b10173727a810c92b83b5ba4a0403eb710d2ca10689a35bec6c3a708e9e92f7d78ff3c5d9989574b00c6736f84c199256e76e19e78f0c98a9d580b4a658c84fc8f2096c2fbea8f5f8c59d0fdacb3be2802ef802abbecb3aba4acaac69a0e965abd8981e9896b1f6ef9d60f7a164b371af869fd0e48073742825e9434fc54da837e120266d53302954843538ea7c6c3dbfb4ff3b2fdbe244437f2a153ccf7bdb4c92aa08102d4f3cff2ae5ef86fab4653595e6a5837fa2f3e29f27a9cde5966843fb847a4a61f1e76c281fe8bb2b0a181d096100db5a1a5ce7a910238251a43ca556712eaadea167fb4d7d75825e440f3ecd782036d7574df8bceacb397abefc5f5254d2722215c53ff54af8299aaaad642c6d72a14d27882d9bbd539e1cc7a527526ba89b8c037ad09120e98ab042d3e8652b31ae0e478516bfaf88efca9f3676ffe99d2819dcaeb7610a626695f53117665d267d3f7abebd6bbd6733f645c72c389f03855bdf1e4b8075b516569b118233a0f0971d24b83113c0b096f5216a207ca99a7cddc81c130923fe3d91e7508c9ac5f2e914ff5dccab9e558566fa14efb34ac98d878580814b94b73acbfde9072f30b881f7f0fff42d4045d1ace6322d86a97d164aa84d93a60498065cc7c20e636f5862dc81531a88c60305a2e59a985be327a6902e4bed986dbf4a0b50c217af0ea7fdf9ab37f9ea1a1aaa72f54cf40154ea9b269f1a7c09f9f43245109431a175d50e2db0132337baa0ef97eed0fcf20489da36b79a1172faccc2f7ded7c60e00694282d93359c4682135642bc81f433574aa8ef0c97b4ade7ca372c5ffc23c7eddd839bab4e0f14d6df15c9dbeab176bec8b5701cf054eb3072f6dadc98f88819042bf10c407516ee58bce33fbe3b3d86a54255e577db4598e30a135361528c101683a5fcde7e8ba53f3456254be8f45fe3a56120ae96ea3773631fcb3873aa3abd91bcff00bd38bd43697a2e789e00da6077482e7b1b1a677b5afae4c54e6cbdf7377b694eb7d7a5b913476a5be923322d3de06060fd5e819635232a2cf4f0731da13b8546d1d6d4f8d75b9fce6c2341a71b0ea6f780df54bfdb0dd5cd9855179f602f9172307c7268724c3618e6817abd793adc214a0dc0bc616816632f27ea336fb56dfd"
                |> hex.DecodeData
            CheckArrayEqual (onion.ToBytes()) (expectedPacket)
            let { Payload = payload0; NextPacket = nextPacket0; SharedSecret = ss0 }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[0]) (associatedData) (onion.ToBytes())
                |> fun rr -> 
                    Expect.isOk(rr) ""
                    Result.defaultWith (fun _ -> failwith "Unreachable") rr
            let { Payload = payload1; NextPacket = nextPacket1; }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[1]) (associatedData) (nextPacket0.ToBytes()) |> Result.defaultWith(fun _ -> failwith "")
            let { Payload = payload2; NextPacket = nextPacket2; }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[2]) (associatedData) (nextPacket1.ToBytes()) |> Result.defaultWith(fun _ -> failwith "")
            let { Payload = payload3; NextPacket = nextPacket3; }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[3]) (associatedData) (nextPacket2.ToBytes()) |> Result.defaultWith(fun _ -> failwith "")
            let { Payload = payload4; NextPacket = nextPacket4; }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[4]) (associatedData) (nextPacket3.ToBytes()) |> Result.defaultWith(fun _ -> failwith "")

            Expect.equal [payload0; payload1; payload2; payload3; payload4] payloads ""

            Expect.equal nextPacket0.HMAC ("2bdc5227c8eb8ba5fcfc15cfc2aa578ff208c106646d0652cd289c0a37e445bb" |> hex.DecodeData |> uint256) ""
            Expect.equal nextPacket1.HMAC ("28430b210c0af631ef80dc8594c08557ce4626bdd3593314624a588cc083a1d9" |> hex.DecodeData |> uint256) ""
            Expect.equal nextPacket2.HMAC ("4e888d0cc6a90e7f857af18ac858834ac251d0d1c196d198df48a0c5bf816803" |> hex.DecodeData |> uint256) ""
            Expect.equal nextPacket3.HMAC ("42c10947e06bda75b35ac2a9e38005479a6feac51468712e751c71a1dcf3e31b" |> hex.DecodeData |> uint256) ""
            Expect.equal nextPacket4.HMAC ("0000000000000000000000000000000000000000000000000000000000000000" |> hex.DecodeData |> uint256) ""
            ()

        testCase "last node replies with an error message" <| fun _ ->
            let (onion, ss) =
                let p = PacketAndSecrets.Create (sessionKey, pubKeys, payloads, associatedData)
                (p.Packet, p.SharedSecrets)
            let { NextPacket = packet1; SharedSecret = ss0 }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[0]) (associatedData) (onion.ToBytes())
                |> fun r -> 
                    Expect.isOk(r) ""
                    Result.defaultWith(fun _ -> failwith "") r
            let { NextPacket = packet2; SharedSecret = ss1 }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[1]) (associatedData) (packet1.ToBytes()) |> Result.defaultWith(fun _ -> failwith "")
            let { NextPacket = packet3; SharedSecret = ss2 }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[2]) (associatedData) (packet2.ToBytes()) |> Result.defaultWith(fun _ -> failwith "")
            let { NextPacket = packet4; SharedSecret = ss3 }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[3]) (associatedData) (packet3.ToBytes()) |> Result.defaultWith(fun _ -> failwith "")
            let { NextPacket = packet5; SharedSecret = ss4 }: ParsedPacket =
                Sphinx.parsePacket (privKeys.[4]) (associatedData) (packet4.ToBytes()) |> Result.defaultWith(fun _ -> failwith "")

            Expect.isTrue (packet5.IsLastPacket) ""
            let error = ErrorPacket.Create(ss4, { FailureMsg.Code = FailureCode (OnionError.TEMPORARY_NODE_FAILURE); Data = TemporaryNodeFailure })
            let _ =
                let expected = "a5e6bd0c74cb347f10cce367f949098f2457d14c046fd8a22cb96efb30b0fdcda8cb9168b50f2fd45edd73c1b0c8b33002df376801ff58aaa94000bf8a86f92620f343baef38a580102395ae3abf9128d1047a0736ff9b83d456740ebbb4aeb3aa9737f18fb4afb4aa074fb26c4d702f42968888550a3bded8c05247e045b866baef0499f079fdaeef6538f31d44deafffdfd3afa2fb4ca9082b8f1c465371a9894dd8c243fb4847e004f5256b3e90e2edde4c9fb3082ddfe4d1e734cacd96ef0706bf63c9984e22dc98851bcccd1c3494351feb458c9c6af41c0044bea3c47552b1d992ae542b17a2d0bba1a096c78d169034ecb55b6e3a7263c26017f033031228833c1daefc0dedb8cf7c3e37c9c37ebfe42f3225c326e8bcfd338804c145b16e34e4" |> hex.DecodeData
                Expect.equal(expected) error ""

            let error1 = Sphinx.forwardErrorPacket(error, ss3)
            let _ =
                let expected = "c49a1ce81680f78f5f2000cda36268de34a3f0a0662f55b4e837c83a8773c22aa081bab1616a0011585323930fa5b9fae0c85770a2279ff59ec427ad1bbff9001c0cd1497004bd2a0f68b50704cf6d6a4bf3c8b6a0833399a24b3456961ba00736785112594f65b6b2d44d9f5ea4e49b5e1ec2af978cbe31c67114440ac51a62081df0ed46d4a3df295da0b0fe25c0115019f03f15ec86fabb4c852f83449e812f141a9395b3f70b766ebbd4ec2fae2b6955bd8f32684c15abfe8fd3a6261e52650e8807a92158d9f1463261a925e4bfba44bd20b166d532f0017185c3a6ac7957adefe45559e3072c8dc35abeba835a8cb01a71a15c736911126f27d46a36168ca5ef7dccd4e2886212602b181463e0dd30185c96348f9743a02aca8ec27c0b90dca270"
                               |> hex.DecodeData
                Expect.equal expected error1 ""

            let error2 = Sphinx.forwardErrorPacket(error1, ss2)
            let _ =
                let expected = "a5d3e8634cfe78b2307d87c6d90be6fe7855b4f2cc9b1dfb19e92e4b79103f61ff9ac25f412ddfb7466e74f81b3e545563cdd8f5524dae873de61d7bdfccd496af2584930d2b566b4f8d3881f8c043df92224f38cf094cfc09d92655989531524593ec6d6caec1863bdfaa79229b5020acc034cd6deeea1021c50586947b9b8e6faa83b81fbfa6133c0af5d6b07c017f7158fa94f0d206baf12dda6b68f785b773b360fd0497e16cc402d779c8d48d0fa6315536ef0660f3f4e1865f5b38ea49c7da4fd959de4e83ff3ab686f059a45c65ba2af4a6a79166aa0f496bf04d06987b6d2ea205bdb0d347718b9aeff5b61dfff344993a275b79717cd815b6ad4c0beb568c4ac9c36ff1c315ec1119a1993c4b61e6eaa0375e0aaf738ac691abd3263bf937e3"
                               |> hex.DecodeData
                Expect.equal expected error2 ""

            let error3 = Sphinx.forwardErrorPacket (error2, ss1)
            let _ =
                let expected = "aac3200c4968f56b21f53e5e374e3a2383ad2b1b6501bbcc45abc31e59b26881b7dfadbb56ec8dae8857add94e6702fb4c3a4de22e2e669e1ed926b04447fc73034bb730f4932acd62727b75348a648a1128744657ca6a4e713b9b646c3ca66cac02cdab44dd3439890ef3aaf61708714f7375349b8da541b2548d452d84de7084bb95b3ac2345201d624d31f4d52078aa0fa05a88b4e20202bd2b86ac5b52919ea305a8949de95e935eed0319cf3cf19ebea61d76ba92532497fcdc9411d06bcd4275094d0a4a3c5d3a945e43305a5a9256e333e1f64dbca5fcd4e03a39b9012d197506e06f29339dfee3331995b21615337ae060233d39befea925cc262873e0530408e6990f1cbd233a150ef7b004ff6166c70c68d9f8c853c1abca640b8660db2921"
                               |> hex.DecodeData
                Expect.equal expected error3 ""
            
            let error4 = Sphinx.forwardErrorPacket(error3, ss0)
            let _ =
                let expected = "9c5add3963fc7f6ed7f148623c84134b5647e1306419dbe2174e523fa9e2fbed3a06a19f899145610741c83ad40b7712aefaddec8c6baf7325d92ea4ca4d1df8bce517f7e54554608bf2bd8071a4f52a7a2f7ffbb1413edad81eeea5785aa9d990f2865dc23b4bc3c301a94eec4eabebca66be5cf638f693ec256aec514620cc28ee4a94bd9565bc4d4962b9d3641d4278fb319ed2b84de5b665f307a2db0f7fbb757366067d88c50f7e829138fde4f78d39b5b5802f1b92a8a820865af5cc79f9f30bc3f461c66af95d13e5e1f0381c184572a91dee1c849048a647a1158cf884064deddbf1b0b88dfe2f791428d0ba0f6fb2f04e14081f69165ae66d9297c118f0907705c9c4954a199bae0bb96fad763d690e7daa6cfda59ba7f2c8d11448b604d12d"
                               |> hex.DecodeData
                Expect.equal expected error4 ""
            let { OriginNode = pubkey; FailureMsg = failure } = ErrorPacket.Parse(error4, ss) |> Result.defaultWith(fun _ -> failwith "")
            Expect.equal (pubKeys.[4]) (pubkey.Value) ""
            Expect.equal (TemporaryNodeFailure) (failure.Data) ""
            ()

        testCase "Intermediate node replies with an error message" <| fun _ ->
            let { Packet = packet; SharedSecrets = ss } =
                Sphinx.PacketAndSecrets.Create(sessionKey, pubKeys, payloads, associatedData)
            let { NextPacket = packet1; SharedSecret = ss0 } =
                Sphinx.parsePacket(privKeys.[0]) (associatedData) (packet.ToBytes()) |> Result.defaultWith(fun _ -> failwith "")
            let { NextPacket = packet2; SharedSecret = ss1 } =
                Sphinx.parsePacket(privKeys.[1]) (associatedData) (packet1.ToBytes()) |> Result.defaultWith(fun _ -> failwith "")
            let { SharedSecret = ss2 } =
                Sphinx.parsePacket(privKeys.[2]) (associatedData) (packet2.ToBytes()) |> Result.defaultWith(fun _ -> failwith "")
            
            let error = ErrorPacket.Create(ss2, { Code = OnionError.FailureCode INVALID_REALM; Data = InvalidRealm })
            let error1 = forwardErrorPacket(error, ss1)
            let error2 = forwardErrorPacket(error1, ss0)
            let { OriginNode = pubkey; FailureMsg = failure } =
                ErrorPacket.Parse(error2, ss) |> Result.defaultWith(fun _ -> failwith "")
            Expect.equal (pubkey.Value) (pubKeys.[2]) ""
            Expect.equal (InvalidRealm) (failure.Data) ""
            ()
    ]