module Serialization
open DotNetLightning.Utils
open DotNetLightning.Utils.Primitives
open DotNetLightning.Serialize
open DotNetLightning.Serialize.Msgs

open Expecto
open NBitcoin
open NBitcoin.Crypto
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open System.Net
open System

module SerializationTest =

    open Utils
    /// helper for more clean error message

    let hex = NBitcoin.DataEncoders.HexEncoder()
    let base64 = NBitcoin.DataEncoders.Base64Encoder()
    let ascii = System.Text.ASCIIEncoding.ASCII
    let signMessageWith (privKey: Key) (msgHash: string) =
        let msgBytes = msgHash |> ascii.GetBytes
        privKey.SignCompact(msgBytes |> uint256, false) |> fun d -> LNECDSASignature.FromBytesCompact(d, true)
    let privKey1 = Key(hex.DecodeData("0101010101010101010101010101010101010101010101010101010101010101"))
    let privKey2 = Key(hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202"))
    let privKey3 = Key(hex.DecodeData("0303030303030303030303030303030303030303030303030303030303030303"))
    let privKey4 = Key(hex.DecodeData("0404040404040404040404040404040404040404040404040404040404040404"))
    let privKey5 = Key(hex.DecodeData("0505050505050505050505050505050505050505050505050505050505050505"))
    let privKey6 = Key(hex.DecodeData("0606060606060606060606060606060606060606060606060606060606060606"))
    let pubkey1 = privKey1.PubKey
    let pubkey2 = privKey2.PubKey
    let pubkey3 = privKey3.PubKey
    let pubkey4 = privKey4.PubKey
    let pubkey5 = privKey5.PubKey
    let pubkey6 = privKey6.PubKey

    [<Tests>]
    let tests =
        testList "Serialization unit tests" [
            testCase "node_announcement" <| fun _ ->
                let sig1 = signMessageWith privKey1 "01010101010101010101010101010101"
                let msg = { NodeAnnouncement.Signature = sig1
                            Contents = { UnsignedNodeAnnouncement.NodeId = NodeId(PubKey("03f3c15dbc4d425a4f4c36162a9159bb83511fa920dba1cc2785c434ecaf094015"))
                                         Features = GlobalFeatures.Flags ([|0uy|])
                                         Timestamp = 1u
                                         RGB = { Red = 217uy; Green = 228uy; Blue = 166uy }
                                         Alias = uint256.Zero
                                         Addresses = [|IPv4({ Addr=[|18uy; 94uy; 0uy; 118uy|]; Port = 7us })|]
                                         ExcessAddressData = [|5uy; 121uy; 62uy; 96uy; 44uy; 34uy|]
                                         ExcessData = [||] }}
                Expect.equal (msg.Clone()) msg ""
        ]

    [<Tests>]
    let testsRustLightningSerilization =
        testList "SerializationTest from rust-lightning" [
            testCase "channel_reestablish no secret" <| fun _ ->
                let cid = ChannelId (uint256([|4; 0; 0; 0; 0; 0; 0; 0; 5; 0; 0; 0; 0; 0; 0; 0; 6; 0; 0; 0; 0; 0; 0; 0; 7; 0; 0; 0; 0; 0; 0; 0|] |> Array.map((uint8)))) 
                let cr = {
                    ChannelId = cid
                    NextLocalCommitmentNumber = 3UL
                    NextRemoteCommitmentNumber = 4UL
                    DataLossProtect = None
                    }
                let actual = cr.ToBytes()
                let expected =
                    [|4; 0; 0; 0; 0; 0; 0; 0; 5; 0; 0; 0; 0; 0; 0; 0; 6; 0; 0; 0; 0; 0; 0; 0; 7; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 3; 0; 0; 0; 0; 0; 0; 0; 4|] 
                    |> Array.map(uint8)
                Expect.equal actual expected "channel_reestablish_no_secret failed"

            testCase "channel_reestablish with secret" <| fun _ ->
                let cr = {
                    ChannelId = ChannelId(uint256([|4; 0; 0; 0; 0; 0; 0; 0; 5; 0; 0; 0; 0; 0; 0; 0; 6; 0; 0; 0; 0; 0; 0; 0; 7; 0; 0; 0; 0; 0; 0; 0 |] |> Array.map(uint8)))
                    NextLocalCommitmentNumber = 3UL
                    NextRemoteCommitmentNumber = 4UL
                    DataLossProtect = OptionalField.Some({
                                          YourLastPerCommitmentSecret = PaymentPreimage([|for _ in 0..31 -> 9uy|])
                                          MyCurrentPerCommitmentPoint = pubkey1
                                      })
                }
                let expected = [|4; 0; 0; 0; 0; 0; 0; 0; 5; 0; 0; 0; 0; 0; 0; 0; 6; 0; 0; 0; 0; 0; 0; 0; 7; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 3; 0; 0; 0; 0; 0; 0; 0; 4; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 3; 27; 132; 197; 86; 123; 18; 100; 64; 153; 93; 62; 213; 170; 186; 5; 101; 215; 30; 24; 52; 96; 72; 25; 255; 156; 23; 245; 233; 213; 221; 7; 143 |] |> Array.map (byte)
                Expect.equal (cr.ToBytes()) expected ""
            testCase "short_channel_id" <| fun _ ->
                let actual = ShortChannelId.FromUInt64(2316138423780173UL)
                let expected = [| 0uy; 8uy; 58uy; 132uy; 0uy; 0uy; 3uy; 77uy;|]
                Expect.equal (actual.ToBytes()) expected ""
                
            testCase "announcement_signatures" <| fun _ ->
                let sig1 = signMessageWith privKey1 "01010101010101010101010101010101"
                let sig2 = signMessageWith privKey1 "02020202020202020202020202020202"
                let actual = { 
                    ChannelId = ChannelId(uint256([| 4; 0; 0; 0; 0; 0; 0; 0; 5; 0; 0; 0; 0; 0; 0; 0; 6; 0; 0; 0; 0; 0; 0; 0; 7; 0; 0; 0; 0; 0; 0; 0 |] |> Array.map(uint8)))
                    ShortChannelId = ShortChannelId.FromUInt64(2316138423780173UL)
                    NodeSignature = sig1
                    BitcoinSignature = sig2
                }
                let expected = hex.DecodeData("040000000000000005000000000000000600000000000000070000000000000000083a840000034dd977cb9b53d93a6ff64bb5f1e158b4094b66e798fb12911168a3ccdf80a83096340a6a95da0ae8d9f776528eecdbb747eb6b545495a4319ed5378e35b21e073acf9953cef4700860f5967838eba2bae89288ad188ebf8b20bf995c3ea53a26df1876d0a3a0e13172ba286a673140190c02ba9da60a2e43a745188c8a83c7f3ef")
                Expect.equal (actual.ToBytes().Length) expected.Length ""
                Expect.equal (actual.ToBytes()) (expected) ""
                CheckArrayEqual (actual.ToBytes()) expected

            testCase "channel_announcement" <| fun _ ->
                let channelAnnouncementTestCore (unknownFeatureBits: bool, nonbitcoinChainHash: bool, excessData: bool) = 
                    let sig1 = signMessageWith privKey1 "01010101010101010101010101010101"
                    let sig2 = signMessageWith privKey2 "01010101010101010101010101010101"
                    let sig3 = signMessageWith privKey3 "01010101010101010101010101010101"
                    let sig4 = signMessageWith privKey4 "01010101010101010101010101010101"
                    let mutable features = Flags([||])
                    if (unknownFeatureBits) then
                        features <- Flags([| 0xFFuy; 0xFFuy |])

                    let unsignedChannelAnnoucement = {
                        Features = features
                        ChainHash = if (not nonbitcoinChainHash) then uint256(hex.DecodeData("6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000")) else uint256(hex.DecodeData("000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"))
                        ShortChannelId = ShortChannelId.FromUInt64(2316138423780173UL)
                        NodeId1 = NodeId(privKey1.PubKey)
                        NodeId2 = NodeId(privKey2.PubKey)
                        BitcoinKey1 = privKey3.PubKey
                        BitcoinKey2 = privKey4.PubKey
                        ExcessData = if excessData then ([| 10; 0; 0; 20; 0; 0; 30; 0; 0; 40 |] |> Array.map(byte)) else [||]
                    }
                    let channelAnnouncement = {
                                                  NodeSignature1 = sig1
                                                  NodeSignature2 = sig2
                                                  BitcoinSignature1 = sig3
                                                  BitcoinSignature2 = sig4
                                                  Contents = unsignedChannelAnnoucement
                                              }

                    let mutable expected = hex.DecodeData("d977cb9b53d93a6ff64bb5f1e158b4094b66e798fb12911168a3ccdf80a83096340a6a95da0ae8d9f776528eecdbb747eb6b545495a4319ed5378e35b21e073a1735b6a427e80d5fe7cd90a2f4ee08dc9c27cda7c35a4172e5d85b12c49d4232537e98f9b1f3c5e6989a8b9644e90e8918127680dbd0d4043510840fc0f1e11a216c280b5395a2546e7e4b2663e04f811622f15a4f91e83aa2e92ba2a573c139142c54ae63072a1ec1ee7dc0c04bde5c847806172aa05c92c22ae8e308d1d2692b12cc195ce0a2d1bda6a88befa19fa07f51caa75ce83837f28965600b8aacab0855ffb0e741ec5f7c41421e9829a9d48611c8c831f71be5ea73e66594977ffd")
                    if (unknownFeatureBits) then
                        expected <- Array.append expected (hex.DecodeData("0002ffff"))
                    else
                        expected <- Array.append expected (hex.DecodeData("0000"))
                    if nonbitcoinChainHash then
                        expected <- Array.append expected (hex.DecodeData("43497fd7f826957108f4a30fd9cec3aeba79972084e90ead01ea330900000000"))
                    else
                        expected <- Array.append expected (hex.DecodeData("000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f"))
                    expected <- Array.append expected (hex.DecodeData("00083a840000034d031b84c5567b126440995d3ed5aaba0565d71e1834604819ff9c17f5e9d5dd078f024d4b6cd1361032ca9bd2aeb9d900aa4d45d9ead80ac9423374c451a7254d076602531fe6068134503d2723133227c867ac8fa6c83c537e9a44c3c5bdbdcb1fe33703462779ad4aad39514614751a71085f2f10e1c7a593e4e030efb5b8721ce55b0b"))
                    if excessData then
                        expected <- Array.append expected (hex.DecodeData("0a00001400001e000028"))
                    Expect.equal (channelAnnouncement.ToBytes().[300..]) expected.[300..] "mismatch in postfix"
                    Expect.equal (channelAnnouncement.ToBytes()) expected ""
                channelAnnouncementTestCore (false, false, false)
                channelAnnouncementTestCore (true, false, false)
                channelAnnouncementTestCore (true, true, false)
                channelAnnouncementTestCore (true, true, true)
                channelAnnouncementTestCore (false, true, true)
                channelAnnouncementTestCore (false, false, true)
                channelAnnouncementTestCore (false, true, false)
                channelAnnouncementTestCore (true, false, true)
            testCase "node_announcement" <| fun _ ->
                let nodeAnnouncementTestCore(unknownFeatureBits: bool, ipv4: bool, ipv6: bool, onionv2: bool, onionv3: bool, excessAddressData: bool, excessData: bool) =
                    let sig1 = signMessageWith privKey1 "01010101010101010101010101010101"
                    let mutable features = GlobalFeatures.Flags [||]
                    if unknownFeatureBits then
                        features <- Flags [| 0xFFuy; 0xFFuy |]
                    let mutable addresses = List.Empty
                    if ipv4 then
                        addresses <- addresses @ [NetAddress.IPv4 ({ IPv4Or6Data.Addr = [|255uy;254uy; 253uy; 252uy; |]
                                                                     Port = 9735us })
                                                                     ] 
                    if ipv6 then
                        addresses <- addresses @
                            [NetAddress.IPv6 ({ IPv4Or6Data.Addr = [|255uy;
                                                                     254uy;
                                                                     253uy;
                                                                     252uy;
                                                                     251uy;
                                                                     250uy;
                                                                     249uy;
                                                                     248uy;
                                                                     247uy;
                                                                     246uy;
                                                                     245uy;
                                                                     244uy;
                                                                     243uy;
                                                                     242uy;
                                                                     241uy;
                                                                     240uy; |]
                                                Port = 9735us })]
                    if onionv2 then
                        addresses <- addresses @ [NetAddress.OnionV2({
                            Addr =[| 255uy
                                     254uy
                                     253uy
                                     252uy
                                     251uy
                                     250uy
                                     249uy
                                     248uy
                                     247uy
                                     246uy |]
                            Port = 9735us } )]
                    if onionv3 then
                        addresses <- addresses @ [NetAddress.OnionV3({ 
                            ed25519PubKey = [| 255uy
                                               254uy
                                               253uy
                                               252uy
                                               251uy
                                               250uy
                                               249uy
                                               248uy
                                               247uy
                                               246uy
                                               245uy
                                               244uy
                                               243uy
                                               242uy
                                               241uy
                                               240uy
                                               239uy
                                               238uy
                                               237uy
                                               236uy
                                               235uy
                                               234uy
                                               233uy
                                               232uy
                                               231uy
                                               230uy
                                               229uy
                                               228uy
                                               227uy
                                               226uy
                                               225uy
                                               224uy|]
                            CheckSum = 32us
                            Version = 16uy
                            Port = 9735us
                         })]
                    let mutable addrLen = 0us
                    for addr in addresses do
                        addrLen <- addrLen + addr.Length + 1us

                    let unsignedNodeAnnouncement = {
                        Features = features
                        Timestamp = 20190119u
                        NodeId = NodeId(pubkey1)
                        RGB = {Blue = 32uy; Red = 32uy; Green = 32uy}
                        Alias = uint256([| for _ in 0..31 -> 16uy|])
                        Addresses = addresses |> Array.ofList
                        ExcessAddressData = if excessAddressData then [|33uy
                                                                        108uy
                                                                        40uy
                                                                        11uy
                                                                        83uy
                                                                        149uy
                                                                        162uy
                                                                        84uy
                                                                        110uy
                                                                        126uy
                                                                        75uy
                                                                        38uy
                                                                        99uy
                                                                        224uy
                                                                        79uy
                                                                        129uy
                                                                        22uy
                                                                        34uy
                                                                        241uy
                                                                        90uy
                                                                        79uy
                                                                        146uy
                                                                        232uy
                                                                        58uy
                                                                        162uy
                                                                        233uy
                                                                        43uy
                                                                        162uy
                                                                        165uy
                                                                        115uy
                                                                        193uy
                                                                        57uy
                                                                        20uy
                                                                        44uy
                                                                        84uy
                                                                        174uy
                                                                        99uy
                                                                        7uy
                                                                        42uy
                                                                        30uy
                                                                        193uy
                                                                        238uy
                                                                        125uy
                                                                        192uy
                                                                        192uy
                                                                        75uy
                                                                        222uy
                                                                        92uy
                                                                        132uy
                                                                        120uy
                                                                        6uy
                                                                        23uy
                                                                        42uy
                                                                        160uy
                                                                        92uy
                                                                        146uy
                                                                        194uy
                                                                        42uy
                                                                        232uy
                                                                        227uy
                                                                        8uy
                                                                        209uy
                                                                        210uy
                                                                        105uy|] else [||]
                        ExcessData = if excessData then [|59uy
                                                          18uy
                                                          204uy
                                                          25uy
                                                          92uy
                                                          224uy
                                                          162uy
                                                          209uy
                                                          189uy
                                                          166uy
                                                          168uy
                                                          139uy
                                                          239uy
                                                          161uy
                                                          159uy
                                                          160uy
                                                          127uy
                                                          81uy
                                                          202uy
                                                          167uy
                                                          92uy
                                                          232uy
                                                          56uy
                                                          55uy
                                                          242uy
                                                          137uy
                                                          101uy
                                                          96uy
                                                          11uy
                                                          138uy
                                                          172uy
                                                          171uy
                                                          8uy
                                                          85uy
                                                          255uy
                                                          176uy
                                                          231uy
                                                          65uy
                                                          236uy
                                                          95uy
                                                          124uy
                                                          65uy
                                                          66uy
                                                          30uy
                                                          152uy
                                                          41uy
                                                          169uy
                                                          212uy
                                                          134uy
                                                          17uy
                                                          200uy
                                                          200uy
                                                          49uy
                                                          247uy
                                                          27uy
                                                          229uy
                                                          234uy
                                                          115uy
                                                          230uy
                                                          101uy
                                                          148uy
                                                          151uy
                                                          127uy
                                                          253uy|] else [||]
                    }
                    addrLen <- addrLen + uint16 unsignedNodeAnnouncement.ExcessAddressData.Length
                    let nodeAnnouncement = {
                        NodeAnnouncement.Signature = sig1
                        Contents = unsignedNodeAnnouncement
                    }

                    let actual = nodeAnnouncement.ToBytes()
                    let mutable expected = hex.DecodeData("d977cb9b53d93a6ff64bb5f1e158b4094b66e798fb12911168a3ccdf80a83096340a6a95da0ae8d9f776528eecdbb747eb6b545495a4319ed5378e35b21e073a")
                    if unknownFeatureBits then
                        expected <- Array.append expected (hex.DecodeData("0002ffff"))
                    else
                        expected <- Array.append expected (hex.DecodeData("0000"))
                    expected <- Array.append expected (hex.DecodeData("013413a7031b84c5567b126440995d3ed5aaba0565d71e1834604819ff9c17f5e9d5dd078f2020201010101010101010101010101010101010101010101010101010101010101010"))
                    expected <- Array.append expected ([| byte(addrLen >>> 8); byte addrLen |])
                    if ipv4 then
                        expected <- Array.append expected (hex.DecodeData("01fffefdfc2607"))
                    if ipv6 then
                        expected <- Array.append expected (hex.DecodeData("02fffefdfcfbfaf9f8f7f6f5f4f3f2f1f02607"))
                    if onionv2 then
                        expected <- Array.append expected (hex.DecodeData("03fffefdfcfbfaf9f8f7f62607"))
                    if onionv3 then
                        expected <- Array.append expected (hex.DecodeData("04fffefdfcfbfaf9f8f7f6f5f4f3f2f1f0efeeedecebeae9e8e7e6e5e4e3e2e1e00020102607"))
                    if excessAddressData then
                        expected <- Array.append expected (hex.DecodeData("216c280b5395a2546e7e4b2663e04f811622f15a4f92e83aa2e92ba2a573c139142c54ae63072a1ec1ee7dc0c04bde5c847806172aa05c92c22ae8e308d1d269"))
                    if excessData then
                        expected <- Array.append expected (hex.DecodeData("3b12cc195ce0a2d1bda6a88befa19fa07f51caa75ce83837f28965600b8aacab0855ffb0e741ec5f7c41421e9829a9d48611c8c831f71be5ea73e66594977ffd"))
                    // CheckArrayEqual actual expected
                    Expect.sequenceContainsOrder actual expected ""
                    Expect.equal actual expected ""
                nodeAnnouncementTestCore(true, true, true, true, true, true, true)
                nodeAnnouncementTestCore(false, false, false, false, false, false, false)
                nodeAnnouncementTestCore(false, true, false, false, false, false, false)
                nodeAnnouncementTestCore(false, false, true, false, false, false, false)
                nodeAnnouncementTestCore(false, false, false, true, false, false, false)
                nodeAnnouncementTestCore(false, false, false, false, true, false, false)
                nodeAnnouncementTestCore(false, false, false, false, false, true, false)
                nodeAnnouncementTestCore(false, true, false, true, false, true, false)
                nodeAnnouncementTestCore(false, false, true, false, true, false, false)
            testCase "channel_update msg" <| fun _ ->
                let channelUpdateTestCore (nonBitcoinChainHash: bool, direction: bool, disable: bool, htlcMaximumMSat: bool) =
                    let sig1 = signMessageWith privKey1 "01010101010101010101010101010101"
                    let unsignedChannelUpdate = {
                        ChainHash = if (not nonBitcoinChainHash) then uint256(hex.DecodeData("6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000")) else uint256(hex.DecodeData("000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"))
                        ShortChannelId = ShortChannelId.FromUInt64(2316138423780173UL)
                        Timestamp = 20190119u
                        Flags = ((if direction && disable then (2us) else if disable then (2us) else if direction then 1us else 0us) ||| (if htlcMaximumMSat then (1us <<< 8) else 0us))
                        CLTVExpiryDelta = !> 144us
                        HTLCMinimumMSat = LNMoney.MilliSatoshis(1000000L)
                        FeeBaseMSat = LNMoney.MilliSatoshis(10000L)
                        FeeProportionalMillionths = 20u
                        ExcessData = if htlcMaximumMSat then [| 0uy; 0uy; 0uy; 0uy; 59uy; 154uy; 202uy; 0uy |] else [||]
                    }
                    let channelUpdate = {
                        ChannelUpdate.Signature = sig1
                        Contents = unsignedChannelUpdate
                    }
                    let actual = channelUpdate.ToBytes()
                    let mutable expected = hex.DecodeData("d977cb9b53d93a6ff64bb5f1e158b4094b66e798fb12911168a3ccdf80a83096340a6a95da0ae8d9f776528eecdbb747eb6b545495a4319ed5378e35b21e073a")
                    if nonBitcoinChainHash then
                        expected <- Array.append expected (hex.DecodeData("43497fd7f826957108f4a30fd9cec3aeba79972084e90ead01ea330900000000"))
                    else
                        expected <- Array.append expected (hex.DecodeData("000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f"))
                    expected <- Array.append expected (hex.DecodeData("00083a840000034d013413a7"))
                    if htlcMaximumMSat then
                        expected <- Array.append expected (hex.DecodeData("01"))
                    else
                        expected <- Array.append expected (hex.DecodeData("00"))
                    expected <- Array.append expected (hex.DecodeData("00"))
                    if direction then
                        expected.[expected.Length - 1] <- 1uy
                    if disable then
                        expected.[expected.Length - 1] <- expected.[expected.Length - 1] ||| 1uy <<< 1
                    expected <- Array.append expected (hex.DecodeData("009000000000000f42400000271000000014"))
                    if htlcMaximumMSat then
                        expected <- Array.append expected (hex.DecodeData("000000003b9aca00"))
                    CheckArrayEqual actual expected
                channelUpdateTestCore(false, false, false, false)
                channelUpdateTestCore(true, false, false, false)
                channelUpdateTestCore(false, true, false, false)
                channelUpdateTestCore(false, false, true, false)
                channelUpdateTestCore(false, false, false, true)
                channelUpdateTestCore(true, true, true, true)

            testCase "open_channel" <| fun _ ->
                let openChannelTestCore(nonBitcoinChainHash: bool, randomBit: bool, shutdown: bool) =
                    let openChannel = {
                        Chainhash = if (not nonBitcoinChainHash) then uint256(hex.DecodeData("6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000")) else uint256(hex.DecodeData("000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"))
                        TemporaryChannelId = ChannelId(uint256([| for _ in 0..31 -> 2uy |]))
                        FundingSatoshis = Money.Satoshis(1311768467284833366UL)
                        PushMSat = LNMoney.MilliSatoshis(2536655962884945560L)
                        DustLimitSatoshis = Money.Satoshis(3608586615801332854UL)
                        MaxHTLCValueInFlightMsat = LNMoney.MilliSatoshis(8517154655701053848L)
                        ChannelReserveSatoshis = Money.Satoshis(8665828695742877976UL)
                        HTLCMinimumMsat = LNMoney.MilliSatoshis(2316138423780173UL)
                        FeeRatePerKw = FeeRatePerKw(821716u)
                        ToSelfDelay = BlockHeightOffset(49340us)
                        MaxAcceptedHTLCs = 49340us
                        FundingPubKey = pubkey1
                        RevocationBasepoint = pubkey2
                        PaymentBasepoint = pubkey3
                        DelayedPaymentBasepoint = pubkey4
                        HTLCBasepoint = pubkey5
                        FirstPerCommitmentPoint = pubkey6
                        ChannelFlags = if randomBit then 1uy <<< 5 else 0uy
                        ShutdownScriptPubKey = if shutdown then Some (pubkey1.Hash.ScriptPubKey) else None
                    }
                    let actual = openChannel.ToBytes()
                    let mutable expected = [||]
                    if nonBitcoinChainHash then
                        expected <- Array.append expected (hex.DecodeData("43497fd7f826957108f4a30fd9cec3aeba79972084e90ead01ea330900000000"))
                    else
                        expected <- Array.append expected (hex.DecodeData("000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f"))
                    expected <- Array.append expected (hex.DecodeData("02020202020202020202020202020202020202020202020202020202020202021234567890123456233403289122369832144668701144767633030896203198784335490624111800083a840000034d000c89d4c0bcc0bc031b84c5567b126440995d3ed5aaba0565d71e1834604819ff9c17f5e9d5dd078f024d4b6cd1361032ca9bd2aeb9d900aa4d45d9ead80ac9423374c451a7254d076602531fe6068134503d2723133227c867ac8fa6c83c537e9a44c3c5bdbdcb1fe33703462779ad4aad39514614751a71085f2f10e1c7a593e4e030efb5b8721ce55b0b0362c0a046dacce86ddd0343c6d3c7c79c2208ba0d9c9cf24a6d046d21d21f90f703f006a18d5653c4edf5391ff23a61f03ff83d237e880ee61187fa9f379a028e0a"))
                    if randomBit then
                        expected <- Array.append expected (hex.DecodeData("20"))
                    else
                        expected <- Array.append expected (hex.DecodeData("00"))
                    if shutdown then
                        expected <- Array.append expected (hex.DecodeData("001976a91479b000887626b294a914501a4cd226b58b23598388ac"))
                    CheckArrayEqual actual expected
                    Expect.equal (openChannel.Clone()) openChannel ""
                openChannelTestCore(false, false, false)
                openChannelTestCore(true, false, false)
                openChannelTestCore(false, true, false)
                openChannelTestCore(false, false, true)
                openChannelTestCore(true, true, true)
            testCase "accept_channel" <| fun _ ->
                let acceptChannelTestCore(shutdown: bool) =
                    let acceptChannel = {
                        AcceptChannel.TemporaryChannelId = ChannelId(uint256([| for _ in 0..31 -> 2uy|]))
                        DustLimitSatoshis = Money.Satoshis(1311768467284833366L)
                        MaxHTLCValueInFlightMsat = LNMoney.MilliSatoshis(2536655962884945560L)
                        ChannelReserveSatoshis = Money.Satoshis(3608586615801332854L)
                        HTLCMinimumMSat = LNMoney.MilliSatoshis(2316138423780173L)
                        MinimumDepth = 821716u |> BlockHeight
                        ToSelfDelay = BlockHeightOffset(49340us)
                        MaxAcceptedHTLCs = 49340us
                        FundingPubKey = pubkey1
                        RevocationBasepoint = pubkey2
                        PaymentBasepoint = pubkey3
                        DelayedPaymentBasepoint = pubkey4
                        HTLCBasepoint = pubkey5
                        FirstPerCommitmentPoint = pubkey6
                        ShutdownScriptPubKey = if shutdown then Some(pubkey1.Hash.ScriptPubKey) else None
                    }
                    let actual = acceptChannel.ToBytes()
                    let mutable expected = hex.DecodeData("020202020202020202020202020202020202020202020202020202020202020212345678901234562334032891223698321446687011447600083a840000034d000c89d4c0bcc0bc031b84c5567b126440995d3ed5aaba0565d71e1834604819ff9c17f5e9d5dd078f024d4b6cd1361032ca9bd2aeb9d900aa4d45d9ead80ac9423374c451a7254d076602531fe6068134503d2723133227c867ac8fa6c83c537e9a44c3c5bdbdcb1fe33703462779ad4aad39514614751a71085f2f10e1c7a593e4e030efb5b8721ce55b0b0362c0a046dacce86ddd0343c6d3c7c79c2208ba0d9c9cf24a6d046d21d21f90f703f006a18d5653c4edf5391ff23a61f03ff83d237e880ee61187fa9f379a028e0a")
                    if shutdown then
                        expected <- Array.append expected (hex.DecodeData("001976a91479b000887626b294a914501a4cd226b58b23598388ac"))
                    CheckArrayEqual actual expected
                acceptChannelTestCore(false)
                acceptChannelTestCore(true)
            testCase "funding_created" <| fun _ ->
                let sig1 = signMessageWith privKey1 "01010101010101010101010101010101"
                let txData = hex.DecodeData("c2d4449afa8d26140898dd54d3390b057ba2a5afcf03ba29d7dc0d8b9ffe966e")
                Array.Reverse txData
                let fundingCreated = {
                    FundingCreated.TemporaryChannelId = ChannelId(uint256[| for _ in 0..31 -> 2uy|])
                    FundingTxId = TxId(uint256(txData, true))
                    FundingOutputIndex = 255us |> TxOutIndex
                    Signature = sig1
                }
                let actual = fundingCreated.ToBytes()
                let expected = hex.DecodeData("02020202020202020202020202020202020202020202020202020202020202026e96fe9f8b0ddcd729ba03cfafa5a27b050b39d354dd980814268dfa9a44d4c200ffd977cb9b53d93a6ff64bb5f1e158b4094b66e798fb12911168a3ccdf80a83096340a6a95da0ae8d9f776528eecdbb747eb6b545495a4319ed5378e35b21e073a")
                // Expect.equal (actual) expected ""
                CheckArrayEqual actual expected
            testCase "funding_signed" <| fun _ ->
                let sig1 = signMessageWith privKey1 "01010101010101010101010101010101"
                let fundingSigned = {
                    FundingSigned.ChannelId = ChannelId(uint256[| for _ in 0..31 -> 2uy|])
                    Signature = sig1
                }
                let expected = hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202d977cb9b53d93a6ff64bb5f1e158b4094b66e798fb12911168a3ccdf80a83096340a6a95da0ae8d9f776528eecdbb747eb6b545495a4319ed5378e35b21e073a")
                CheckArrayEqual (fundingSigned.ToBytes()) expected
                ()
            testCase "funding_locked" <| fun _ ->
                let fundingLocked = {
                    FundingLocked.ChannelId = ChannelId(uint256[| for _ in 0..31 -> 2uy|])
                    NextPerCommitmentPoint = pubkey1
                }
                let expected = hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202031b84c5567b126440995d3ed5aaba0565d71e1834604819ff9c17f5e9d5dd078f")
                CheckArrayEqual (fundingLocked.ToBytes()) expected
            testCase "shutdown" <| fun _ ->
                let shutDownTestCore (scriptType: uint8) =
                    let script = Script("OP_TRUE")
                    let spk =
                        if (scriptType = 1uy) then
                            pubkey1.Hash.ScriptPubKey
                        else if (scriptType = 2uy) then
                            script.Hash.ScriptPubKey
                        else if (scriptType = 3uy) then
                            pubkey1.WitHash.ScriptPubKey
                        else
                            script.WitHash.ScriptPubKey
                    let shutdown = {
                        Shutdown.ChannelId = ChannelId(uint256[| for _ in 0..31 -> 2uy|])
                        ScriptPubKey = spk
                    }
                    let mutable expected = hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202")
                    expected <- Array.append expected (if (scriptType = 1uy) then
                                                           hex.DecodeData("001976a91479b000887626b294a914501a4cd226b58b23598388ac")
                                                       else if (scriptType = 2uy) then
                                                           hex.DecodeData("0017a914da1745e9b549bd0bfa1a569971c77eba30cd5a4b87")
                                                       else if (scriptType = 3uy) then
                                                           hex.DecodeData("0016001479b000887626b294a914501a4cd226b58b235983")
                                                       else
                                                           hex.DecodeData("002200204ae81572f06e1b88fd5ced7a1a000945432e83e1551e6f721ee9c00b8cc33260"))
                    CheckArrayEqual (shutdown.ToBytes()) expected
                shutDownTestCore(1uy)
                shutDownTestCore(2uy)
                shutDownTestCore(3uy)
                shutDownTestCore(4uy)
            testCase "update_add_htlc" <| fun _ ->
                let onionRoutingPacket = {
                    Version = 255uy
                    PublicKey = pubkey1.ToBytes()
                    HopData = [| for _ in 1..(20*65) -> 1uy |]
                    HMAC = uint256([| for _ in 0..31 -> 2uy |])
                }
                let updateAddHtlc = {
                    UpdateAddHTLC.ChannelId = ChannelId(uint256([| for _ in 0..31 -> 2uy |]))
                    HTLCId = HTLCId(2316138423780173UL)
                    AmountMSat = LNMoney.MilliSatoshis(3608586615801332854L)
                    PaymentHash = PaymentHash(uint256[| for _ in 0..31 -> 1uy |])
                    CLTVExpiry = 821716u |> BlockHeight
                    OnionRoutingPacket = onionRoutingPacket
                }
                let expected = hex.DecodeData("020202020202020202020202020202020202020202020202020202020202020200083a840000034d32144668701144760101010101010101010101010101010101010101010101010101010101010101000c89d4ff031b84c5567b126440995d3ed5aaba0565d71e1834604819ff9c17f5e9d5dd078f010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010202020202020202020202020202020202020202020202020202020202020202")
                CheckArrayEqual (updateAddHtlc.ToBytes()) expected

            testCase "update_fulfill_htlc" <| fun _ ->
                let updateFulfillHTLC = {
                    ChannelId = ChannelId(uint256([| for _ in 0..31 -> 2uy |]))
                    HTLCId = HTLCId(2316138423780173UL)
                    PaymentPreimage = PaymentPreimage([| for _ in 0..31 -> 1uy |])
                }
                let expected = hex.DecodeData("020202020202020202020202020202020202020202020202020202020202020200083a840000034d0101010101010101010101010101010101010101010101010101010101010101")
                CheckArrayEqual (updateFulfillHTLC.ToBytes()) expected
            testCase "update_fail_htlc" <| fun _ ->
                let reason = {
                    Data = [| for _ in 0..31 -> 1uy |]
                }
                let updateFailHTLC = {
                    ChannelId = ChannelId(uint256([| for _ in 0..31 -> 2uy |]))
                    HTLCId = HTLCId(2316138423780173UL)
                    Reason = reason
                }
                let expected = hex.DecodeData("020202020202020202020202020202020202020202020202020202020202020200083a840000034d00200101010101010101010101010101010101010101010101010101010101010101")
                CheckArrayEqual (updateFailHTLC.ToBytes()) expected
            testCase "update_fail_malformed_htlc" <| fun _ ->
                let updateFailMalformedHTLC = {
                    ChannelId = ChannelId(uint256([| for _ in 0..31 -> 2uy |]))
                    HTLCId = HTLCId(2316138423780173UL)
                    Sha256OfOnion = uint256([| for _ in 0..31 -> 1uy |])
                    FailureCode = Error.ErrorCode(255us)
                } 
                let expected = hex.DecodeData("020202020202020202020202020202020202020202020202020202020202020200083a840000034d010101010101010101010101010101010101010101010101010101010101010100ff")
                Expect.equal (updateFailMalformedHTLC.ToBytes()) expected ""
            testCase "commitment_signed" <| fun _ ->
                let testCommitmentSignedCore (htlcs: bool) =
                    let sig1 = signMessageWith privKey1 "01010101010101010101010101010101"
                    let sig2 = signMessageWith privKey2 "01010101010101010101010101010101"
                    let sig3 = signMessageWith privKey3 "01010101010101010101010101010101"
                    let sig4 = signMessageWith privKey4 "01010101010101010101010101010101"
                    let commitmentSigned = {
                        ChannelId = ChannelId(uint256([| for _ in 0..31 -> 2uy |]))
                        Signature = sig1
                        HTLCSignatures = if htlcs then [ sig2; sig3; sig4 ] else []
                    }
                    let mutable expected = hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202d977cb9b53d93a6ff64bb5f1e158b4094b66e798fb12911168a3ccdf80a83096340a6a95da0ae8d9f776528eecdbb747eb6b545495a4319ed5378e35b21e073a")
                    if htlcs then
                        expected <- Array.append expected (hex.DecodeData("00031735b6a427e80d5fe7cd90a2f4ee08dc9c27cda7c35a4172e5d85b12c49d4232537e98f9b1f3c5e6989a8b9644e90e8918127680dbd0d4043510840fc0f1e11a216c280b5395a2546e7e4b2663e04f811622f15a4f91e83aa2e92ba2a573c139142c54ae63072a1ec1ee7dc0c04bde5c847806172aa05c92c22ae8e308d1d2692b12cc195ce0a2d1bda6a88befa19fa07f51caa75ce83837f28965600b8aacab0855ffb0e741ec5f7c41421e9829a9d48611c8c831f71be5ea73e66594977ffd"))
                    else
                        expected <- Array.append expected (hex.DecodeData("0000"))
                    CheckArrayEqual (commitmentSigned.ToBytes()) expected

                testCommitmentSignedCore true
                testCommitmentSignedCore false
            testCase "revoke_and_ack" <| fun _ ->
                let raa = {
                    ChannelId = ChannelId(uint256([| for _ in 0..31 -> 2uy |]))
                    PerCommitmentSecret = PaymentPreimage([| for _ in 0..31 -> 1uy |])
                    NextPerCommitmentPoint = pubkey1
                }
                let expected = hex.DecodeData("02020202020202020202020202020202020202020202020202020202020202020101010101010101010101010101010101010101010101010101010101010101031b84c5567b126440995d3ed5aaba0565d71e1834604819ff9c17f5e9d5dd078f")
                CheckArrayEqual (raa.ToBytes()) expected
            testCase "update_fee" <| fun _ ->
                let updateFee = {
                    ChannelId = ChannelId(uint256([| for _ in 0..31 -> 2uy |]))
                    FeeRatePerKw = FeeRatePerKw(20190119u)
                }
                let expected = hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202013413a7")
                // not using CheckArrayEqual since it is smaller than 50
                Expect.equal (updateFee.ToBytes()) expected ""

            testCase "init" <| fun _ ->
                let initTestCore (unknownGlobalBits: bool, initialRoutingSync: bool) =
                    let flags = if unknownGlobalBits then [| 0xffuy; 0xffuy |] else [||]
                    let globalFeatures = GlobalFeatures.Flags(flags)
                    let mutable localFeatures = LocalFeatures.Flags([||])
                    if initialRoutingSync then
                        localFeatures <- localFeatures.SetInitialRoutingSync()

                    let init = {
                        GlobalFeatures = globalFeatures
                        LocalFeatures = localFeatures
                    }
                    let mutable expected = [||]
                    if unknownGlobalBits then
                        expected <- Array.append expected (hex.DecodeData("0002ffff"))
                    else
                        expected <- Array.append expected (hex.DecodeData("0000"))
                    if initialRoutingSync then
                        expected <- Array.append expected (hex.DecodeData("000108"))
                    else
                        expected <- Array.append expected (hex.DecodeData("0000"))
                    Expect.equal (init.ToBytes()) (expected) ""
                initTestCore(false, false)
                initTestCore(true, false)
                initTestCore(false, true)
                initTestCore(true, true)

            testCase "error" <| fun _ ->
                let err = {
                    ChannelId = WhichChannel.SpecificChannel(ChannelId(uint256([| for _ in 0..31 -> 2uy |])))
                    Data = ascii.GetBytes("rust-lightning")
                }
                let expected = hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202000e727573742d6c696768746e696e67")
                Expect.equal (err.ToBytes()) (expected) ""

            testCase "ping" <| fun _ ->
                let ping = {
                    PongLen = 64us
                    BytesLen = 64us
                }
                let expected = hex.DecodeData("0040004000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
                Expect.equal (ping.ToBytes()) (expected) ""
            testCase "pong" <| fun _ ->
                let ping = {
                    BytesLen = 64us
                }
                let expected = hex.DecodeData("004000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
                Expect.equal (ping.ToBytes()) (expected) ""
      ]
