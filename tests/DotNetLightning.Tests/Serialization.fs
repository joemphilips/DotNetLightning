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
    let CheckArrayEqual (actual: 'a array) (expected: 'a array) =
        // Expect.hasLength actual (expected.Length) ""
        let mutable index = 0
        try
            for offset in seq { for x in 1..Int32.MaxValue do if x % 50 = 0 then yield x} do
                index <- offset
                Expect.equal actual.[offset..(offset + 50)] expected.[offset..(offset + 50)] (sprintf "failed in %d" offset)
        with
        | :? IndexOutOfRangeException as ex-> 
            try
                Expect.equal actual.[(actual.Length - 50)..(actual.Length - 1)] expected.[(actual.Length - 50)..(actual.Length - 1)] (sprintf "failed in last 50 of actual: %d (expected length was %d)" (actual.Length) (expected.Length))
                Expect.equal actual expected ""
            with
            | :? IndexOutOfRangeException as ex ->
                Expect.equal actual.[(expected.Length - 50)..(expected.Length - 1)] expected.[(expected.Length - 50)..(expected.Length - 1)] (sprintf "failed in last 50 of expected: %d (actual length was %d)" (expected.Length) (actual.Length))
                Expect.equal actual expected ""

    let hex = NBitcoin.DataEncoders.HexEncoder()
    let base64 = NBitcoin.DataEncoders.Base64Encoder()
    let ascii = System.Text.ASCIIEncoding.ASCII
    let signMessageWith (privKey: Key) (msgHash: string) =
        let msgBytes = msgHash |> ascii.GetBytes
        privKey.SignCompact(msgBytes |> uint256, false) |> fun d -> ECDSASignature.FromBytesCompact(d, true)

    [<Tests>]
    let tests =
        testList "SerializationTest" [
            testCase "channel_reestablish no secret" <| fun _ ->
                let cid = ChannelId (uint256([|4; 0; 0; 0; 0; 0; 0; 0; 5; 0; 0; 0; 0; 0; 0; 0; 6; 0; 0; 0; 0; 0; 0; 0; 7; 0; 0; 0; 0; 0; 0; 0|] |> Array.map((uint8)))) 
                let cr = {
                    ChannelId = cid
                    NextLocalCommitmentNumber = 3UL
                    NextRemoteCommitmentNumber = 4UL
                    DataLossProtect = None
                    }
                let actual = (LightningMsg.ChannelReestablish cr).ToBytes()
                let expected =
                    [|4; 0; 0; 0; 0; 0; 0; 0; 5; 0; 0; 0; 0; 0; 0; 0; 6; 0; 0; 0; 0; 0; 0; 0; 7; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 3; 0; 0; 0; 0; 0; 0; 0; 4|] 
                    |> Array.map(uint8)
                Expect.equal actual expected "channel_reestablish_no_secret failed"

            testCase "channel_reestablish with secret" <| fun _ ->
                let pubkey = Key(hex.DecodeData("0101010101010101010101010101010101010101010101010101010101010101")).PubKey
                let cr = ChannelReestablish{
                    ChannelId = ChannelId(uint256([|4; 0; 0; 0; 0; 0; 0; 0; 5; 0; 0; 0; 0; 0; 0; 0; 6; 0; 0; 0; 0; 0; 0; 0; 7; 0; 0; 0; 0; 0; 0; 0 |] |> Array.map(uint8)))
                    NextLocalCommitmentNumber = 3UL
                    NextRemoteCommitmentNumber = 4UL
                    DataLossProtect = OptionalField.Some({
                                          YourLastPerCommitmentSecret = PaymentPreimage(uint256([|for _ in 0..31 -> 9uy|]))
                                          MyCurrentPerCommitmentPoint = pubkey
                                      })
                }
                let expected = [|4; 0; 0; 0; 0; 0; 0; 0; 5; 0; 0; 0; 0; 0; 0; 0; 6; 0; 0; 0; 0; 0; 0; 0; 7; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 3; 0; 0; 0; 0; 0; 0; 0; 4; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 3; 27; 132; 197; 86; 123; 18; 100; 64; 153; 93; 62; 213; 170; 186; 5; 101; 215; 30; 24; 52; 96; 72; 25; 255; 156; 23; 245; 233; 213; 221; 7; 143 |] |> Array.map (byte)
                Expect.equal (cr.ToBytes()) expected ""
            testCase "short_channel_id" <| fun _ ->
                let actual = ShortChannelId.FromUInt64(2316138423780173UL)
                let expected = [| 0uy; 8uy; 58uy; 132uy; 0uy; 0uy; 3uy; 77uy;|]
                Expect.equal (actual.ToBytes()) expected ""
                
            testCase "announcement_signatures" <| fun _ ->
                let privKey = Key(hex.DecodeData("0101010101010101010101010101010101010101010101010101010101010101"))
                let sig1 = signMessageWith privKey "01010101010101010101010101010101"
                let sig2 = signMessageWith privKey "02020202020202020202020202020202"
                let actual = LightningMsg.AnnouncementSignatures{ 
                    ChannelId = ChannelId(uint256([| 4; 0; 0; 0; 0; 0; 0; 0; 5; 0; 0; 0; 0; 0; 0; 0; 6; 0; 0; 0; 0; 0; 0; 0; 7; 0; 0; 0; 0; 0; 0; 0 |] |> Array.map(uint8)))
                    ShortChannelId = ShortChannelId.FromUInt64(2316138423780173UL)
                    NodeSignature = sig1
                    BitcoinSignature = sig2
                }
                let expected = hex.DecodeData("040000000000000005000000000000000600000000000000070000000000000000083a840000034dd977cb9b53d93a6ff64bb5f1e158b4094b66e798fb12911168a3ccdf80a83096340a6a95da0ae8d9f776528eecdbb747eb6b545495a4319ed5378e35b21e073acf9953cef4700860f5967838eba2bae89288ad188ebf8b20bf995c3ea53a26df1876d0a3a0e13172ba286a673140190c02ba9da60a2e43a745188c8a83c7f3ef")
                Expect.equal (actual.ToBytes().Length) expected.Length ""
                Expect.equal (actual.ToBytes()) expected ""

            testCase "channel_announcement" <| fun _ ->
                let channelAnnouncementTestCore (unknownFeatureBits: bool, nonbitcoinChainHash: bool, excessData: bool) = 
                    let privKey1 = Key(hex.DecodeData("0101010101010101010101010101010101010101010101010101010101010101"))
                    let privKey2 = Key(hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202"))
                    let privKey3 = Key(hex.DecodeData("0303030303030303030303030303030303030303030303030303030303030303"))
                    let privKey4 = Key(hex.DecodeData("0404040404040404040404040404040404040404040404040404040404040404"))
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
                    let channelAnnouncement = LightningMsg.ChannelAnnouncement({
                                                  NodeSignature1 = sig1
                                                  NodeSignature2 = sig2
                                                  BitcoinSignature1 = sig3
                                                  BitcoinSignature2 = sig4
                                                  Contents = unsignedChannelAnnoucement
                                              })

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
                    let privKey1 = Key(hex.DecodeData("0101010101010101010101010101010101010101010101010101010101010101"))
                    let sig1 = signMessageWith privKey1 "01010101010101010101010101010101"
                    let mutable features = GlobalFeatures.Flags [||]
                    if unknownFeatureBits then
                        features <- Flags [| 0xFFuy; 0xFFuy |]
                    let mutable addresses = List.Empty
                    if ipv4 then
                        addresses <- addresses @ [NetAddress.IPv4 (IPEndPoint(IPAddress([|255uy;
                                                                                          254uy;
                                                                                          253uy;
                                                                                          252uy|]), 9735))]
                    if ipv6 then
                        addresses <- addresses @ [NetAddress.IPv6 (IPEndPoint(IPAddress([|255uy;
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
                                                                                          240uy|]), 9735))]
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
                        NodeId = NodeId(privKey1.PubKey)
                        RGB = {Blue = 32uy; Red = 32uy; Green = 32uy}
                        Alias = uint256([| for _ in 0..31 -> 16uy|])
                        Addresses = addresses
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
                    let nodeAnnouncement = LightningMsg.NodeAnnouncement{
                        Signature = sig1
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
                    let privKey = Key(hex.DecodeData("0101010101010101010101010101010101010101010101010101010101010101"))
                    let sig1 = signMessageWith privKey "01010101010101010101010101010101"
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
                    let channelUpdate = LightningMsg.ChannelUpdate{
                        Signature = sig1
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
      ]
