module Serialization

open DotNetLightning.Utils
open DotNetLightning.Core.Utils.Extensions
open DotNetLightning.Serialization.Msgs
open DotNetLightning.Crypto

open DotNetLightning.Serialization
open Expecto
open NBitcoin
open System
open System.Collections
open FsCheck

open ResultUtils
open ResultUtils.Portability

module SerializationTest =

    open Utils
    /// helper for more clean error message
    
    let parseBitArray (str: string) =
        match BitArray.TryParse str with
        | Ok ba -> ba
        | Error e -> failwith e

    let hex = NBitcoin.DataEncoders.HexEncoder()
    let base64 = NBitcoin.DataEncoders.Base64Encoder()
    let ascii = System.Text.ASCIIEncoding.ASCII
    let signMessageWith (privKey: Key) (msgHash: string) =
        let msgBytes = msgHash |> ascii.GetBytes
        privKey.SignCompact(msgBytes |> uint256, false) |> fun d -> LNECDSASignature.FromBytesCompact(d, true)
    let privKey1 = new Key(hex.DecodeData("0101010101010101010101010101010101010101010101010101010101010101"))
    let privKey2 = new Key(hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202"))
    let privKey3 = new Key(hex.DecodeData("0303030303030303030303030303030303030303030303030303030303030303"))
    let privKey4 = new Key(hex.DecodeData("0404040404040404040404040404040404040404040404040404040404040404"))
    let privKey5 = new Key(hex.DecodeData("0505050505050505050505050505050505050505050505050505050505050505"))
    let privKey6 = new Key(hex.DecodeData("0606060606060606060606060606060606060606060606060606060606060606"))
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
                let msg = { NodeAnnouncementMsg.Signature = sig1
                            Contents = { UnsignedNodeAnnouncementMsg.NodeId = NodeId(PubKey("03f3c15dbc4d425a4f4c36162a9159bb83511fa920dba1cc2785c434ecaf094015"))
                                         Features = FeatureBits.CreateUnsafe [|0uy|]
                                         Timestamp = 1u
                                         RGB = { Red = 217uy; Green = 228uy; Blue = 166uy }
                                         Alias = uint256.Zero
                                         Addresses = [|IPv4({ Addr=[|18uy; 94uy; 0uy; 118uy|]; Port = 7us })|]
                                         ExcessAddressData = [|5uy; 121uy; 62uy; 96uy; 44uy; 34uy|]
                                         ExcessData = [||] }}
                Expect.equal (msg.Clone()) msg ""
            testCase "short channel id test" <| fun _ ->
                let shortChannelId = (ShortChannelId.FromUInt64(140737488420865UL))
                Expect.equal (shortChannelId.BlockHeight.Value) 128u ""
                Expect.equal (shortChannelId.BlockIndex.Value) 1u ""
                Expect.equal (shortChannelId.TxOutIndex.Value) 1us ""
                ()
        ]

    [<Tests>]
    let testsRustLightningSerialization =
        testList "SerializationTest ported from rust-lightning" [
            testCase "channel_reestablish no secret" <| fun _ ->
                let cid = ChannelId (uint256([|4; 0; 0; 0; 0; 0; 0; 0; 5; 0; 0; 0; 0; 0; 0; 0; 6; 0; 0; 0; 0; 0; 0; 0; 7; 0; 0; 0; 0; 0; 0; 0|] |> Array.map((uint8)))) 
                let channelReestablishMsg = {
                    ChannelId = cid
                    NextCommitmentNumber = CommitmentNumber <| (UInt48.MaxValue - UInt48.FromUInt64 3UL)
                    NextRevocationNumber = CommitmentNumber <| (UInt48.MaxValue - UInt48.FromUInt64 4UL)
                    DataLossProtect = None
                    }
                let actual = channelReestablishMsg.ToBytes()
                let expected =
                    [|4; 0; 0; 0; 0; 0; 0; 0; 5; 0; 0; 0; 0; 0; 0; 0; 6; 0; 0; 0; 0; 0; 0; 0; 7; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 3; 0; 0; 0; 0; 0; 0; 0; 4|] 
                    |> Array.map(uint8)
                Expect.equal actual expected "channel_reestablish_no_secret failed"

            testCase "channel_reestablish with secret" <| fun _ ->
                let channelReestablishMsg = {
                    ChannelId = ChannelId(uint256([|4; 0; 0; 0; 0; 0; 0; 0; 5; 0; 0; 0; 0; 0; 0; 0; 6; 0; 0; 0; 0; 0; 0; 0; 7; 0; 0; 0; 0; 0; 0; 0 |] |> Array.map(uint8)))
                    NextCommitmentNumber = CommitmentNumber <| (UInt48.MaxValue - UInt48.FromUInt64 3UL)
                    NextRevocationNumber = CommitmentNumber <| (UInt48.MaxValue - UInt48.FromUInt64 4UL)
                    DataLossProtect = OptionalField.Some <| {
                        YourLastPerCommitmentSecret = 
                            Some <| PerCommitmentSecret.FromBytes
                                [| for _ in 0..(PerCommitmentSecret.BytesLength - 1) -> 9uy |]
                        MyCurrentPerCommitmentPoint = PerCommitmentPoint pubkey1
                    }
                }
                let expected = [|4; 0; 0; 0; 0; 0; 0; 0; 5; 0; 0; 0; 0; 0; 0; 0; 6; 0; 0; 0; 0; 0; 0; 0; 7; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 3; 0; 0; 0; 0; 0; 0; 0; 4; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 3; 27; 132; 197; 86; 123; 18; 100; 64; 153; 93; 62; 213; 170; 186; 5; 101; 215; 30; 24; 52; 96; 72; 25; 255; 156; 23; 245; 233; 213; 221; 7; 143 |] |> Array.map (byte)
                Expect.equal (channelReestablishMsg.ToBytes()) expected ""
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
                let channelAnnouncementTestCore (nonbitcoinChainHash: bool, excessData: bool) = 
                    let sig1 = signMessageWith privKey1 "01010101010101010101010101010101"
                    let sig2 = signMessageWith privKey2 "01010101010101010101010101010101"
                    let sig3 = signMessageWith privKey3 "01010101010101010101010101010101"
                    let sig4 = signMessageWith privKey4 "01010101010101010101010101010101"
                    let mutable features = FeatureBits.CreateUnsafe [||]

                    let unsignedChannelAnnoucement = {
                        Features = features
                        ChainHash = if (not nonbitcoinChainHash) then uint256(hex.DecodeData("000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f")) else uint256(hex.DecodeData("000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"))
                        ShortChannelId = ShortChannelId.FromUInt64(2316138423780173UL)
                        NodeId1 = NodeId(privKey1.PubKey)
                        NodeId2 = NodeId(privKey2.PubKey)
                        BitcoinKey1 = !> privKey3.PubKey
                        BitcoinKey2 = !> privKey4.PubKey
                        ExcessData = if excessData then ([| 10; 0; 0; 20; 0; 0; 30; 0; 0; 40 |] |> Array.map(byte)) else [||]
                    }
                    let channelAnnouncementMsg = {
                                                  NodeSignature1 = sig1
                                                  NodeSignature2 = sig2
                                                  BitcoinSignature1 = sig3
                                                  BitcoinSignature2 = sig4
                                                  Contents = unsignedChannelAnnoucement
                                              }

                    let mutable expected = hex.DecodeData("d977cb9b53d93a6ff64bb5f1e158b4094b66e798fb12911168a3ccdf80a83096340a6a95da0ae8d9f776528eecdbb747eb6b545495a4319ed5378e35b21e073a1735b6a427e80d5fe7cd90a2f4ee08dc9c27cda7c35a4172e5d85b12c49d4232537e98f9b1f3c5e6989a8b9644e90e8918127680dbd0d4043510840fc0f1e11a216c280b5395a2546e7e4b2663e04f811622f15a4f91e83aa2e92ba2a573c139142c54ae63072a1ec1ee7dc0c04bde5c847806172aa05c92c22ae8e308d1d2692b12cc195ce0a2d1bda6a88befa19fa07f51caa75ce83837f28965600b8aacab0855ffb0e741ec5f7c41421e9829a9d48611c8c831f71be5ea73e66594977ffd")
                    expected <- Array.append expected (hex.DecodeData("0000"))
                    if nonbitcoinChainHash then
                        expected <- Array.append expected (hex.DecodeData("000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"))
                    else
                        expected <- Array.append expected (hex.DecodeData("000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f"))
                    expected <- Array.append expected (hex.DecodeData("00083a840000034d031b84c5567b126440995d3ed5aaba0565d71e1834604819ff9c17f5e9d5dd078f024d4b6cd1361032ca9bd2aeb9d900aa4d45d9ead80ac9423374c451a7254d076602531fe6068134503d2723133227c867ac8fa6c83c537e9a44c3c5bdbdcb1fe33703462779ad4aad39514614751a71085f2f10e1c7a593e4e030efb5b8721ce55b0b"))
                    if excessData then
                        expected <- Array.append expected (hex.DecodeData("0a00001400001e000028"))
                    Expect.equal (channelAnnouncementMsg.ToBytes().[300..]) expected.[300..] "mismatch in postfix"
                    Expect.equal (channelAnnouncementMsg.ToBytes()) expected ""
                channelAnnouncementTestCore (false, false)
                channelAnnouncementTestCore (false, false)
                channelAnnouncementTestCore (true, false)
                channelAnnouncementTestCore (true, true)
                channelAnnouncementTestCore (true, true)
                channelAnnouncementTestCore (false, true)
                channelAnnouncementTestCore (true, false)
                channelAnnouncementTestCore (false, true)
            testCase "node_announcement" <| fun _ ->
                let nodeAnnouncementTestCore(ipv4: bool, ipv6: bool, onionv2: bool, onionv3: bool, excessAddressData: bool, excessData: bool) =
                    let sig1 = signMessageWith privKey1 "01010101010101010101010101010101"
                    let mutable features = FeatureBits.CreateUnsafe [||]
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

                    let unsignedNodeAnnouncementMsg = {
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
                    addrLen <- addrLen + uint16 unsignedNodeAnnouncementMsg.ExcessAddressData.Length
                    let nodeAnnouncementMsg = {
                        NodeAnnouncementMsg.Signature = sig1
                        Contents = unsignedNodeAnnouncementMsg
                    }

                    let actual = nodeAnnouncementMsg.ToBytes()
                    let mutable expected = hex.DecodeData("d977cb9b53d93a6ff64bb5f1e158b4094b66e798fb12911168a3ccdf80a83096340a6a95da0ae8d9f776528eecdbb747eb6b545495a4319ed5378e35b21e073a")
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
                nodeAnnouncementTestCore(true, true, true, true, true, true)
                nodeAnnouncementTestCore(false, false, false, false, false, false)
                nodeAnnouncementTestCore(true, false, false, false, false, false)
                nodeAnnouncementTestCore(false, true, false, false, false, false)
                nodeAnnouncementTestCore(false, false, true, false, false, false)
                nodeAnnouncementTestCore(false, false, false, true, false, false)
                nodeAnnouncementTestCore(false, false, false, false, true, false)
                nodeAnnouncementTestCore(true, false, true, false, true, false)
                nodeAnnouncementTestCore(false, true, false, true, false, false)
            testCase "channel_update msg" <| fun _ ->
                let channelUpdateTestCore (nonBitcoinChainHash: bool, direction: bool, disable: bool, htlcMaximumMSat: bool) =
                    let sig1 = signMessageWith privKey1 "01010101010101010101010101010101"
                    let unsignedChannelUpdateMsg = {
                        UnsignedChannelUpdateMsg.ChainHash = if (not nonBitcoinChainHash) then uint256(hex.DecodeData("000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f")) else uint256(hex.DecodeData("000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"))
                        ShortChannelId = ShortChannelId.FromUInt64(2316138423780173UL)
                        Timestamp = 20190119u
                        MessageFlags = (if htlcMaximumMSat then 1uy else 0uy)
                        ChannelFlags = ((if direction && disable then (2uy) else if disable then (2uy) else if direction then 1uy else 0uy))
                        CLTVExpiryDelta = !> 144us
                        HTLCMinimumMSat = LNMoney.MilliSatoshis(1000000L)
                        FeeBaseMSat = LNMoney.MilliSatoshis(10000L)
                        FeeProportionalMillionths = 20u
                        HTLCMaximumMSat =
                            if htlcMaximumMSat then
                                [| 0uy; 0uy; 0uy; 0uy; 59uy; 154uy; 202uy; 0uy |]
                                |> fun b -> NBitcoin.Utils.ToUInt64(b, false)
                                |> LNMoney.MilliSatoshis
                                |> Some
                            else
                                None
                    }
                    let channelUpdateMsg = {
                        ChannelUpdateMsg.Signature = sig1
                        Contents = unsignedChannelUpdateMsg
                    }
                    let actual = channelUpdateMsg.ToBytes()
                    let mutable expected = hex.DecodeData("d977cb9b53d93a6ff64bb5f1e158b4094b66e798fb12911168a3ccdf80a83096340a6a95da0ae8d9f776528eecdbb747eb6b545495a4319ed5378e35b21e073a")
                    if nonBitcoinChainHash then
                        expected <- Array.append expected (hex.DecodeData("000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"))
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
                    let openChannelMsg = {
                        Chainhash = if (not nonBitcoinChainHash) then uint256(hex.DecodeData("000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f")) else uint256(hex.DecodeData("000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"))
                        TemporaryChannelId = ChannelId(uint256([| for _ in 0..31 -> 2uy |]))
                        FundingSatoshis = Money.Satoshis(1311768467284833366UL)
                        PushMSat = LNMoney.MilliSatoshis(2536655962884945560L)
                        DustLimitSatoshis = Money.Satoshis(3608586615801332854UL)
                        MaxHTLCValueInFlightMsat = LNMoney.MilliSatoshis(8517154655701053848L)
                        ChannelReserveSatoshis = Money.Satoshis(8665828695742877976UL)
                        HTLCMinimumMsat = LNMoney.MilliSatoshis(2316138423780173UL)
                        FeeRatePerKw = FeeRatePerKw(821716u)
                        ToSelfDelay = BlockHeightOffset16(49340us)
                        MaxAcceptedHTLCs = 49340us
                        FundingPubKey = FundingPubKey pubkey1
                        RevocationBasepoint = RevocationBasepoint pubkey2
                        PaymentBasepoint = PaymentBasepoint pubkey3
                        DelayedPaymentBasepoint = DelayedPaymentBasepoint pubkey4
                        HTLCBasepoint = HtlcBasepoint pubkey5
                        FirstPerCommitmentPoint = PerCommitmentPoint pubkey6
                        ChannelFlags = {
                            AnnounceChannel = randomBit
                        }
                        TLVs = [|
                            OpenChannelTLV.UpfrontShutdownScript (
                                if shutdown then
                                    Some <| ShutdownScriptPubKey.FromPubKeyP2pkh pubkey1
                                else
                                    None
                            )
                        |]
                    } 
                    let actual = openChannelMsg.ToBytes()
                    let mutable expected = [||]
                    if nonBitcoinChainHash then
                        expected <- Array.append expected (hex.DecodeData("000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"))
                    else
                        expected <- Array.append expected (hex.DecodeData("000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f"))
                    expected <- Array.append expected (hex.DecodeData("02020202020202020202020202020202020202020202020202020202020202021234567890123456233403289122369832144668701144767633030896203198784335490624111800083a840000034d000c89d4c0bcc0bc031b84c5567b126440995d3ed5aaba0565d71e1834604819ff9c17f5e9d5dd078f024d4b6cd1361032ca9bd2aeb9d900aa4d45d9ead80ac9423374c451a7254d076602531fe6068134503d2723133227c867ac8fa6c83c537e9a44c3c5bdbdcb1fe33703462779ad4aad39514614751a71085f2f10e1c7a593e4e030efb5b8721ce55b0b0362c0a046dacce86ddd0343c6d3c7c79c2208ba0d9c9cf24a6d046d21d21f90f703f006a18d5653c4edf5391ff23a61f03ff83d237e880ee61187fa9f379a028e0a"))
                    if randomBit then
                        expected <- Array.append expected (hex.DecodeData("01"))
                    else
                        expected <- Array.append expected (hex.DecodeData("00"))
                    if shutdown then
                        expected <- Array.append expected (hex.DecodeData("001976a91479b000887626b294a914501a4cd226b58b23598388ac"))
                    else
                        expected <- Array.append expected (hex.DecodeData("0000"))
                    CheckArrayEqual actual expected
                    Expect.equal (openChannelMsg.Clone()) openChannelMsg ""
                openChannelTestCore(false, false, false)
                openChannelTestCore(true, false, false)
                openChannelTestCore(false, true, false)
                openChannelTestCore(false, false, true)
                openChannelTestCore(true, true, true)
            testCase "accept_channel" <| fun _ ->
                let acceptChannelTestCore(shutdown: bool) =
                    let acceptChannelMsg = {
                        AcceptChannelMsg.TemporaryChannelId = ChannelId(uint256([| for _ in 0..31 -> 2uy|]))
                        DustLimitSatoshis = Money.Satoshis(1311768467284833366L)
                        MaxHTLCValueInFlightMsat = LNMoney.MilliSatoshis(2536655962884945560L)
                        ChannelReserveSatoshis = Money.Satoshis(3608586615801332854L)
                        HTLCMinimumMSat = LNMoney.MilliSatoshis(2316138423780173L)
                        MinimumDepth = 821716u |> BlockHeightOffset32
                        ToSelfDelay = BlockHeightOffset16(49340us)
                        MaxAcceptedHTLCs = 49340us
                        FundingPubKey = FundingPubKey pubkey1
                        RevocationBasepoint = RevocationBasepoint pubkey2
                        PaymentBasepoint = PaymentBasepoint pubkey3
                        DelayedPaymentBasepoint = DelayedPaymentBasepoint pubkey4
                        HTLCBasepoint = HtlcBasepoint pubkey5
                        FirstPerCommitmentPoint = PerCommitmentPoint pubkey6
                        TLVs = [|
                            AcceptChannelTLV.UpfrontShutdownScript (
                                if shutdown then
                                    Some <| ShutdownScriptPubKey.FromPubKeyP2pkh pubkey1
                                else
                                    None
                            )
                        |]
                    }
                    let actual = acceptChannelMsg.ToBytes()
                    let mutable expected = hex.DecodeData("020202020202020202020202020202020202020202020202020202020202020212345678901234562334032891223698321446687011447600083a840000034d000c89d4c0bcc0bc031b84c5567b126440995d3ed5aaba0565d71e1834604819ff9c17f5e9d5dd078f024d4b6cd1361032ca9bd2aeb9d900aa4d45d9ead80ac9423374c451a7254d076602531fe6068134503d2723133227c867ac8fa6c83c537e9a44c3c5bdbdcb1fe33703462779ad4aad39514614751a71085f2f10e1c7a593e4e030efb5b8721ce55b0b0362c0a046dacce86ddd0343c6d3c7c79c2208ba0d9c9cf24a6d046d21d21f90f703f006a18d5653c4edf5391ff23a61f03ff83d237e880ee61187fa9f379a028e0a")
                    if shutdown then
                        expected <- Array.append expected (hex.DecodeData("001976a91479b000887626b294a914501a4cd226b58b23598388ac"))
                    else
                        expected <- Array.append expected (hex.DecodeData("0000"))
                    CheckArrayEqual actual expected
                acceptChannelTestCore(false)
                acceptChannelTestCore(true)
            testCase "funding_created" <| fun _ ->
                let sig1 = signMessageWith privKey1 "01010101010101010101010101010101"
                let txData = hex.DecodeData("c2d4449afa8d26140898dd54d3390b057ba2a5afcf03ba29d7dc0d8b9ffe966e")
                Array.Reverse txData
                let fundingCreatedMsg = {
                    FundingCreatedMsg.TemporaryChannelId = ChannelId(uint256[| for _ in 0..31 -> 2uy|])
                    FundingTxId = TxId(uint256(txData, true))
                    FundingOutputIndex = 255us |> TxOutIndex
                    Signature = sig1
                }
                let actual = fundingCreatedMsg.ToBytes()
                let expected = hex.DecodeData("02020202020202020202020202020202020202020202020202020202020202026e96fe9f8b0ddcd729ba03cfafa5a27b050b39d354dd980814268dfa9a44d4c200ffd977cb9b53d93a6ff64bb5f1e158b4094b66e798fb12911168a3ccdf80a83096340a6a95da0ae8d9f776528eecdbb747eb6b545495a4319ed5378e35b21e073a")
                // Expect.equal (actual) expected ""
                CheckArrayEqual actual expected
            testCase "funding_signed" <| fun _ ->
                let sig1 = signMessageWith privKey1 "01010101010101010101010101010101"
                let fundingSignedMsg = {
                    FundingSignedMsg.ChannelId = ChannelId(uint256[| for _ in 0..31 -> 2uy|])
                    Signature = sig1
                }
                let expected = hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202d977cb9b53d93a6ff64bb5f1e158b4094b66e798fb12911168a3ccdf80a83096340a6a95da0ae8d9f776528eecdbb747eb6b545495a4319ed5378e35b21e073a")
                CheckArrayEqual (fundingSignedMsg.ToBytes()) expected
                ()
            testCase "funding_locked" <| fun _ ->
                let fundingLockedMsg = {
                    FundingLockedMsg.ChannelId = ChannelId(uint256[| for _ in 0..31 -> 2uy|])
                    NextPerCommitmentPoint = PerCommitmentPoint pubkey1
                }
                let expected = hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202031b84c5567b126440995d3ed5aaba0565d71e1834604819ff9c17f5e9d5dd078f")
                CheckArrayEqual (fundingLockedMsg.ToBytes()) expected
            testCase "shutdown" <| fun _ ->
                let shutDownTestCore (scriptType: uint8) =
                    let script = Script("OP_TRUE")
                    let spk =
                        if (scriptType = 1uy) then
                            ShutdownScriptPubKey.FromPubKeyP2pkh pubkey1
                        else if (scriptType = 2uy) then
                            ShutdownScriptPubKey.FromScriptP2sh script
                        else if (scriptType = 3uy) then
                            ShutdownScriptPubKey.FromPubKeyP2wpkh pubkey1
                        else
                            ShutdownScriptPubKey.FromScriptP2wsh script
                    let shutdownMsg = {
                        ShutdownMsg.ChannelId = ChannelId(uint256[| for _ in 0..31 -> 2uy|])
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
                    CheckArrayEqual (shutdownMsg.ToBytes()) expected
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
                let updateAddHtlcMsg = {
                    UpdateAddHTLCMsg.ChannelId = ChannelId(uint256([| for _ in 0..31 -> 2uy |]))
                    HTLCId = HTLCId(2316138423780173UL)
                    Amount = LNMoney.MilliSatoshis 3608586615801332854L
                    PaymentHash = PaymentHash(uint256[| for _ in 0..31 -> 1uy |])
                    CLTVExpiry = 821716u |> BlockHeight
                    OnionRoutingPacket = onionRoutingPacket
                }
                let expected = hex.DecodeData("020202020202020202020202020202020202020202020202020202020202020200083a840000034d32144668701144760101010101010101010101010101010101010101010101010101010101010101000c89d4ff031b84c5567b126440995d3ed5aaba0565d71e1834604819ff9c17f5e9d5dd078f010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010202020202020202020202020202020202020202020202020202020202020202")
                CheckArrayEqual (updateAddHtlcMsg.ToBytes()) expected

            testCase "update_fulfill_htlc" <| fun _ ->
                let updateFulfillHTLCMsg = {
                    ChannelId = ChannelId(uint256([| for _ in 0..31 -> 2uy |]))
                    HTLCId = HTLCId(2316138423780173UL)
                    PaymentPreimage = PaymentPreimage.Create([| for _ in 0..(PaymentPreimage.LENGTH - 1) -> 1uy |])
                }
                let expected = hex.DecodeData("020202020202020202020202020202020202020202020202020202020202020200083a840000034d0101010101010101010101010101010101010101010101010101010101010101")
                CheckArrayEqual (updateFulfillHTLCMsg.ToBytes()) expected
            testCase "update_fail_htlc" <| fun _ ->
                let reason = {
                    Data = [| for _ in 0..31 -> 1uy |]
                }
                let updateFailHTLCMsg = {
                    ChannelId = ChannelId(uint256([| for _ in 0..31 -> 2uy |]))
                    HTLCId = HTLCId(2316138423780173UL)
                    Reason = reason
                }
                let expected = hex.DecodeData("020202020202020202020202020202020202020202020202020202020202020200083a840000034d00200101010101010101010101010101010101010101010101010101010101010101")
                CheckArrayEqual (updateFailHTLCMsg.ToBytes()) expected
            testCase "update_fail_malformed_htlc" <| fun _ ->
                let updateFailMalformedHTLCMsg = {
                    ChannelId = ChannelId(uint256([| for _ in 0..31 -> 2uy |]))
                    HTLCId = HTLCId(2316138423780173UL)
                    Sha256OfOnion = uint256([| for _ in 0..31 -> 1uy |])
                    FailureCode = OnionError.FailureCode(255us)
                } 
                let expected = hex.DecodeData("020202020202020202020202020202020202020202020202020202020202020200083a840000034d010101010101010101010101010101010101010101010101010101010101010100ff")
                Expect.equal (updateFailMalformedHTLCMsg.ToBytes()) expected ""
            testCase "commitment_signed" <| fun _ ->
                let testCommitmentSignedCore (htlcs: bool) =
                    let sig1 = signMessageWith privKey1 "01010101010101010101010101010101"
                    let sig2 = signMessageWith privKey2 "01010101010101010101010101010101"
                    let sig3 = signMessageWith privKey3 "01010101010101010101010101010101"
                    let sig4 = signMessageWith privKey4 "01010101010101010101010101010101"
                    let commitmentSignedMsg = {
                        ChannelId = ChannelId(uint256([| for _ in 0..31 -> 2uy |]))
                        Signature = sig1
                        HTLCSignatures = if htlcs then [ sig2; sig3; sig4 ] else []
                    }
                    let mutable expected = hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202d977cb9b53d93a6ff64bb5f1e158b4094b66e798fb12911168a3ccdf80a83096340a6a95da0ae8d9f776528eecdbb747eb6b545495a4319ed5378e35b21e073a")
                    if htlcs then
                        expected <- Array.append expected (hex.DecodeData("00031735b6a427e80d5fe7cd90a2f4ee08dc9c27cda7c35a4172e5d85b12c49d4232537e98f9b1f3c5e6989a8b9644e90e8918127680dbd0d4043510840fc0f1e11a216c280b5395a2546e7e4b2663e04f811622f15a4f91e83aa2e92ba2a573c139142c54ae63072a1ec1ee7dc0c04bde5c847806172aa05c92c22ae8e308d1d2692b12cc195ce0a2d1bda6a88befa19fa07f51caa75ce83837f28965600b8aacab0855ffb0e741ec5f7c41421e9829a9d48611c8c831f71be5ea73e66594977ffd"))
                    else
                        expected <- Array.append expected (hex.DecodeData("0000"))
                    CheckArrayEqual (commitmentSignedMsg.ToBytes()) expected

                testCommitmentSignedCore true
                testCommitmentSignedCore false
            testCase "revoke_and_ack" <| fun _ ->
                let revokeAndACKMsg = {
                    ChannelId = ChannelId(uint256([| for _ in 0..31 -> 2uy |]))
                    PerCommitmentSecret =
                        PerCommitmentSecret.FromBytes
                            [| for _ in 0..(PaymentPreimage.LENGTH - 1) -> 1uy |]
                    NextPerCommitmentPoint = PerCommitmentPoint pubkey1
                }
                let expected = hex.DecodeData("02020202020202020202020202020202020202020202020202020202020202020101010101010101010101010101010101010101010101010101010101010101031b84c5567b126440995d3ed5aaba0565d71e1834604819ff9c17f5e9d5dd078f")
                CheckArrayEqual (revokeAndACKMsg.ToBytes()) expected
            testCase "update_fee" <| fun _ ->
                let updateFeeMsg = {
                    ChannelId = ChannelId(uint256([| for _ in 0..31 -> 2uy |]))
                    FeeRatePerKw = FeeRatePerKw(20190119u)
                }
                let expected = hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202013413a7")
                // not using CheckArrayEqual since it is smaller than 50
                Expect.equal (updateFeeMsg.ToBytes()) expected ""

            testCase "init" <| fun _ ->
                let initTestCore (initialRoutingSync: bool) =
                    let flags = [||]
                    let globalFeatures = flags |> BitArray.FromBytes
                    let localFeatures =
                        if initialRoutingSync then "0b1000" |> BitArray.TryParse else BitArray.TryParse("")
                        |> function Ok ba -> ba | Error e -> failwith e

                    let initMsg = {
                        Features = [| globalFeatures; localFeatures |] |> BitArray.Concat |> FeatureBits.CreateUnsafe
                        TLVStream = [||]
                    }
                    let mutable expected = [||]
                    expected <- Array.append expected (hex.DecodeData("0000"))
                    if initialRoutingSync then
                        expected <- Array.append expected (hex.DecodeData("000108"))
                    else
                        expected <- Array.append expected (hex.DecodeData("0000"))
                    Expect.equal (initMsg.ToBytes()) (expected) ""
                initTestCore(false)
                initTestCore(true)

            testCase "error" <| fun _ ->
                let errorMsg = {
                    ChannelId = WhichChannel.SpecificChannel(ChannelId(uint256([| for _ in 0..31 -> 2uy |])))
                    Data = ascii.GetBytes("rust-lightning")
                }
                let expected = hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202000e727573742d6c696768746e696e67")
                Expect.equal (errorMsg.ToBytes()) (expected) ""

            testCase "ping" <| fun _ ->
                let pingMsg = {
                    PongLen = 64us
                    BytesLen = 64us
                }
                let expected = hex.DecodeData("0040004000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
                Expect.equal (pingMsg.ToBytes()) (expected) ""
            testCase "pong" <| fun _ ->
                let pongMsg = {
                    BytesLen = 64us
                }
                let expected = hex.DecodeData("004000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
                Expect.equal (pongMsg.ToBytes()) (expected) ""
                
      ]
        
    [<Tests>]
    let testFeaturesSerialization =
        testList "Features serialization" [
            testCase "initial_routing_sync" <| fun _ ->
                Expect.isTrue (Feature.hasFeature(parseBitArray"0b00001000") (Feature.InitialRoutingSync) (Some Optional)) ""
                Expect.isFalse(Feature.hasFeature(parseBitArray"0b00001000") (Feature.InitialRoutingSync) (Some Mandatory)) ""
                
            testCase "data_loss_protect" <| fun _ ->
                Expect.isTrue (Feature.hasFeature(parseBitArray"0b00000001") (Feature.OptionDataLossProtect) (Some Mandatory)) ""
                Expect.isTrue (Feature.hasFeature(parseBitArray"0b00000010") (Feature.OptionDataLossProtect) (Some Optional)) ""
                
            testCase "initial_routing_sync, data_loss_protect and option_upfront_shutdown_script features" <| fun _ ->
                let features = parseBitArray("0000000000101010")
                Expect.isTrue (Feature.areSupported(features)) ""
                Expect.isTrue (Feature.hasFeature(features) (Feature.InitialRoutingSync) (None)) ""
                Expect.isTrue (Feature.hasFeature(features) (Feature.OptionDataLossProtect) (None)) ""
                Expect.isTrue (Feature.hasFeature(features) (Feature.OptionUpfrontShutdownScript) (None)) ""
                
            testCase "variable_length_onion feature" <| fun _ ->
                Expect.isTrue (Feature.hasFeature("0b0000000100000000" |> parseBitArray) (Feature.VariableLengthOnion) (None)) ""
                Expect.isTrue (Feature.hasFeature("0b0000000100000000" |> parseBitArray) (Feature.VariableLengthOnion) (Some(Mandatory))) ""
                Expect.isTrue (Feature.hasFeature("0b0000001000000000" |> parseBitArray) (Feature.VariableLengthOnion) (None)) ""
                Expect.isTrue (Feature.hasFeature("0b0000001000000000" |> parseBitArray) (Feature.VariableLengthOnion) (Some(Optional))) ""
                ()
                
            testProperty "BitArray serialization" <| fun (ba : NonNull<byte[]>) ->
                let backAndForth = BitArray.FromBytes(ba.Get).ToByteArray()
                let finalArray = Array.zeroCreate ba.Get.Length
                Array.Copy(backAndForth, 0, finalArray, ba.Get.Length - backAndForth.Length, backAndForth.Length)
                Expect.equal ba.Get finalArray "BitArray.ToByteArray does not invert and trim BitArray.FromBytes"

            testCase "FeatureBits to/from byte array preserves byte order, bit order and trims zero bytes" <| fun _ ->
                let features = FeatureBits.Zero
                features.ByteArray <- [| 0b00000000uy; 0b00100000uy; 0b10000010uy |]
                Expect.equal
                    features.ByteArray
                    [| 0b00100000uy; 0b10000010uy |]
                    "unexpected ByteArray value"

            testCase "features dependencies" <| fun _ ->
                let testCases =
                    Map.empty
                    |> Map.add "" true
                    |> Map.add                   "00000000" true
                    |> Map.add                   "01011000" true
                    // gossip_queries_ex depend on gossip_queries
                    |> Map.add "0b000000000000010000000000" false
                    |> Map.add "0b000000000000100000000000" false
                    
                    |> Map.add "0b000000100100000100000000" true
                    |> Map.add "0b000000000000100010000000" true
                    // payment_secret depends on var_onion_optin
                    |> Map.add "0b000000000100000000000000" false
                    // event the feature is set by odd bit(optional), then deps must set flags (either optional/mandatory)
                    |> Map.add "0b000000001000000000000000" false
                    
                    |> Map.add "0b000000000100001000000000" true
                    // basic_mpp depends on payment_secret
                    |> Map.add "0b000000100000000000000000" false
                    |> Map.add "0b000000010000000000000000" false
                    
                    |> Map.add "0b000000101000000000000000" false
                    |> Map.add "0b000000011000000000000000" false
                    |> Map.add "0b000000011000001000000000" true
                    |> Map.add "0b000000100100000100000000" true
                     
                testCases
                |> Map.iter(fun testCase valid ->
                    let ba = testCase |> parseBitArray
                    let result = Feature.validateFeatureGraph (ba)
                    if valid then
                        Expect.isOk (Result.ToFSharpCoreResult result) (testCase)
                    else
                        Expect.isError (Result.ToFSharpCoreResult result) (testCase)
                    )
                
            testCase "features compatibility (in int64)" <| fun _ ->
                let testCases =
                    [
                        1L <<< Feature.OptionDataLossProtect.MandatoryBitPosition, true
                        1L <<< Feature.OptionDataLossProtect.OptionalBitPosition, true
                        
                        1L <<< Feature.InitialRoutingSync.OptionalBitPosition, true
                        
                        1L <<< Feature.OptionUpfrontShutdownScript.MandatoryBitPosition, true
                        1L <<< Feature.OptionUpfrontShutdownScript.OptionalBitPosition, true
                        
                        1L <<< Feature.ChannelRangeQueries.MandatoryBitPosition, true
                        1L <<< Feature.ChannelRangeQueries.OptionalBitPosition, true
                        
                        1L <<< Feature.VariableLengthOnion.MandatoryBitPosition, true
                        1L <<< Feature.VariableLengthOnion.OptionalBitPosition, true
                        
                        1L <<< Feature.ChannelRangeQueriesExtended.MandatoryBitPosition, false
                        1L <<< Feature.ChannelRangeQueriesExtended.OptionalBitPosition, true
                        
                        1L <<< Feature.OptionStaticRemoteKey.MandatoryBitPosition, true
                        1L <<< Feature.OptionStaticRemoteKey.OptionalBitPosition, true
                        
                        1L <<< Feature.PaymentSecret.MandatoryBitPosition, true
                        1L <<< Feature.PaymentSecret.OptionalBitPosition, true
                        
                        1L <<< Feature.BasicMultiPartPayment.MandatoryBitPosition, false
                        1L <<< Feature.BasicMultiPartPayment.OptionalBitPosition, true
                        
                        1L <<< Feature.OptionSupportLargeChannel.MandatoryBitPosition, true
                        1L <<< Feature.OptionSupportLargeChannel.OptionalBitPosition, true
                    ]
                for (s, expected) in testCases do
                    let ba = BitArray.FromInt64(s)
                    Expect.equal(Feature.areSupported(ba)) expected (sprintf "%s" (ba.PrintBits()))
                    
            testCase "features compatibility (in parsed string)" <| fun _ ->
                let testCases =
                    Map.empty
                    |> Map.add "            00000000000000001011" true
                    // option_upfront_shutdown_script
                    |> Map.add "            00000000000000010000" true
                    // gossip_queries (mandatory), gossip_queries_ex (optional)
                    |> Map.add "            00000000100001000000" true
                    // gossip_queries_ex (mandatory)
                    |> Map.add "            00000000010001000000" false
                    // option_static_remote_key
                    |> Map.add "            00000001000000000000" true
                    // initial_routing_sync, payment_secret(mandatory)
                    |> Map.add "            00000100000000001000" true
                    // var_onion_secret(optional) payment_secret(optional)
                    |> Map.add "            00001000001000000000" true
                    // unknown optional feature bits
                    |> Map.add "            10000000000000000000" true
                    |> Map.add "        001000000000000000000000" true
                    // support_large_channel_option(mandatory)
                    |> Map.add "        000001000000000000000000" true
                    // those are useful for nonreg testing of the areSupported method (which needs to be updated with every new supported mandatory bit)
                    |> Map.add "        000100000000000000000000" false
                    |> Map.add "        010000000000000000000000" false
                    |> Map.add "    0001000000000000000000000000" false
                    |> Map.add "    0100000000000000000000000000" false
                    |> Map.add "00010000000000000000000000000000" false
                    |> Map.add "01000000000000000000000000000000" false
                testCases
                |> Map.iter(fun testCase expected ->
                    let fb = testCase |> parseBitArray
                    Expect.equal (Feature.areSupported(fb)) expected (sprintf "%A" (fb.PrintBits()))
                    )
        ]
