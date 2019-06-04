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

module SerializationTest =
    let hex = NBitcoin.DataEncoders.HexEncoder()
    let base64 = NBitcoin.DataEncoders.Base64Encoder()
    let ascii = System.Text.ASCIIEncoding.ASCII
    let signMessageWith (privKey: Key) (msgHash: string) =
        let msgBytes = msgHash |> ascii.GetBytes
        printfn "%A, %d" msgBytes msgBytes.Length
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
                printfn "sig1 is DER: %s\nCompact representation: %s" (sig1.ToDER() |> hex.EncodeData) (sig1.ToBytesCompact() |> hex.EncodeData)
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
      ]
