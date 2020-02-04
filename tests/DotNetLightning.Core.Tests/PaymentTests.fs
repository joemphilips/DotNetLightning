module DotNetLightning.Tests.PaymentTests

open System
open System.IO
open System.Text.Json

open DotNetLightning.Payment
open DotNetLightning.Utils

open DotNetLightning.Utils
open ResultUtils
open Expecto
open NBitcoin
open NBitcoin.Crypto

[<Tests>]
let tests =
    let hex = NBitcoin.DataEncoders.HexEncoder()
    let ascii = System.Text.ASCIIEncoding.ASCII
    let priv = "e126f68f7eafcc8b74f54d269fe206be715000f94dac067d1c04a8ca3b2db734" |> hex.DecodeData |> Key
    let msgSigner  = { new IMessageSigner
                       with
                           member this.SignMessage(data) = let signature = priv.SignCompact(data) in signature }

    testList "BOLT-11 tests" [
        testCase "check minimal unit is used" <| fun _ ->
            Expect.equal 'p' (Amount.unit(LNMoney.MilliSatoshis(1L))) ""
            Expect.equal 'p' (Amount.unit(LNMoney.MilliSatoshis(99L))) ""
            Expect.equal 'n' (Amount.unit(LNMoney.MilliSatoshis(100L))) ""
            Expect.equal 'p' (Amount.unit(LNMoney.MilliSatoshis(101L))) ""
            Expect.equal 'n' (Amount.unit(LNMoney.Satoshis(1L))) ""
            Expect.equal 'u' (Amount.unit(LNMoney.Satoshis(100L))) ""
            Expect.equal 'n' (Amount.unit(LNMoney.Satoshis(101L))) ""
            Expect.equal 'u' (Amount.unit(LNMoney.Satoshis(1155400L))) ""
            Expect.equal 'm' (Amount.unit(LNMoney.Coins(1m / 1000m))) ""
            Expect.equal 'm' (Amount.unit(LNMoney.Coins(10m / 1000m))) ""
            Expect.equal 'm' (Amount.unit(LNMoney.Coins(1m))) ""
            
        testCase "check that we can still decode non-minimal amount encoding" <| fun _ ->
            Expect.equal (Amount.decode("1000u")) (Ok(LNMoney.MilliSatoshis(100000000L))) ""
            Expect.equal (Amount.decode("1000000n")) (Ok(LNMoney.MilliSatoshis(100000000L))) ""
            Expect.equal (Amount.decode("1000000000p")) (Ok(LNMoney.MilliSatoshis(100000000L))) ""
            
        testCase "Please make a donation of any amount using payment_hash 0001020304050607080900010203040506070809000102030405060708090102 to me @03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad" <| fun _ ->
            let data = "lnbc1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdpl2pkx2ctnv5sxxmmwwd5kgetjypeh2ursdae8g6twvus8g6rfwvs8qun0dfjkxaq8rkx3yf5tcsyz3d73gafnh3cax9rn449d9p5uxz9ezhhypd0elx87sjle52x86fux2ypatgddc6k63n7erqz25le42c4u4ecky03ylcqca784w"
            let d = PaymentRequest.Parse(data) |> Result.deref
            Expect.equal (d.PrefixValue) ("lnbc") ""
            Expect.isTrue (d.AmountValue.IsNone) ""
            Expect.equal (d.PaymentHash) (PaymentHash(uint256.Parse("0001020304050607080900010203040506070809000102030405060708090102"))) ""
            Expect.equal (d.TimestampValue.ToUnixTimeSeconds()) (1496314658L) ""
            Expect.equal (d.NodeIdValue) ("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad" |> hex.DecodeData |> PubKey |> NodeId) ""
            Expect.equal (d.Description) (Choice1Of2 "Please consider supporting this project") ""
            Expect.equal (d.TagsValue.Fields.Length) (2) ""
            Expect.equal (d.ToString()) data ""
            Expect.equal (d.ToString(msgSigner)) data ""
            
        testCase "Please send $3 for a cup of coffee to the same peer, within one minute" <| fun _ ->
            let data = "lnbc2500u1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5xysxxatsyp3k7enxv4jsxqzpuaztrnwngzn3kdzw5hydlzf03qdgm2hdq27cqv3agm2awhz5se903vruatfhq77w3ls4evs3ch9zw97j25emudupq63nyw24cg27h2rspfj9srp"
            let pr = PaymentRequest.Parse(data) |> Result.deref
            Expect.equal pr.PrefixValue "lnbc" ""
            Expect.equal pr.AmountValue (Some(250000000L |> LNMoney.MilliSatoshis)) ""
            Expect.equal pr.PaymentHash.Value ("0001020304050607080900010203040506070809000102030405060708090102" |> uint256.Parse) ""
            Expect.equal pr.TimestampValue (DateTimeOffset.FromUnixTimeSeconds 1496314658L) ""
            Expect.equal pr.NodeIdValue ("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad" |> hex.DecodeData |> PubKey |> NodeId) ""
            Expect.equal pr.Description (Choice1Of2 "1 cup coffee") ""
            Expect.equal pr.FallbackAddresses ([]) ""
            Expect.equal (pr.TagsValue.Fields.Length) 3 ""
            Expect.equal (pr.ToString()) data ""
            Expect.equal (pr.ToString(msgSigner)) data ""
            
        testCase "Please send 0.0025 BTC for a cup of nonsense (ナンセンス 1杯) to the same peer, within one minute" <| fun _ ->
            let data = "lnbc2500u1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdpquwpc4curk03c9wlrswe78q4eyqc7d8d0xqzpuyk0sg5g70me25alkluzd2x62aysf2pyy8edtjeevuv4p2d5p76r4zkmneet7uvyakky2zr4cusd45tftc9c5fh0nnqpnl2jfll544esqchsrny"
            let pr = PaymentRequest.Parse(data) |> Result.deref
            Expect.equal (pr.ToString()) data ""
            ()
            
        testCase "Now send $24 for an entire list of things (hashed)" <| fun _ ->
            let data = "lnbc20m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqscc6gd6ql3jrc5yzme8v4ntcewwz5cnw92tz0pc8qcuufvq7khhr8wpald05e92xw006sq94mg8v2ndf4sefvf9sygkshp5zfem29trqq2yxxz7"
            let pr = PaymentRequest.Parse(data) |> Result.deref
            Expect.equal pr.PrefixValue "lnbc" ""
            Expect.equal pr.AmountValue (2000000000L |> LNMoney.MilliSatoshis |> Some) ""
            Expect.equal pr.PaymentHash ("0001020304050607080900010203040506070809000102030405060708090102" |> uint256.Parse |> PaymentHash) ""
            Expect.equal pr.TimestampValue (DateTimeOffset.FromUnixTimeSeconds 1496314658L) ""
            Expect.equal pr.NodeIdValue ("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad" |> hex.DecodeData |> PubKey |> NodeId) ""
            Expect.equal pr.Description ("One piece of chocolate cake, one icecream cone, one pickle, one slice of swiss cheese, one slice of salami, one lollypop, one piece of cherry pie, one sausage, one cupcake, and one slice of watermelon" |> ascii.GetBytes |> Hashes.SHA256 |> uint256 |> Choice2Of2) ""
            Expect.equal pr.FallbackAddresses [] ""
            Expect.equal pr.TagsValue.Fields.Length 2 ""
            Expect.equal (pr.ToString()) data ""
            Expect.equal (pr.ToString(msgSigner)) data ""
            
        testCase "The same, on testnet, with a fallback address mk2QpYatsKicvFVuTAQLBryyccRXMUaGHP" <| fun _ ->
            let data = "lntb20m1pvjluezhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqspp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqfpp3x9et2e20v6pu37c5d9vax37wxq72un98kmzzhznpurw9sgl2v0nklu2g4d0keph5t7tj9tcqd8rexnd07ux4uv2cjvcqwaxgj7v4uwn5wmypjd5n69z2xm3xgksg28nwht7f6zspwp3f9t"
            let pr = PaymentRequest.Parse(data) |> Result.deref
            Expect.equal pr.PrefixValue "lntb" ""
            Expect.equal pr.AmountValue (2000000000L |> LNMoney.MilliSatoshis |> Some) ""
            Expect.equal pr.PaymentHash ("0001020304050607080900010203040506070809000102030405060708090102" |> uint256.Parse |> PaymentHash) ""
            Expect.equal (pr.TimestampValue.ToUnixTimeSeconds()) 1496314658L ""
            Expect.equal (pr.NodeIdValue) ("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad" |> hex.DecodeData |> PubKey |> NodeId) ""
            Expect.equal (pr.FallbackAddresses) (["mk2QpYatsKicvFVuTAQLBryyccRXMUaGHP"]) ""
            Expect.equal (pr.TagsValue.Fields.Length) 3 ""
            Expect.equal (pr.ToString()) data ""
            Expect.equal (pr.ToString(msgSigner)) data ""
            
        testCase "On mainnet, with fallback address 1RustyRX2oai4EYYDpQGWvEL62BBGqN9T with extra routing info to go via nodes 029e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255 then 039e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255" <| fun _ ->
            let data = "lnbc20m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqsfpp3qjmp7lwpagxun9pygexvgpjdc4jdj85fr9yq20q82gphp2nflc7jtzrcazrra7wwgzxqc8u7754cdlpfrmccae92qgzqvzq2ps8pqqqqqqpqqqqq9qqqvpeuqafqxu92d8lr6fvg0r5gv0heeeqgcrqlnm6jhphu9y00rrhy4grqszsvpcgpy9qqqqqqgqqqqq7qqzqj9n4evl6mr5aj9f58zp6fyjzup6ywn3x6sk8akg5v4tgn2q8g4fhx05wf6juaxu9760yp46454gpg5mtzgerlzezqcqvjnhjh8z3g2qqdhhwkj"
            let pr = PaymentRequest.Parse(data) |> Result.deref
            Expect.equal pr.PrefixValue "lnbc" ""
            Expect.equal pr.AmountValue (2000000000L |> LNMoney.MilliSatoshis |> Some) ""
            Expect.equal pr.PaymentHash ("0001020304050607080900010203040506070809000102030405060708090102" |> uint256.Parse |> PaymentHash) ""
            Expect.equal (pr.TimestampValue.ToUnixTimeSeconds()) 1496314658L ""
            Expect.equal (pr.NodeIdValue) ("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad" |> hex.DecodeData |> PubKey |> NodeId) ""
            Expect.equal (pr.FallbackAddresses) (["1RustyRX2oai4EYYDpQGWvEL62BBGqN9T"]) ""
            let routingInfo =
                [ { ExtraHop.NodeId = "029e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255" |> hex.DecodeData |> PubKey |> NodeId
                    ShortChannelId = ShortChannelId.FromUInt64(72623859790382856UL)
                    FeeBase = 1L |> LNMoney.MilliSatoshis
                    FeeProportionalMillionths = 20L
                    CLTVExpiryDelta = 3us |> BlockHeightOffset }
                  { ExtraHop.NodeId = "039e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255" |> hex.DecodeData |> PubKey |> NodeId
                    ShortChannelId =  217304205466536202UL |> ShortChannelId.FromUInt64
                    FeeBase = 2L |> LNMoney.MilliSatoshis
                    FeeProportionalMillionths = 30L
                    CLTVExpiryDelta = 4us |> BlockHeightOffset }
                ]
            Expect.equal pr.RoutingInfo ([routingInfo]) ""
            Expect.equal pr.TagsValue.Fields.Length 4 ""
            Expect.equal (pr.ToString()) data ""
            Expect.equal (pr.ToString(msgSigner)) data ""
            
        testCase "On mainnet, with fallback (P2SH) address 3EktnHQD7RiAE6uzMj2ZifT9YgRrkSgzQX" <| fun _ ->
            let data = "lnbc20m1pvjluezhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqspp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqfppj3a24vwu6r8ejrss3axul8rxldph2q7z9kmrgvr7xlaqm47apw3d48zm203kzcq357a4ls9al2ea73r8jcceyjtya6fu5wzzpe50zrge6ulk4nvjcpxlekvmxl6qcs9j3tz0469gq5g658y"
            let pr = PaymentRequest.Parse(data) |> Result.deref
            Expect.equal pr.PrefixValue "lnbc" ""
            Expect.equal pr.PaymentHash ("0001020304050607080900010203040506070809000102030405060708090102" |> uint256.Parse |> PaymentHash) ""
            Expect.equal (pr.TimestampValue.ToUnixTimeSeconds()) 1496314658L ""
            Expect.equal (pr.NodeIdValue) ("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad" |> hex.DecodeData |> PubKey |> NodeId) ""
            Expect.equal (pr.Description) ("One piece of chocolate cake, one icecream cone, one pickle, one slice of swiss cheese, one slice of salami, one lollypop, one piece of cherry pie, one sausage, one cupcake, and one slice of watermelon" |> ascii.GetBytes |> Hashes.SHA256 |> uint256 |> Choice2Of2) ""
            Expect.equal (pr.FallbackAddresses) (["3EktnHQD7RiAE6uzMj2ZifT9YgRrkSgzQX"]) ""
            Expect.equal (pr.TagsValue.Fields.Length) 3 ""
            Expect.equal (pr.ToString()) data ""
            Expect.equal (pr.ToString(msgSigner)) data ""
            
        testCase "On mainnet, with fallback (P2WPKH) address bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4" <| fun _ ->
            let data = "lnbc20m1pvjluezhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqspp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqfppqw508d6qejxtdg4y5r3zarvary0c5xw7kepvrhrm9s57hejg0p662ur5j5cr03890fa7k2pypgttmh4897d3raaq85a293e9jpuqwl0rnfuwzam7yr8e690nd2ypcq9hlkdwdvycqa0qza8"
            let pr = PaymentRequest.Parse(data) |> Result.deref
            Expect.equal pr.PrefixValue "lnbc" ""
            Expect.equal pr.PaymentHash ("0001020304050607080900010203040506070809000102030405060708090102" |> uint256.Parse |> PaymentHash) ""
            Expect.equal (pr.TimestampValue.ToUnixTimeSeconds()) 1496314658L ""
            Expect.equal (pr.NodeIdValue) ("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad" |> hex.DecodeData |> PubKey |> NodeId) ""
            Expect.equal (pr.Description) ("One piece of chocolate cake, one icecream cone, one pickle, one slice of swiss cheese, one slice of salami, one lollypop, one piece of cherry pie, one sausage, one cupcake, and one slice of watermelon" |> ascii.GetBytes |> Hashes.SHA256 |> uint256 |> Choice2Of2) ""
            Expect.equal (pr.FallbackAddresses) (["bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"]) ""
            Expect.isNone (pr.Features) ""
            Expect.equal (pr.ToString()) data ""
            Expect.equal (pr.ToString(msgSigner)) data ""
            
        testCase "On mainnet, with fallback (P2WSH) address bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3" <| fun _ ->
            let data = "lnbc20m1pvjluezhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqspp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqfp4qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q28j0v3rwgy9pvjnd48ee2pl8xrpxysd5g44td63g6xcjcu003j3qe8878hluqlvl3km8rm92f5stamd3jw763n3hck0ct7p8wwj463cql26ava"
            let pr = PaymentRequest.Parse(data) |> Result.deref
            Expect.equal pr.PrefixValue "lnbc" ""
            Expect.equal pr.PaymentHash ("0001020304050607080900010203040506070809000102030405060708090102" |> uint256.Parse |> PaymentHash) ""
            Expect.equal (pr.TimestampValue.ToUnixTimeSeconds()) 1496314658L ""
            Expect.equal (pr.NodeIdValue) ("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad" |> hex.DecodeData |> PubKey |> NodeId) ""
            Expect.equal (pr.Description) ("One piece of chocolate cake, one icecream cone, one pickle, one slice of swiss cheese, one slice of salami, one lollypop, one piece of cherry pie, one sausage, one cupcake, and one slice of watermelon" |> ascii.GetBytes |> Hashes.SHA256 |> uint256 |> Choice2Of2) ""
            Expect.equal (pr.FallbackAddresses) (["bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3"]) ""
            Expect.equal (pr.ToString()) data ""
            Expect.equal (pr.ToString(msgSigner)) data ""
            
        testCase "Please send $30 for coffee beans to the same peer, which supports features 9, 15 and 99, using secret 0x1111111111111111111111111111111111111111111111111111111111111111" <| fun _ ->
            let data = "lnbc25m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5vdhkven9v5sxyetpdeessp5zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zygs9q5sqqqqqqqqqqqqqqqpqsq67gye39hfg3zd8rgc80k32tvy9xk2xunwm5lzexnvpx6fd77en8qaq424dxgt56cag2dpt359k3ssyhetktkpqh24jqnjyw6uqd08sgptq44qu"
            let pr = PaymentRequest.Parse(data) |> Result.deref
            Expect.equal pr.PrefixValue "lnbc" ""
            Expect.equal pr.PaymentHash ("0001020304050607080900010203040506070809000102030405060708090102" |> uint256.Parse |> PaymentHash) ""
            Expect.equal (pr.TimestampValue.ToUnixTimeSeconds()) 1496314658L ""
            Expect.equal (pr.NodeIdValue) ("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad" |> hex.DecodeData |> PubKey |> NodeId) ""
            Expect.equal (pr.Description) (Choice1Of2 "coffee beans") ""
            Expect.isEmpty pr.FallbackAddresses ""
            Expect.isSome (pr.Features) ""
            Expect.equal (pr.ToString()) data ""
            Expect.equal (pr.ToString(msgSigner)) data ""
            
        testCase "Same, but adding invalid unknown feature 100" <| fun _ ->
            let data = "lnbc25m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5vdhkven9v5sxyetpdeessp5zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zygs9q4psqqqqqqqqqqqqqqqpqsqq40wa3khl49yue3zsgm26jrepqr2eghqlx86rttutve3ugd05em86nsefzh4pfurpd9ek9w2vp95zxqnfe2u7ckudyahsa52q66tgzcp6t2dyk"
            let pr = PaymentRequest.Parse(data) |> Result.deref
            Expect.isSome (pr.Features) ""
            Expect.equal (pr.ToString()) data ""
            Expect.equal (pr.ToString(msgSigner)) data ""
    ]
