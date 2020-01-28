module DotNetLightning.Tests.PaymentTests

open System
open System.IO
open System.Text.Json

open DotNetLightning.Payment
open DotNetLightning.Utils

open ResultUtils
open Expecto
open NBitcoin

[<Tests>]
let tests =
    let dataPath1 = Path.Join(AppDomain.CurrentDomain.BaseDirectory, "../../..", "Data/bolt11.json")
    let data1 = dataPath1 |> File.ReadAllText |> JsonDocument.Parse
    let hex = NBitcoin.DataEncoders.HexEncoder()
    let priv = data1.RootElement.GetProperty("priv_key").GetString() |> hex.DecodeData |> Key

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
            
        ftestCase "Please make a donation of any amount using payment_hash 0001020304050607080900010203040506070809000102030405060708090102 to me @03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad" <| fun _ ->
            let data = "lnbc1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdpl2pkx2ctnv5sxxmmwwd5kgetjypeh2ursdae8g6twvus8g6rfwvs8qun0dfjkxaq8rkx3yf5tcsyz3d73gafnh3cax9rn449d9p5uxz9ezhhypd0elx87sjle52x86fux2ypatgddc6k63n7erqz25le42c4u4ecky03ylcqca784w"
            let d = PaymentRequest.Parse(data) |> Result.deref
            Expect.equal (d.PrefixValue) ("lnbc") ""
            Expect.isTrue (d.AmountValue.IsNone) ""
            Expect.equal (d.PaymentHash.Value) (PaymentHash(uint256.Parse("0001020304050607080900010203040506070809000102030405060708090102"))) ""
            Expect.equal (d.TimestampValue.ToUnixTimeSeconds()) (1496314658L) ""
            Expect.equal (d.NodeIdValue) ("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad" |> hex.DecodeData |> PubKey |> NodeId) ""
            Expect.equal (d.Description) (Some(Choice1Of2 "Please consider supporting this project")) ""
            Expect.equal (d.TagsValue.Length) (2) ""
            Expect.equal (d.ToString(d.Sign(priv))) data ""
            
        testCase "" <| fun _ ->
            failwith ""
        testCase "" <| fun _ ->
            failwith ""
        testCase "" <| fun _ ->
            failwith ""
        testCase "" <| fun _ ->
            failwith ""
        testCase "" <| fun _ ->
            failwith ""
        testCase "" <| fun _ ->
            failwith ""
        testCase "" <| fun _ ->
            failwith ""
        testCase "" <| fun _ ->
            failwith ""
        testCase "" <| fun _ ->
            failwith ""
        testCase "" <| fun _ ->
            failwith ""
        testCase "" <| fun _ ->
            failwith ""
        testCase "" <| fun _ ->
            failwith ""
    ]
