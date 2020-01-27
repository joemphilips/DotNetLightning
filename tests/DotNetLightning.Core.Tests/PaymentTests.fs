module DotNetLightning.Tests.PaymentTests

open System
open System.IO
open System.Text.Json

open DotNetLightning.Payment

open DotNetLightning.Utils
open Expecto
open NBitcoin

[<Tests>]
let tests =
    let dataPath1 = Path.Join(AppDomain.CurrentDomain.BaseDirectory, "../../..", "Data/bolt11.json")
    let data1 = dataPath1 |> File.ReadAllText |> JsonDocument.Parse
    let hex = NBitcoin.DataEncoders.HexEncoder()
    let priv = data1.RootElement.GetProperty("priv_key").GetString() |> hex.DecodeData |> Key

    testList "BOLT-11 tests" [
        ftestCase "check minimal unit is used" <| fun _ ->
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
            ()
    ]
