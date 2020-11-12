namespace DotNetLightning.Utils
open System
open System.Globalization
open NBitcoin

[<Flags>]
type LNMoneyUnit =
    | BTC = 100000000000UL
    | MilliBTC = 100000000UL
    | Bit = 100000UL
    | Micro = 100000UL
    | Satoshi = 1000UL
    | Nano = 100UL
    | MilliSatoshi = 1UL
    | Pico = 1UL

/// Port from `LightMoney` class in BTCPayServer.Lightning
/// Represents millisatoshi amount of money
///
/// Why not use the package directly? because it might cause circular dependency in the future.
/// (i.e. We might want to support this package in BTCPayServer.Lightning)
/// refs: https://github.com/btcpayserver/BTCPayServer.Lightning/blob/f65a883a63bf607176a3b7b0baa94527ac592f5e/src/BTCPayServer.Lightning.Common/LightMoney.cs
[<Struct>]
type LNMoney = | LNMoney of int64 with

    static member private BitcoinStyle =
        NumberStyles.AllowLeadingWhite ||| NumberStyles.AllowTrailingWhite |||
        NumberStyles.AllowLeadingSign ||| NumberStyles.AllowDecimalPoint
    // --- constructors -----
    static member private CheckMoneyUnit(v: LNMoneyUnit, paramName: string) =
        let typeOfMoneyUnit = typeof<LNMoneyUnit>
        if not (Enum.IsDefined(typeOfMoneyUnit, v)) then
            raise (ArgumentException(sprintf "Invalid value for MoneyUnit %s" paramName))

    static member private FromUnit(amount: decimal, lnUnit: LNMoneyUnit) =
        LNMoney.CheckMoneyUnit(lnUnit, "unit") |> ignore
        let satoshi = Checked.op_Multiply (amount) (decimal lnUnit)
        LNMoney(Checked.int64 satoshi)

    static member FromMoney (money: Money) =
        LNMoney.Satoshis money.Satoshi

    static member Coins(coins: decimal) =
        LNMoney.FromUnit(coins * (decimal LNMoneyUnit.BTC), LNMoneyUnit.MilliSatoshi)

    static member Satoshis(satoshis: decimal) =
        LNMoney.FromUnit(satoshis * (decimal LNMoneyUnit.Satoshi), LNMoneyUnit.MilliSatoshi)

    static member Satoshis(sats: int64) =
        LNMoney.MilliSatoshis(Checked.op_Multiply 1000L sats)

    static member inline Satoshis(sats) =
        LNMoney.Satoshis(int64 sats)

    static member Satoshis(sats: uint64) =
        LNMoney.MilliSatoshis(Checked.op_Multiply 1000UL sats)

    static member MilliSatoshis(sats: int64) =
        LNMoney(sats)

    static member inline MilliSatoshis(sats) =
        LNMoney(int64 sats)

    static member MilliSatoshis(sats: uint64) =
        LNMoney(Checked.int64 sats)

    static member Zero = LNMoney(0L)
    static member One = LNMoney(1L)
    static member TryParse(bitcoin: string, result: outref<LNMoney>) =
        match Decimal.TryParse(bitcoin, LNMoney.BitcoinStyle, CultureInfo.InvariantCulture) with
        | false, _ -> false
        | true, v ->
            try
                result <- LNMoney.FromUnit(v, LNMoneyUnit.BTC)
                true
            with
                | :? OverflowException -> false
    static member Parse(bitcoin: string) =
        match LNMoney.TryParse(bitcoin) with
        | true, v -> v
        | _ -> raise (FormatException("Impossible to parse the string in a bitcoin amount"))

    // -------- Arithmetic operations
    static member (+) (LNMoney a, LNMoney b) = LNMoney(a + b)
    static member (-) (LNMoney a, LNMoney b) = LNMoney(a - b)
    static member (*) (LNMoney a, LNMoney b) = LNMoney(a * b)
    static member (/) (LNMoney a, LNMoney b) = LNMoney(a / b)
    static member inline (/) (LNMoney a, b) = LNMoney(a / (int64 b))
    static member inline (+) (LNMoney a, b) = LNMoney(a + (int64 b))
    static member inline (-) (LNMoney a, b) = LNMoney(a - (int64 b))
    static member inline (*) (LNMoney a, b) = LNMoney(a * (int64 b))
    static member Max(LNMoney a, LNMoney b) = if a >= b then LNMoney a else LNMoney b
    static member Min(LNMoney a, LNMoney b) = if a <= b then LNMoney a else LNMoney b
    
    static member MaxValue =
        let maxSatoshis = 21000000UL * (uint64 Money.COIN)
        LNMoney.Satoshis maxSatoshis

    static member op_Implicit (money: Money) = LNMoney.Satoshis(money.Satoshi)

    // --------- Utilities
    member this.Abs() =
        if this < LNMoney.Zero then LNMoney(-this.Value) else this

    member this.MilliSatoshi = let (LNMoney v) = this in v
    member this.Satoshi = this.MilliSatoshi / 1000L
    member this.BTC = this.MilliSatoshi / (int64 LNMoneyUnit.BTC)
    member this.Value = this.MilliSatoshi
    member this.ToMoney() = this.Satoshi |> Money

    member this.Split(parts: int): LNMoney seq =
        if parts <= 0 then
            raise (ArgumentOutOfRangeException("parts"))
        else
            let mutable remain = 0L
            let res = Math.DivRem(this.MilliSatoshi, int64 parts, &remain)
            seq {
                for _ in 0..(parts - 1) do
                    yield LNMoney.Satoshis (decimal (res + (if remain > 0L then 1L else 0L)))
                    remain <- remain - 1L
            }
