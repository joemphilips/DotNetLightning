namespace DotNetLightning.ClnRpc

open System
open DotNetLightning.Utils

/// <namespacedoc>
///     <summary>
///        "DotNetLightning.ClnRpc" contains a type-safe client for calling
///        c-lightning rpc.
///     </summary>
/// </namespacedoc>
/// <exclude />
module NamespaceDocDummy =
    ()

// fsharplint:disable enumCasesNames
type ChannelState =
    | OPENINGD = 0
    | CHANNELD_AWAITING_LOCKIN = 1
    | CHANNELD_NORMAL = 2
    | CHANNELD_SHUTTING_DOWN = 3
    | CLOSINGD_SIGEXCHANGE = 4
    | CLOSINGD_COMPLETE = 5
    | AWAITING_UNILATERAL = 6
    | FUNDING_SPEND_SEEN = 7
    | ONCHAIN = 8
    | DUALOPEND_OPEN_INIT = 9
    | DUALOPEND_AWAITING_LOCKIN = 10

// fsharplint:enable

type ChannelStateChangeCause =
    | [<System.Runtime.Serialization.EnumMember(Value = "unknown")>] UNKNOWN = 0
    | [<System.Runtime.Serialization.EnumMember(Value = "local")>] LOCAL = 1
    | [<System.Runtime.Serialization.EnumMember(Value = "user")>] USER = 2
    | [<System.Runtime.Serialization.EnumMember(Value = "remote")>] REMOTE = 3
    | [<System.Runtime.Serialization.EnumMember(Value = "protocol")>] PROTOCOL = 4
    | [<System.Runtime.Serialization.EnumMember(Value = "onchain")>] ONCHAIN = 5

type AmountOrAny =
    | Amount of LNMoney
    | Any

type AmountOrAll =
    | Amount of LNMoney
    | All

type Feerate =
    | Slow
    | Normal
    | Urgent
    | PerKb of uint32
    | PerKw of uint32

    override this.ToString() =
        match this with
        | Slow -> "slow"
        | Normal -> "normal"
        | Urgent -> "urgent"
        | PerKb v -> $"{v} perkb"
        | PerKw v -> $"{v} perkw"

type ChannelSide =
    | [<System.Runtime.Serialization.EnumMember(Value = "local")>] LOCAL = 0
    | [<System.Runtime.Serialization.EnumMember(Value = "remote")>] REMOTE = 1

[<AutoOpen>]
module internal PrimitiveExtensions =
    let parseClnAmount(s: string) : LNMoney =
        if s |> String.IsNullOrWhiteSpace then
            raise <| FormatException($"Invalid string for money. null")

        let v =
            if s.EndsWith("msat") then
                s.Substring(0, s.Length - 4) |> uint64
            else if s.EndsWith("sat") then
                s.Substring(0, s.Length - 3) |> uint64 |> (*) 1000UL
            else if s.EndsWith("btc") then
                s.Substring(0, s.Length - 3)
                |> int64
                |> fun i -> LNMoney.Coins(i).MilliSatoshi |> uint64
            else
                raise <| FormatException $"Invalid string for money {s}"

        // some rpc endpoint returns UInt64.MaxValue to represent "arbitrary
        // big number" (e.g. `htlc_max_value_in_flight_msat` in `listpeers`)
        // we represent sats (and msats) as int64, thus it will throw
        // overflow exception when parsing.
        // so we use `Int64.MaxValue` for that case, this is fine because
        // technically speaking largest possible number for msat is
        // 2_100_000_000_000_000 * 1_000
        // which is smaller than `Int64.MaxValue`
        if v > (Int64.MaxValue |> uint64) then
            Int64.MaxValue |> LNMoney.MilliSatoshis
        else
            v |> int64 |> LNMoney.MilliSatoshis
