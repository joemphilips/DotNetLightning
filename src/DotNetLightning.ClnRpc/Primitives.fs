namespace DotNetLightning.ClnRpc

open System

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

[<Measure>]
type msat

type AmountOrAny =
    | Amount of int64<msat>
    | Any

type AmountOrAll =
    | Amount of int64<msat>
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
    let parseClnAmount(s: string) : int64<msat> =
        if s |> String.IsNullOrWhiteSpace then
            raise <| FormatException($"Invalid string for money. null")
        else if s.EndsWith("msat") then
            s.Substring(0, s.Length - 4) |> int64 |> unbox
        else if s.EndsWith("sat") then
            s.Substring(0, s.Length - 3) |> int64 |> unbox
        else if s.EndsWith("btc") then
            s.Substring(0, s.Length - 3) |> int64 |> unbox
        else
            raise <| FormatException $"Invalid string for money {s}"
