namespace DotNetLightning.Payment

open System
open DotNetLightning.Utils

open ResultUtils
open ResultUtils.Portability

[<RequireQualifiedAccess>]
module Amount =
    let unit (amount: LNMoney): char =
        match amount.MilliSatoshi * 10L with
        | pico when pico % 1000L > 0L -> 'p'
        | nano when nano % 1000000L > 0L -> 'n'
        | micro when micro % 1000000000L > 0L -> 'u'
        | _ -> 'm'
        
    let decode (input: string): Result<LNMoney, string> =
        let parseCore (a: string) multi =
            match a |> Int64.TryParse with
            | true, aValue ->
                LNMoney.MilliSatoshis(aValue * multi) |> Ok
            | false, _ -> sprintf "Could not parse %s into Int64" a |> Error
        match input with
        | "" -> Error "Empty input"
        | a when a.[a.Length - 1] = 'p' ->
            (parseCore (a.Substring(0, a.Length - 1)) 1L)
            // 1 milli-satoshis == 10 pico-bitcoin, so we must divide it here.
            |> Result.map(fun lnMoney -> (lnMoney.MilliSatoshi / 10L) |> LNMoney.MilliSatoshis)
        | a when a.[a.Length - 1] = 'n' ->
            parseCore (a.Substring(0, a.Length - 1)) 100L
        | a when a.[a.Length - 1] = 'u' ->
            parseCore (a.Substring(0, a.Length - 1)) 100000L
        | a when a.[a.Length - 1] = 'm' ->
            parseCore (a.Substring(0, a.Length - 1)) 100000000L
        | a ->
            parseCore a 100000000000L
            
    let encode (amount: LNMoney option) =
        match amount with
        | None -> ""
        | Some(amt) when unit(amt) = 'p' -> sprintf "%dp" (amt.MilliSatoshi * 10L)
        | Some(amt) when unit(amt) = 'n' -> sprintf "%dn" (amt.MilliSatoshi / 100L )
        | Some(amt) when unit(amt) = 'u' -> sprintf "%du" (amt.MilliSatoshi / 100000L)
        | Some(amt) when unit(amt) = 'm' -> sprintf "%dm" (amt.MilliSatoshi / 100000000L)
        | x -> failwithf "Unreachable! %A" x
