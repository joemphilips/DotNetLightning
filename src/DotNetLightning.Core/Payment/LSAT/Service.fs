namespace DotNetLightning.Payment.LSAT

open System
open System.Collections.Generic

open Macaroons

open System.Text
open DotNetLightning.Utils

open ResultUtils
open ResultUtils.Portability

/// See: https://github.com/lightninglabs/LSAT/blob/master/macaroons.md#target-services
type Service = {
    Name: string
    ServiceTier: uint8
    Price: LNMoney option
}
    with
    static member Create(name, tier) =
        {
            Name = name
            ServiceTier = tier
            Price = None
        }
    static member ParseMany(s: string) =
        if (String.IsNullOrEmpty(s)) then Error("empty service") else
        let rawServices = s.Split ','
        result {
            let mutable res = ResizeArray()
            for rawService in rawServices do
                let serviceInfo = rawService.Split ':'
                if serviceInfo.Length <> 2 then return! Error(sprintf "Invalid value for service %A" serviceInfo) else
                let (name, tier) = serviceInfo.[0], serviceInfo.[1]
                let! t =
                    match Byte.TryParse tier with
                    | true, t -> Ok t
                    | false, _ -> Error(sprintf "invalid service caveat value %s Service tier must be uint8 (%s)" s tier)
                res.Add(Service.Create(name, t))
            return res
        }
        
    static member EncodeToCaveat(services: IList<Service>): Result<Caveat, _> =
        result {
            if (services.Count = 0) then return! Error("empty service") else
            if (String.IsNullOrEmpty services.[0].Name) then return! Error ("Missing service name") else
            let sb = StringBuilder()
            sb.Append("services=") |> ignore
            sb.Append(sprintf "%s:%d" services.[0].Name services.[0].ServiceTier) |> ignore
            for s in services do
                if (String.IsNullOrEmpty s.Name) then return! Error ("Missing service name") else
                sb.Append(sprintf ",%s:%d" s.Name s.ServiceTier) |> ignore
            return sb.ToString() |> Caveat
        }
