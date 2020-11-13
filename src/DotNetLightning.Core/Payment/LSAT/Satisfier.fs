namespace DotNetLightning.Payment.LSAT

open DotNetLightning.Utils
open Macaroons
open System.Collections.Generic
open System.Runtime.CompilerServices
open NBitcoin

open ResultUtils
open ResultUtils.Portability

/// When we verify a macaroon for its caveats, usually it check each caveats independently.
/// In case of LSAT, this does not work since the validity of a caveat depends on a previous caveat
/// (more specifically, if there were two caveats with a same Condition, we usually check that whether the restriction
/// does not get more loose than before.)
/// So we can not just rely on Macaroon's <see cref="Verifier"/> . The additional check is done with this ISatisfier
/// Interface
type ISatisfier =
    /// This is the left side of caveat equation. e.g. for caveat "service=my_awesome_service", it is "service"
    abstract Condition: string
    /// ensures a caveat is in accordance with a previous one with the same condition. This is needed since caveats
    /// of the same condition can be used multiple times as long as they enforce more permissions than the previous.
    ///
    /// For example, we have a caveat that only allows us to use an LSAT for 7 more days. we can add another caveat
    /// that only allows for 3 more days of use and lend it to another party.
    abstract SatisfyPrevious: Caveat * Caveat -> Result<unit, string>
    /// Satisfies the final caveat of an LSAT. If multiple caveats with the same condition exist, this will only
    /// be executed once all previous caveats are also satisfied.
    abstract SatisfyFinal: Caveat -> Result<unit, string>

type ServiceSatisfier(targetService: string) =
    do
        checkNull "targetService" targetService
        
    interface ISatisfier with
        member this.Condition = "service"
        member this.SatisfyPrevious(prev, curr) =
            result {
                let! prevServiceValue = prev.TryGetValue()
                let! prevServices = Service.ParseMany(prevServiceValue.ToString())
                let prevServices = prevServices |> HashSet
                
                let! currentServiceValue = curr.TryGetValue()
                let! currentServices = Service.ParseMany(currentServiceValue.ToString())
                for s in currentServices do
                    if not <| prevServices.Contains(s) then
                        return! Error(sprintf "Service (%s) was not previously allowed!" s.Name)
                    else
                        ()
            }
        member this.SatisfyFinal(finalCaveat) =
            result {
                let! serviceValue = finalCaveat.TryGetValue()
                let! services = Service.ParseMany(serviceValue.ToString())
                if services |> Seq.exists(fun s -> s.Name = targetService) then
                    return ()
                else
                    return! Error(sprintf "Target service %s not found" targetService)
            }
            
type CapabilitiesSatisfier(service: string, targetCapability: string) =
    do
        checkNull "service" service
        checkNull "targetCapability" targetCapability
    member val Condition = sprintf "%s%s" service Constants.CAPABILITIES_CONDITION_PREFIX with get
    
    interface ISatisfier with
        member this.Condition = this.Condition
        member this.SatisfyPrevious(prev, curr) =
            result {
                let! prevValue = prev.TryGetValue()
                let prevCapabilities = prevValue.Split ',' |> HashSet
                let! currentValue = curr.TryGetValue()
                let currentCapabilities = currentValue.Split ','
                for c in currentCapabilities do
                    if (not <| prevCapabilities.Contains(c)) then
                        return! Error(sprintf "Capability (%A) was not previously allowed!" c)
                    ()
            }
        member this.SatisfyFinal(finalCaveat) =
            result {
                let! caps = finalCaveat.TryGetValue()
                let caps = caps.Split ','
                if (caps |> Seq.exists((=)targetCapability)) then
                    return ()
                else return! Error(sprintf "target capability (%A) not found" targetCapability)
            }
            
[<Extension;AbstractClass;Sealed>]
type MacaroonExtensions() =
    /// Verifies that the macaroon is compliant with LSAT.
    [<Extension>]
    static member VerifyLSATCaveats(macaroon: Macaroon, satisfiers: IList<ISatisfier>, secret: string): VerificationResult =
        macaroon.VerifyLSATCaveats(macaroon.Caveats, satisfiers, secret)
        
    /// Verifies that the macaroon is compliant with LSAT.
    [<Extension>]
    static member VerifyLSATCaveats(macaroon: Macaroon, caveats: IList<Caveat>, satisfiers: IList<ISatisfier>, secret: string): VerificationResult =
        result {
            let caveatSatisfiers = Dictionary()
            for s in satisfiers do
                caveatSatisfiers.AddOrReplace(s.Condition, s)
                
            let relevantCaveats = Dictionary<string, ResizeArray<Caveat>>()
            for c in caveats do
                let! condition = c.TryGetCondition()
                if (caveatSatisfiers.ContainsKey(condition)) then
                    match relevantCaveats.TryGetValue(condition) with
                    | true, caveatsSoFar ->
                        caveatsSoFar.Add(c)
                        relevantCaveats.AddOrReplace(condition, caveatsSoFar)
                    | false, _ ->
                        let cs = ResizeArray()
                        cs.Add(c)
                        relevantCaveats.Add(condition, cs)
            
            for kv in relevantCaveats do
                let condition, caveats = kv.Key, kv.Value
                let s = caveatSatisfiers.[condition]
                for i in 0..(caveats.Count - 2) do
                    let prev = caveats.[i]
                    let curr = caveats.[i + 1]
                    do! s.SatisfyPrevious(prev, curr)
                do! s.SatisfyFinal(caveats |> Seq.last)
        }
        |>
        function
            | Ok _ ->
                // finally check each caveats independently
                let v = Verifier()
                for c in caveats do
                    v.SatisfyExact(c.CId)
                macaroon.Verify(v, secret)
            | Error e ->
                VerificationResult(e)
