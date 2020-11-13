namespace DotNetLightning.Payment.LSAT

open Macaroons
open System.Runtime.CompilerServices

open ResultUtils
open ResultUtils.Portability

[<Extension;AbstractClass;Sealed>]
type CaveatsExtensions() =
    
    /// 'Value' is right hand of '=' in caveats, if it does not contain '=', it will return Error
    [<Extension>]
    static member TryGetValue(caveat: Caveat) =
        let s = caveat.ToString().Split('=')
        if (s.Length <> 2) then Error(sprintf "invalid caveat for lsat %s" (caveat.ToString())) else
        Ok(s.[1].Trim())
        
    /// 'Condition' is left hand of '=' in caveats, if it does not contain '=', it will return Error
    [<Extension>]
    static member TryGetCondition(caveat: Caveat) =
        let s = caveat.ToString().Split('=')
        if (s.Length <> 2) then Error(sprintf "invalid caveat for lsat %s" (caveat.ToString())) else
        Ok(s.[0].Trim())

