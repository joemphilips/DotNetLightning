module DotNetLightning.Tests.PaymentPropertyTests

open DotNetLightning.Payment
open DotNetLightning.Payment.LSAT
open Expecto
open Generators
open ResultUtils


[<Tests>]
let lsatTests =
    let config =
        { FsCheckConfig.defaultConfig with
                arbitrary = [ typeof<PaymentGenerators>; ]
                maxTest = 300
            }
    testList "Macaroon identifier" [
        testPropertyWithConfig config "macaroon identifier serialize/deserialize" <| fun (i: MacaroonIdentifier) ->
            let i2 = MacaroonIdentifier.TryCreateFromBytes(i.ToBytes()) |> Result.deref
            Expect.equal i i2 "failed to de/serialize macaroon id"
    ]
    
