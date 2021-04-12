module DotNetLightning.Tests.PaymentPropertyTests

#if BouncyCastle

open DotNetLightning.Payment
open DotNetLightning.Payment.LSAT
open Expecto
open PaymentGenerators
open FsCheck
open Generators
open ResultUtils

type PaymentGenerators =
    static member MacaroonIdentifier: Arbitrary<MacaroonIdentifier> =
        macaroonIdGen |> Arb.fromGen

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

#endif

