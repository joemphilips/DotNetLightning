module DotNetLightning.Tests.PaymentPropertyTests


open System
open DotNetLightning.Payment
open Expecto
open PaymentGenerators
open FsCheck
open Generators
open ResultUtils
type PaymentRequestGenerators =
  static member PaymentRequest() =
    paymentRequestGen
    |> Arb.fromGen


[<Tests>]
let tests =
  let config = {
    FsCheckConfig.defaultConfig with
      arbitrary = [ typeof<PaymentRequestGenerators> ]
      maxTest = 100
  }
  testList "PaymentRequest property tests" [
    testPropertyWithConfig config "PaymentRequest Serialization" <| fun (p: PaymentRequest) ->
      let p2 = p.ToString() |> PaymentRequest.Parse |> Result.deref
      Expect.equal p (p2) (sprintf "PaymentRequest before/after serialization is not equal %A" p)
  ]

#if BouncyCastle
open DotNetLightning.Payment.LSAT
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

