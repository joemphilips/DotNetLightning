module PaymentGenerators

open DotNetLightning.Payment
open DotNetLightning.Payment.LSAT
open DotNetLightning.Utils.Primitives
open FsCheck
open PrimitiveGenerators


let private macaroonIdV1Gen =
    (uint256Gen |> Gen.map(PaymentHash.PaymentHash), uint256Gen)
    ||> Gen.map2(fun p u -> { MacaroonIdentifierV0.PaymentHash = p
                              TokenId = u })
    |> Gen.map(MacaroonIdentifier.V0)   
let private macaroonUnknownIdGen(knownVersions: uint16 seq) =
    gen {
        let! t =
            Arb.generate<uint16>
            |> Gen.filter(fun v -> not <| (Seq.exists(fun knownInt -> v = knownInt) knownVersions))
        let! v = Arb.generate<NonNull<byte[]>>
        return MacaroonIdentifier.UnknownVersion(t, v.Get)
    }

let macaroonIdGen: Gen<MacaroonIdentifier> =
    Gen.oneof [
        macaroonIdV1Gen
        macaroonUnknownIdGen([0us])
    ]
