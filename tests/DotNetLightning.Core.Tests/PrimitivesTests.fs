module PrimitivesTests

open NBitcoin
open Expecto
open DotNetLightning.Utils

[<Tests>]
let tests = 
    let feeRateFromFeeTest (fee: Money) (weight: uint64) (expected: FeeRatePerKw): unit =
        Expect.equal (FeeRatePerKw.FromFee(fee, weight)) expected "fee rate mismatch"

    testList "fee rate per kw tests" [
        testCase "feeRateFromFee test 0" <| fun _ ->
            feeRateFromFeeTest (Money(1.0m, MoneyUnit.Satoshi)) 1000UL (FeeRatePerKw 1u)
        testCase "feeRateFromFee test 1" <| fun _ ->
            feeRateFromFeeTest (Money(10.0m, MoneyUnit.Satoshi)) 1000UL (FeeRatePerKw 10u)
        testCase "feeRateFromFee test 2" <| fun _ ->
            feeRateFromFeeTest (Money(10.0m, MoneyUnit.Satoshi)) 10000UL (FeeRatePerKw 1u)
    ]

