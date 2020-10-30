module TxOutLexicographicCompareTests

open NBitcoin
open Expecto
open DotNetLightning.Utils

[<Tests>]
let tests =
    let test (amount0: int)
             (amount1: int)
             (script0: array<byte>)
             (script1: array<byte>)
             (expected: int) =
        let txOut0 = TxOut(Money amount0, Script script0)
        let txOut1 = TxOut(Money amount1, Script script1)
        let result = TxOut.LexicographicCompare txOut0 txOut1
        Expect.equal
            result
            expected
            (sprintf
                "unexpected comparison result: \
                TxOut.LexicographicCompare(TxOut(%i, %A), TxOut(%i, %A))"
                amount0
                script0
                amount1
                script1
            )

    testList "TxOutLexicographicCompare tests" [
        testCase "smaller amount, shorter pubkey, lower bytes values" <| fun _ ->
            test 1 2 [| 0uy |] [| 1uy; 1uy |] -1
        testCase "smaller amount, shorter pubkey, equal bytes values" <| fun _ ->
            test 1 2 [| 0uy |] [| 0uy; 0uy |] -1
        testCase "smaller amount, shorter pubkey, greater bytes values" <| fun _ ->
            test 1 2 [| 1uy |] [| 0uy; 0uy |] -1
        testCase "smaller amount, equal lengths, lower bytes values" <| fun _ ->
            test 1 2 [| 0uy |] [| 1uy |] -1
        testCase "smaller amount, equal lengths, equal bytes values" <| fun _ ->
            test 1 2 [| 0uy |] [| 0uy |] -1
        testCase "smaller amount, equal lengths, greater bytes values" <| fun _ ->
            test 1 2 [| 1uy |] [| 0uy |] -1
        testCase "smaller amount, longer pubkey, lower bytes values" <| fun _ ->
            test 1 2 [| 0uy; 0uy |] [| 1uy |] -1
        testCase "smaller amount, longer pubkey, equal bytes values" <| fun _ ->
            test 1 2 [| 0uy; 0uy |] [| 0uy |] -1
        testCase "smaller amount, longer pubkey, greater bytes values" <| fun _ ->
            test 1 2 [| 1uy; 1uy |] [| 0uy |] -1

        testCase "same amount, shorter pubkey, lower bytes values" <| fun _ ->
            test 1 1 [| 0uy |] [| 1uy; 1uy |] -1
        testCase "same amount, shorter pubkey, equal bytes values" <| fun _ ->
            test 1 1 [| 0uy |] [| 0uy; 0uy |] -1
        testCase "same amount, shorter pubkey, greater bytes values" <| fun _ ->
            test 1 1 [| 1uy |] [| 0uy; 0uy |] 1
        testCase "same amount, equal lengths, lower bytes values" <| fun _ ->
            test 1 1 [| 0uy |] [| 1uy |] -1
        testCase "same amount, equal lengths, equal bytes values" <| fun _ ->
            test 1 1 [| 0uy |] [| 0uy |] 0
        testCase "same amount, equal lengths, greater bytes values" <| fun _ ->
            test 1 1 [| 1uy |] [| 0uy |] 1
        testCase "same amount, longer pubkey, lower bytes values" <| fun _ ->
            test 1 1 [| 0uy; 0uy |] [| 1uy |] -1
        testCase "same amount, longer pubkey, equal bytes values" <| fun _ ->
            test 1 1 [| 0uy; 0uy |] [| 0uy |] 1
        testCase "same amount, longer pubkey, greater bytes values" <| fun _ ->
            test 1 1 [| 1uy; 1uy |] [| 0uy |] 1

        testCase "greater amount, shorter pubkey, lower bytes values" <| fun _ ->
            test 2 1 [| 0uy |] [| 1uy; 1uy |] 1
        testCase "greater amount, shorter pubkey, equal bytes values" <| fun _ ->
            test 2 1 [| 0uy |] [| 0uy; 0uy |] 1
        testCase "greater amount, shorter pubkey, greater bytes values" <| fun _ ->
            test 2 1 [| 1uy |] [| 0uy; 0uy |] 1
        testCase "greater amount, equal lengths, lower bytes values" <| fun _ ->
            test 2 1 [| 0uy |] [| 1uy |] 1
        testCase "greater amount, equal lengths, equal bytes values" <| fun _ ->
            test 2 1 [| 0uy |] [| 0uy |] 1
        testCase "greater amount, equal lengths, greater bytes values" <| fun _ ->
            test 2 1 [| 1uy |] [| 0uy |] 1
        testCase "greater amount, longer pubkey, lower bytes values" <| fun _ ->
            test 2 1 [| 0uy; 0uy |] [| 1uy |] 1
        testCase "greater amount, longer pubkey, equal bytes values" <| fun _ ->
            test 2 1 [| 0uy; 0uy |] [| 0uy |] 1
        testCase "greater amount, longer pubkey, greater bytes values" <| fun _ ->
            test 2 1 [| 1uy; 1uy |] [| 0uy |] 1
    ]
