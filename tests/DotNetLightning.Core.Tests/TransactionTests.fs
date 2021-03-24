module TransactionTests

open ResultUtils

open DotNetLightning.Transactions
open DotNetLightning.Transactions.Transactions
open DotNetLightning.Utils
open DotNetLightning.Crypto
open Expecto
open NBitcoin

let n = Network.RegTest

[<Tests>]
let testList = [
    testCase "check pre-computed transaction weights" <| fun _ ->
        let localPaymentPriv = [| for _ in 0..31 -> 0xdduy |] |> fun b -> new Key(b)
        let finalSpk =
            let s = [| for _ in 0..31 -> 0xfeuy |] |> fun b -> new Key(b)
            s.PubKey.WitHash
        let localDustLimit = 546L |> Money.Satoshis
        let feeRatePerKw = 1000u |> FeeRatePerKw
        
        let _ =
            let pubkeyScript = localPaymentPriv.PubKey.WitHash.ScriptPubKey
            let commitTx =
                let t = n.CreateTransaction()
                t.Version <- 0u
                t.Outputs.Add(TxOut(Money.Satoshis(20000L), pubkeyScript)) |> ignore
                t.LockTime <- LockTime.Zero
                t
            let claimP2WPKHOutputTx =
                Transactions.makeClaimP2WPKHOutputTx(commitTx)
                                                    (localDustLimit)
                                                    (PaymentPubKey localPaymentPriv.PubKey)
                                                    (finalSpk)
                                                    (feeRatePerKw)
                                                    n |> Result.defaultWith (fun _  -> failwith "fail: precomputed tx weights")
            let weight =
                let tx = claimP2WPKHOutputTx.Value.GetGlobalTransaction()
                let witScript =
                    let dummySig = [| for _ in 0..70 -> 0xbbuy |]
                    let dummyPk = (new Key()).PubKey.ToBytes()
                    let dummy = seq[ Op.GetPushOp(dummySig); Op.GetPushOp(dummyPk)]
                    Script(dummy).ToWitScript()
                tx.Inputs.[0].WitScript <- witScript
                tx.GetVirtualSize() |> uint64
            Expect.equal(Constants.CLAIM_P2WPKH_OUTPUT_WEIGHT) (weight) ""
            ()
            
        ()
]
