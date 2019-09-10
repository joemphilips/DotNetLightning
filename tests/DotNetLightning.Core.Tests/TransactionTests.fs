module TransactionTests

open DotNetLightning.Transactions
open DotNetLightning.Transactions.Transactions
open DotNetLightning.Utils
open DotNetLightning.Utils.Primitives
open Expecto
open NBitcoin

let n = Network.RegTest

[<Tests>]
let testList = [
    testCase "check pre-computed transaction weights" <| fun _ ->
        let localRevocationPriv = [| for _ in 0..31 -> 0xccuy |] |> Key
        let localPaymentPriv = [| for _ in 0..31 -> 0xdduy |] |> Key
        let remotePaymentPriv = [| for _ in 0..31 -> 0xeeuy |] |> Key
        let localHtlcPriv = [| for _ in 0..31 -> 0xeauy |] |> Key
        let remoteHtlcPriv = [| for _ in 0..31 -> 0xebuy |] |> Key
        let localFinalPriv = [| for _ in 0..31 -> 0xffuy |] |> Key
        let finalSpk =
            let s = [| for _ in 0..31 -> 0xfeuy |] |> Key
            s.PubKey.WitHash
        let localDustLimit = 546L |> Money.Satoshis
        let toLocalDelay= 144us |> BlockHeightOffset
        let feeRatePerKw = 1000u |> FeeRatePerKw
        
        let _ =
            let pubkeyScript = localPaymentPriv.PubKey.WitHash.ScriptPubKey
            let commitTx =
                let t = n.CreateTransaction()
                t.Version <- 0u
                t.Outputs.Add(TxOut(Money.Satoshis(20000L), pubkeyScript)) |> ignore
                t.LockTime <- LockTime.Zero
                t
            let claimP2WPKHOutputTx = Transactions.makeClaimP2WPKHOutputTx(commitTx)
                                                                          (localDustLimit)
                                                                          (localPaymentPriv.PubKey)
                                                                          (finalSpk)
                                                                          (feeRatePerKw)
                                                                          n |> RResult.rderef
            let weight =
                let tx = claimP2WPKHOutputTx.Value.GetGlobalTransaction()
                let witScript =
                    let dummySig = [| for _ in 0..70 -> 0xbbuy |]
                    let dummyPk = Key().PubKey.ToBytes()
                    let dummy = seq[ Op.GetPushOp(dummySig); Op.GetPushOp(dummyPk)]
                    Script(dummy).ToWitScript()
                tx.Inputs.[0].WitScript <- witScript
                tx.GetVirtualSize() |> uint64
            Expect.equal(Constants.CLAIM_P2WPKH_OUTPUT_WEIGHT) (weight) ""
            ()
            
        ()
]
