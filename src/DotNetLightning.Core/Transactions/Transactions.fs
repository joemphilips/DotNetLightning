namespace DotNetLightning.Transactions

open System
open System.IO
open System.Linq

open NBitcoin

open DotNetLightning.Utils.Primitives
open DotNetLightning.Utils
open DotNetLightning.Core.Utils.Extensions
open DotNetLightning.Utils.Aether
open DotNetLightning.Crypto
open DotNetLightning.Serialization.Msgs

open ResultUtils
open ResultUtils.Portability

/// We define all possible txs here.
/// internal representation is psbt. But this is just for convenience since
/// in current spec we don't have to send PSBT with each other node in case of Lightning.
/// NOTE: we are assuming ILightningTx will never have redeem_script as an input script.
/// And we are also ignoring `SigHash` field in its input (always `SigHash.All`)
type ILightningTx =
    abstract member Value: PSBT
    abstract member WhichInput: int

type IHTLCTx =
    inherit ILightningTx


type CommitTx = {
    Value: PSBT
}
    with    
        interface ILightningTx with
            member this.Value: PSBT = 
                this.Value
            member this.WhichInput: int = 0

        static member WhichInput: int = 0
        member this.GetTxId() =
            this.Value.GetGlobalTransaction().GetTxId()

module private HTLCHelper =
    let createHTLCWitScript(localSig: TransactionSignature, remoteSig: TransactionSignature, witScript: Script, paymentPreimage: PaymentPreimage option) =
        let l = ResizeArray<Op>()
        l.Add(!> OpcodeType.OP_0)
        l.Add(Op.GetPushOp(remoteSig.ToBytes()))
        l.Add(Op.GetPushOp(localSig.ToBytes()))
        match paymentPreimage with
        | Some p -> l.Add(Op.GetPushOp(p.ToByteArray()))
        | None -> l.Add(!> OpcodeType.OP_0)
        l.Add(Op.GetPushOp(witScript.ToBytes()))
        let s = Script(l)
        s.ToWitScript()
        
    /// PSBT does not support finalizing script with payment preimage.
    /// (And it never does unless scripts in BOLT3 follows some other static analysis scheme such as Miniscript.)
    /// We must finalize manually here.
    let finalize(htlcTx: IHTLCTx, localSig, remoteSig, paymentPreimage: PaymentPreimage option) =
        // Clone this to make the operation atomic. we don't want to mutate `this` in case of failure
        let psbt = htlcTx.Value.Clone() 
        let psbtIn = psbt.Inputs.[htlcTx.WhichInput]
        let witScript = psbtIn.WitnessScript
        let finalWit = createHTLCWitScript(localSig, remoteSig, witScript, paymentPreimage)
        psbtIn.FinalScriptWitness <- finalWit
        let coin = psbtIn.GetCoin()
        let txIn = psbt.GetGlobalTransaction().Inputs.FindIndexedInput(coin.Outpoint)
        txIn.WitScript <- finalWit
        let errors = ref ScriptError.OK
        match txIn.VerifyScript(coin, ScriptVerify.Standard, errors) with
        | true ->
            htlcTx.Value.Inputs.[htlcTx.WhichInput].FinalScriptWitness <- finalWit
            htlcTx.Value.Inputs.[htlcTx.WhichInput].WitnessScript <- null
            htlcTx.Value.Inputs.[htlcTx.WhichInput].PartialSigs.Clear()
            htlcTx.Value.ExtractTransaction()
            |> Ok
        | false ->
            sprintf "Failed to finalize PSBT. error (%A) " errors
            |> FailedToFinalizeScript
            |> Error
        
type HTLCSuccessTx = {
    Value: PSBT
    PaymentHash: PaymentHash
}
    with
        static member WhichInput: int = 0

        member this.GetRedeem() =
            this.Value.Inputs.[HTLCSuccessTx.WhichInput].WitnessScript
            
        member this.Finalize(localPubkey, remotePubKey, paymentPreimage) =
            let localSig = this.Value.Inputs.[HTLCSuccessTx.WhichInput].PartialSigs.[localPubkey]
            let remoteSig = this.Value.Inputs.[HTLCSuccessTx.WhichInput].PartialSigs.[remotePubKey]
            this.Finalize(localSig, remoteSig, paymentPreimage)
            
        member this.Finalize(localSig, remoteSig, paymentPreimage: PaymentPreimage) =
            HTLCHelper.finalize(this, localSig, remoteSig, Some paymentPreimage)
            
        interface IHTLCTx
            with
                member this.Value = this.Value
                member this.WhichInput = HTLCSuccessTx.WhichInput

type HTLCTimeoutTx = {
    Value: PSBT
}
    with
        static member WhichInput: int = 0

        member this.Finalize(localSig: TransactionSignature, remoteSig: TransactionSignature) =
            HTLCHelper.finalize(this, localSig, remoteSig, None)
            
        interface IHTLCTx
            with
                member this.Value = this.Value
                member this.WhichInput = HTLCTimeoutTx.WhichInput


type ClaimHTLCSuccessTx = ClaimHTLCSuccessTx of PSBT
    with
        interface ILightningTx with
            member this.Value = 
                this.Value
            member this.WhichInput: int = 
                0
        member this.Value = let (ClaimHTLCSuccessTx v) = this in v;
type ClaimHTLCTimeoutTx = ClaimHTLCTimeoutTx of PSBT
    with
        interface ILightningTx with
            member this.Value = 
                this.Value
            member this.WhichInput: int = 
                0
        member this.Value = let (ClaimHTLCTimeoutTx v) = this in v;
type ClaimP2WPKHOutputTx = ClaimP2WPKHOutputTx of PSBT
    with
        interface ILightningTx with
            member this.Value = 
                this.Value
            member this.WhichInput: int = 
                0
        member this.Value = let (ClaimP2WPKHOutputTx v) = this in v;
type ClaimDelayedOutputTx = ClaimDelayedOutputTx of PSBT
    with
        interface ILightningTx with
            member this.Value = 
                this.Value
            member this.WhichInput: int = 
                0
        member this.Value = let (ClaimDelayedOutputTx v) = this in v;
type MainPenaltyTx = MainPenaltyTx of PSBT
    with
        interface ILightningTx with
            member this.Value = 
                this.Value
            member this.WhichInput: int = 
                0
        member this.Value = let (MainPenaltyTx v) = this in v;
type HTLCPenaltyTx = HTLCPenaltyTx of PSBT
    with
        interface ILightningTx with
            member this.Value = 
                this.Value
            member this.WhichInput: int = 
                0
        member this.Value = let (HTLCPenaltyTx v) = this in v;
type ClosingTx = ClosingTx of PSBT
    with
        member this.Value = let (ClosingTx v) = this in v;
        member this.WhichInput = 0

        interface ILightningTx with
            member this.Value = 
                this.Value
            member this.WhichInput: int = 
                this.WhichInput


/// Tx already verified and it can be published anytime
type FinalizedTx =
    FinalizedTx of Transaction
    with
        member this.Value = let (FinalizedTx v) = this in v

type InputInfo = {
    OutPoint: OutPoint
    RedeemScript: Script
}

// Write
[<CustomComparison;CustomEquality>]
type SortableTxOut = {
       TxOut: TxOut
       UpdateAddHTLC: UpdateAddHTLCMsg option
    }
    with
        override this.Equals(other: obj): bool =
            match other with
            | :? SortableTxOut as o -> (this :> IEquatable<SortableTxOut>).Equals(o)
            | _ -> false

        override this.GetHashCode() =
            match this.UpdateAddHTLC with
            | None -> this.TxOut.GetHashCode()
            | Some htlc -> Array.append (this.TxOut.ToBytes()) (htlc.ToBytes()) |> hash

        interface IEquatable<SortableTxOut> with
            member this.Equals(other) =
                this.TxOut.ScriptPubKey.Equals(other.TxOut.ScriptPubKey)
                && this.TxOut.Value.Equals(other.TxOut.Value)
                && this.UpdateAddHTLC.Equals(other.UpdateAddHTLC)

        interface IComparable with
            member this.CompareTo(obj: obj): int =
                match obj with
                | :? SortableTxOut as other -> (this :> IComparable<SortableTxOut>).CompareTo(other)
                | _ -> 1

        interface IComparable<SortableTxOut> with
            member this.CompareTo(obj: SortableTxOut) =
                let (txout1) = this.TxOut
                let (txout2) = obj.TxOut
                let c1 = txout1.Value.CompareTo(txout2.Value)
                if (c1 <> 0) then
                    c1
                else
                    let c2 = BytesComparer.Instance.Compare(txout1.ScriptPubKey.ToBytes(), txout2.ScriptPubKey.ToBytes())
                    if (c2 <> 0) then
                        c2
                    else
                        // tie-breaker
                        match (this.UpdateAddHTLC), (obj.UpdateAddHTLC) with
                        | None, _ -> 0
                        | _, None -> 0
                        | Some a, Some b ->
                            let c3  = a.CLTVExpiry.Value.CompareTo(b.CLTVExpiry.Value)
                            if c3 <> 0 then
                                c3
                            else
                                let c4 = a.PaymentHash.Value.CompareTo(b.PaymentHash.Value)
                                if (c4 <> 0) then
                                    c4
                                else
                                    0

module Transactions =

    [<AutoOpen>]
    module Constants =
        // The lightning spec specifies that commitment txs use version 2 bitcoin transactions.
        [<Literal>]
        let TxVersionNumberOfCommitmentTxs = 2u

        // ------- From eclair ---------
        [<Literal>]
        let COMMITMENT_TX_BASE_WEIGHT = 724UL

        [<Literal>]
        let COMMIT_WEIGHT = COMMITMENT_TX_BASE_WEIGHT

        [<Literal>]
        let HTLC_TIMEOUT_WEIGHT = 663UL

        [<Literal>]
        let HTLC_SUCCESS_WEIGHT = 703UL

        [<Literal>]
        let CLAIM_P2WPKH_OUTPUT_WEIGHT = 437UL

        [<Literal>]
        let CLAIM_HTLC_DELAYED_WEIGHT = 482UL

        [<Literal>]
        let CLAIM_HTLC_SUCCESS_WEIGHT = 570UL

        [<Literal>]
        let CLAIM_HTLC_TIMEOUT_WEIGHT = 544UL

        [<Literal>]
        let MAIN_PENALTY_WEIGHT = 483UL


        // ----- from rust-lightning -----
        [<Literal>]
        let INITIAL_COMMITMENT_NUMBER = 281474976710655UL // (1 << 48 - 1)

        [<Literal>]
        let COMMITMENT_TX_WEIGHT_PER_HTLC = 172UL

        // prevout: 36, nSequence: 4, script len: 1, witness lengths: (3+1)/4, sig: 73/4, if-selector: 1, redeemScript: (6 ops + 2*33 pubkeys + 1*2 delay)/4
        [<Literal>]
        let SPENDING_INPUT_FOR_A_OUTPUT_WEIGHT = 79UL 
        // prevout: 40, nSequence: 4, script len: 1, witness lengths: 3/4, sig: 73/4, pubkey: 33/4, output: 31
        [<Literal>]
        let B_OUTPUT_PLUS_SPENDING_INPUT_WEIGHT = 104UL

        // Specified in BOLT #2
        let MAX_FUNDING_SATOSHIS = Money.Satoshis(16777216m) // (1 << 24)

        [<Literal>]
        let ACCEPTED_HTLC_SCRIPT_WEIGHT = 139uy
        [<Literal>]
        let OFFERED_HTLC_SCRIPT_WEIGHT = 133uy

    let internal createDeterministicTransactionBuilder (network: Network) =
        let txb = network.CreateTransactionBuilder()
        txb.ShuffleOutputs <- false
        txb.ShuffleInputs <- false
        txb.CoinSelector <- DefaultCoinSelector 0
        txb

    let UINT32_MAX = 0xffffffffu

    let private trimOfferedHTLCs (dustLimit: Money) (spec: CommitmentSpec): list<UpdateAddHTLCMsg> =
        let htlcTimeoutFee = spec.FeeRatePerKw.CalculateFeeFromWeight(HTLC_TIMEOUT_WEIGHT)
        spec.OutgoingHTLCs
            |> Map.toList
            |> List.map snd
            |> List.filter(fun v -> (v.Amount.ToMoney()) >= (dustLimit + htlcTimeoutFee))

    let private trimReceivedHTLCs (dustLimit: Money) (spec: CommitmentSpec) : list<UpdateAddHTLCMsg> =
        let htlcSuccessFee = spec.FeeRatePerKw.CalculateFeeFromWeight(HTLC_SUCCESS_WEIGHT)
        spec.IncomingHTLCs
            |> Map.toList
            |> List.map snd
            |> List.filter(fun v -> (v.Amount.ToMoney()) >= (dustLimit + htlcSuccessFee))

    let internal commitTxFee (dustLimit: Money) (spec: CommitmentSpec): Money =
        let trimmedOfferedHTLCs = trimOfferedHTLCs (dustLimit) (spec)
        let trimmedReceivedHTLCs = trimReceivedHTLCs dustLimit spec
        let weight = COMMIT_WEIGHT + 172UL * (uint64 trimmedOfferedHTLCs.Length + uint64 trimmedReceivedHTLCs.Length)
        spec.FeeRatePerKw.CalculateFeeFromWeight(weight)

    let getCommitTxNumber (commitTx: Transaction)
                          (isFunder: bool)
                          (localPaymentBasepoint: PaymentBasepoint)
                          (remotePaymentBasepoint: PaymentBasepoint)
                              : Option<CommitmentNumber> =
        let obscuredCommitmentNumberOpt =
            ObscuredCommitmentNumber.TryFromLockTimeAndSequence
                commitTx.LockTime
                commitTx.Inputs.[0].Sequence
        match obscuredCommitmentNumberOpt with
        | None -> None
        | Some obscuredCommitmentNumber ->
            Some <| obscuredCommitmentNumber.Unobscure
                isFunder
                localPaymentBasepoint
                remotePaymentBasepoint

    /// Sort by BOLT 3: Compliant way (i.e. BIP69 + CLTV order)
    let sortTxOut (txOutsWithMeta: (TxOut * Option<UpdateAddHTLCMsg>) list) =
        txOutsWithMeta
        |> List.sortBy(fun txom -> { SortableTxOut.TxOut = (fst txom);UpdateAddHTLC = (snd txom) })
        |> List.map(fst)

    let makeCommitTx (inputInfo: ScriptCoin)
                     (commitmentNumber: CommitmentNumber)
                     (localPaymentBasepoint: PaymentBasepoint)
                     (remotePaymentBasepoint: PaymentBasepoint)
                     (localIsFunder: bool)
                     (localDustLimit: Money)
                     (localRevocationPubKey: RevocationPubKey)
                     (toLocalDelay: BlockHeightOffset16)
                     (localDelayedPaymentPubKey: DelayedPaymentPubKey)
                     (remotePaymentPubkey: PaymentPubKey)
                     (localHTLCPubKey: HtlcPubKey)
                     (remoteHTLCPubkey: HtlcPubKey)
                     (spec: CommitmentSpec)
                     (network: Network)
                         =
        let commitFee = commitTxFee localDustLimit spec
        let (toLocalAmount, toRemoteAmount) =
            if (localIsFunder) then
                (spec.ToLocal.Satoshi |> Money.Satoshis) - commitFee, spec.ToRemote.Satoshi |> Money.Satoshis
            else
                (spec.ToLocal.Satoshi |> Money.Satoshis), (spec.ToRemote.Satoshi |> Money.Satoshis) - commitFee

        let toLocalDelayedOutput_opt =
            if (toLocalAmount >= localDustLimit) then
                Some (TxOut(toLocalAmount, (Scripts.toLocalDelayed(localRevocationPubKey) (toLocalDelay) (localDelayedPaymentPubKey)).WitHash.ScriptPubKey))
            else
                None
        let toRemoteOutput_opt =
            if (toRemoteAmount >= localDustLimit) then 
                Some(TxOut(toRemoteAmount, (remotePaymentPubkey.RawPubKey().WitHash.ScriptPubKey)))
            else
                None

        let htlcOfferedOutputsWithMetadata =
            trimOfferedHTLCs (localDustLimit) (spec)
            |> List.map(fun htlc ->
                let redeem = Scripts.htlcOffered (localHTLCPubKey) (remoteHTLCPubkey) localRevocationPubKey (htlc.PaymentHash)
                (TxOut(htlc.Amount.ToMoney(), redeem.WitHash.ScriptPubKey)), Some htlc)
        let htlcReceivedOutputsWithMetadata =
            trimReceivedHTLCs(localDustLimit) (spec)
            |> List.map(fun htlc ->
                    let redeem = Scripts.htlcReceived (localHTLCPubKey) (remoteHTLCPubkey) (localRevocationPubKey) (htlc.PaymentHash) (htlc.CLTVExpiry.Value)
                    TxOut(htlc.Amount.ToMoney(), redeem.WitHash.ScriptPubKey), Some htlc)
        
        let obscuredCommitmentNumber =
            commitmentNumber.Obscure localIsFunder localPaymentBasepoint remotePaymentBasepoint
        let sequence = obscuredCommitmentNumber.Sequence
        let lockTime = obscuredCommitmentNumber.LockTime

        let tx =

            let txb, outAmount =
                let txb =
                    (createDeterministicTransactionBuilder network)
                        .SetVersion(TxVersionNumberOfCommitmentTxs)
                        .SetLockTime(lockTime)
                        .AddCoin(
                            inputInfo,
                            CoinOptions (
                                Sequence = Nullable sequence
                            )
                        )
                let txOuts =
                    ([toLocalDelayedOutput_opt; toRemoteOutput_opt] |> List.choose id |> List.map(fun x -> x, None))
                    @ (htlcOfferedOutputsWithMetadata)
                    @ htlcReceivedOutputsWithMetadata
                List.fold
                    (
                        fun ((txb, outAmount): TransactionBuilder * Money) (txOut: TxOut) ->
                            txb.Send(txOut.ScriptPubKey, txOut.Value) |> ignore
                            (txb, outAmount + txOut.Value)
                    )
                    (txb, Money 0UL)
                    (sortTxOut txOuts)

            let actualFee = inputInfo.Amount - outAmount
            txb.SendFees(actualFee)
                .BuildTransaction true

        let psbt =
            let p = PSBT.FromTransaction(tx, network)
            p.AddCoins(inputInfo)
        { CommitTx.Value = psbt }


    let private findScriptPubKeyIndex(tx: Transaction) (spk: Script) =
        tx.Outputs |> List.ofSeq |> List.findIndex(fun o -> o.ScriptPubKey = spk)

    let checkTxFinalized (psbt: PSBT) (inputIndex: int) (additionalKnownSigs: (PubKey * TransactionSignature) seq): Result<FinalizedTx, _> =
        let checkTxFinalizedCore (psbt: PSBT): Result<_, _> =
            match psbt.TryFinalize() with
            | false, e ->
                (sprintf "failed to finalize psbt Errors: %A \n PSBTInput: %A \n base64 PSBT: %s" e psbt.Inputs.[inputIndex] (psbt.ToBase64()))
                |> FailedToFinalizeScript
                |> Error
            | true, _ ->
                psbt.ExtractTransaction() |> FinalizedTx |> Ok
        match psbt.Inputs.[inputIndex].GetTxOut() with
        | null -> failwithf "Bug: prevout does not exist in %d for psbt: %A" inputIndex (psbt)
        | _ ->
            additionalKnownSigs |> Seq.iter (fun kv ->
                psbt.Inputs.[inputIndex].PartialSigs.AddOrReplace(kv)
            )

            checkTxFinalizedCore psbt

    let checkSigAndAdd (tx: ILightningTx) (signature: TransactionSignature) (pk: PubKey) =
        if (tx.Value.IsAllFinalized()) then
            Ok tx
        else
            let psbt = tx.Value
            let spentOutput = psbt.Inputs.[tx.WhichInput].GetTxOut()
            let scriptCode = psbt.Inputs.[tx.WhichInput].WitnessScript
            let globalTx = psbt.GetGlobalTransaction()

            let ctx = ScriptEvaluationContext()
            ctx.SigHash <- signature.SigHash
            ctx.ScriptVerify <- ScriptVerify.Standard
            if ctx.CheckSig(signature.ToBytes(), pk.ToBytes(), scriptCode, globalTx, tx.WhichInput, 1, spentOutput) then
                tx.Value.Inputs.[tx.WhichInput].PartialSigs.AddOrReplace(pk, signature)
                tx |> Ok
            else
                InvalidSignature signature |> Error
                
    /// Sign psbt inside ILightningTx and returns
    /// 1. Newly created signature
    /// 2. ILightningTx updated
    /// Technically speaking, we could just return one of them.
    /// Returning both is just for ergonomic reason. (pretending to be referential transparent)
    let signCore(tx: ILightningTx, key: Key, enforceLowR) =
        let signingOptions = SigningOptions()
        signingOptions.EnforceLowR <- enforceLowR
        try
            tx.Value.SignWithKeys(signingOptions, key) |> ignore
            match tx.Value.GetMatchingSig(key.PubKey) with
            | Some signature -> (signature, tx)
            | None -> failwith "unreachable"
        with
        /// Sadly, psbt does not support signing for script other than predefined template.
        /// So we must fallback to more low level way.
        /// This should be removed when NBitcoin supports finalization for Miniscript.
        | :? NotSupportedException ->
            let psbt = tx.Value
            let psbtIn = psbt.Inputs.[tx.WhichInput]
            let coin =
                let c = psbtIn.GetCoin()
                ScriptCoin(c, psbtIn.WitnessScript)
            let txIn =
                let globalTx = psbt.GetGlobalTransaction()
                globalTx.Inputs.AsIndexedInputs()
                |> Seq.indexed
                |> Seq.find(fun (i, _txIn) -> i = tx.WhichInput)
                |> snd
            let txSig = txIn.Sign(key, coin, SigHash.All, enforceLowR)
            match checkSigAndAdd tx txSig key.PubKey with
            | Ok txWithSig ->
                (txSig, txWithSig)
            | Error(InvalidSignature signature) ->
                failwithf "Failed to check signature. (%A) This should never happen." signature
            | Error e -> failwithf "%A" e

    let sign(tx, key) = signCore(tx, key, true)
    let makeHTLCTimeoutTx (commitTx: Transaction)
                          (localDustLimit: Money)
                          (localRevocationPubKey: RevocationPubKey)
                          (toLocalDelay: BlockHeightOffset16)
                          (localDelayedPaymentPubKey: DelayedPaymentPubKey)
                          (localHTLCPubKey: HtlcPubKey)
                          (remoteHTLCPubKey: HtlcPubKey)
                          (feeratePerKw: FeeRatePerKw)
                          (htlc: UpdateAddHTLCMsg)
                          (network: Network)
                              =
        let fee = feeratePerKw.CalculateFeeFromWeight(HTLC_TIMEOUT_WEIGHT)
        let redeem = Scripts.htlcOffered(localHTLCPubKey) (remoteHTLCPubKey) (localRevocationPubKey) (htlc.PaymentHash)
        let spk = redeem.WitHash.ScriptPubKey
        let spkIndex = findScriptPubKeyIndex commitTx spk
        let amount = htlc.Amount.ToMoney() - fee
        if (amount < localDustLimit) then
            AmountBelowDustLimit amount |> Error
        else
            let psbt = 
                let txb = createDeterministicTransactionBuilder network
                let indexedTxOut = commitTx.Outputs.AsIndexedOutputs().ElementAt(spkIndex)
                let scriptCoin = ScriptCoin(indexedTxOut, redeem)
                let dest = Scripts.toLocalDelayed localRevocationPubKey toLocalDelay localDelayedPaymentPubKey
                // we have already done dust limit check above
                txb.DustPrevention <- false
                let tx = txb.AddCoins(scriptCoin)
                            .Send(dest.WitHash, amount)
                            .SendFees(fee)
                            .SetLockTime(!> htlc.CLTVExpiry.Value)
                            .BuildTransaction(false)
                tx.Version <- 2u
                
                /// We must set 0 to sequence for HTLC-success/timeout (defined in bolt3)
                for i in tx.Inputs do
                    i.Sequence <- Sequence(0)

                PSBT.FromTransaction(tx, network)
                    .AddCoins scriptCoin
            { HTLCTimeoutTx.Value = psbt } |> Ok

    let makeHTLCSuccessTx (commitTx: Transaction)
                          (localDustLimit: Money)
                          (localRevocationPubKey: RevocationPubKey)
                          (toLocalDelay: BlockHeightOffset16)
                          (localDelayedPaymentPubKey: DelayedPaymentPubKey)
                          (localHTLCPubKey: HtlcPubKey)
                          (remoteHTLCPubKey: HtlcPubKey)
                          (feeratePerKw: FeeRatePerKw)
                          (htlc: UpdateAddHTLCMsg)
                          (network: Network)
                              =
        let fee = feeratePerKw.CalculateFeeFromWeight(HTLC_SUCCESS_WEIGHT)
        let redeem = Scripts.htlcReceived (localHTLCPubKey) (remoteHTLCPubKey) (localRevocationPubKey) (htlc.PaymentHash) (htlc.CLTVExpiry.Value)
        let spk = redeem.WitHash.ScriptPubKey
        let spkIndex = findScriptPubKeyIndex commitTx spk
        let amount = htlc.Amount.ToMoney() - fee
        if (amount < localDustLimit) then
            AmountBelowDustLimit amount |> Error
        else
            let psbt = 
                let txb = createDeterministicTransactionBuilder network
                let scriptCoin =
                    let coin = commitTx.Outputs.AsIndexedOutputs().ElementAt(spkIndex)
                    ScriptCoin(coin, redeem)
                let dest = Scripts.toLocalDelayed localRevocationPubKey toLocalDelay localDelayedPaymentPubKey
                // we have already done dust limit check above
                txb.DustPrevention <- false
                let tx = txb.AddCoins(scriptCoin)
                            .Send(dest.WitHash, amount)
                            .SendFees(fee)
                            .SetLockTime(!> 0u)
                            .BuildTransaction(false)
                tx.Version <- 2u
                
                /// We must set 0 to sequence for HTLC-success/timeout (defined in bolt3)
                for i in tx.Inputs do
                    i.Sequence <- Sequence(0)

                PSBT.FromTransaction(tx, network)
                    .AddCoins scriptCoin
            { HTLCSuccessTx.Value = psbt; PaymentHash = htlc.PaymentHash } |> Ok

    let makeHTLCTxs (commitTx: Transaction)
                    (localDustLimit: Money)
                    (localRevocationPubKey: RevocationPubKey)
                    (toLocalDelay)
                    (toLocalDelayedPaymentPubKey: DelayedPaymentPubKey)
                    (localHTLCPubKey)
                    (remoteHTLCPubKey)
                    (spec: CommitmentSpec)
                    (network: Network)
                        : Result<(HTLCTimeoutTx list) * (HTLCSuccessTx list), TransactionError list> =
        let htlcTimeoutTxs = (trimOfferedHTLCs localDustLimit spec)
                             |> List.map(fun htlc -> makeHTLCTimeoutTx commitTx
                                                                       localDustLimit
                                                                       localRevocationPubKey
                                                                       toLocalDelay
                                                                       toLocalDelayedPaymentPubKey
                                                                       localHTLCPubKey
                                                                       remoteHTLCPubKey
                                                                       spec.FeeRatePerKw
                                                                       htlc
                                                                       network
                                        )
                             |> List.sequenceResultA
        
        let htlcSuccessTxs = (trimReceivedHTLCs localDustLimit spec)
                             |> List.map(fun htlc -> makeHTLCSuccessTx commitTx
                                                                       localDustLimit
                                                                       localRevocationPubKey
                                                                       toLocalDelay
                                                                       toLocalDelayedPaymentPubKey
                                                                       localHTLCPubKey
                                                                       remoteHTLCPubKey
                                                                       spec.FeeRatePerKw
                                                                       htlc
                                                                       network
                                        )
                             |> List.sequenceResultA
        (fun a b -> (a, b)) <!> htlcTimeoutTxs <*> htlcSuccessTxs

    let makeClaimHTLCSuccessTx (commitTx: Transaction)
                               (localDustLimit: Money)
                               (localHTLCPubKey: HtlcPubKey)
                               (remoteHTLCPubKey: HtlcPubKey)
                               (remoteRevocationPubKey: RevocationPubKey)
                               (localFinalScriptPubKey: Script)
                               (htlc: UpdateAddHTLCMsg)
                               (feeRatePerKw: FeeRatePerKw)
                               (network: Network)
                                   : Result<ClaimHTLCSuccessTx, TransactionError> =
        let fee = feeRatePerKw.CalculateFeeFromWeight(CLAIM_HTLC_SUCCESS_WEIGHT)
        let redeem = Scripts.htlcOffered(remoteHTLCPubKey) (localHTLCPubKey) (remoteRevocationPubKey) (htlc.PaymentHash)
        let spk = redeem.WitHash.ScriptPubKey
        let spkIndex = findScriptPubKeyIndex commitTx spk
        let amount = htlc.Amount.ToMoney() - fee
        if (amount < localDustLimit) then
            AmountBelowDustLimit amount |> Error
        else
            let psbt = 
                let txb = createDeterministicTransactionBuilder network
                let coin = Coin(commitTx.Outputs.AsIndexedOutputs().ElementAt(spkIndex))
                let tx = txb.AddCoins(coin)
                            .Send(localFinalScriptPubKey, amount)
                            .SendFees(fee)
                            .SetLockTime(!> 0u)
                            .BuildTransaction(false)
                tx.Version <- 2u
                tx.Inputs.[0].Sequence <- !> UINT32_MAX
                PSBT.FromTransaction(tx, network)
                    .AddCoins(coin)
            psbt |> ClaimHTLCSuccessTx |> Ok

    let makeClaimHTLCTimeoutTx (commitTx: Transaction)
                               (localDustLimit: Money)
                               (localHTLCPubKey: HtlcPubKey)
                               (remoteHTLCPubKey: HtlcPubKey)
                               (remoteRevocationPubKey: RevocationPubKey)
                               (localFinalScriptPubKey: Script)
                               (htlc: UpdateAddHTLCMsg)
                               (feeRatePerKw: FeeRatePerKw)
                               (network: Network)
                                   : Result<_, _> =
        let fee = feeRatePerKw.CalculateFeeFromWeight(CLAIM_HTLC_TIMEOUT_WEIGHT)
        let redeem = Scripts.htlcReceived remoteHTLCPubKey localHTLCPubKey remoteRevocationPubKey htlc.PaymentHash htlc.CLTVExpiry.Value
        let spk = redeem.WitHash.ScriptPubKey
        let spkIndex = findScriptPubKeyIndex commitTx spk
        let amount = htlc.Amount.ToMoney() - fee
        if (amount < localDustLimit) then
            AmountBelowDustLimit amount |> Error
        else
            let psbt = 
                let coin = Coin(commitTx.Outputs.AsIndexedOutputs().ElementAt(spkIndex))
                let tx = (createDeterministicTransactionBuilder network)
                          .AddCoins(coin)
                          .Send(localFinalScriptPubKey, amount)
                          .SendFees(fee)
                          .SetLockTime(!> 0u)
                          .BuildTransaction(false)
                tx.Version <- 2u
                tx.Inputs.[0].Sequence <- !> UINT32_MAX
                PSBT.FromTransaction(tx, network)
                    .AddCoins(coin)
            psbt |> ClaimHTLCTimeoutTx |> Ok

    let makeClaimP2WPKHOutputTx (delayedOutputTx: Transaction)
                                (localDustLimit: Money)
                                (localPaymentPubKey: PaymentPubKey)
                                (localFinalDestination: IDestination)
                                (feeRatePerKw: FeeRatePerKw)
                                (network: Network)
                                    : Result<ClaimP2WPKHOutputTx, _> =
        let fee = feeRatePerKw.CalculateFeeFromWeight(CLAIM_P2WPKH_OUTPUT_WEIGHT)
        let spk = localPaymentPubKey.RawPubKey().WitHash.ScriptPubKey
        let spkIndex = findScriptPubKeyIndex delayedOutputTx spk
        let outPut = delayedOutputTx.Outputs.AsIndexedOutputs().ElementAt(spkIndex)
        let amount = (outPut).TxOut.Value - fee
        if (amount < localDustLimit) then
            AmountBelowDustLimit amount |> Error
        else
            let psbt = 
                let coin = Coin(outPut)
                let txb = createDeterministicTransactionBuilder network
                // we have already done dust limit check above
                txb.DustPrevention <- false
                let tx = txb
                          .AddCoins(coin)
                          .Send(localFinalDestination, amount)
                          .SendFees(fee)
                          .SetLockTime(!> 0u)
                          .BuildTransaction(false)
                tx.Version <- 2u
                tx.Inputs.[0].Sequence <- !> UINT32_MAX
                PSBT.FromTransaction(tx, network)
                    .AddCoins(coin)
            psbt |> ClaimP2WPKHOutputTx|> Ok

    let makeMainPenaltyTx (commitTx: Transaction)
                          (localDustLimit: Money)
                          (remoteRevocationKey: RevocationPubKey)
                          (localFinalDestination: IDestination)
                          (toRemoteDelay: BlockHeightOffset16)
                          (remoteDelayedPaymentPubKey: DelayedPaymentPubKey)
                          (feeRatePerKw: FeeRatePerKw)
                          (network: Network)
                              : Result<MainPenaltyTx, _>  =
        let fee = feeRatePerKw.CalculateFeeFromWeight(MAIN_PENALTY_WEIGHT)
        let redeem = Scripts.toLocalDelayed remoteRevocationKey toRemoteDelay remoteDelayedPaymentPubKey
        let spk = redeem.WitHash.ScriptPubKey
        let spkIndex = findScriptPubKeyIndex commitTx spk
        let outPut = commitTx.Outputs.AsIndexedOutputs().ElementAt(spkIndex)
        let amount = (outPut).TxOut.Value - fee
        if (amount < localDustLimit) then
            AmountBelowDustLimit amount |> Error
        else
            let psbt = 
                let coin = Coin(outPut)
                let txb = createDeterministicTransactionBuilder network
                // we have already done dust limit check above
                txb.DustPrevention <- false
                let tx = txb
                          .AddCoins(coin)
                          .Send(localFinalDestination, amount)
                          .SendFees(fee)
                          .SetLockTime(!> 0u)
                          .BuildTransaction(false)
                tx.Version <- 2u
                tx.Inputs.[0].Sequence <- !> UINT32_MAX
                PSBT.FromTransaction(tx, network)
                    .AddCoins(coin)
            psbt |> MainPenaltyTx |> Ok
            
    let makeHTLCPenaltyTx (_commitTx: Transaction) (_localDustLimit: Money): HTLCPenaltyTx =
        raise <| NotImplementedException()

    let makeClosingTx (commitTxInput: ScriptCoin)
                      (localDestination: ShutdownScriptPubKey)
                      (remoteDestination: ShutdownScriptPubKey)
                      (localIsFunder: bool)
                      (dustLimit: Money)
                      (closingFee: Money)
                      (spec: CommitmentSpec)
                      (network: Network)
                          : Result<ClosingTx, _> =
        if (not spec.OutgoingHTLCs.IsEmpty) || (not spec.IncomingHTLCs.IsEmpty) then
            HTLCNotClean
                ((spec.OutgoingHTLCs |> Map.toList |> List.map(fst)) @ (spec.IncomingHTLCs |> Map.toList |> List.map(fst)))
            |> Error
        else
            let toLocalAmount, toRemoteAmount =
                if (localIsFunder) then
                    spec.ToLocal.ToMoney() - closingFee, spec.ToRemote.ToMoney()
                else
                    spec.ToLocal.ToMoney(), spec.ToRemote.ToMoney() - closingFee

            let outputs =
                seq {
                    if toLocalAmount >= dustLimit then
                        yield TxOut(toLocalAmount, localDestination.ScriptPubKey())
                    if toRemoteAmount >= dustLimit then
                        yield TxOut(toRemoteAmount, remoteDestination.ScriptPubKey())
                }
                |> Seq.sortWith TxOut.LexicographicCompare
            let psbt = 
                let txb = (createDeterministicTransactionBuilder network)
                           .AddCoins(commitTxInput)
                           .SendFees(closingFee)
                           .SetLockTime(!> 0u)
                for txOut in outputs do
                    txb.Send(txOut.ScriptPubKey, txOut.Value) |> ignore
                let tx = txb.BuildTransaction(false)
                tx.Version <- 2u
                tx.Inputs.[0].Sequence <- !> UINT32_MAX
                PSBT.FromTransaction(tx, network)
                    .AddCoins(commitTxInput)
            psbt |> ClosingTx |> Ok

