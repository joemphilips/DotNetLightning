namespace DotNetLightning.Transactions

open System
open System.Linq

open NBitcoin
open NBitcoin.Crypto

open ResultUtils

open DotNetLightning.Utils.Primitives
open DotNetLightning.Utils
open DotNetLightning.Core.Utils.Extensions
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Utils.Aether
open DotNetLightning.Serialize.Msgs

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
    WhichInput: int
}
    with    
        interface ILightningTx with
            member this.Value: PSBT = 
                this.Value
            member this.WhichInput: int = 
                this.WhichInput

        member this.GetTxId() =
            this.Value.GetGlobalTransaction().GetTxId()

module private HTLCHelper =
    let createHTLCWitScript(localSig: TransactionSignature, remoteSig: TransactionSignature, witScript: Script, paymentPreimage: PaymentPreimage option) =
        let l = new ResizeArray<Op>()
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
    WhichInput: int
    PaymentHash: PaymentHash
}
    with
        member this.GetRedeem() =
            this.Value.Inputs.[this.WhichInput].WitnessScript
            
        member this.Finalize(localPubkey, remotePubKey, paymentPreimage) =
            let localSig = this.Value.Inputs.[this.WhichInput].PartialSigs.[localPubkey]
            let remoteSig = this.Value.Inputs.[this.WhichInput].PartialSigs.[remotePubKey]
            this.Finalize(localSig, remoteSig, paymentPreimage)
            
        member this.Finalize(localSig, remoteSig, paymentPreimage: PaymentPreimage) =
            HTLCHelper.finalize(this, localSig, remoteSig, Some paymentPreimage)
            
        interface IHTLCTx
            with
                member this.Value = this.Value
                member this.WhichInput = this.WhichInput

type HTLCTimeoutTx = {
    Value: PSBT
    WhichInput: int
}
    with
        member this.Finalize(localSig: TransactionSignature, remoteSig: TransactionSignature) =
            HTLCHelper.finalize(this, localSig, remoteSig, None)
            
        interface IHTLCTx
            with
                member this.Value = this.Value
                member this.WhichInput = this.WhichInput


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


    let private trimOfferedHTLCs (dustLimit: Money) (spec: CommitmentSpec): DirectedHTLC list =
        let htlcTimeoutFee = spec.FeeRatePerKw.ToFee(HTLC_TIMEOUT_WEIGHT)
        spec.HTLCs
            |> Map.toList
            |> List.map snd
            |> List.filter(fun v -> v.Direction = Out)
            |> List.filter(fun v -> (v.Add.Amount.ToMoney()) >= (dustLimit + htlcTimeoutFee))

    let private trimReceivedHTLCs (dustLimit: Money) (spec: CommitmentSpec) : DirectedHTLC list =
        let htlcSuccessFee = spec.FeeRatePerKw.ToFee(HTLC_SUCCESS_WEIGHT)
        spec.HTLCs
            |> Map.toList
            |> List.map snd
            |> List.filter(fun v -> v.Direction = In)
            |> List.filter(fun v -> (v.Add.Amount.ToMoney()) >= (dustLimit + htlcSuccessFee))

    let internal commitTxFee (dustLimit: Money) (spec: CommitmentSpec): Money =
        let trimmedOfferedHTLCs = trimOfferedHTLCs (dustLimit) (spec)
        let trimmedReceivedHTLCs = trimReceivedHTLCs dustLimit spec
        let weight = COMMIT_WEIGHT + 172UL * (uint64 trimmedOfferedHTLCs.Length + uint64 trimmedReceivedHTLCs.Length)
        spec.FeeRatePerKw.ToFee(weight)

    let getCommitTxNumber (commitTx: Transaction)
                          (isFunder: bool)
                          (localPaymentBasePoint: PubKey)
                          (remotePaymentBasePoint: PubKey)
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
                localPaymentBasePoint
                remotePaymentBasePoint

    /// Sort by BOLT 3: Compliant way (i.e. BIP69 + CLTV order)
    let sortTxOut (txOutsWithMeta: (TxOut * _) list) =
        txOutsWithMeta
        |> List.sortBy(fun txom -> { SortableTxOut.TxOut = (fst txom);UpdateAddHTLC = (snd txom) })
        |> List.map(fst)

    let makeCommitTx (inputInfo: ScriptCoin)
                     (commitmentNumber: CommitmentNumber)
                     (localPaymentBasePoint: PubKey)
                     (remotePaymentBasePoint: PubKey)
                     (localIsFunder: bool)
                     (localDustLimit: Money)
                     (localRevocationPubKey: PubKey)
                     (toLocalDelay: BlockHeightOffset16)
                     (localDelayedPaymentPubKey)
                     (remotePaymentPubkey: PubKey)
                     (localHTLCPubKey: PubKey)
                     (remoteHTLCPubkey: PubKey)
                     (spec: CommitmentSpec)
                     (n: Network)=
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
                Some(TxOut(toRemoteAmount, (remotePaymentPubkey.WitHash.ScriptPubKey)))
            else
                None

        let htlcOfferedOutputsWithMetadata =
            trimOfferedHTLCs (localDustLimit) (spec)
            |> List.map(fun htlc ->
                let redeem = Scripts.htlcOffered (localHTLCPubKey) (remoteHTLCPubkey) localRevocationPubKey (htlc.Add.PaymentHash)
                (TxOut(htlc.Add.Amount.ToMoney(), redeem.WitHash.ScriptPubKey)), Some htlc.Add)
        let htlcReceivedOutputsWithMetadata =
            trimReceivedHTLCs(localDustLimit) (spec)
            |> List.map(fun htlc ->
                    let redeem = Scripts.htlcReceived (localHTLCPubKey) (remoteHTLCPubkey) (localRevocationPubKey) (htlc.Add.PaymentHash) (htlc.Add.CLTVExpiry.Value)
                    TxOut(htlc.Add.Amount.ToMoney(), redeem.WitHash.ScriptPubKey), Some htlc.Add)
        
        let obscuredCommitmentNumber =
            commitmentNumber.Obscure localIsFunder localPaymentBasePoint remotePaymentBasePoint
        let sequence = obscuredCommitmentNumber.Sequence
        let lockTime = obscuredCommitmentNumber.LockTime

        let tx =
            let tx = n.CreateTransaction()
            tx.Version <- 2u
            let txin = TxIn(inputInfo.Outpoint)
            txin.Sequence <- sequence
            tx.Inputs.Add(txin) |> ignore
            let txOuts =
                ([toLocalDelayedOutput_opt; toRemoteOutput_opt;] |> List.choose id |> List.map(fun x -> x, None))
                @ (htlcOfferedOutputsWithMetadata)
                @ htlcReceivedOutputsWithMetadata
            tx.Outputs.AddRange(txOuts |> sortTxOut)
            tx.LockTime <- lockTime
            tx
        let psbt =
            let p = PSBT.FromTransaction(tx, n)
            p.AddCoins(inputInfo)
        { CommitTx.Value = psbt; WhichInput = 0 }


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
    let sign(tx: ILightningTx, key: Key) =
        try
            tx.Value.SignWithKeys(key) |> ignore
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
            let txSig = txIn.Sign(key, coin, SigHash.All, psbt.Settings.UseLowR)
            match checkSigAndAdd tx txSig key.PubKey with
            | Ok txWithSig ->
                (txSig, txWithSig)
            | Error(InvalidSignature signature) ->
                failwithf "Failed to check signature. (%A) This should never happen." signature
            | Error e -> failwith "%A" e

    let makeHTLCTimeoutTx (commitTx: Transaction)
                          (localDustLimit: Money)
                          (localRevocationPubKey: PubKey)
                          (toLocalDelay: BlockHeightOffset16)
                          (localDelayedPaymentPubKey: PubKey)
                          (localHTLCPubKey: PubKey)
                          (remoteHTLCPubKey: PubKey)
                          (feeratePerKw: FeeRatePerKw)
                          (htlc: UpdateAddHTLCMsg)
                          (n: Network) =
        let fee = feeratePerKw.ToFee(HTLC_TIMEOUT_WEIGHT)
        let redeem = Scripts.htlcOffered(localHTLCPubKey) (remoteHTLCPubKey) (localRevocationPubKey) (htlc.PaymentHash)
        let spk = redeem.WitHash.ScriptPubKey
        let spkIndex = findScriptPubKeyIndex commitTx spk
        let amount = htlc.Amount.ToMoney() - fee
        if (amount < localDustLimit) then
            AmountBelowDustLimit amount |> Error
        else
            let psbt = 
                let txb = n.CreateTransactionBuilder()
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
                PSBT.FromTransaction(tx, n)
                    .AddCoins scriptCoin
            let whichInput = psbt.Inputs |> Seq.findIndex(fun i -> not (isNull i.WitnessScript))
            { HTLCTimeoutTx.Value = psbt; WhichInput = whichInput } |> Ok

    let makeHTLCSuccessTx (commitTx: Transaction)
                          (localDustLimit: Money)
                          (localRevocationPubKey: PubKey)
                          (toLocalDelay: BlockHeightOffset16)
                          (localDelayedPaymentPubKey: PubKey)
                          (localHTLCPubKey: PubKey)
                          (remoteHTLCPubKey: PubKey)
                          (feeratePerKw: FeeRatePerKw)
                          (htlc: UpdateAddHTLCMsg)
                          (n: Network)=
        let fee = feeratePerKw.ToFee(HTLC_SUCCESS_WEIGHT)
        let redeem = Scripts.htlcReceived (localHTLCPubKey) (remoteHTLCPubKey) (localRevocationPubKey) (htlc.PaymentHash) (htlc.CLTVExpiry.Value)
        let spk = redeem.WitHash.ScriptPubKey
        let spkIndex = findScriptPubKeyIndex commitTx spk
        let amount = htlc.Amount.ToMoney() - fee
        if (amount < localDustLimit) then
            AmountBelowDustLimit amount |> Error
        else
            let psbt = 
                let txb = n.CreateTransactionBuilder()
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
                PSBT.FromTransaction(tx, n)
                    .AddCoins scriptCoin
            let whichInput = psbt.Inputs |> Seq.findIndex(fun i -> not (isNull i.WitnessScript))
            { HTLCSuccessTx.Value = psbt; WhichInput = whichInput; PaymentHash = htlc.PaymentHash } |> Ok

    let makeHTLCTxs (commitTx: Transaction)
                    (localDustLimit: Money)
                    (localRevocationPubKey: PubKey)
                    (toLocalDelay)
                    (toLocalDelayedPaymentPubKey)
                    (localHTLCPubKey)
                    (remoteHTLCPubKey)
                    (spec: CommitmentSpec)
                    (n): Result<(HTLCTimeoutTx list) * (HTLCSuccessTx list), TransactionError list> =
        let htlcTimeoutTxs = (trimOfferedHTLCs localDustLimit spec)
                             |> List.map(fun htlc -> makeHTLCTimeoutTx (commitTx) (localDustLimit) (localRevocationPubKey) (toLocalDelay) (toLocalDelayedPaymentPubKey) (localHTLCPubKey) (remoteHTLCPubKey) (spec.FeeRatePerKw) (htlc.Add) n)
                             |> List.sequenceResultA
        
        let htlcSuccessTxs = (trimReceivedHTLCs localDustLimit spec)
                             |> List.map(fun htlc -> makeHTLCSuccessTx (commitTx) (localDustLimit) (localRevocationPubKey) (toLocalDelay) (toLocalDelayedPaymentPubKey) (localHTLCPubKey) (remoteHTLCPubKey) (spec.FeeRatePerKw) (htlc.Add) n)
                             |> List.sequenceResultA
        (fun a b -> (a, b)) <!> htlcTimeoutTxs <*> htlcSuccessTxs

    let makeClaimHTLCSuccessTx (commitTx: Transaction)
                               (localDustLimit: Money)
                               (localHTLCPubKey: PubKey)
                               (remoteHTLCPubKey: PubKey)
                               (remoteRevocationPubKey: PubKey)
                               (localFinalScriptPubKey: Script)
                               (htlc: UpdateAddHTLCMsg)
                               (feeRatePerKw: FeeRatePerKw)
                               (n: Network): Result<ClaimHTLCSuccessTx, TransactionError> =
        let fee = feeRatePerKw.ToFee(CLAIM_HTLC_SUCCESS_WEIGHT)
        let redeem = Scripts.htlcOffered(remoteHTLCPubKey) (localHTLCPubKey) (remoteRevocationPubKey) (htlc.PaymentHash)
        let spk = redeem.WitHash.ScriptPubKey
        let spkIndex = findScriptPubKeyIndex commitTx spk
        let amount = htlc.Amount.ToMoney() - fee
        if (amount < localDustLimit) then
            AmountBelowDustLimit amount |> Error
        else
            let psbt = 
                let txb = n.CreateTransactionBuilder()
                let coin = Coin(commitTx.Outputs.AsIndexedOutputs().ElementAt(spkIndex))
                let tx = txb.AddCoins(coin)
                            .Send(localFinalScriptPubKey, amount)
                            .SendFees(fee)
                            .SetLockTime(!> 0u)
                            .BuildTransaction(false)
                tx.Version <- 2u
                tx.Inputs.[0].Sequence <- !> 0xffffffffu
                PSBT.FromTransaction(tx, n)
                    .AddCoins(coin)
            psbt |> ClaimHTLCSuccessTx |> Ok

    let makeClaimHTLCTimeoutTx (commitTx: Transaction)
                               (localDustLimit: Money)
                               (localHTLCPubKey: PubKey)
                               (remoteHTLCPubKey: PubKey)
                               (remoteRevocationPubKey: PubKey)
                               (localFinalScriptPubKey: Script)
                               (htlc: UpdateAddHTLCMsg)
                               (feeRatePerKw: FeeRatePerKw)
                               (n: Network): Result<_, _> =
        let fee = feeRatePerKw.ToFee(CLAIM_HTLC_TIMEOUT_WEIGHT)
        let redeem = Scripts.htlcReceived remoteHTLCPubKey localHTLCPubKey remoteRevocationPubKey htlc.PaymentHash htlc.CLTVExpiry.Value
        let spk = redeem.WitHash.ScriptPubKey
        let spkIndex = findScriptPubKeyIndex commitTx spk
        let amount = htlc.Amount.ToMoney() - fee
        if (amount < localDustLimit) then
            AmountBelowDustLimit amount |> Error
        else
            let psbt = 
                let coin = Coin(commitTx.Outputs.AsIndexedOutputs().ElementAt(spkIndex))
                let tx = n.CreateTransactionBuilder()
                          .AddCoins(coin)
                          .Send(localFinalScriptPubKey, amount)
                          .SendFees(fee)
                          .SetLockTime(!> 0u)
                          .BuildTransaction(false)
                tx.Version <- 2u
                tx.Inputs.[0].Sequence <- !> 0xffffffffu
                PSBT.FromTransaction(tx, n)
                    .AddCoins(coin)
            psbt |> ClaimHTLCTimeoutTx |> Ok

    let makeClaimP2WPKHOutputTx (delayedOutputTx: Transaction)
                                (localDustLimit: Money)
                                (localPaymentPubKey: PubKey)
                                (localFinalDestination: IDestination)
                                (feeRatePerKw: FeeRatePerKw)
                                (n: Network): Result<ClaimP2WPKHOutputTx, _> =
        let fee = feeRatePerKw.ToFee(CLAIM_P2WPKH_OUTPUT_WEIGHT)
        let spk = localPaymentPubKey.WitHash.ScriptPubKey
        let spkIndex = findScriptPubKeyIndex delayedOutputTx spk
        let outPut = delayedOutputTx.Outputs.AsIndexedOutputs().ElementAt(spkIndex)
        let amount = (outPut).TxOut.Value - fee
        if (amount < localDustLimit) then
            AmountBelowDustLimit amount |> Error
        else
            let psbt = 
                let coin = Coin(outPut)
                let txb = n.CreateTransactionBuilder()
                // we have already done dust limit check above
                txb.DustPrevention <- false
                let tx = txb
                          .AddCoins(coin)
                          .Send(localFinalDestination, amount)
                          .SendFees(fee)
                          .SetLockTime(!> 0u)
                          .BuildTransaction(false)
                tx.Version <- 2u
                tx.Inputs.[0].Sequence <- !> 0xffffffffu
                PSBT.FromTransaction(tx, n)
                    .AddCoins(coin)
            psbt |> ClaimP2WPKHOutputTx|> Ok

    let makeMainPenaltyTx (commitTx: Transaction)
                          (localDustLimit: Money)
                          (remoteRevocationKey: PubKey)
                          (localFinalDestination: IDestination)
                          (toRemoteDelay: BlockHeightOffset16)
                          (remoteDelayedPaymentPubKey: PubKey)
                          (feeRatePerKw: FeeRatePerKw)
                          (n: Network): Result<MainPenaltyTx, _>  =
        let fee = feeRatePerKw.ToFee(MAIN_PENALTY_WEIGHT)
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
                let txb = n.CreateTransactionBuilder()
                // we have already done dust limit check above
                txb.DustPrevention <- false
                let tx = txb
                          .AddCoins(coin)
                          .Send(localFinalDestination, amount)
                          .SendFees(fee)
                          .SetLockTime(!> 0u)
                          .BuildTransaction(false)
                tx.Version <- 2u
                tx.Inputs.[0].Sequence <- !> 0xffffffffu
                PSBT.FromTransaction(tx, n)
                    .AddCoins(coin)
            psbt |> MainPenaltyTx |> Ok
            
    let makeHTLCPenaltyTx (commitTx: Transaction) (localDustLimit: Money): HTLCPenaltyTx =
        raise <| NotImplementedException()

    let makeClosingTx (commitTxInput: ScriptCoin)
                      (localDestination: Script)
                      (remoteDestination: Script)
                      (localIsFunder: bool)
                      (dustLimit: Money)
                      (closingFee: Money)
                      (spec: CommitmentSpec)
                      (n: Network): Result<ClosingTx, _> =
        if (not spec.HTLCs.IsEmpty) then
            HTLCNotClean (spec.HTLCs |> Map.toList |> List.map(fst)) |> Error
        else
            let toLocalAmount, toRemoteAmount =
                if (localIsFunder) then
                    spec.ToLocal.ToMoney() - closingFee, spec.ToRemote.ToMoney()
                else
                    spec.ToLocal.ToMoney(), spec.ToRemote.ToMoney() - closingFee

            let maybeToLocalOutput = if (toLocalAmount >= dustLimit) then Some(toLocalAmount, localDestination) else None
            let maybeToRemoteOutput = if (toRemoteAmount >= dustLimit) then Some(toRemoteAmount, remoteDestination) else None
            let outputs = seq [maybeToLocalOutput; maybeToRemoteOutput] |> Seq.choose id
            let psbt = 
                let txb = n.CreateTransactionBuilder()
                           .AddCoins(commitTxInput)
                           .SendFees(closingFee)
                           .SetLockTime(!> 0u)
                for (money, dest) in outputs do
                    txb.Send(dest, money) |> ignore
                let tx =  txb.BuildTransaction(false)
                tx.Version <- 2u
                tx.Inputs.[0].Sequence <- !> 0xffffffffu
                PSBT.FromTransaction(tx, n)
                    .AddCoins(commitTxInput)
            psbt |> ClosingTx |> Ok

