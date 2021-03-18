namespace DotNetLightning.Channel

open System
open NBitcoin

open DotNetLightning.Utils
open DotNetLightning.Transactions
open DotNetLightning.Transactions.Transactions
open DotNetLightning.Crypto
open DotNetLightning.Chain
open DotNetLightning.Serialization.Msgs

open ResultUtils
open ResultUtils.Portability

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Commitments =
    module private Helpers =
        let isAlreadySent (htlc: UpdateAddHTLCMsg) (proposed: IUpdateMsg list) =
            proposed
            |> List.exists(fun p -> match p with
                                    | :? UpdateFulfillHTLCMsg as u -> u.HTLCId = htlc.HTLCId
                                    | :? UpdateFailHTLCMsg as u -> u.HTLCId = htlc.HTLCId
                                    | :? UpdateFailMalformedHTLCMsg as u -> u.HTLCId = htlc.HTLCId
                                    | _ -> false)

        let makeRemoteTxs
            (staticChannelConfig: StaticChannelConfig)
            (commitTxNumber: CommitmentNumber)
            (localChannelPubKeys: ChannelPubKeys)
            (remotePerCommitmentPoint: PerCommitmentPoint)
            (spec: CommitmentSpec) =
            let localCommitmentPubKeys = remotePerCommitmentPoint.DeriveCommitmentPubKeys localChannelPubKeys
            let remoteCommitmentPubKeys = remotePerCommitmentPoint.DeriveCommitmentPubKeys staticChannelConfig.RemoteChannelPubKeys
            let commitTx =
                Transactions.makeCommitTx staticChannelConfig.FundingScriptCoin
                                          commitTxNumber
                                          staticChannelConfig.RemoteChannelPubKeys.PaymentBasepoint
                                          localChannelPubKeys.PaymentBasepoint
                                          (not staticChannelConfig.IsFunder)
                                          (staticChannelConfig.RemoteParams.DustLimitSatoshis)
                                          localCommitmentPubKeys.RevocationPubKey
                                          staticChannelConfig.LocalParams.ToSelfDelay
                                          remoteCommitmentPubKeys.DelayedPaymentPubKey
                                          localCommitmentPubKeys.PaymentPubKey
                                          remoteCommitmentPubKeys.HtlcPubKey
                                          localCommitmentPubKeys.HtlcPubKey
                                          spec
                                          staticChannelConfig.Network
            result {
                 let! (htlcTimeoutTxs, htlcSuccessTxs) =
                     Transactions.makeHTLCTxs
                         (commitTx.Value.GetGlobalTransaction())
                         (staticChannelConfig.RemoteParams.DustLimitSatoshis)
                         localCommitmentPubKeys.RevocationPubKey
                         (staticChannelConfig.LocalParams.ToSelfDelay)
                         remoteCommitmentPubKeys.DelayedPaymentPubKey
                         remoteCommitmentPubKeys.HtlcPubKey
                         localCommitmentPubKeys.HtlcPubKey
                         spec
                         staticChannelConfig.Network
                return (commitTx, htlcTimeoutTxs, htlcSuccessTxs)
            }

        let makeLocalTXs
            (staticChannelConfig: StaticChannelConfig)
            (commitTxNumber: CommitmentNumber)
            (localChannelPubKeys: ChannelPubKeys)
            (localPerCommitmentPoint: PerCommitmentPoint)
            (spec: CommitmentSpec)
                : Result<(CommitTx * HTLCTimeoutTx list * HTLCSuccessTx list), _> =
            let localCommitmentPubKeys = localPerCommitmentPoint.DeriveCommitmentPubKeys localChannelPubKeys
            let remoteCommitmentPubKeys = localPerCommitmentPoint.DeriveCommitmentPubKeys staticChannelConfig.RemoteChannelPubKeys

            let commitTx =
                Transactions.makeCommitTx staticChannelConfig.FundingScriptCoin
                                          commitTxNumber
                                          localChannelPubKeys.PaymentBasepoint
                                          staticChannelConfig.RemoteChannelPubKeys.PaymentBasepoint
                                          staticChannelConfig.IsFunder
                                          staticChannelConfig.LocalParams.DustLimitSatoshis
                                          remoteCommitmentPubKeys.RevocationPubKey
                                          staticChannelConfig.RemoteParams.ToSelfDelay
                                          localCommitmentPubKeys.DelayedPaymentPubKey
                                          remoteCommitmentPubKeys.PaymentPubKey
                                          localCommitmentPubKeys.HtlcPubKey
                                          remoteCommitmentPubKeys.HtlcPubKey
                                          spec
                                          staticChannelConfig.Network
            result {
                let! (htlcTimeoutTxs, htlcSuccessTxs) =
                    Transactions.makeHTLCTxs (commitTx.Value.GetGlobalTransaction())
                                             (staticChannelConfig.LocalParams.DustLimitSatoshis)
                                             remoteCommitmentPubKeys.RevocationPubKey
                                             staticChannelConfig.RemoteParams.ToSelfDelay
                                             localCommitmentPubKeys.DelayedPaymentPubKey
                                             localCommitmentPubKeys.HtlcPubKey
                                             remoteCommitmentPubKeys.HtlcPubKey
                                             (spec)
                                             staticChannelConfig.Network
                return (commitTx, htlcTimeoutTxs, htlcSuccessTxs)
            }


        let sortBothHTLCs (htlcTimeoutTxs: HTLCTimeoutTx list) (htlcSuccessTxs: HTLCSuccessTx list) =
            let timeoutTXsV = (htlcTimeoutTxs |> Seq.cast<IHTLCTx>)
            let successTXsV = (htlcSuccessTxs |> Seq.cast<IHTLCTx>)
            Seq.append timeoutTXsV successTXsV
            |> List.ofSeq
            |> List.sortBy(fun htlc -> htlc.Value.GetGlobalTransaction().Inputs.[htlc.WhichInput].PrevOut.N)

        let checkUpdateFee (channelOptions: ChannelOptions)
                           (msg: UpdateFeeMsg)
                           (localFeeRate: FeeRatePerKw) =
            let maxMismatch = channelOptions.MaxFeeRateMismatchRatio
            UpdateFeeValidation.checkFeeDiffTooHigh (msg) (localFeeRate) (maxMismatch)

    let sendFulfill (op: OperationFulfillHTLC)
                    (cm: Commitments)
                    (savedChannelState: SavedChannelState)
                    (remoteNextCommitInfo: RemoteNextCommitInfo) =
        match savedChannelState.GetIncomingHTLCCrossSigned remoteNextCommitInfo op.Id with
        | Some htlc when (cm.LocalChanges.Proposed |> Helpers.isAlreadySent htlc) ->
            htlc.HTLCId |> htlcAlreadySent
        | Some htlc when (htlc.PaymentHash = op.PaymentPreimage.Hash) ->
            let msgToSend: UpdateFulfillHTLCMsg = {
                ChannelId = savedChannelState.StaticChannelConfig.ChannelId()
                HTLCId = op.Id
                PaymentPreimage = op.PaymentPreimage
            }
            let newCommitments = cm.AddLocalProposal(msgToSend)
            (msgToSend, newCommitments) |> Ok
        | Some htlc ->
            (htlc.PaymentHash, op.PaymentPreimage)
            |> invalidPaymentPreimage
        | None ->
            op.Id
            |> unknownHTLCId

    let receiveFulfill (msg: UpdateFulfillHTLCMsg)
                       (cm: Commitments)
                       (savedChannelState: SavedChannelState)
                       (remoteNextCommitInfo: RemoteNextCommitInfo) =
        match savedChannelState.GetOutgoingHTLCCrossSigned remoteNextCommitInfo msg.HTLCId with
        | Some htlc when htlc.PaymentHash = msg.PaymentPreimage.Hash ->
            let commitments = cm.AddRemoteProposal(msg)
            commitments |> Ok
        | Some htlc ->
            (htlc.PaymentHash, msg.PaymentPreimage)
            |> invalidPaymentPreimage
        | None ->
            msg.HTLCId
            |> unknownHTLCId

    let sendFail (nodeSecret: NodeSecret)
                 (op: OperationFailHTLC)
                 (cm: Commitments)
                 (savedChannelState: SavedChannelState)
                 (remoteNextCommitInfo: RemoteNextCommitInfo) =
        match savedChannelState.GetIncomingHTLCCrossSigned remoteNextCommitInfo op.Id with
        | Some htlc when  (cm.LocalChanges.Proposed |> Helpers.isAlreadySent htlc) ->
            htlc.HTLCId |> htlcAlreadySent
        | Some htlc ->
            let ad = htlc.PaymentHash.ToBytes()
            let rawPacket = htlc.OnionRoutingPacket.ToBytes()
            Sphinx.parsePacket (nodeSecret.RawKey()) ad rawPacket
            |> Result.mapError(ChannelError.CryptoError)
            >>= fun ({ SharedSecret = ss}) ->
                let reason =
                    op.Reason
                    |> function Choice1Of2 b -> Sphinx.forwardErrorPacket(b, ss) | Choice2Of2 f -> Sphinx.ErrorPacket.Create(ss, f)
                let f = {
                    UpdateFailHTLCMsg.ChannelId = savedChannelState.StaticChannelConfig.ChannelId()
                    HTLCId = op.Id
                    Reason = { Data = reason }
                }
                let nextCommitments = cm.AddLocalProposal(f)
                Ok (f, nextCommitments)
        | None ->
            op.Id |> unknownHTLCId

    let receiveFail (msg: UpdateFailHTLCMsg)
                    (cm: Commitments)
                    (savedChannelState: SavedChannelState)
                    (remoteNextCommitInfo: RemoteNextCommitInfo) =
        match savedChannelState.GetOutgoingHTLCCrossSigned remoteNextCommitInfo msg.HTLCId with
        | Some _htlc ->
            result {
                let! _origin =
                    match cm.OriginChannels.TryGetValue(msg.HTLCId) with
                    | true, origin -> Ok origin
                    | false, _ ->
                        msg.HTLCId |> htlcOriginNowKnown
                let nextC = cm.AddRemoteProposal(msg)
                return nextC
            }
        | None ->
            msg.HTLCId |> unknownHTLCId


    let sendFailMalformed (op: OperationFailMalformedHTLC)
                          (cm: Commitments)
                          (savedChannelState: SavedChannelState)
                          (remoteNextCommitInfo: RemoteNextCommitInfo) =
        // BADONION bit must be set in failure code
        if (op.FailureCode.Value &&& OnionError.BADONION) = 0us then
            op.FailureCode |> invalidFailureCode
        else
            match savedChannelState.GetIncomingHTLCCrossSigned remoteNextCommitInfo op.Id with
            | Some htlc when (cm.LocalChanges.Proposed |> Helpers.isAlreadySent htlc) ->
                htlc.HTLCId |> htlcAlreadySent
            | Some _htlc ->
                let msg = {
                    UpdateFailMalformedHTLCMsg.ChannelId = savedChannelState.StaticChannelConfig.ChannelId()
                    HTLCId = op.Id
                    Sha256OfOnion = op.Sha256OfOnion
                    FailureCode = op.FailureCode
                }
                let nextCommitments = cm.AddLocalProposal(msg)
                Ok (msg, nextCommitments)
            | None ->
                op.Id |> unknownHTLCId

    let receiveFailMalformed (msg: UpdateFailMalformedHTLCMsg)
                             (cm: Commitments)
                             (savedChannelState: SavedChannelState)
                             (remoteNextCommitInfo: RemoteNextCommitInfo) =
        if msg.FailureCode.Value &&& OnionError.BADONION = 0us then
            msg.FailureCode |> invalidFailureCode
        else
            match savedChannelState.GetOutgoingHTLCCrossSigned remoteNextCommitInfo msg.HTLCId with
            | Some _htlc ->
                result {
                    let! _origin =
                        match cm.OriginChannels.TryGetValue(msg.HTLCId) with
                        | true, o -> Ok o
                        | false, _ ->
                            msg.HTLCId |> htlcOriginNowKnown
                    let nextC = cm.AddRemoteProposal(msg)
                    return nextC
                }
            | None ->
                msg.HTLCId |> unknownHTLCId

    let sendFee (op: OperationUpdateFee)
                (savedChannelState: SavedChannelState)
                (cm: Commitments) =
            if (not savedChannelState.StaticChannelConfig.IsFunder) then
                "Local is Fundee so it cannot send update fee" |> apiMisuse
            else
                let fee = {
                    UpdateFeeMsg.ChannelId = savedChannelState.StaticChannelConfig.ChannelId()
                    FeeRatePerKw = op.FeeRatePerKw
                }
                let c1 = cm.AddLocalProposal(fee)
                result {
                    let! reduced =
                        savedChannelState.RemoteCommit.Spec.Reduce(c1.RemoteChanges.ACKed, c1.LocalChanges.Proposed) |> expectTransactionError
                    // A node cannot spend pending incoming htlcs, and need to keep funds above the reserve required by
                    // the counter party, after paying the fee, we look from remote's point of view, so if local is funder
                    // remote doesn't pay the fees.
                    let fees = Transactions.commitTxFee(savedChannelState.StaticChannelConfig.RemoteParams.DustLimitSatoshis) reduced
                    let missing = reduced.ToRemote.ToMoney() - savedChannelState.StaticChannelConfig.RemoteParams.ChannelReserveSatoshis - fees
                    if (missing < Money.Zero) then
                        return!
                            (savedChannelState.StaticChannelConfig.LocalParams.ChannelReserveSatoshis, fees,  (-1 * missing))
                            |> cannotAffordFee
                    else
                        return fee, c1
                }

    let receiveFee (channelOptions: ChannelOptions)
                   (localFeerate)
                   (msg: UpdateFeeMsg)
                   (savedChannelState: SavedChannelState)
                   (cm: Commitments) =
        if savedChannelState.StaticChannelConfig.IsFunder then
            "Remote is Fundee so it cannot send update fee" |> apiMisuse
        else
            result {
                do! Helpers.checkUpdateFee channelOptions msg localFeerate
                let nextCommitments = cm.AddRemoteProposal(msg)
                let! reduced =
                    savedChannelState.LocalCommit.Spec.Reduce(
                        nextCommitments.LocalChanges.ACKed,
                        nextCommitments.RemoteChanges.Proposed
                    ) |> expectTransactionError
                
                let fees = Transactions.commitTxFee(savedChannelState.StaticChannelConfig.RemoteParams.DustLimitSatoshis) reduced
                let missing = reduced.ToRemote.ToMoney() - savedChannelState.StaticChannelConfig.RemoteParams.ChannelReserveSatoshis - fees
                if (missing < Money.Zero) then
                    return!
                        (savedChannelState.StaticChannelConfig.LocalParams.ChannelReserveSatoshis, fees,  (-1 * missing))
                        |> cannotAffordFee
                else
                    return nextCommitments
            }

    let sendCommit (channelPrivKeys: ChannelPrivKeys)
                   (cm: Commitments)
                   (savedChannelState: SavedChannelState)
                   (remoteNextCommitInfo: RemoteNextCommitInfo) =
        match remoteNextCommitInfo with
        | RemoteNextCommitInfo.Revoked remoteNextPerCommitmentPoint ->
            result {
                // remote commitment will include all local changes + remote acked changes
                let! spec = savedChannelState.RemoteCommit.Spec.Reduce(cm.RemoteChanges.ACKed, cm.LocalChanges.Proposed) |> expectTransactionError
                let! (remoteCommitTx, htlcTimeoutTxs, htlcSuccessTxs) =
                    Helpers.makeRemoteTxs savedChannelState.StaticChannelConfig
                                          (savedChannelState.RemoteCommit.Index.NextCommitment())
                                          (channelPrivKeys.ToChannelPubKeys())
                                          (remoteNextPerCommitmentPoint)
                                          (spec)
                    |> expectTransactionErrors
                let signature,_ = channelPrivKeys.SignWithFundingPrivKey remoteCommitTx.Value
                let sortedHTLCTXs = Helpers.sortBothHTLCs htlcTimeoutTxs htlcSuccessTxs
                let htlcSigs =
                    sortedHTLCTXs
                    |> List.map(
                            (fun htlc -> channelPrivKeys.SignHtlcTx htlc.Value remoteNextPerCommitmentPoint)
                            >> fst
                            >> (fun txSig -> txSig.Signature)
                            )
                let msg = {
                    CommitmentSignedMsg.ChannelId = savedChannelState.StaticChannelConfig.ChannelId()
                    Signature = !> signature.Signature
                    HTLCSignatures = htlcSigs |> List.map (!>)
                }
                let nextRemoteCommitInfo = {
                    savedChannelState.RemoteCommit
                    with
                        Index = savedChannelState.RemoteCommit.Index.NextCommitment()
                        Spec = spec
                        RemotePerCommitmentPoint = remoteNextPerCommitmentPoint
                        TxId = remoteCommitTx.GetTxId()
                }
                let nextCommitments = {
                    cm with
                        LocalChanges = {
                            cm.LocalChanges with
                                Proposed = []
                                Signed = cm.LocalChanges.Proposed
                        }
                        RemoteChanges = {
                            cm.RemoteChanges with
                                ACKed = []
                                Signed = cm.RemoteChanges.ACKed
                        }
                }
                return msg, nextCommitments, nextRemoteCommitInfo
            }
        | RemoteNextCommitInfo.Waiting _ ->
            CanNotSignBeforeRevocation |> Error

    let private checkSignatureCountMismatch(sortedHTLCTXs: IHTLCTx list) (msg) =
        if (sortedHTLCTXs.Length <> msg.HTLCSignatures.Length) then
            signatureCountMismatch (sortedHTLCTXs.Length, msg.HTLCSignatures.Length)
        else
            Ok()

    let receiveCommit (channelPrivKeys: ChannelPrivKeys)
                      (msg: CommitmentSignedMsg)
                      (cm: Commitments)
                      (savedChannelState: SavedChannelState)
                          : Result<RevokeAndACKMsg * SavedChannelState * Commitments, ChannelError> =
        if cm.RemoteHasChanges() |> not then
            ReceivedCommitmentSignedWhenWeHaveNoPendingChanges |> Error
        else
            let commitmentSeed = channelPrivKeys.CommitmentSeed
            let localChannelKeys = channelPrivKeys.ToChannelPubKeys()
            let remoteChannelKeys = savedChannelState.StaticChannelConfig.RemoteChannelPubKeys
            let nextI = savedChannelState.LocalCommit.Index.NextCommitment()
            result {
                let! spec = savedChannelState.LocalCommit.Spec.Reduce(cm.LocalChanges.ACKed, cm.RemoteChanges.Proposed) |> expectTransactionError
                let localPerCommitmentPoint = commitmentSeed.DerivePerCommitmentPoint nextI
                let! (localCommitTx, htlcTimeoutTxs, htlcSuccessTxs) =
                    Helpers.makeLocalTXs
                        savedChannelState.StaticChannelConfig
                        nextI
                        (channelPrivKeys.ToChannelPubKeys())
                        localPerCommitmentPoint
                        spec
                    |> expectTransactionErrors
                let signature, signedCommitTx = channelPrivKeys.SignWithFundingPrivKey localCommitTx.Value

                let sigPair =
                    let localSigPair = seq [(localChannelKeys.FundingPubKey.RawPubKey(), signature)]
                    let remoteSigPair = seq[ (remoteChannelKeys.FundingPubKey.RawPubKey(), TransactionSignature(msg.Signature.Value, SigHash.All)) ]
                    Seq.append localSigPair remoteSigPair
                let tmp =
                    Transactions.checkTxFinalized signedCommitTx CommitTx.WhichInput sigPair
                    |> expectTransactionError
                let! finalizedCommitTx = tmp
                let sortedHTLCTXs = Helpers.sortBothHTLCs htlcTimeoutTxs htlcSuccessTxs
                do! checkSignatureCountMismatch sortedHTLCTXs msg
                
                let _localHTLCSigs, sortedHTLCTXs =
                    let localHtlcSigsAndHTLCTxs =
                        sortedHTLCTXs |> List.map(fun htlc ->
                            channelPrivKeys.SignHtlcTx htlc.Value localPerCommitmentPoint
                        )
                    localHtlcSigsAndHTLCTxs |> List.map(fst), localHtlcSigsAndHTLCTxs |> List.map(snd) |> Seq.cast<IHTLCTx> |> List.ofSeq

                let remoteHTLCPubKey = localPerCommitmentPoint.DeriveHtlcPubKey remoteChannelKeys.HtlcBasepoint

                let checkHTLCSig (htlc: IHTLCTx, remoteECDSASig: LNECDSASignature): Result<_, _> =
                    let remoteS = TransactionSignature(remoteECDSASig.Value, SigHash.All)
                    match htlc with
                    | :? HTLCTimeoutTx ->
                        (Transactions.checkTxFinalized (htlc.Value) (0) (seq [(remoteHTLCPubKey.RawPubKey(), remoteS)]))
                        |> Result.map(box)
                    // we cannot check that htlc-success tx are spendable because we need the payment preimage; thus we only check the remote sig
                    | :? HTLCSuccessTx ->
                        (Transactions.checkSigAndAdd (htlc) (remoteS) (remoteHTLCPubKey.RawPubKey()))
                        |> Result.map(box)
                    | _ -> failwith "Unreachable!"

                let! txList =
                    List.zip sortedHTLCTXs msg.HTLCSignatures
                    |> List.map(checkHTLCSig)
                    |> List.sequenceResultA
                    |> expectTransactionErrors
                let successTxs =
                    txList |> List.choose(fun o ->
                        match o with
                        | :? HTLCSuccessTx as tx -> Some tx
                        | _ -> None
                    )
                let finalizedTxs =
                    txList |> List.choose(fun o ->
                        match o with
                        | :? FinalizedTx as tx -> Some tx
                        | _ -> None
                    )
                let localPerCommitmentSecret =
                    channelPrivKeys.CommitmentSeed.DerivePerCommitmentSecret savedChannelState.LocalCommit.Index
                let localNextPerCommitmentPoint =
                    let perCommitmentSecret =
                        channelPrivKeys.CommitmentSeed.DerivePerCommitmentSecret
                            (savedChannelState.LocalCommit.Index.NextCommitment().NextCommitment())
                    perCommitmentSecret.PerCommitmentPoint()

                let nextMsg = {
                    RevokeAndACKMsg.ChannelId = savedChannelState.StaticChannelConfig.ChannelId()
                    PerCommitmentSecret = localPerCommitmentSecret
                    NextPerCommitmentPoint = localNextPerCommitmentPoint
                }
                
                let localCommit1 = { LocalCommit.Index = savedChannelState.LocalCommit.Index.NextCommitment()
                                     Spec = spec
                                     PublishableTxs = { PublishableTxs.CommitTx = finalizedCommitTx
                                                        HTLCTxs = finalizedTxs }
                                     PendingHTLCSuccessTxs = successTxs }
                let nextSavedChannelState = {
                    savedChannelState with
                        LocalCommit = localCommit1
                }
                let nextCommitments =
                    let ourChanges1 = { cm.LocalChanges with ACKed = []}
                    let theirChanges1 = { cm.RemoteChanges with Proposed = []; ACKed = (cm.RemoteChanges.ACKed @ cm.RemoteChanges.Proposed) }
                    let completedOutgoingHTLCs =
                        let t1 = savedChannelState.LocalCommit.Spec.OutgoingHTLCs
                                 |> Map.toSeq |> Seq.map (fun (k, _) -> k) |> Set.ofSeq
                        let t2 = localCommit1.Spec.OutgoingHTLCs
                                 |> Map.toSeq |> Seq.map (fun (k, _) -> k) |> Set.ofSeq
                        Set.difference t1 t2
                    let originChannels1 = cm.OriginChannels |> Map.filter(fun k _ -> Set.contains k completedOutgoingHTLCs)
                    {
                        cm with
                            LocalChanges = ourChanges1
                            RemoteChanges = theirChanges1
                            OriginChannels = originChannels1
                    }
                return nextMsg, nextSavedChannelState, nextCommitments
            }

module ForceCloseFundsRecovery =
    type ValidateCommitmentTxError =
        | InvalidTxVersionForCommitmentTx of uint32
        | TxHasNoInputs
        | TxHasMultipleInputs of int
        | DoesNotSpendChannelFunds of OutPoint
        | InvalidLockTimeAndSequenceForCommitmentTx of LockTime * Sequence
        member this.Message: string =
            match this with
            | InvalidTxVersionForCommitmentTx version ->
                sprintf "invalid tx version for commitment tx (%i)" version
            | TxHasNoInputs ->
                "tx has no inputs"
            | TxHasMultipleInputs n ->
                sprintf "tx has multiple inputs (%i)" n
            | DoesNotSpendChannelFunds outPoint ->
                sprintf
                    "tx does not spend from the channel funds but spends from a different \
                    outpoint (%s)"
                    (outPoint.ToString())
            | InvalidLockTimeAndSequenceForCommitmentTx(lockTime, sequence) ->
                sprintf
                    "invalid lock time and sequence for commitment tx \
                    (locktime = %s, sequence = %s)"
                    (lockTime.ToString())
                    (sequence.ToString())

    let tryGetObscuredCommitmentNumber (fundingOutPoint: OutPoint)
                                       (transaction: Transaction)
                                           : Result<ObscuredCommitmentNumber, ValidateCommitmentTxError> = result {
        if transaction.Version <> TxVersionNumberOfCommitmentTxs then
            return! Error <| InvalidTxVersionForCommitmentTx transaction.Version
        if transaction.Inputs.Count = 0 then
            return! Error <| TxHasNoInputs
        if transaction.Inputs.Count > 1 then
            return! Error <| TxHasMultipleInputs transaction.Inputs.Count
        let txIn = Seq.exactlyOne transaction.Inputs
        if fundingOutPoint <> txIn.PrevOut then
            return! Error <| DoesNotSpendChannelFunds txIn.PrevOut
        match ObscuredCommitmentNumber.TryFromLockTimeAndSequence transaction.LockTime txIn.Sequence with
        | None ->
            return! Error <| InvalidLockTimeAndSequenceForCommitmentTx(transaction.LockTime, txIn.Sequence)
        | Some obscuredCommitmentNumber ->
            return obscuredCommitmentNumber
    }

    let createPenaltyTx (perCommitmentSecret: PerCommitmentSecret)
                        (savedChannelState: SavedChannelState)
                        (localChannelPrivKeys: ChannelPrivKeys)
                            : TransactionBuilder =
        let localChannelPubKeys = localChannelPrivKeys.ToChannelPubKeys()

        let perCommitmentPoint = perCommitmentSecret.PerCommitmentPoint()

        let localCommitmentPubKeys =
            perCommitmentPoint.DeriveCommitmentPubKeys localChannelPubKeys

        let remoteCommitmentPubKeys =
            perCommitmentPoint.DeriveCommitmentPubKeys savedChannelState.StaticChannelConfig.RemoteChannelPubKeys

        let remoteParams = 
            savedChannelState.StaticChannelConfig.RemoteParams
        
        let remoteCommit = 
            savedChannelState.RemoteCommit

        let transactionBuilder = createDeterministicTransactionBuilder savedChannelState.StaticChannelConfig.Network

        let toRemoteScriptPubKey =
            localCommitmentPubKeys
                .PaymentPubKey
                .RawPubKey()
                .WitHash.ScriptPubKey

        let toLocalScriptPubKey =
            Scripts.toLocalDelayed
                localCommitmentPubKeys.RevocationPubKey
                remoteParams.ToSelfDelay
                remoteCommitmentPubKeys.DelayedPaymentPubKey

        let toLocalWitScriptPubKey = toLocalScriptPubKey.WitHash.ScriptPubKey

        let amounts = Commitments.RemoteCommitAmount savedChannelState.StaticChannelConfig.IsFunder remoteParams remoteCommit

        let toLocalTxOut = 
            TxOut(amounts.ToLocal, toLocalWitScriptPubKey)
        let toRemoteTxOut = 
            TxOut(amounts.ToRemote, toRemoteScriptPubKey)

        let outputs = 
            seq {
                if amounts.ToLocal > remoteParams.DustLimitSatoshis then
                    yield toLocalTxOut

                if amounts.ToRemote > remoteParams.DustLimitSatoshis then
                    yield toRemoteTxOut
            }
            |> Seq.sortWith TxOut.LexicographicCompare

        let toRemoteIndexOpt =
            outputs
            |> Seq.tryFindIndex (fun out -> out.ScriptPubKey = toRemoteScriptPubKey)

        toRemoteIndexOpt
        |> Option.iter(fun toRemoteIndex ->
            let localPaymentPrivKey =
                perCommitmentPoint.DerivePaymentPrivKey localChannelPrivKeys.PaymentBasepointSecret

            transactionBuilder
                .AddKeys(localPaymentPrivKey.RawKey())
                .AddCoins(
                    Coin(
                        remoteCommit.TxId.Value,
                        toRemoteIndex |> uint32,
                        toRemoteTxOut.Value,
                        toRemoteTxOut.ScriptPubKey
                    )
                )
            |> ignore
        )

        let toLocalIndexOpt =
            outputs
            |> Seq.tryFindIndex (fun out -> out.ScriptPubKey = toLocalWitScriptPubKey)

        toLocalIndexOpt
        |> Option.iter(fun toLocalIndex ->
            let revocationPrivKey =
                perCommitmentSecret.DeriveRevocationPrivKey
                    localChannelPrivKeys.RevocationBasepointSecret

            transactionBuilder.Extensions.Add(CommitmentToLocalExtension())
            transactionBuilder
                .AddKeys(revocationPrivKey.RawKey())
                .AddCoins(
                    ScriptCoin(
                        remoteCommit.TxId.Value,
                        toLocalIndex |> uint32,
                        toLocalTxOut.Value,
                        toLocalWitScriptPubKey,
                        toLocalScriptPubKey
                    )
                )
            |> ignore
        )

        transactionBuilder

    type RemoteCommitmentTxRecoveryError =
        | InvalidCommitmentTx of ValidateCommitmentTxError
        | CommitmentNumberFromTheFuture of CommitmentNumber
        | BalanceBelowDustLimit
        member this.Message: string =
            match this with
            | InvalidCommitmentTx validateCommitmentTxError ->
                sprintf "invalid commitment tx: %s" (validateCommitmentTxError.Message)
            | CommitmentNumberFromTheFuture commitmentNumber ->
                sprintf "commitment number from the future (%s)" (commitmentNumber.ToString())
            | BalanceBelowDustLimit ->
                "balance below dust limit"

    /// This function returns a TransactionBuilder with the necessary inputs
    /// added to the transaction to reclaim the funds from the supplied
    /// commitment transaction. It is the caller's responsibility to add outputs
    /// and fees to the TransactionBuilder before building the transaction.
    let tryGetFundsFromRemoteCommitmentTx (localChannelPrivKeys: ChannelPrivKeys)
                                          (savedChannelState: SavedChannelState)
                                          (transaction: Transaction)
                                              : Result<TransactionBuilder, RemoteCommitmentTxRecoveryError> = result {
        let! obscuredCommitmentNumber =
            tryGetObscuredCommitmentNumber
                savedChannelState.StaticChannelConfig.FundingScriptCoin.Outpoint
                transaction
            |> Result.mapError InvalidCommitmentTx
        let localChannelPubKeys = localChannelPrivKeys.ToChannelPubKeys()
        let remoteChannelPubKeys = savedChannelState.StaticChannelConfig.RemoteChannelPubKeys
        let commitmentNumber =
            obscuredCommitmentNumber.Unobscure
                savedChannelState.StaticChannelConfig.IsFunder
                localChannelPubKeys.PaymentBasepoint
                remoteChannelPubKeys.PaymentBasepoint
        let perCommitmentSecretOpt =
            savedChannelState.RemotePerCommitmentSecrets.GetPerCommitmentSecret commitmentNumber
        let! perCommitmentPoint =
            match perCommitmentSecretOpt with
            | Some perCommitmentSecret -> Ok <| perCommitmentSecret.PerCommitmentPoint()
            | None ->
                if savedChannelState.RemoteCommit.Index = commitmentNumber then
                    Ok <| savedChannelState.RemoteCommit.RemotePerCommitmentPoint
                else
                    Error <| CommitmentNumberFromTheFuture commitmentNumber

        let localCommitmentPubKeys =
            perCommitmentPoint.DeriveCommitmentPubKeys localChannelPubKeys

        let toRemoteScriptPubKey =
            localCommitmentPubKeys.PaymentPubKey.RawPubKey().WitHash.ScriptPubKey
        let toRemoteIndexOpt =
            Seq.tryFindIndex
                (fun (txOut: TxOut) -> txOut.ScriptPubKey = toRemoteScriptPubKey)
                transaction.Outputs
        let! toRemoteIndex =
            match toRemoteIndexOpt with
            | Some toRemoteIndex -> Ok toRemoteIndex
            | None -> Error BalanceBelowDustLimit
        let localPaymentPrivKey =
            perCommitmentPoint.DerivePaymentPrivKey
                localChannelPrivKeys.PaymentBasepointSecret

        return
            (createDeterministicTransactionBuilder savedChannelState.StaticChannelConfig.Network)
                .SetVersion(TxVersionNumberOfCommitmentTxs)
                .AddKeys(localPaymentPrivKey.RawKey())
                .AddCoins(Coin(transaction, uint32 toRemoteIndex))
    }

    type LocalCommitmentTxRecoveryError =
        | InvalidCommitmentTx of ValidateCommitmentTxError
        | BalanceBelowDustLimit
        member this.Message: string =
            match this with
            | InvalidCommitmentTx validateCommitmentTxError ->
                sprintf "invalid commitment tx: %s" (validateCommitmentTxError.Message)
            | BalanceBelowDustLimit ->
                "balance below dust limit"

    /// This function returns a TransactionBuilder with the necessary inputs
    /// added to the transaction to reclaim the funds from the supplied
    /// commitment transaction. It is the caller's responsibility to add outputs
    /// and fees to the TransactionBuilder before building the transaction.
    let tryGetFundsFromLocalCommitmentTx (localChannelPrivKeys: ChannelPrivKeys)
                                         (staticChannelConfig: StaticChannelConfig)
                                         (transaction: Transaction)
                                             : Result<TransactionBuilder, LocalCommitmentTxRecoveryError> = result {
        let! obscuredCommitmentNumber =
            tryGetObscuredCommitmentNumber
                staticChannelConfig.FundingScriptCoin.Outpoint
                transaction
            |> Result.mapError InvalidCommitmentTx
        let localChannelPubKeys = localChannelPrivKeys.ToChannelPubKeys()
        let remoteChannelPubKeys = staticChannelConfig.RemoteChannelPubKeys
        let commitmentNumber =
            obscuredCommitmentNumber.Unobscure
                staticChannelConfig.IsFunder
                localChannelPubKeys.PaymentBasepoint
                remoteChannelPubKeys.PaymentBasepoint

        let perCommitmentPoint =
            localChannelPrivKeys.CommitmentSeed.DerivePerCommitmentPoint commitmentNumber
        let localCommitmentPubKeys =
            perCommitmentPoint.DeriveCommitmentPubKeys localChannelPubKeys
        let remoteCommitmentPubKeys =
            perCommitmentPoint.DeriveCommitmentPubKeys remoteChannelPubKeys

        let transactionBuilder = staticChannelConfig.Network.CreateTransactionBuilder()

        let toLocalScriptPubKey =
            Scripts.toLocalDelayed
                remoteCommitmentPubKeys.RevocationPubKey
                staticChannelConfig.LocalParams.ToSelfDelay
                localCommitmentPubKeys.DelayedPaymentPubKey
        let toLocalIndexOpt =
            let toLocalWitScriptPubKey = toLocalScriptPubKey.WitHash.ScriptPubKey
            Seq.tryFindIndex
                (fun (txOut: TxOut) -> txOut.ScriptPubKey = toLocalWitScriptPubKey)
                transaction.Outputs
        let! toLocalIndex =
            match toLocalIndexOpt with
            | Some toLocalIndex -> Ok toLocalIndex
            | None -> Error BalanceBelowDustLimit

        let delayedPaymentPrivKey =
            perCommitmentPoint.DeriveDelayedPaymentPrivKey
                localChannelPrivKeys.DelayedPaymentBasepointSecret

        transactionBuilder
            .SetVersion(TxVersionNumberOfCommitmentTxs)
            .Extensions.Add(CommitmentToLocalExtension())
        return
            transactionBuilder
                .AddKeys(delayedPaymentPrivKey.RawKey())
                .AddCoin(
                    ScriptCoin(transaction, uint32 toLocalIndex, toLocalScriptPubKey),
                    CoinOptions(
                        Sequence = (Nullable <| Sequence(uint32 staticChannelConfig.LocalParams.ToSelfDelay.Value))
                    )
                )
    }

