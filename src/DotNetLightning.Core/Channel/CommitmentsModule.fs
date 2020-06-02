namespace DotNetLightning.Channel

open NBitcoin

open ResultUtils

open DotNetLightning.Utils
open DotNetLightning.Transactions
open DotNetLightning.Crypto
open DotNetLightning.Chain
open DotNetLightning.Serialize.Msgs

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
            (ctx: ISecp256k1)
            (commitTxNumber: CommitmentNumber)
            (localParams: LocalParams)
            (remoteParams: RemoteParams)
            (commitmentInput: ScriptCoin)
            (remotePerCommitmentPoint: CommitmentPubKey)
            (spec) (n) =
            let channelKeys = localParams.ChannelPubKeys
            let pkGen = Generators.derivePubKey ctx remotePerCommitmentPoint.PubKey
            let localPaymentPK = pkGen channelKeys.PaymentBasePubKey
            let localHTLCPK = pkGen channelKeys.HTLCBasePubKey
            let remotePaymentPK = pkGen remoteParams.PaymentBasePoint
            let remoteDelayedPaymentPK = pkGen remoteParams.DelayedPaymentBasePoint
            let remoteHTLCPK = pkGen remoteParams.HTLCBasePoint
            let remoteRevocationPK = pkGen channelKeys.RevocationBasePubKey
            let commitTx =
                Transactions.makeCommitTx commitmentInput 
                                          commitTxNumber
                                          remoteParams.PaymentBasePoint
                                          channelKeys.PaymentBasePubKey
                                          (not localParams.IsFunder)
                                          (remoteParams.DustLimitSatoshis)
                                          (remoteRevocationPK)
                                          (localParams.ToSelfDelay)
                                          (remoteDelayedPaymentPK)
                                          (localPaymentPK)
                                          (remoteHTLCPK)
                                          (localHTLCPK)
                                          (spec)
                                          (n)
            result {
                 let! (htlcTimeoutTxs, htlcSuccessTxs) =
                     Transactions.makeHTLCTxs
                         (commitTx.Value.GetGlobalTransaction())
                         (remoteParams.DustLimitSatoshis)
                         (remoteRevocationPK)
                         (localParams.ToSelfDelay)
                         (remoteDelayedPaymentPK)
                         (remoteHTLCPK)
                         (localHTLCPK)
                         (spec) (n)
                return (commitTx, htlcTimeoutTxs, htlcSuccessTxs)
            }

        let makeLocalTXs
            (ctx: ISecp256k1)
            (commitTxNumber: CommitmentNumber)
            (localParams: LocalParams)
            (remoteParams: RemoteParams)
            (commitmentInput: ScriptCoin)
            (localPerCommitmentPoint: CommitmentPubKey)
            (spec: CommitmentSpec)
            n: Result<(CommitTx * HTLCTimeoutTx list * HTLCSuccessTx list), _> =
            let channelKeys = localParams.ChannelPubKeys
            let pkGen = Generators.derivePubKey ctx localPerCommitmentPoint.PubKey
            let localPaymentPK = pkGen channelKeys.PaymentBasePubKey
            let localDelayedPaymentPK = pkGen channelKeys.DelayedPaymentBasePubKey
            let localHTLCPK = pkGen channelKeys.HTLCBasePubKey
            let remotePaymentPK = pkGen remoteParams.PaymentBasePoint
            let remoteHTLCPK = pkGen remoteParams.HTLCBasePoint
            let localRevocationPK = pkGen remoteParams.RevocationBasePoint
            let commitTx =
                Transactions.makeCommitTx commitmentInput
                                          commitTxNumber
                                          channelKeys.PaymentBasePubKey
                                          remoteParams.PaymentBasePoint
                                          localParams.IsFunder
                                          localParams.DustLimitSatoshis
                                          localRevocationPK
                                          remoteParams.ToSelfDelay
                                          localDelayedPaymentPK
                                          remotePaymentPK
                                          localHTLCPK
                                          remoteHTLCPK
                                          spec n
            result {
                let! (htlcTimeoutTxs, htlcSuccessTxs) =
                    Transactions.makeHTLCTxs (commitTx.Value.GetGlobalTransaction())
                                             (localParams.DustLimitSatoshis)
                                             (localRevocationPK)
                                             (remoteParams.ToSelfDelay)
                                             (localDelayedPaymentPK)
                                             (localHTLCPK)
                                             (remoteHTLCPK)
                                             (spec)
                                             (n)
                return (commitTx, htlcTimeoutTxs, htlcSuccessTxs)
            }


        let sortBothHTLCs (htlcTimeoutTxs: HTLCTimeoutTx list) (htlcSuccessTxs: HTLCSuccessTx list) =
            let timeoutTXsV = (htlcTimeoutTxs |> Seq.cast<IHTLCTx>)
            let successTXsV = (htlcSuccessTxs |> Seq.cast<IHTLCTx>)
            Seq.append timeoutTXsV successTXsV
            |> List.ofSeq
            |> List.sortBy(fun htlc -> htlc.Value.GetGlobalTransaction().Inputs.[htlc.WhichInput].PrevOut.N)

        let checkUpdateFee (config: ChannelConfig) (msg: UpdateFeeMsg) (localFeeRate: FeeRatePerKw) =
            let maxMismatch = config.ChannelOptions.MaxFeeRateMismatchRatio
            UpdateFeeValidation.checkFeeDiffTooHigh (msg) (localFeeRate) (maxMismatch)

    let sendFulfill (op: OperationFulfillHTLC) (cm: Commitments) =
        match cm.GetHTLCCrossSigned(Direction.In, op.Id) with
        | Some htlc when (cm.LocalChanges.Proposed |> Helpers.isAlreadySent htlc) ->
            htlc.HTLCId |> htlcAlreadySent
        | Some htlc when (htlc.PaymentHash = op.PaymentPreimage.Hash) ->
            let msgToSend: UpdateFulfillHTLCMsg =
                { ChannelId = cm.ChannelId; HTLCId = op.Id; PaymentPreimage = op.PaymentPreimage }
            let newCommitments = cm.AddLocalProposal(msgToSend)
            (msgToSend, newCommitments) |> Ok
        | Some htlc ->
            (htlc.PaymentHash, op.PaymentPreimage)
            |> invalidPaymentPreimage
        | None ->
            op.Id
            |> unknownHTLCId

    let receiveFulfill(msg: UpdateFulfillHTLCMsg) (cm: Commitments) =
        match cm.GetHTLCCrossSigned(Direction.Out, msg.HTLCId) with
        | Some htlc when htlc.PaymentHash = msg.PaymentPreimage.Hash ->
            let commitments = cm.AddRemoteProposal(msg)
            let origin = cm.OriginChannels |> Map.find(msg.HTLCId)
            [WeAcceptedFulfillHTLC(msg, origin, htlc, commitments)] |> Ok
        | Some htlc ->
            (htlc.PaymentHash, msg.PaymentPreimage)
            |> invalidPaymentPreimage
        | None ->
            msg.HTLCId
            |> unknownHTLCId

    let sendFail (localKey: Key) (op: OperationFailHTLC) (cm: Commitments) =
        match cm.GetHTLCCrossSigned(Direction.In, op.Id) with
        | Some htlc when  (cm.LocalChanges.Proposed |> Helpers.isAlreadySent htlc) ->
            htlc.HTLCId |> htlcAlreadySent
        | Some htlc ->
            let ad = htlc.PaymentHash.ToBytes()
            let rawPacket = htlc.OnionRoutingPacket.ToBytes()
            Sphinx.parsePacket localKey ad rawPacket |> Result.mapError(ChannelError.CryptoError)
            >>= fun ({ SharedSecret = ss}) ->
                let reason =
                    op.Reason
                    |> function Choice1Of2 b -> Sphinx.forwardErrorPacket(b, ss) | Choice2Of2 f -> Sphinx.ErrorPacket.Create(ss, f)
                let f = { UpdateFailHTLCMsg.ChannelId = cm.ChannelId
                          HTLCId = op.Id
                          Reason = { Data = reason } }
                let nextComitments = cm.AddLocalProposal(f)
                [ WeAcceptedOperationFailHTLC(f, nextComitments) ]
                |> Ok
        | None ->
            op.Id |> unknownHTLCId

    let receiveFail (msg: UpdateFailHTLCMsg) (cm: Commitments) =
        match cm.GetHTLCCrossSigned(Direction.Out, msg.HTLCId) with
        | Some htlc ->
            result {
                let! o =
                    match cm.OriginChannels.TryGetValue(msg.HTLCId) with
                    | true, origin -> Ok origin
                    | false, _ ->
                        msg.HTLCId |> htlcOriginNowKnown
                let nextC = cm.AddRemoteProposal(msg)
                return [WeAcceptedFailHTLC(o, htlc, nextC)]
            }
        | None ->
            msg.HTLCId |> unknownHTLCId


    let sendFailMalformed (op: OperationFailMalformedHTLC) (cm: Commitments) =
        // BADONION bit must be set in failure code
        if (op.FailureCode.Value &&& OnionError.BADONION) = 0us then
            op.FailureCode |> invalidFailureCode
        else
            match cm.GetHTLCCrossSigned(Direction.In, op.Id) with
            | Some htlc when (cm.LocalChanges.Proposed |> Helpers.isAlreadySent htlc) ->
                htlc.HTLCId |> htlcAlreadySent
            | Some _htlc ->
                let msg = { UpdateFailMalformedHTLCMsg.ChannelId = cm.ChannelId
                            HTLCId = op.Id
                            Sha256OfOnion = op.Sha256OfOnion
                            FailureCode = op.FailureCode }
                let nextCommitments = cm.AddLocalProposal(msg)
                [ WeAcceptedOperationFailMalformedHTLC(msg, nextCommitments) ]
                |> Ok
            | None ->
                op.Id |> unknownHTLCId

    let receiveFailMalformed (msg: UpdateFailMalformedHTLCMsg) (cm: Commitments) =
        if msg.FailureCode.Value &&& OnionError.BADONION = 0us then
            msg.FailureCode |> invalidFailureCode
        else
            match cm.GetHTLCCrossSigned(Direction.Out, msg.HTLCId) with
            | Some htlc ->
                result {
                    let! o =
                        match cm.OriginChannels.TryGetValue(msg.HTLCId) with
                        | true, o -> Ok o
                        | false, _ ->
                            msg.HTLCId |> htlcOriginNowKnown
                    let nextC = cm.AddRemoteProposal(msg)
                    return [WeAcceptedFailMalformedHTLC(o, htlc, nextC)]
                }
            | None ->
                msg.HTLCId |> unknownHTLCId

    let sendFee(op: OperationUpdateFee) (cm: Commitments) =
            if (not cm.LocalParams.IsFunder) then
                "Local is Fundee so it cannot send update fee" |> apiMisuse
            else
                let fee = { UpdateFeeMsg.ChannelId = cm.ChannelId
                            FeeRatePerKw = op.FeeRatePerKw }
                let c1 = cm.AddLocalProposal(fee)
                result {
                    let! reduced =
                        c1.RemoteCommit.Spec.Reduce(c1.RemoteChanges.ACKed, c1.LocalChanges.Proposed) |> expectTransactionError
                    // A node cannot spend pending incoming htlcs, and need to keep funds above the reserve required by
                    // the counter party, after paying the fee, we look from remote's point of view, so if local is funder
                    // remote doesn't pay the fees.
                    let fees = Transactions.commitTxFee(c1.RemoteParams.DustLimitSatoshis) reduced
                    let missing = reduced.ToRemote.ToMoney() - c1.RemoteParams.ChannelReserveSatoshis - fees
                    if (missing < Money.Zero) then
                        return!
                            (c1.LocalParams.ChannelReserveSatoshis, fees,  (-1 * missing))
                            |> cannotAffordFee
                    else
                        return
                            [ WeAcceptedOperationUpdateFee(fee, c1) ]
                }

    let receiveFee (config: ChannelConfig) (localFeerate) (msg: UpdateFeeMsg) (cm: Commitments) =
        if (cm.LocalParams.IsFunder) then
            "Remote is Fundee so it cannot send update fee" |> apiMisuse
        else
            result {
                do! Helpers.checkUpdateFee (config) (msg) (localFeerate)
                let c1 = cm.AddRemoteProposal(msg)
                let! reduced =
                    c1.LocalCommit.Spec.Reduce(c1.LocalChanges.ACKed, c1.RemoteChanges.Proposed) |> expectTransactionError
                
                let fees = Transactions.commitTxFee(c1.RemoteParams.DustLimitSatoshis) reduced
                let missing = reduced.ToRemote.ToMoney() - c1.RemoteParams.ChannelReserveSatoshis - fees
                if (missing < Money.Zero) then
                    return!
                        (c1.LocalParams.ChannelReserveSatoshis, fees,  (-1 * missing))
                        |> cannotAffordFee
                else
                    return
                        [ WeAcceptedUpdateFee msg ]
            }

    let sendCommit (ctx: ISecp256k1) (keyRepo: IKeysRepository) (n: Network) (cm: Commitments) =
        match cm.RemoteNextCommitInfo with
        | RemoteNextCommitInfo.Revoked remoteNextPerCommitmentPoint ->
            result {
                // remote commitment will include all local changes + remote acked changes
                let! spec = cm.RemoteCommit.Spec.Reduce(cm.RemoteChanges.ACKed, cm.LocalChanges.Proposed) |> expectTransactionError
                let! (remoteCommitTx, htlcTimeoutTxs, htlcSuccessTxs) =
                    Helpers.makeRemoteTxs (ctx)
                                          cm.RemoteCommit.Index.NextCommitment
                                          (cm.LocalParams)
                                          (cm.RemoteParams)
                                          (cm.FundingScriptCoin)
                                          (remoteNextPerCommitmentPoint)
                                          (spec) n |> expectTransactionErrors
                let signature,_ = keyRepo.GetSignatureFor(remoteCommitTx.Value, cm.LocalParams.ChannelPubKeys.FundingPubKey)
                let sortedHTLCTXs = Helpers.sortBothHTLCs htlcTimeoutTxs htlcSuccessTxs
                let htlcSigs =
                    sortedHTLCTXs
                    |> List.map(
                            (fun htlc -> keyRepo.GenerateKeyFromBasePointAndSign(htlc.Value, cm.LocalParams.ChannelPubKeys.HTLCBasePubKey, remoteNextPerCommitmentPoint.PubKey))
                            >> fst
                            >> (fun txSig -> txSig.Signature)
                            )
                let msg = { CommitmentSignedMsg.ChannelId = cm.ChannelId
                            Signature = !> signature.Signature
                            HTLCSignatures = htlcSigs |> List.map (!>) }
                let nextCommitments =
                    let nextRemoteCommitInfo = {
                        WaitingForRevocation.NextRemoteCommit = {
                            cm.RemoteCommit
                            with
                                Index = cm.RemoteCommit.Index.NextCommitment
                                Spec = spec
                                RemotePerCommitmentPoint = remoteNextPerCommitmentPoint
                                TxId = remoteCommitTx.GetTxId()
                        }
                        Sent = msg
                        SentAfterLocalCommitmentIndex = cm.LocalCommit.Index
                        ReSignASAP = false
                    }
                    { cm with RemoteNextCommitInfo = RemoteNextCommitInfo.Waiting(nextRemoteCommitInfo)
                              LocalChanges = { cm.LocalChanges with Proposed = []; Signed = cm.LocalChanges.Proposed }
                              RemoteChanges = { cm.RemoteChanges with ACKed = []; Signed = cm.RemoteChanges.ACKed } }
                return [ WeAcceptedOperationSign (msg, nextCommitments) ]
            }
        | RemoteNextCommitInfo.Waiting _ ->
            CanNotSignBeforeRevocation |> Error

    let private checkSignatureCountMismatch(sortedHTLCTXs: IHTLCTx list) (msg) =
        if (sortedHTLCTXs.Length <> msg.HTLCSignatures.Length) then
            signatureCountMismatch (sortedHTLCTXs.Length, msg.HTLCSignatures.Length)
        else
            Ok()
    let receiveCommit (ctx) (keyRepo: IKeysRepository) (msg: CommitmentSignedMsg) (n: Network) (cm: Commitments): Result<ChannelEvent list, ChannelError> =
        if cm.RemoteHasChanges() |> not then
            ReceivedCommitmentSignedWhenWeHaveNoPendingChanges |> Error
        else
            let chanPrivateKeys = keyRepo.GetChannelKeys cm.LocalParams.IsFunder
            let commitmentSeed = chanPrivateKeys.CommitmentSeed
            let chanKeys = cm.LocalParams.ChannelPubKeys
            let nextI = cm.LocalCommit.Index.NextCommitment
            result {
                let! spec = cm.LocalCommit.Spec.Reduce(cm.LocalChanges.ACKed, cm.RemoteChanges.Proposed) |> expectTransactionError
                let localPerCommitmentPoint = commitmentSeed.DeriveCommitmentPubKey nextI
                let! (localCommitTx, htlcTimeoutTxs, htlcSuccessTxs) =
                    Helpers.makeLocalTXs (ctx) (nextI) (cm.LocalParams) (cm.RemoteParams) (cm.FundingScriptCoin) (localPerCommitmentPoint) spec n
                    |> expectTransactionErrors
                let signature, signedCommitTx = keyRepo.GetSignatureFor (localCommitTx.Value, chanKeys.FundingPubKey)

                let sigPair =
                    let localSigPair = seq [(chanKeys.FundingPubKey, signature)]
                    let remoteSigPair = seq[ (cm.RemoteParams.FundingPubKey, TransactionSignature(msg.Signature.Value, SigHash.All)) ]
                    Seq.append localSigPair remoteSigPair
                let tmp = 
                    Transactions.checkTxFinalized signedCommitTx localCommitTx.WhichInput sigPair
                    |> expectTransactionError
                let! finalizedCommitTx = tmp
                let sortedHTLCTXs = Helpers.sortBothHTLCs htlcTimeoutTxs htlcSuccessTxs
                do! checkSignatureCountMismatch sortedHTLCTXs msg
                
                let _localHTLCSigs, sortedHTLCTXs =
                    let localHtlcSigsAndHTLCTxs =
                        sortedHTLCTXs |> List.map(fun htlc ->
                            keyRepo.GenerateKeyFromBasePointAndSign(htlc.Value, cm.LocalParams.ChannelPubKeys.HTLCBasePubKey, localPerCommitmentPoint.PubKey)
                        )
                    localHtlcSigsAndHTLCTxs |> List.map(fst), localHtlcSigsAndHTLCTxs |> List.map(snd) |> Seq.cast<IHTLCTx> |> List.ofSeq

                let remoteHTLCPubKey = Generators.derivePubKey ctx (cm.RemoteParams.HTLCBasePoint) localPerCommitmentPoint.PubKey

                let checkHTLCSig (htlc: IHTLCTx, remoteECDSASig: LNECDSASignature): Result<_, _> =
                    let remoteS = TransactionSignature(remoteECDSASig.Value, SigHash.All)
                    match htlc with
                    | :? HTLCTimeoutTx ->
                        (Transactions.checkTxFinalized (htlc.Value) (0) (seq [(remoteHTLCPubKey, remoteS)]))
                        |> Result.map(box)
                    // we cannot check that htlc-success tx are spendable because we need the payment preimage; thus we only check the remote sig
                    | :? HTLCSuccessTx ->
                        (Transactions.checkSigAndAdd (htlc) (remoteS) (remoteHTLCPubKey))
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
                    chanPrivateKeys.CommitmentSeed.DeriveRevocationKey cm.LocalCommit.Index
                let localNextPerCommitmentPoint =
                    let revocationKey =
                        chanPrivateKeys.CommitmentSeed.DeriveRevocationKey
                            cm.LocalCommit.Index.NextCommitment.NextCommitment
                    revocationKey.CommitmentPubKey

                let nextMsg = { RevokeAndACKMsg.ChannelId = cm.ChannelId
                                PerCommitmentSecret = localPerCommitmentSecret
                                NextPerCommitmentPoint = localNextPerCommitmentPoint }
                
                let nextCommitments =
                    let localCommit1 = { LocalCommit.Index = cm.LocalCommit.Index.NextCommitment
                                         Spec = spec
                                         PublishableTxs = { PublishableTxs.CommitTx = finalizedCommitTx
                                                            HTLCTxs = finalizedTxs }
                                         PendingHTLCSuccessTxs = successTxs }
                    let ourChanges1 = { cm.LocalChanges with ACKed = []}
                    let theirChanges1 = { cm.RemoteChanges with Proposed = []; ACKed = (cm.RemoteChanges.ACKed @ cm.RemoteChanges.Proposed) }
                    let completedOutgoingHTLCs =
                        let t1 = cm.LocalCommit.Spec.HTLCs
                                 |> Map.filter(fun _ v -> v.Direction = Out)
                                 |> Map.toSeq |> Seq.map (fun (k, _) -> k) |> Set.ofSeq
                        let t2 = localCommit1.Spec.HTLCs |> Map.filter(fun _ v -> v.Direction = Out)
                                 |> Map.toSeq |> Seq.map (fun (k, _) -> k) |> Set.ofSeq
                        Set.difference t1 t2
                    let originChannels1 = cm.OriginChannels |> Map.filter(fun k _ -> Set.contains k completedOutgoingHTLCs)
                    { cm with LocalCommit = localCommit1
                              LocalChanges = ourChanges1
                              RemoteChanges = theirChanges1
                              OriginChannels = originChannels1 }
                return [ WeAcceptedCommitmentSigned(nextMsg, nextCommitments) ;]
            }
