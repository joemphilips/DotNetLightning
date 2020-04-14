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
        let isAlreadySent (htlc: UpdateAddHTLC) (proposed: IUpdateMsg list) =
            proposed
            |> List.exists(fun p -> match p with
                                    | :? UpdateFulfillHTLC as u -> u.HTLCId = htlc.HTLCId
                                    | :? UpdateFailHTLC as u -> u.HTLCId = htlc.HTLCId
                                    | :? UpdateFailMalformedHTLC as u -> u.HTLCId = htlc.HTLCId
                                    | _ -> false)

        let makeRemoteTxs
            (ctx: ISecp256k1)
            (channelKeys: ChannelKeys)
            (commitTxNumber: uint64)
            (localParams: LocalParams)
            (remoteParams: RemoteParams)
            (commitmentInput: ScriptCoin)
            (remotePerCommitmentPoint: PubKey)
            (spec) (n) =
            let pkGen = Generators.derivePubKey ctx remotePerCommitmentPoint
            let localPaymentPK = pkGen (channelKeys.PaymentBaseKey.PubKey)
            let localHTLCPK = pkGen channelKeys.HTLCBaseKey.PubKey
            let remotePaymentPK = pkGen (remoteParams.PaymentBasePoint)
            let remoteDelayedPaymentPK = pkGen (remoteParams.DelayedPaymentBasePoint)
            let remoteHTLCPK = pkGen (remoteParams.HTLCBasePoint)
            let remoteRevocationPK = pkGen (channelKeys.RevocationBaseKey.PubKey)
            let commitTx =
                Transactions.makeCommitTx commitmentInput 
                                          commitTxNumber
                                          remoteParams.PaymentBasePoint
                                          channelKeys.PaymentBaseKey.PubKey
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
            (channelKeys: ChannelPubKeys)
            (commitTxNumber: uint64)
            (localParams: LocalParams)
            (remoteParams: RemoteParams)
            (commitmentInput: ScriptCoin)
            (localPerCommitmentPoint: PubKey)
            (spec: CommitmentSpec)
            n: Result<(CommitTx * HTLCTimeoutTx list * HTLCSuccessTx list), _> =
            let pkGen = Generators.derivePubKey ctx localPerCommitmentPoint
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

        let checkUpdateFee (config: ChannelConfig) (msg: UpdateFee) (localFeeRate: FeeRatePerKw) =
            let maxMismatch = config.ChannelOptions.MaxFeeRateMismatchRatio
            UpdateFeeValidation.checkFeeDiffTooHigh (msg) (localFeeRate) (maxMismatch)

    let sendFulfill (cmd: CMDFulfillHTLC) (cm: Commitments) =
        match cm.GetHTLCCrossSigned(Direction.In, cmd.Id) with
        | Some htlc when (cm.LocalChanges.Proposed |> Helpers.isAlreadySent htlc) ->
            htlc.HTLCId |> htlcAlreadySent
        | Some htlc when (htlc.PaymentHash = cmd.PaymentPreimage.Hash) ->
            let msgToSend: UpdateFulfillHTLC = { ChannelId = cm.ChannelId; HTLCId = cmd.Id; PaymentPreimage = cmd.PaymentPreimage }
            let newCommitments = cm.AddLocalProposal(msgToSend)
            (msgToSend, newCommitments) |> Ok
        | Some htlc ->
            (htlc.PaymentHash, cmd.PaymentPreimage)
            |> invalidPaymentPreimage
        | None ->
            cmd.Id
            |> unknownHTLCId

    let receiveFulfill(msg: UpdateFulfillHTLC) (cm: Commitments) =
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

    let sendFail (localKey: Key) (cmd: CMDFailHTLC) (cm: Commitments) =
        match cm.GetHTLCCrossSigned(Direction.In, cmd.Id) with
        | Some htlc when  (cm.LocalChanges.Proposed |> Helpers.isAlreadySent htlc) ->
            htlc.HTLCId |> htlcAlreadySent
        | Some htlc ->
            let ad = htlc.PaymentHash.ToBytes()
            let rawPacket = htlc.OnionRoutingPacket.ToBytes()
            Sphinx.parsePacket localKey ad rawPacket |> Result.mapError(ChannelError.CryptoError)
            >>= fun ({ SharedSecret = ss}) ->
                let reason =
                    cmd.Reason
                    |> function Choice1Of2 b -> Sphinx.forwardErrorPacket(b, ss) | Choice2Of2 f -> Sphinx.ErrorPacket.Create(ss, f)
                let f = { UpdateFailHTLC.ChannelId = cm.ChannelId
                          HTLCId = cmd.Id
                          Reason = { Data = reason } }
                let nextComitments = cm.AddLocalProposal(f)
                [ WeAcceptedCMDFailHTLC(f, nextComitments) ]
                |> Ok
        | None ->
            cmd.Id |> unknownHTLCId

    let receiveFail (msg: UpdateFailHTLC) (cm: Commitments) =
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


    let sendFailMalformed (cmd: CMDFailMalformedHTLC) (cm: Commitments) =
        // BADONION bit must be set in failure code
        if ((cmd.FailureCode.Value &&& OnionError.BADONION) = 0us) then
            cmd.FailureCode |> invalidFailureCode
        else
            match cm.GetHTLCCrossSigned(Direction.In, cmd.Id) with
            | Some htlc when (cm.LocalChanges.Proposed |> Helpers.isAlreadySent htlc) ->
                htlc.HTLCId |> htlcAlreadySent
            | Some _htlc ->
                let msg = { UpdateFailMalformedHTLC.ChannelId = cm.ChannelId
                            HTLCId = cmd.Id
                            Sha256OfOnion = cmd.Sha256OfOnion
                            FailureCode = cmd.FailureCode }
                let nextCommitments = cm.AddLocalProposal(msg)
                [ WeAcceptedCMDFailMalformedHTLC(msg, nextCommitments) ]
                |> Ok
            | None ->
                cmd.Id |> unknownHTLCId

    let receiveFailMalformed (msg: UpdateFailMalformedHTLC) (cm: Commitments) =
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

    let sendFee(cmd: CMDUpdateFee) (cm: Commitments) =
            if (not cm.LocalParams.IsFunder) then
                "Local is Fundee so it cannot send update fee" |> apiMisuse
            else
                let fee = { UpdateFee.ChannelId = cm.ChannelId
                            FeeRatePerKw = cmd.FeeRatePerKw }
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
                            [ WeAcceptedCMDUpdateFee(fee, c1) ]
                }

    let receiveFee (config: ChannelConfig) (localFeerate) (msg: UpdateFee) (cm: Commitments) =
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
                let localKeys = keyRepo.GetChannelKeys(not cm.LocalParams.IsFunder)
                let! (remoteCommitTx, htlcTimeoutTxs, htlcSuccessTxs) =
                    Helpers.makeRemoteTxs (ctx)
                                          (localKeys)
                                          (cm.RemoteCommit.Index + 1UL)
                                          (cm.LocalParams)
                                          (cm.RemoteParams)
                                          (cm.FundingSCoin)
                                          (remoteNextPerCommitmentPoint)
                                          (spec) n |> expectTransactionErrors
                let signature,_ = keyRepo.GetSignatureFor(remoteCommitTx.Value, cm.LocalParams.ChannelPubKeys.FundingPubKey)
                let sortedHTLCTXs = Helpers.sortBothHTLCs htlcTimeoutTxs htlcSuccessTxs
                let htlcSigs =
                    sortedHTLCTXs
                    |> List.map(
                            (fun htlc -> keyRepo.GenerateKeyFromBasePointAndSign(htlc.Value, cm.LocalParams.ChannelPubKeys.HTLCBasePubKey, remoteNextPerCommitmentPoint))
                            >> fst
                            >> (fun txSig -> txSig.Signature)
                            )
                let msg = { CommitmentSigned.ChannelId = cm.ChannelId
                            Signature = !> signature.Signature
                            HTLCSignatures = htlcSigs |> List.map (!>) }
                let nextCommitments =
                    let nextRemoteCommitInfo = { WaitingForRevocation.NextRemoteCommit =
                                                    { cm.RemoteCommit
                                                        with
                                                            Index = cm.RemoteCommit.Index + 1UL;
                                                            Spec = spec;
                                                            RemotePerCommitmentPoint = remoteNextPerCommitmentPoint
                                                            TxId = remoteCommitTx.GetTxId() }
                                                 Sent = msg
                                                 SentAfterLocalCommitmentIndex = cm.LocalCommit.Index
                                                 ReSignASAP = false }

                    { cm with RemoteNextCommitInfo = RemoteNextCommitInfo.Waiting(nextRemoteCommitInfo)
                              LocalChanges = { cm.LocalChanges with Proposed = []; Signed = cm.LocalChanges.Proposed }
                              RemoteChanges = { cm.RemoteChanges with ACKed = []; Signed = cm.RemoteChanges.ACKed } }
                return [ WeAcceptedCMDSign (msg, nextCommitments) ]
            }
        | RemoteNextCommitInfo.Waiting _ ->
            CanNotSignBeforeRevocation |> Error

    let private checkSignatureCountMismatch(sortedHTLCTXs: IHTLCTx list) (msg) =
        if (sortedHTLCTXs.Length <> msg.HTLCSignatures.Length) then
            signatureCountMismatch (sortedHTLCTXs.Length, msg.HTLCSignatures.Length)
        else
            Ok()
    let receiveCommit (ctx) (keyRepo: IKeysRepository) (msg: CommitmentSigned) (n: Network) (cm: Commitments): Result<ChannelEvent list, ChannelError> =
        if cm.RemoteHasChanges() |> not then
            ReceivedCommitmentSignedWhenWeHaveNoPendingChanges |> Error
        else
            let chanKeys = cm.LocalParams.ChannelPubKeys
            let nextI = cm.LocalCommit.Index + 1UL
            result {
                let! spec = cm.LocalCommit.Spec.Reduce(cm.LocalChanges.ACKed, cm.RemoteChanges.Proposed) |> expectTransactionError
                let localPerCommitmentPoint = ChannelUtils.buildCommitmentPoint (chanKeys.CommitmentSeed, nextI)
                let! (localCommitTx, htlcTimeoutTxs, htlcSuccessTxs) =
                    Helpers.makeLocalTXs (ctx) chanKeys (nextI) (cm.LocalParams) (cm.RemoteParams) (cm.FundingSCoin) (localPerCommitmentPoint) spec n
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
                    let localHtlcSigsAndHTLCTxs = sortedHTLCTXs |> List.map(fun htlc -> keyRepo.GenerateKeyFromBasePointAndSign(htlc.Value, cm.LocalParams.ChannelPubKeys.HTLCBasePubKey, localPerCommitmentPoint))
                    localHtlcSigsAndHTLCTxs |> List.map(fst), localHtlcSigsAndHTLCTxs |> List.map(snd) |> Seq.cast<IHTLCTx> |> List.ofSeq

                let remoteHTLCPubKey = Generators.derivePubKey ctx (cm.RemoteParams.HTLCBasePoint) (localPerCommitmentPoint)

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
                let successTxs = txList |> List.choose(fun o -> match o with | :? HTLCSuccessTx as tx -> Some tx | _ -> None)
                let finalizedTxs = txList |> List.choose(fun o -> match o with | :? FinalizedTx as tx -> Some tx | _ -> None)
                let localPerCommitmentSecret =
                    ChannelUtils.buildCommitmentSecret(cm.LocalParams.ChannelPubKeys.CommitmentSeed, cm.LocalCommit.Index)
                let localNextPerCommitmentPoint =
                    ChannelUtils.buildCommitmentPoint(cm.LocalParams.ChannelPubKeys.CommitmentSeed, cm.LocalCommit.Index + 2UL)

                let nextMsg = { RevokeAndACK.ChannelId = cm.ChannelId
                                PerCommitmentSecret = localPerCommitmentSecret.ToBytes() |> PaymentPreimage.Create
                                NextPerCommitmentPoint = localNextPerCommitmentPoint }
                
                let nextCommitments =
                    let localCommit1 = { LocalCommit.Index = cm.LocalCommit.Index + 1UL
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
