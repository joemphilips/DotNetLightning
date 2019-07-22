namespace DotNetLightning.LN

open NBitcoin
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
            (channelKeys: ChannelKeys)
            (commitTxNumber: uint64)
            (localParams: LocalParams)
            (remoteParams: RemoteParams)
            (commitmentInput: ScriptCoin)
            (remotePerCommitmentPoint: PubKey)
            (spec) (n) =
            let pkGen = Generators.derivePubKey remotePerCommitmentPoint
            let localPaymentPK = pkGen (channelKeys.PaymentBaseKey.PubKey)
            let localHTLCPK = pkGen channelKeys.HTLCBaseKey.PubKey
            let remotePaymentPK = pkGen (remoteParams.PaymentBasePoint)
            let remoteDelayedPaymentPK = pkGen (remoteParams.PaymentBasePoint)
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
            Transactions.makeHTLCTxs (commitTx.Value.GetGlobalTransaction())
                                     (remoteParams.DustLimitSatoshis)
                                     (remoteRevocationPK)
                                     (localParams.ToSelfDelay)
                                     (remoteDelayedPaymentPK)
                                     (remoteHTLCPK)
                                     (localHTLCPK)
                                     (spec) (n)
            |>> fun (htlcTimeoutTxs, htlcSuccessTxs) ->
                (commitTx, htlcTimeoutTxs, htlcSuccessTxs)

        let makeLocalTXs
            (channelKeys: ChannelPubKeys)
            (commitTxNumber: uint64)
            (localParams: LocalParams)
            (remoteParams: RemoteParams)
            (commitmentInput: ScriptCoin)
            (localPerCommitmentPoint: PubKey)
            (spec: CommitmentSpec)
            n: RResult<(CommitTx * HTLCTimeoutTx list * HTLCSuccessTx list)> =
            let pkGen = Generators.derivePubKey localPerCommitmentPoint
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
            let r =
                Transactions.makeHTLCTxs (commitTx.Value.GetGlobalTransaction())
                                         (localParams.DustLimitSatoshis)
                                         (localRevocationPK)
                                         (remoteParams.ToSelfDelay)
                                         (localDelayedPaymentPK)
                                         (localHTLCPK)
                                         (remoteHTLCPK)
                                         (spec)
                                         (n)
            r |>> fun (htlcTimeoutTxs, htlcSuccessTxs) ->
                (commitTx, htlcTimeoutTxs, htlcSuccessTxs)


        let sortBothHTLCs (htlcTimeoutTxs: HTLCTimeoutTx list) (htlcSuccessTxs: HTLCSuccessTx list) =
            let timeoutTXsV = (htlcTimeoutTxs |> Seq.cast<IHTLCTx>)
            let successTXsV = (htlcSuccessTxs |> Seq.cast<IHTLCTx>)
            Seq.append timeoutTXsV successTXsV
            |> List.ofSeq
            |> List.sortBy(fun htlc -> htlc.Value.GetGlobalTransaction().Inputs.[htlc.WhichInput].PrevOut.N)
    module private Validation =
        let check left predicate right msg =
            if predicate left right then
                sprintf msg left right |> RResult.rmsg
            else
                Good ()
        module private UpdateFeeValidator =
            let private feeRateMismatch (FeeRatePerKw remote, FeeRatePerKw local) =
                (2.0 * float (remote - local) / float (remote + local))
                |> abs
            let checkFeeDiffTooHigh (remoteFeeRatePerKw: FeeRatePerKw) (localFeeRatePerKw: FeeRatePerKw) (maxFeeRateMismatchRatio) =
                let diff = feeRateMismatch(remoteFeeRatePerKw, localFeeRatePerKw)
                if (diff > maxFeeRateMismatchRatio) then
                    sprintf "FeeReate Delta is too big. Local: %A ; remote %A ; So it will be %.2f%% higher. But it must be lower than %.2f%%"
                            (localFeeRatePerKw) (remoteFeeRatePerKw) (diff * 100.0) (maxFeeRateMismatchRatio * 100.0)
                    |> RResult.rmsg
                else
                    Good ()

        let checkUpdateFee (config: UserConfig) (msg: UpdateFee) (localFeeRate: FeeRatePerKw) =
            let maxMismatch = config.ChannelOptions.MaxFeeRateMismatchRatio
            UpdateFeeValidator.checkFeeDiffTooHigh (msg.FeeRatePerKw) (localFeeRate) (maxMismatch)

    let sendFulfill (cmd: CMDFulfillHTLC) (cm: Commitments) =
        match cm.GetHTLCCrossSigned(Direction.In, cmd.Id) with
        | Some htlc when (cm.LocalChanges.Proposed |> Helpers.isAlreadySent htlc) ->
            sprintf "We have already sent a fail/fulfill for this htlc: %A" htlc
            |> RResult.rmsg
        | Some htlc when (htlc.PaymentHash = cmd.PaymentPreimage.GetHash()) ->
            let msgToSend: UpdateFulfillHTLC = { ChannelId = cm.ChannelId; HTLCId = cmd.Id; PaymentPreimage = cmd.PaymentPreimage }
            let newCommitments = cm.AddLocalProposal(msgToSend)
            [ WeAcceptedCMDFulfillHTLC (msgToSend, newCommitments)] |> Good
        | Some htlc ->
            sprintf "Invalid HTLC PreImage %A. Hash (%A) does not match the one expected %A"
                    cmd.PaymentPreimage
                    (cmd.PaymentPreimage.GetHash())
                    (htlc.PaymentHash)
            |> RResult.rmsg
        | None ->
            sprintf "Unknown HTLCId (%A)" cmd.Id
            |> RResult.rmsg

    let receiveFulfill(msg: UpdateFulfillHTLC) (cm: Commitments) =
        match cm.GetHTLCCrossSigned(Direction.Out, msg.HTLCId) with
        | Some htlc when htlc.PaymentHash = msg.PaymentPreimage.GetHash() ->
            let commitments = cm.AddRemoteProposal(msg)
            let origin = cm.OriginChannels |> Map.find(msg.HTLCId)
            [WeAcceptedFulfillHTLC(msg, origin, htlc, commitments)] |> Good
        | Some htlc ->
            sprintf "Invalid HTLC PreImage %A. Hash (%A) does not match the one expected %A"
                    msg.PaymentPreimage
                    (msg.PaymentPreimage.GetHash())
                    (htlc.PaymentHash)
            |> RResult.rmsg
        | None ->
            sprintf "Unknown HTLCId (%A)" msg.HTLCId
            |> RResult.rmsg

    let sendFail (localKey: Key) (cmd: CMDFailHTLC) (cm: Commitments) =
        match cm.GetHTLCCrossSigned(Direction.In, cmd.Id) with
        | Some htlc when  (cm.LocalChanges.Proposed |> Helpers.isAlreadySent htlc) ->
            sprintf "We have already sent a fail/fulfill for this htlc: %A" htlc
            |> RResult.rmsg
        | Some htlc ->
            let ad = htlc.PaymentHash.ToBytes()
            let rawPacket = htlc.OnionRoutingPacket.ToBytes()
            Sphinx.parsePacket localKey ad rawPacket
            >>= fun ({ SharedSecret = ss}) ->
                let reason =
                    cmd.Reason
                    |> function Choice1Of2 b -> Sphinx.forwardErrorPacket(b, ss) | Choice2Of2 f -> Sphinx.ErrorPacket.Create(ss, f)
                let f = { UpdateFailHTLC.ChannelId = cm.ChannelId
                          HTLCId = cmd.Id
                          Reason = { Data = reason } }
                let nextComitments = cm.AddLocalProposal(f)
                [ WeAcceptedCMDFailHTLC(f, nextComitments) ]
                |> Good
        | None ->
            sprintf "Unknown HTLCId (%A)" cmd.Id
            |> RResult.rmsg

    let receiveFail (msg: UpdateFailHTLC) (cm: Commitments) =
        match cm.GetHTLCCrossSigned(Direction.Out, msg.HTLCId) with
        | Some htlc ->
            match cm.OriginChannels.TryGetValue(msg.HTLCId) with
            | true, origin -> Good origin
            | false, _ -> sprintf "Invalid HTLCId, Unknown Origin. HTLCId (%A)" msg.HTLCId |> RResult.rmsg
            >>= fun o ->
                let nextC = cm.AddRemoteProposal(msg)
                [WeAcceptedFailHTLC(o, htlc, nextC)]
                |> Good
        | None ->
            sprintf "Unknown HTLCId (%A)" msg.HTLCId |> RResult.rmsg


    let sendFailMalformed (cmd: CMDFailMalformedHTLC) (cm: Commitments) =
        // BADONION bit must be set in failure code
        if ((cmd.FailureCode.Value &&& Error.BADONION) = 0us) then
            sprintf "invalid failure code %A" (cmd.FailureCode.GetOnionErrorDescription())
            |> RResult.rmsg
        else
            match cm.GetHTLCCrossSigned(Direction.In, cmd.Id) with
            | Some htlc when (cm.LocalChanges.Proposed |> Helpers.isAlreadySent htlc) ->
                sprintf "We have already sent a fail/fulfill for this htlc: %A" htlc
                |> RResult.rmsg
            | Some htlc ->
                let msg = { UpdateFailMalformedHTLC.ChannelId = cm.ChannelId
                            HTLCId = cmd.Id
                            Sha256OfOnion = cmd.Sha256OfOnion
                            FailureCode = cmd.FailureCode }
                let nextCommitments = cm.AddLocalProposal(msg)
                [ WeAcceptedCMDFailMalformedHTLC(msg, nextCommitments) ]
                |> Good
            | None ->
                sprintf "Unknown HTLCId (%A)" cmd.Id |> RResult.rmsg

    let receiveFailMalformed (msg: UpdateFailMalformedHTLC) (cm: Commitments) =
        if msg.FailureCode.Value &&& Error.BADONION = 0us then
            sprintf "invalid failure code %A" (msg.FailureCode.GetOnionErrorDescription()) |> RResult.rmsg
        else
            match cm.GetHTLCCrossSigned(Direction.Out, msg.HTLCId) with
            | Some htlc ->
                match cm.OriginChannels.TryGetValue(msg.HTLCId) with
                | true, o -> Good o
                | false, _ -> sprintf "Invalid HTLCId, Unknown Origin. HTLCId(%A)" msg.HTLCId |> RResult.rmsg
                >>= fun o ->
                    let nextC = cm.AddRemoteProposal(msg)
                    [WeAcceptedFailMalformedHTLC(o, htlc, nextC)]
                    |> Good
            | None ->
                sprintf "Unknown HTLCId (%A)" msg.HTLCId |> RResult.rmsg

    let sendFee(cmd: CMDUpdateFee) (cm: Commitments) =
            if (not cm.LocalParams.IsFunder) then
                "Local is Fundee so it cannot send update fee" |> RResult.rmsg
            else
                let fee = { UpdateFee.ChannelId = cm.ChannelId
                            FeeRatePerKw = cmd.FeeRatePerKw }
                let c1 = cm.AddLocalProposal(fee)
                c1.RemoteCommit.Spec.Reduce(c1.RemoteChanges.ACKed, c1.LocalChanges.Proposed)
                >>= fun reduced ->
                    // A node cannot spend pending incoming htlcs, and need to keep funds above the reserve required by
                    // the counterparty, after paying the fee, we look from remote'S point of view, so if local is funder
                    // remote doesn'T pay the fees.
                    let fees = Transactions.commitTxFee(c1.RemoteParams.DustLimitSatoshis) reduced
                    let missing = reduced.ToRemote.ToMoney() - c1.RemoteParams.ChannelReserveSatoshis - fees
                    if (missing < Money.Zero) then
                        sprintf "Cannot affored fees. Missing Satoshis are: %A . Reserve Satoshis are %A . Fees are %A" (-1 * missing) (c1.LocalParams.ChannelReserveSatoshis) (fees)
                        |> RResult.rmsg
                    else
                        [ WeAcceptedCMDUpdateFee(fee, c1) ]
                        |> Good

    let receiveFee (config: UserConfig) (localFeerate) (msg: UpdateFee) (cm: Commitments) =
        if (cm.LocalParams.IsFunder) then
            "Remote is Fundee so it cannot send update fee" |> RResult.rmsg
        else
            Validation.checkUpdateFee (config) (msg) (localFeerate)
            >>= fun _ ->
                let c1 = cm.AddRemoteProposal(msg)
                c1.LocalCommit.Spec.Reduce(c1.LocalChanges.ACKed, c1.RemoteChanges.Proposed)
                >>= fun reduced ->
                    let fees = Transactions.commitTxFee(c1.RemoteParams.DustLimitSatoshis) reduced
                    let missing = reduced.ToRemote.ToMoney() - c1.RemoteParams.ChannelReserveSatoshis - fees
                    if (missing < Money.Zero) then
                        sprintf "Cannot affored fees. Missing Satoshis are: %A . Reserve Satoshis are %A . Fees are %A" (-1 * missing) (c1.LocalParams.ChannelReserveSatoshis) (fees)
                        |> RResult.rmsg
                    else
                        [ WeAcceptedUpdateFee msg ]
                        |> Good

    let sendCommit(keyRepo: IKeysRepository) (n: Network) (cm: Commitments) =
        match cm.RemoteNextCommitInfo with
        | Choice2Of2 remoteNextPerCommitmentPoint ->
            // remote commitment will include all local changes + remote acked changes
            cm.RemoteCommit.Spec.Reduce(cm.RemoteChanges.ACKed, cm.LocalChanges.Proposed)
            >>= fun spec ->
                let localKeys = keyRepo.GetChannelKeys(not cm.LocalParams.IsFunder)
                Helpers.makeRemoteTxs (localKeys)
                                      (cm.RemoteCommit.Index + 1UL)
                                      (cm.LocalParams)
                                      (cm.RemoteParams)
                                      (cm.FundingSCoin)
                                      (remoteNextPerCommitmentPoint)
                                      (spec) n
                >>= fun (remoteCommitTx, htlcTimeoutTxs, htlcSuccessTxs) ->
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
                                Signature = signature.Signature
                                HTLCSignatures = htlcSigs }
                    let nextCommitments =
                        let nextRemoteCommitInfo = { WaitingForRevocation.NextRemoteCommit =
                                                        { cm.RemoteCommit
                                                            with
                                                                Index = 1UL;
                                                                RemotePerCommitmentPoint = remoteNextPerCommitmentPoint
                                                                TxId = remoteCommitTx.GetTxId() }
                                                     Sent = msg
                                                     SentAfterLocalCommitmentIndex = cm.LocalCommit.Index
                                                     ReSignASAP = false }

                        { cm with RemoteNextCommitInfo = Choice1Of2(nextRemoteCommitInfo)
                                  LocalChanges = { cm.LocalChanges with Proposed = []; Signed = cm.LocalChanges.Proposed }
                                  RemoteChanges = { cm.RemoteChanges with ACKed = []; Signed = cm.RemoteChanges.ACKed } }
                    [ WeAcceptedCMDSign (msg, nextCommitments) ] |> Good
        | Choice1Of2 _ ->
            "Can not sign before Revocation"
            |> RResult.rmsg

    let receiveCommit (keyRepo: IKeysRepository) (msg: CommitmentSigned) (n: Network) (cm: Commitments) =
        if cm.RemoteHasChanges() |> not then
            sprintf "Remote has sent commitment_signed but we have no pending changes" |> RResult.rmsg
        else
            let chanKeys = cm.LocalParams.ChannelPubKeys
            let nextI = cm.LocalCommit.Index + 1UL
            cm.LocalCommit.Spec.Reduce(cm.LocalChanges.ACKed, cm.RemoteChanges.Proposed)
            >>= fun spec ->
                let localPerCommitmentPoint = ChannelUtils.buildCommitmentPoint (chanKeys.CommitmentSeed, nextI)
                Helpers.makeLocalTXs chanKeys (nextI) (cm.LocalParams) (cm.RemoteParams) (cm.FundingSCoin) (localPerCommitmentPoint) spec n
                >>= fun (localCommitTx, htlcTimeoutTxs, htlcSuccessTxs) ->
                    let signature, signedCommitTx = keyRepo.GetSignatureFor (localCommitTx.Value, chanKeys.FundingPubKey)

                    let sigPair =
                        let localSigPair = seq [(chanKeys.FundingPubKey, signature)]
                        let remoteSigPair = seq[ (cm.RemoteParams.FundingPubKey, TransactionSignature(msg.Signature, SigHash.All)) ]
                        Seq.append localSigPair remoteSigPair
                    Transactions.checkTxFinalized signedCommitTx localCommitTx.WhichInput sigPair
                    >>= fun finalizedCommitTx ->
                        let sortedHTLCTXs = Helpers.sortBothHTLCs htlcTimeoutTxs htlcSuccessTxs
                        Validation.check sortedHTLCTXs.Length (<>) msg.HTLCSignatures.Length "Number of signatures went from the remote (%A) does not match the number expected (%A)"
                        >>= fun _ ->
                            let localHTLCSigs, sortedHTLCTXs =
                                let localHtlcSigsAndHTLCTxs = sortedHTLCTXs |> List.map(fun htlc -> keyRepo.GenerateKeyFromBasePointAndSign(htlc.Value, cm.LocalParams.ChannelPubKeys.HTLCBasePubKey, localPerCommitmentPoint))
                                localHtlcSigsAndHTLCTxs |> List.map(fst), localHtlcSigsAndHTLCTxs |> List.map(snd) |> Seq.cast<IHTLCTx> |> List.ofSeq

                            let remoteHTLCPubKey = Generators.derivePubKey(cm.RemoteParams.HTLCBasePoint) (localPerCommitmentPoint)

                            let checkHTLCSig (htlc: IHTLCTx, remoteECDSASig: Crypto.ECDSASignature): RResult<_> =
                                let remoteS = TransactionSignature(remoteECDSASig, SigHash.All)
                                match htlc with
                                | :? HTLCTimeoutTx ->
                                    (Transactions.checkTxFinalized (htlc.Value) (0) (seq [(remoteHTLCPubKey, remoteS)]))
                                    |>> box
                                // we cannot check that htlc-success tx are spendable because we need the payment preimage; thus we only check the remote sig
                                | :? HTLCSuccessTx ->
                                    (Transactions.checkSigAndAdd (htlc) (remoteS) (remoteHTLCPubKey))
                                    |>> box
                                | _ -> failwith "Unreachable!"

                            List.zip sortedHTLCTXs msg.HTLCSignatures
                            |> List.map(checkHTLCSig)
                            |> List.sequenceRResult
                            >>= fun txList ->
                                let successTxs = txList |> List.choose(fun o -> match o with | :? HTLCSuccessTx as tx -> Some tx | _ -> None)
                                let finalizedTxs = txList |> List.choose(fun o -> match o with | :? FinalizedTx as tx -> Some tx | _ -> None)
                                let localPerCommitmentSecret =
                                    ChannelUtils.buildCommitmentSecret(cm.LocalParams.ChannelPubKeys.CommitmentSeed, cm.LocalCommit.Index)
                                let localNextPerCommitmentPoint =
                                    ChannelUtils.buildCommitmentPoint(cm.LocalParams.ChannelPubKeys.CommitmentSeed, cm.LocalCommit.Index + 2UL)

                                let nextMsg = { RevokeAndACK.ChannelId = cm.ChannelId
                                                PerCommitmentSecret = localPerCommitmentSecret.ToBytes() |> uint256 |> PaymentPreimage
                                                NextPerCommitmentPoint = localNextPerCommitmentPoint }
                                
                                let nextCommitments =
                                    let localCommit1 = { LocalCommit.Index = cm.LocalCommit.Index + 1UL
                                                         Spec = cm.LocalCommit.Spec
                                                         PublishableTxs = { PublishableTxs.CommitTx = finalizedCommitTx
                                                                            HTLCTxs = finalizedTxs }
                                                         PendingHTLCSuccessTxs = successTxs }
                                    let ourChanges1 = { cm.LocalChanges with ACKed = []}
                                    let theirChanges1 = { cm.RemoteChanges with Proposed = []; ACKed = (cm.RemoteChanges.ACKed @ cm.RemoteChanges.Proposed) }
                                    let completedOutgoingHTLCs =
                                        let t1 = cm.LocalCommit.Spec.HTLCs
                                                 |> Map.filter(fun k v -> v.Direction = Out)
                                                 |> Map.toSeq |> Seq.map (fun (k, v) -> k) |> Set.ofSeq
                                        let t2 = localCommit1.Spec.HTLCs |> Map.filter(fun k v -> v.Direction = Out)
                                                 |> Map.toSeq |> Seq.map (fun (k, v) -> k) |> Set.ofSeq
                                        Set.difference t1 t2
                                    let originChannels1 = cm.OriginChannels |> Map.filter(fun k v -> Set.contains k completedOutgoingHTLCs)
                                    { cm with LocalCommit = localCommit1
                                              LocalChanges = ourChanges1
                                              RemoteChanges = theirChanges1
                                              OriginChannels = originChannels1 }
                                Good ([ WeAcceptedCommitmentSigned(nextMsg, nextCommitments) ;])