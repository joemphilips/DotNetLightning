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

[<RequireQualifiedAccess;
  CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Commitments =
    module Helpers =
        let isAlreadySent
            (htlc: UpdateAddHTLCMsg)
            (proposed: list<IUpdateMsg>)
            =
            proposed
            |> List.exists(fun p ->
                match p with
                | :? UpdateFulfillHTLCMsg as u -> u.HTLCId = htlc.HTLCId
                | :? UpdateFailHTLCMsg as u -> u.HTLCId = htlc.HTLCId
                | :? UpdateFailMalformedHTLCMsg as u -> u.HTLCId = htlc.HTLCId
                | _ -> false
            )

        let makeRemoteTxs
            (staticChannelConfig: StaticChannelConfig)
            (commitTxNumber: CommitmentNumber)
            (localChannelPubKeys: ChannelPubKeys)
            (remotePerCommitmentPoint: PerCommitmentPoint)
            (spec: CommitmentSpec)
            =
            let localCommitmentPubKeys =
                remotePerCommitmentPoint.DeriveCommitmentPubKeys
                    localChannelPubKeys

            let remoteCommitmentPubKeys =
                remotePerCommitmentPoint.DeriveCommitmentPubKeys
                    staticChannelConfig.RemoteChannelPubKeys

            let commitTx =
                Transactions.makeCommitTx
                    staticChannelConfig.FundingScriptCoin
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
            : Result<(CommitTx * list<HTLCTimeoutTx> * list<HTLCSuccessTx>), _> =
            let localCommitmentPubKeys =
                localPerCommitmentPoint.DeriveCommitmentPubKeys
                    localChannelPubKeys

            let remoteCommitmentPubKeys =
                localPerCommitmentPoint.DeriveCommitmentPubKeys
                    staticChannelConfig.RemoteChannelPubKeys

            let commitTx =
                Transactions.makeCommitTx
                    staticChannelConfig.FundingScriptCoin
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
                    Transactions.makeHTLCTxs
                        (commitTx.Value.GetGlobalTransaction())
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


        let sortBothHTLCs
            (htlcTimeoutTxs: list<HTLCTimeoutTx>)
            (htlcSuccessTxs: list<HTLCSuccessTx>)
            =
            let timeoutTXsV = (htlcTimeoutTxs |> Seq.cast<IHTLCTx>)
            let successTXsV = (htlcSuccessTxs |> Seq.cast<IHTLCTx>)

            Seq.append timeoutTXsV successTXsV
            |> List.ofSeq
            |> List.sortBy(fun htlc ->
                htlc.Value.GetGlobalTransaction().Inputs.[htlc.WhichInput]
                    .PrevOut
                    .N
            )

        let checkUpdateFee
            (channelOptions: ChannelOptions)
            (msg: UpdateFeeMsg)
            (localFeeRate: FeeRatePerKw)
            =
            let maxMismatch = channelOptions.MaxFeeRateMismatchRatio

            UpdateFeeValidation.checkFeeDiffTooHigh
                (msg)
                (localFeeRate)
                (maxMismatch)

    let sendFulfill
        (op: OperationFulfillHTLC)
        (cm: Commitments)
        (savedChannelState: SavedChannelState)
        (remoteNextCommitInfo: RemoteNextCommitInfo)
        =
        match
            savedChannelState.GetIncomingHTLCCrossSigned
                remoteNextCommitInfo
                op.Id
            with
        | Some htlc when (cm.ProposedLocalChanges |> Helpers.isAlreadySent htlc) ->
            htlc.HTLCId |> htlcAlreadySent
        | Some htlc when (htlc.PaymentHash = op.PaymentPreimage.Hash) ->
            let msgToSend: UpdateFulfillHTLCMsg =
                {
                    ChannelId =
                        savedChannelState.StaticChannelConfig.ChannelId()
                    HTLCId = op.Id
                    PaymentPreimage = op.PaymentPreimage
                }

            let newCommitments = cm.AddLocalProposal(msgToSend)
            (msgToSend, newCommitments) |> Ok
        | Some htlc ->
            (htlc.PaymentHash, op.PaymentPreimage) |> invalidPaymentPreimage
        | None -> op.Id |> unknownHTLCId

    let receiveFulfill
        (msg: UpdateFulfillHTLCMsg)
        (cm: Commitments)
        (savedChannelState: SavedChannelState)
        (remoteNextCommitInfo: RemoteNextCommitInfo)
        =
        match
            savedChannelState.GetOutgoingHTLCCrossSigned
                remoteNextCommitInfo
                msg.HTLCId
            with
        | Some htlc when htlc.PaymentHash = msg.PaymentPreimage.Hash ->
            let commitments = cm.AddRemoteProposal(msg)
            commitments |> Ok
        | Some htlc ->
            (htlc.PaymentHash, msg.PaymentPreimage) |> invalidPaymentPreimage
        | None -> msg.HTLCId |> unknownHTLCId

    let sendFail
        (nodeSecret: NodeSecret)
        (op: OperationFailHTLC)
        (cm: Commitments)
        (savedChannelState: SavedChannelState)
        (remoteNextCommitInfo: RemoteNextCommitInfo)
        =
        match
            savedChannelState.GetIncomingHTLCCrossSigned
                remoteNextCommitInfo
                op.Id
            with
        | Some htlc when (cm.ProposedLocalChanges |> Helpers.isAlreadySent htlc) ->
            htlc.HTLCId |> htlcAlreadySent
        | Some htlc ->
            let ad = htlc.PaymentHash.ToBytes()
            let rawPacket = htlc.OnionRoutingPacket.ToBytes()

            Sphinx.parsePacket (nodeSecret.RawKey()) ad rawPacket
            |> Result.mapError(ChannelError.CryptoError)
            >>= fun ({
                         SharedSecret = ss
                     }) ->
                    let reason =
                        op.Reason
                        |> function
                            | Choice1Of2 b -> Sphinx.forwardErrorPacket(b, ss)
                            | Choice2Of2 f -> Sphinx.ErrorPacket.Create(ss, f)

                    let f =
                        {
                            UpdateFailHTLCMsg.ChannelId =
                                savedChannelState.StaticChannelConfig.ChannelId
                                    ()
                            HTLCId = op.Id
                            Reason =
                                {
                                    Data = reason
                                }
                        }

                    let nextCommitments = cm.AddLocalProposal(f)
                    Ok(f, nextCommitments)
        | None -> op.Id |> unknownHTLCId

    let receiveFail
        (msg: UpdateFailHTLCMsg)
        (cm: Commitments)
        (savedChannelState: SavedChannelState)
        (remoteNextCommitInfo: RemoteNextCommitInfo)
        =
        match
            savedChannelState.GetOutgoingHTLCCrossSigned
                remoteNextCommitInfo
                msg.HTLCId
            with
        | Some _htlc ->
            result {
                let! _origin =
                    match cm.OriginChannels.TryGetValue(msg.HTLCId) with
                    | true, origin -> Ok origin
                    | false, _ -> msg.HTLCId |> htlcOriginNotKnown

                let nextC = cm.AddRemoteProposal(msg)
                return nextC
            }
        | None -> msg.HTLCId |> unknownHTLCId


    let sendFailMalformed
        (op: OperationFailMalformedHTLC)
        (cm: Commitments)
        (savedChannelState: SavedChannelState)
        (remoteNextCommitInfo: RemoteNextCommitInfo)
        =
        // BADONION bit must be set in failure code
        if (op.FailureCode.Value &&& OnionError.BADONION) = 0us then
            op.FailureCode |> invalidFailureCode
        else
            match
                savedChannelState.GetIncomingHTLCCrossSigned
                    remoteNextCommitInfo
                    op.Id
                with
            | Some htlc when
                (cm.ProposedLocalChanges |> Helpers.isAlreadySent htlc)
                ->
                htlc.HTLCId |> htlcAlreadySent
            | Some _htlc ->
                let msg =
                    {
                        UpdateFailMalformedHTLCMsg.ChannelId =
                            savedChannelState.StaticChannelConfig.ChannelId()
                        HTLCId = op.Id
                        Sha256OfOnion = op.Sha256OfOnion
                        FailureCode = op.FailureCode
                    }

                let nextCommitments = cm.AddLocalProposal(msg)
                Ok(msg, nextCommitments)
            | None -> op.Id |> unknownHTLCId

    let receiveFailMalformed
        (msg: UpdateFailMalformedHTLCMsg)
        (cm: Commitments)
        (savedChannelState: SavedChannelState)
        (remoteNextCommitInfo: RemoteNextCommitInfo)
        =
        if msg.FailureCode.Value &&& OnionError.BADONION = 0us then
            msg.FailureCode |> invalidFailureCode
        else
            match
                savedChannelState.GetOutgoingHTLCCrossSigned
                    remoteNextCommitInfo
                    msg.HTLCId
                with
            | Some _htlc ->
                result {
                    let! _origin =
                        match cm.OriginChannels.TryGetValue(msg.HTLCId) with
                        | true, o -> Ok o
                        | false, _ -> msg.HTLCId |> htlcOriginNotKnown

                    let nextC = cm.AddRemoteProposal(msg)
                    return nextC
                }
            | None -> msg.HTLCId |> unknownHTLCId

    let sendFee
        (op: OperationUpdateFee)
        (savedChannelState: SavedChannelState)
        (cm: Commitments)
        =
        if (not savedChannelState.StaticChannelConfig.IsFunder) then
            "Local is Fundee so it cannot send update fee" |> apiMisuse
        else
            let fee =
                {
                    UpdateFeeMsg.ChannelId =
                        savedChannelState.StaticChannelConfig.ChannelId()
                    FeeRatePerKw = op.FeeRatePerKw
                }

            let c1 = cm.AddLocalProposal(fee)

            result {
                let! reduced =
                    savedChannelState.RemoteCommit.Spec.Reduce(
                        savedChannelState.RemoteChanges.ACKed,
                        c1.ProposedLocalChanges
                    )
                    |> expectTransactionError
                // A node cannot spend pending incoming htlcs, and need to keep funds above the reserve required by
                // the counter party, after paying the fee, we look from remote's point of view, so if local is funder
                // remote doesn't pay the fees.
                let fees =
                    Transactions.commitTxFee
                        (savedChannelState.StaticChannelConfig.RemoteParams.DustLimitSatoshis)
                        reduced

                let missing =
                    reduced.ToRemote.ToMoney()
                    - savedChannelState.StaticChannelConfig.RemoteParams.ChannelReserveSatoshis
                    - fees

                if (missing < Money.Zero) then
                    return!
                        (savedChannelState.StaticChannelConfig.LocalParams.ChannelReserveSatoshis,
                         fees,
                         (-1 * missing))
                        |> cannotAffordFee
                else
                    return fee, c1
            }

    let receiveFee
        (channelOptions: ChannelOptions)
        localFeerate
        (msg: UpdateFeeMsg)
        (savedChannelState: SavedChannelState)
        (cm: Commitments)
        =
        if savedChannelState.StaticChannelConfig.IsFunder then
            "Remote is Fundee so it cannot send update fee" |> apiMisuse
        else
            result {
                do! Helpers.checkUpdateFee channelOptions msg localFeerate
                let nextCommitments = cm.AddRemoteProposal(msg)

                let! reduced =
                    savedChannelState.LocalCommit.Spec.Reduce(
                        savedChannelState.LocalChanges.ACKed,
                        nextCommitments.ProposedRemoteChanges
                    )
                    |> expectTransactionError

                let fees =
                    Transactions.commitTxFee
                        (savedChannelState.StaticChannelConfig.RemoteParams.DustLimitSatoshis)
                        reduced

                let missing =
                    reduced.ToRemote.ToMoney()
                    - savedChannelState.StaticChannelConfig.RemoteParams.ChannelReserveSatoshis
                    - fees

                if (missing < Money.Zero) then
                    return!
                        (savedChannelState.StaticChannelConfig.LocalParams.ChannelReserveSatoshis,
                         fees,
                         (-1 * missing))
                        |> cannotAffordFee
                else
                    return nextCommitments
            }

    let checkSignatureCountMismatch (sortedHTLCTXs: list<IHTLCTx>) msg =
        if (sortedHTLCTXs.Length <> msg.HTLCSignatures.Length) then
            signatureCountMismatch(
                sortedHTLCTXs.Length,
                msg.HTLCSignatures.Length
            )
        else
            Ok()
