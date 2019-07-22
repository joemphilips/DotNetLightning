namespace DotNetLightning.LN

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Transactions
open DotNetLightning.Crypto
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

    module private Validation =
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