namespace DotNetLightning.Channel

open ResultUtils
open DotNetLightning.Utils
open NBitcoinExtensions
open DotNetLightning.Utils.OnionError
open DotNetLightning.Chain
open DotNetLightning.Crypto
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Transactions

open NBitcoin

type ChannelError =
    | CryptoError of CryptoError
    | TransactionRelatedErrors of TransactionError list
    
    | HTLCAlreadySent of HTLCId
    | InvalidPaymentPreimage of expectedHash: PaymentHash * actualPreimage: PaymentPreimage
    | UnknownHTLCId of HTLCId
    | HTLCOriginNotKnown of HTLCId
    | InvalidFailureCode of FailureCode
    | APIMisuse of string
    | WeCannotAffordFee of localChannelReserve: Money * requiredFee: Money * missingAmount: Money
    | CanNotSignBeforeRevocation
    | ReceivedCommitmentSignedWhenWeHaveNoPendingChanges
    | SignatureCountMismatch of expected: int * actual: int
    /// protocol violation defined in BOLT 02
    | ReceivedShutdownWhenRemoteHasUnsignedOutgoingHTLCs of msg: Shutdown
    
    /// When we create the first commitment txs as fundee,
    /// There might be a case that their initial funding amount is too low that it
    /// cannot afford fee
    | TheyCannotAffordFee of toRemote: LNMoney * fee: Money * channelReserve: Money
    
    // --- case they sent unacceptable msg ---
    | InvalidOpenChannel of InvalidOpenChannelError
    | InvalidAcceptChannel of InvalidAcceptChannelError
    | InvalidUpdateAddHTLC of InvalidUpdateAddHTLCError
    | InvalidRevokeAndACK of InvalidRevokeAndACKError
    | InvalidUpdateFee of InvalidUpdateFeeError
    // ------------------
    
    /// Consumer of the api (usually, that is wallet) failed to give an funding tx
    | FundingTxNotGiven of msg: string
    | OnceConfirmedFundingTxHasBecomeUnconfirmed of height: BlockHeight * depth: BlockHeightOffset
    | CannotCloseChannel of msg: string
    | UndefinedStateAndCmdPair of state: ChannelState * cmd: ChannelCommand
    | RemoteProposedHigherFeeThanBefore of previous: Money * current: Money
    // ---- invalid command ----
    | InvalidCMDAddHTLC of InvalidCMDAddHTLCError
    // -------------------------
    
    member this.RecommendedAction =
        match this with
        | CryptoError _ -> ReportAndCrash
        | TransactionRelatedErrors _ -> Close
        | HTLCAlreadySent _ -> Ignore
        | InvalidPaymentPreimage (_, _) -> Close
        | UnknownHTLCId _ -> Close
        | HTLCOriginNotKnown _ -> Close
        | InvalidFailureCode _ -> Close
        | APIMisuse _ -> Ignore
        | WeCannotAffordFee _ -> Close
        | CanNotSignBeforeRevocation -> Close
        | ReceivedCommitmentSignedWhenWeHaveNoPendingChanges -> Close
        | ReceivedShutdownWhenRemoteHasUnsignedOutgoingHTLCs _ -> Close
        | SignatureCountMismatch (_, _) -> Close
        | TheyCannotAffordFee (_, _, _) -> Close
        | InvalidOpenChannel _ -> DistrustPeer
        | InvalidAcceptChannel _ -> DistrustPeer
        | InvalidUpdateAddHTLC _ -> Close
        | InvalidRevokeAndACK _ -> Close
        | InvalidUpdateFee _ -> Close
        | FundingTxNotGiven _ -> Ignore
        | OnceConfirmedFundingTxHasBecomeUnconfirmed _ -> Close
        | CannotCloseChannel _ -> Ignore
        | UndefinedStateAndCmdPair _ -> Ignore
        | InvalidCMDAddHTLC _ -> Ignore
        | RemoteProposedHigherFeeThanBefore(_, _) -> Close
    
    override this.ToString() =
        match this with
        | HTLCAlreadySent htlcId ->
            sprintf "We have already sent a fail/fulfill for this htlc: %A" htlcId
        | InvalidPaymentPreimage(e, actual) ->
            sprintf "Invalid HTLC PreImage %A. Hash (%A) does not match the one expected %A"
                    actual
                    (actual.GetSha256())
                    e
        | InvalidFailureCode errorCode ->
            sprintf "invalid failure code %A" (errorCode.GetOnionErrorDescription())
        | WeCannotAffordFee (channelReserve, fees, missing) ->
            sprintf
                "Cannot afford fees. Missing Satoshis are: %A . Reserve Satoshis are %A . Fees are %A"
                channelReserve fees (missing)
        | SignatureCountMismatch(expected, actual) ->
            sprintf "Number of signatures went from the remote (%A) does not match the number expected (%A)" actual expected
        | TheyCannotAffordFee (toRemote, fee, channelReserve) ->
            sprintf "they are funder but cannot afford their fee. to_remote output is: %A; actual fee is %A; channel_reserve_satoshis is: %A" toRemote fee channelReserve
        | InvalidOpenChannel x ->
            sprintf "Invalid open_channel from the peer. \n %s" (x.ToString())
        | OnceConfirmedFundingTxHasBecomeUnconfirmed (height, depth) ->
            sprintf "once confirmed funding tx has become less confirmed than threshold %A! This is probably caused by reorg. current depth is: %A " height depth
        | ReceivedShutdownWhenRemoteHasUnsignedOutgoingHTLCs msg ->
            sprintf "They sent shutdown msg (%A) while they have pending unsigned HTLCs, this is protocol violation" msg
        | UndefinedStateAndCmdPair (state, cmd) ->
            sprintf "DotNetLightning does not know how to handle command (%A) while in state (%A)" cmd state
        | RemoteProposedHigherFeeThanBefore(prev, current) ->
            "remote proposed a commitment fee higher than the last commitment fee in the course of fee negotiation"
            + sprintf "previous fee=%A; fee remote proposed=%A;" prev current
        | x -> sprintf "%A" x
and ChannelConsumerAction =
    /// The error which should never happen.
    /// This implies a bug so we must do something on this Library
    /// For the sake of saving funds, the API consumer might attempt a channel close.
    /// But they cannot be sure that closing will always succeed.
    | ReportAndCrash
    /// Close channel immediately. The most popular action.
    /// Might be good to lower the trust score of the peer.
    | Close
    /// Same with the `Close` in the sense that the peer did something unacceptable to us.
    /// But the channel is not open yet.
    | DistrustPeer
    /// The error is not critical to the channel operation.
    /// But it maybe good to report the log message, or maybe lower the peer score.
    | Ignore
and InvalidOpenChannelError = {
    Msg: OpenChannel
    Errors: string list
}
    with
    static member Create msg e = {
        Msg = msg
        Errors = e
    }
    
and InvalidAcceptChannelError = {
    Msg: AcceptChannel
    Errors: string list
}
    with
    static member Create msg e = {
        Msg = msg
        Errors = e
    }
    
and InvalidUpdateAddHTLCError = {
    Msg: UpdateAddHTLC
    Errors: string list
}
    with
    static member Create msg e = {
        Msg = msg
        Errors = e
    }
and InvalidRevokeAndACKError = {
    Msg: RevokeAndACK
    Errors: string list
}
    with
    static member Create msg e = {
        Msg = msg
        Errors = e
    }
and InvalidUpdateFeeError = {
    Msg: UpdateFee
    Errors: string list
}
    with
    static member Create msg e = {
        Msg = msg
        Errors = e
    }
and InvalidCMDAddHTLCError = {
    CMD: CMDAddHTLC
    Errors: string list
}
    with
    static member Create cmd e = {
        CMD = cmd
        Errors = e
    }
[<AutoOpen>]
module private ValidationHelper =
    let check left predicate right msg =
        if predicate left right then
            sprintf msg left right
            |> Error
        else
            Ok()

/// Helpers to create channel error
[<AutoOpen>]
module internal ChannelError =
    let inline feeDeltaTooHigh msg (actualDelta, maxAccepted) =
        InvalidUpdateFeeError.Create
            msg
            [sprintf "delta is %.2f%% . But it must be lower than %.2f%%" (actualDelta * 100.0) (maxAccepted * 100.0)]
            |> InvalidUpdateFee |> Error
        
    let inline htlcAlreadySent htlcId =
        htlcId |> HTLCAlreadySent |> Error
        
    let inline invalidPaymentPreimage (e, a) =
        (e, a) |> InvalidPaymentPreimage |> Error
        
    let inline unknownHTLCId x =
        x |> UnknownHTLCId |> Error
        
    let inline htlcOriginNowKnown x =
        x |> HTLCOriginNotKnown |> Error
    let inline invalidFailureCode x =
        x |> InvalidFailureCode |> Error
        
    let inline apiMisuse x =
        x |> APIMisuse |> Error
        
    let inline cannotAffordFee x =
        x |> WeCannotAffordFee |> Error
    
    let inline signatureCountMismatch x =
        x |> SignatureCountMismatch |> Error
        
    let inline theyCannotAffordFee x =
        x |> TheyCannotAffordFee |> Error
        
    let onceConfirmedFundingTxHasBecomeUnconfirmed (height, depth) =
        (height, depth) |> OnceConfirmedFundingTxHasBecomeUnconfirmed |> Error
    
    let expectTransactionError result =
        Result.mapError (List.singleton >> TransactionRelatedErrors) result
        
    let expectTransactionErrors result =
        Result.mapError (TransactionRelatedErrors) result
        
    let expectFundingTxError msg =
        Result.mapError(FundingTxNotGiven) msg
        
    let invalidRevokeAndACK msg e =
        InvalidRevokeAndACKError.Create msg ([e]) |> InvalidRevokeAndACK |> Error
        
    let cannotCloseChannel msg =
        msg |> CannotCloseChannel|> Error

    let receivedShutdownWhenRemoteHasUnsignedOutgoingHTLCs msg =
        msg |> ReceivedShutdownWhenRemoteHasUnsignedOutgoingHTLCs |> Error
    let undefinedStateAndCmdPair state cmd =
        UndefinedStateAndCmdPair (state, cmd) |> Error
        
    let checkRemoteProposedHigherFeeThanBefore prev curr =
        if (prev < curr) then
            RemoteProposedHigherFeeThanBefore(prev, curr) |> Error
        else
            Ok()
module internal OpenChannelMsgValidation =
    let checkMaxAcceptedHTLCs (msg: OpenChannel) =
        if (msg.MaxAcceptedHTLCs < 1us) || (msg.MaxAcceptedHTLCs > 483us) then
            sprintf "max_accepted_htlcs must be in between %d and %d. But it was %d" 1us 483us msg.MaxAcceptedHTLCs
            |> Error
        else
            Ok()

    let checkFundingSatoshisLessThanMax (msg: OpenChannel) =
        if (msg.FundingSatoshis >= ChannelConstants.MAX_FUNDING_SATOSHIS) then
            sprintf "funding_satoshis must be less than %A. It was %A" ChannelConstants.MAX_FUNDING_SATOSHIS msg.FundingSatoshis
            |> Error
        else
            Ok()
    let checkChannelReserveSatohisLessThanFundingSatoshis (msg: OpenChannel) =
        if (msg.ChannelReserveSatoshis > msg.FundingSatoshis) then
            sprintf
                "Bogus channel_reserve_satoshis (%A). Must be bigger than funding_satoshis(%A)"
                msg.ChannelReserveSatoshis msg.FundingSatoshis
            |> Error
        else
            Ok()
    let checkPushMSatLesserThanFundingValue msg =
        let fundingValue =(msg.FundingSatoshis - msg.ChannelReserveSatoshis)
        if (msg.PushMSat.ToMoney() > fundingValue) then
            sprintf
                "push_msat(%A) larger than funding value(%A)"
                msg.PushMSat fundingValue
            |> Error
        else
            Ok()

    let checkFundingSatoshisLessThanDustLimitSatoshis (msg: OpenChannel) =
        if (msg.DustLimitSatoshis > msg.FundingSatoshis) then
            sprintf
                "Peer never wants payout outputs? dust_limit_satoshis: %A; funding_satoshi %A"
                msg.FundingSatoshis msg.DustLimitSatoshis
            |> Error
        else
            Ok()
            
    let checkRemoteFee (feeEstimator: IFeeEstimator) (feeRate: FeeRatePerKw) =
        let ourLowest = feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Background)
        let ourHighest = (feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority) * 2u)
        if feeRate < ourLowest || ourHighest < feeRate then
            sprintf
                "Peer's feerate (%A) was in unacceptable range, it must in between %A and %A"
                feeRate ourLowest ourHighest
            |> Error
        else
            Ok()

    let checkToSelfDelayIsInAcceptableRange (msg: OpenChannel) =
        if msg.ToSelfDelay > (MAX_LOCAL_BREAKDOWN_TIMEOUT) then
            sprintf "They wanted our payments to be delayed by a needlessly long period (%A) ." msg.ToSelfDelay
            |> Error
        else
            Ok()


    let checkConfigPermits (config: ChannelHandshakeLimits) (msg: OpenChannel) =
        let check1 =
            check
                msg.FundingSatoshis (<) config.MinFundingSatoshis
                "funding satoshis is less than the user specified limit. received: %A; limit: %A"
        let check2 =
            check
                (msg.HTLCMinimumMsat.ToMoney()) (>) (config.MinFundingSatoshis)
                "htlc minimum msat is higher than the users specified limit. received %A; limit: %A"
        let check3 =
            check
                msg.MaxHTLCValueInFlightMsat (<) config.MinMaxHTLCValueInFlightMSat
                "max htlc value in light msat is less than the user specified limit. received: %A; limit %A"
        let check4 =
            check
                msg.ChannelReserveSatoshis (>) config.MaxChannelReserveSatoshis
                "channel reserve satoshis is higher than the user specified limit. received %A; limit: %A"
        let check5 =
            check
                msg.MaxAcceptedHTLCs (<) config.MinMaxAcceptedHTLCs
                 "max accepted htlcs is less than the user specified limit. received: %A; limit: %A"
        let check6 =
            check
                msg.DustLimitSatoshis (<) config.MinDustLimitSatoshis
                "dust_limit_satoshis is less than the user specified limit. received: %A; limit: %A"
        let check7 =
            check
                msg.DustLimitSatoshis (>) config.MaxDustLimitSatoshis
                "dust_limit_satoshis is greater than the user specified limit. received: %A; limit: %A"
        Validation.ofResult(check1) *^> check2 *^> check3 *^> check4 *^> check5 *^> check6 *^> check7
    let checkChannelAnnouncementPreferenceAcceptable (config: ChannelConfig) (msg) =
        let theirAnnounce = (msg.ChannelFlags &&& 1uy) = 1uy
        if (config.PeerChannelConfigLimits.ForceChannelAnnouncementPreference) && config.ChannelOptions.AnnounceChannel <> theirAnnounce then
            "Peer tried to open channel but their announcement preference is different from ours"
            |> Error
        else
            Ok()

    let checkIsAcceptableByCurrentFeeRate (feeEstimator: IFeeEstimator) msg =
        let ourDustLimit = ChannelConstantHelpers.deriveOurDustLimitSatoshis feeEstimator
        let ourChannelReserve = ChannelConstantHelpers.getOurChannelReserve (msg.FundingSatoshis)
        let check left predicate right msg =
            if predicate left right then
                sprintf msg left right
                |> Error
            else
                Ok()

        let check1 =
            check
                ourChannelReserve (<) ourDustLimit
                "Suitable channel reserve not found. Aborting. (our channel reserve was (%A). and our dust limit was(%A))"
        let check2 =
            check
                msg.ChannelReserveSatoshis (<) ourDustLimit
                "channel_reserve_satoshis too small. It was: %A; but our dust_limit is: %A"
        let check3 =
            check
                ourChannelReserve (<) msg.DustLimitSatoshis
                "Dust limit too high for our channel reserve. our channel reserve is: %A . received dust_limit is: %A"
        Validation.ofResult(check1) *^> check2 *^> check3
        
    let checkIfFundersAmountSufficient (feeEst: IFeeEstimator) msg =
        let fundersAmount = LNMoney.Satoshis(msg.FundingSatoshis.Satoshi) - msg.PushMSat
        let backgroundFeeRate = feeEst.GetEstSatPer1000Weight(ConfirmationTarget.Background)
        let backgroundFee = backgroundFeeRate.ToFee COMMITMENT_TX_BASE_WEIGHT
        if (fundersAmount.ToMoney() < backgroundFee) then
            (sprintf "Insufficient funding amount for initial commitment. BackgroundFee %A. funders amount %A" backgroundFee fundersAmount)
            |> Error
        else
            let ourChannelReserve = ChannelConstantHelpers.getOurChannelReserve msg.FundingSatoshis
            let toLocalMSat = msg.PushMSat
            let toRemoteMSat = fundersAmount - backgroundFeeRate.ToFee(COMMITMENT_TX_BASE_WEIGHT).ToLNMoney()
            if (toLocalMSat <= (msg.ChannelReserveSatoshis.ToLNMoney()) && toRemoteMSat <= ourChannelReserve.ToLNMoney()) then
                ("Insufficient funding amount for initial commitment. ")
                |> Error
            else
                Ok()

module internal AcceptChannelMsgValidation =
    let private check left predicate right msg =
        if predicate left right then
            sprintf msg left right
            |> Error
        else
            Ok()

    let checkMaxAcceptedHTLCs (msg: AcceptChannel) =
        if (msg.MaxAcceptedHTLCs < 1us) || (msg.MaxAcceptedHTLCs > 483us) then
            sprintf "max_accepted_htlcs must be in between %d and %d. But it was %d" 1us 483us msg.MaxAcceptedHTLCs
            |> Error
        else
            Ok()

    let checkDustLimit msg =
        if msg.DustLimitSatoshis > Money.Satoshis(21000000L * 100000L) then
            sprintf "Peer never wants payout outputs? dust_limit_satoshis was: %A" msg.DustLimitSatoshis
            |> Error
        else
            Ok()

    let checkChannelReserveSatoshis (state: Data.WaitForAcceptChannelData) msg =
        if msg.ChannelReserveSatoshis > state.LastSent.FundingSatoshis then
            sprintf "bogus channel_reserve_satoshis %A . Must be larger than funding_satoshis %A" (msg.ChannelReserveSatoshis) (state.InputInitFunder.FundingSatoshis)
            |> Error
        else if msg.DustLimitSatoshis > state.LastSent.ChannelReserveSatoshis then
            sprintf "Bogus channel_reserve and dust_limit. dust_limit: %A; channel_reserve %A" msg.DustLimitSatoshis (state.LastSent.ChannelReserveSatoshis)
            |> Error
        else if msg.ChannelReserveSatoshis < state.LastSent.DustLimitSatoshis then
            sprintf "Peer never wants payout outputs? channel_reserve_satoshis are %A; dust_limit_satoshis in our last sent msg is %A" msg.ChannelReserveSatoshis (state.LastSent.DustLimitSatoshis)
            |> Error
        else
            Ok()

    let checkDustLimitIsLargerThanOurChannelReserve (state: Data.WaitForAcceptChannelData) msg =
        let reserve = ChannelConstantHelpers.getOurChannelReserve state.LastSent.FundingSatoshis
        check
            msg.DustLimitSatoshis (>) reserve
            "dust limit (%A) is bigger than our channel reserve (%A)" 

    let checkMinimumHTLCValueIsAcceptable (state: Data.WaitForAcceptChannelData) (msg: AcceptChannel) =
        if (msg.HTLCMinimumMSat.ToMoney() >= (state.LastSent.FundingSatoshis - msg.ChannelReserveSatoshis)) then
            sprintf "Minimum HTLC value is greater than full channel value HTLCMinimum %A satoshi; funding_satoshis %A; channel_reserve: %A" (msg.HTLCMinimumMSat.ToMoney()) (state.LastSent.FundingSatoshis) (msg.ChannelReserveSatoshis)
            |> Error
        else
            Ok()

    let checkToSelfDelayIsAcceptable (msg) =
        if (msg.ToSelfDelay > MAX_LOCAL_BREAKDOWN_TIMEOUT) then
            sprintf "They wanted our payments to be delayed by a needlessly long period (%A)" msg.ToSelfDelay
            |> Error
        else
            Ok()

    let checkConfigPermits (config: ChannelHandshakeLimits) (msg: AcceptChannel) =
        let check1 = check msg.HTLCMinimumMSat (>) config.MaxHTLCMinimumMSat "HTLC Minimum msat in accept_channel (%A) is higher than the user specified limit (%A)"
        let check2 = check msg.MaxHTLCValueInFlightMsat (<) config.MinMaxHTLCValueInFlightMSat "max htlc value in flight msat (%A) is less than the user specified limit (%A)"
        let check3 = check msg.ChannelReserveSatoshis (>) config.MaxChannelReserveSatoshis "max reserve_satoshis (%A) is higher than the user specified limit (%A)"
        let check4 = check msg.MaxAcceptedHTLCs (<) config.MinMaxAcceptedHTLCs "max accepted htlcs (%A) is less than the user specified limit (%A)"
        let check5 = check msg.DustLimitSatoshis (<) config.MinDustLimitSatoshis "dust limit satoshis (%A) is less then the user specified limit (%A)"
        let check6 = check msg.DustLimitSatoshis (>) config.MaxDustLimitSatoshis "dust limit satoshis (%A) is greater then the user specified limit (%A)"
        let check7 = check (msg.MinimumDepth.Value) (>) (config.MaxMinimumDepth.Value |> uint32) "We consider the minimum depth (%A) to be unreasonably large. Our max minimum depth is (%A)"

        (check1 |> Validation.ofResult) *^> check2 *^> check3 *^> check4 *^> check5 *^> check6 *^> check7
        

module UpdateAddHTLCValidation =
    let internal checkExpiryIsNotPast (current: BlockHeight) (expiry) =
        check (expiry) (<=) (current) "AddHTLC's Expiry was %A but it must be larger than current height %A"


    let internal checkExpiryIsInAcceptableRange (current: BlockHeight) (expiry) =
        let checkIsToSoon = check (expiry) (<=) (current + MIN_CLTV_EXPIRY) "CMD_ADD_HTLC.Expiry was %A but it was too close to current height. Minimum is: %A"
        let checkIsToFar = check (expiry) (>=) (current + MAX_CLTV_EXPIRY) "CMD_ADD_HTLC.Expiry was %A but it was too far from current height. Maximum is: %A"
        Validation.ofResult(checkIsToSoon) *^> checkIsToFar

    let internal checkAmountIsLargerThanMinimum (htlcMinimum: LNMoney) (amount) =
        check (amount) (<) (htlcMinimum) "htlc value (%A) is too small. must be greater or equal to %A"

    
module internal UpdateAddHTLCValidationWithContext =
    let checkLessThanHTLCValueInFlightLimit (currentSpec: CommitmentSpec) (limit) (add: UpdateAddHTLC) =
        let htlcValueInFlight = currentSpec.HTLCs |> Map.toSeq |> Seq.sumBy (fun (_, v) -> v.Add.AmountMSat)
        if (htlcValueInFlight > limit) then
            sprintf "Too much HTLC value is in flight. Current: %A. Limit: %A \n Could not add new one with value: %A"
                    htlcValueInFlight
                    limit
                    add.AmountMSat
            |> Error
        else
            Ok()

    let checkLessThanMaxAcceptedHTLC (currentSpec: CommitmentSpec) (limit: uint16) =
        let acceptedHTLCs = currentSpec.HTLCs |> Map.toSeq |> Seq.filter (fun kv -> (snd kv).Direction = In) |> Seq.length
        check acceptedHTLCs (>) (int limit) "We have much number of HTLCs (%A). Limit specified by remote is (%A). So not going to relay"

    let checkWeHaveSufficientFunds (state: Commitments) (currentSpec) =
        let fees = if (state.LocalParams.IsFunder) then (Transactions.commitTxFee (state.RemoteParams.DustLimitSatoshis) currentSpec) else Money.Zero
        let missing = currentSpec.ToRemote.ToMoney() - state.RemoteParams.ChannelReserveSatoshis - fees
        if (missing < Money.Zero) then
            sprintf "We don't have sufficient funds to send HTLC. current to_remote amount is: %A. Remote Channel Reserve is: %A. and fee is %A"
                    (currentSpec.ToRemote.ToMoney())
                    (state.RemoteParams.ChannelReserveSatoshis)
                    (fees)
            |> Error
        else
            Ok()
module internal UpdateFeeValidation =
    let private feeRateMismatch (FeeRatePerKw remote, FeeRatePerKw local) =
        (2.0 * float (remote - local) / float (remote + local))
        |> abs
    let checkFeeDiffTooHigh (msg: UpdateFee) (localFeeRatePerKw: FeeRatePerKw) (maxFeeRateMismatchRatio) =
        let remoteFeeRatePerKw = msg.FeeRatePerKw
        let diff = feeRateMismatch(remoteFeeRatePerKw, localFeeRatePerKw)
        if (diff > maxFeeRateMismatchRatio) then
            (diff, maxFeeRateMismatchRatio)
            |> feeDeltaTooHigh msg
            else
                Ok ()
