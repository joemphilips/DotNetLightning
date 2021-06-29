namespace DotNetLightning.Channel

open DotNetLightning.Utils
open NBitcoinExtensions
open DotNetLightning.Utils.OnionError
open DotNetLightning.Chain
open DotNetLightning.Crypto
open DotNetLightning.Serialization
open DotNetLightning.Serialization.Msgs
open DotNetLightning.Transactions

open NBitcoin

open ResultUtils
open ResultUtils.Portability

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
    | ReceivedShutdownWhenRemoteHasUnsignedOutgoingHTLCs of msg: ShutdownMsg
    
    /// When we create the first commitment txs as fundee,
    /// There might be a case that their initial funding amount is too low that it
    /// cannot afford fee
    | TheyCannotAffordFee of toRemote: LNMoney * fee: Money * channelReserve: Money
    
    // --- case they sent unacceptable msg ---
    | InvalidFundingLocked of InvalidFundingLockedError
    | InvalidOpenChannel of InvalidOpenChannelError
    | InvalidAcceptChannel of InvalidAcceptChannelError
    | InvalidUpdateAddHTLC of InvalidUpdateAddHTLCError
    | InvalidRevokeAndACK of InvalidRevokeAndACKError
    | InvalidUpdateFee of InvalidUpdateFeeError
    // ------------------
    
    /// Consumer of the api (usually, that is wallet) failed to give an funding tx
    | ReceivedClosingSignedBeforeReceivingShutdown
    | ReceivedClosingSignedBeforeSendingShutdown
    | ReceivedClosingSignedBeforeSendingOrReceivingShutdown
    | FundingTxNotGiven of msg: string
    | OnceConfirmedFundingTxHasBecomeUnconfirmed of height: BlockHeight * depth: BlockHeightOffset32
    | CannotCloseChannel of msg: string
    | RemoteProposedHigherFeeThanBaseFee of baseFee: Money * proposedFee: Money
    | RemoteProposedFeeOutOfNegotiatedRange of ourPreviousFee: Money * theirPreviousFee: Money * theirNextFee: Money
    | NoUpdatesToSign
    | CannotSignCommitmentBeforeRevocation
    | InsufficientConfirmations of requiredDepth: BlockHeightOffset32 * currentDepth: BlockHeightOffset32
    // ---- invalid command ----
    | InvalidOperationAddHTLC of InvalidOperationAddHTLCError
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
        | InvalidFundingLocked _ -> DistrustPeer
        | InvalidOpenChannel _ -> DistrustPeer
        | InvalidAcceptChannel _ -> DistrustPeer
        | InvalidUpdateAddHTLC _ -> Close
        | InvalidRevokeAndACK _ -> Close
        | InvalidUpdateFee _ -> Close
        | ReceivedClosingSignedBeforeReceivingShutdown -> Close
        | ReceivedClosingSignedBeforeSendingShutdown -> Close
        | ReceivedClosingSignedBeforeSendingOrReceivingShutdown -> Close
        | FundingTxNotGiven _ -> Ignore
        | OnceConfirmedFundingTxHasBecomeUnconfirmed _ -> Close
        | CannotCloseChannel _ -> Ignore
        | NoUpdatesToSign -> Ignore
        | CannotSignCommitmentBeforeRevocation -> Ignore
        | InsufficientConfirmations(_, _) -> Ignore
        | InvalidOperationAddHTLC _ -> Ignore
        | RemoteProposedHigherFeeThanBaseFee(_, _) -> Close
        | RemoteProposedFeeOutOfNegotiatedRange(_, _, _) -> Close
    
    member this.Message =
        match this with
        | HTLCAlreadySent htlcId ->
            sprintf "We have already sent a fail/fulfill for htlc id %i" htlcId.Value
        | InvalidPaymentPreimage(e, actual) ->
            sprintf "Invalid HTLC PreImage %A. Hash (%A) does not match the one expected %A"
                    actual
                    actual.Hash
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
        | InvalidFundingLocked invalidFundingLockedError ->
            sprintf "Invalid funding_locked from the peer: %s" invalidFundingLockedError.Message
        | InvalidOpenChannel invalidOpenChannelError ->
            sprintf "Invalid open_channel from the peer.: %s" invalidOpenChannelError.Message
        | OnceConfirmedFundingTxHasBecomeUnconfirmed (height, depth) ->
            sprintf "once confirmed funding tx has become less confirmed than threshold %A! This is probably caused by reorg. current depth is: %A " height depth
        | ReceivedShutdownWhenRemoteHasUnsignedOutgoingHTLCs msg ->
            sprintf "They sent shutdown msg (%A) while they have pending unsigned HTLCs, this is protocol violation" msg
        | RemoteProposedHigherFeeThanBaseFee(baseFee, proposedFee) ->
            "remote proposed a closing fee higher than commitment fee of the final commitment transaction. "
            + sprintf "commitment fee=%A; fee remote proposed=%A;" baseFee proposedFee
        | RemoteProposedFeeOutOfNegotiatedRange(ourPreviousFee, theirPreviousFee, theirNextFee) ->
            "remote proposed a closing fee which was not strictly between the previous fee that \
            we proposed and the previous fee that they proposed. "
            + sprintf
                "our previous fee = %A; their previous fee = %A; their next fee = %A"
                ourPreviousFee theirPreviousFee theirNextFee
        | CryptoError cryptoError ->
            sprintf "Crypto error: %s" cryptoError.Message
        | TransactionRelatedErrors transactionErrors ->
            let getMessage(transactionError: TransactionError): string =
                transactionError.Message
            sprintf "Transaction errors: %s" (String.concat "; " (Seq.map getMessage transactionErrors))
        | UnknownHTLCId htlcId ->
            sprintf "Unknown HTLC id (%i)" htlcId.Value
        | HTLCOriginNotKnown htlcId ->
            sprintf "Origin of HTLC %i not known" htlcId.Value
        | APIMisuse error ->
            sprintf "Internal error (API misuse): %s" error
        | CanNotSignBeforeRevocation ->
            "Cannot sign before revocation"
        | ReceivedCommitmentSignedWhenWeHaveNoPendingChanges ->
            "Received commitment signed when we have not pending changes"
        | InvalidAcceptChannel invalidAcceptChannelError ->
            sprintf "Invalid accept_channel msg: %s" invalidAcceptChannelError.Message
        | InvalidUpdateAddHTLC invalidUpdateAddHTLCError ->
            sprintf "Invalid udpate_add_htlc msg: %s" invalidUpdateAddHTLCError.Message
        | InvalidRevokeAndACK invalidRevokeAndACKError ->
            sprintf "Invalid revoke_and_ack msg: %s" invalidRevokeAndACKError.Message
        | InvalidUpdateFee invalidUpdateFeeError ->
            sprintf "Invalid update_fee msg: %s" invalidUpdateFeeError.Message
        | ReceivedClosingSignedBeforeReceivingShutdown ->
            sprintf "received closing_signed before receiving shutdown"
        | ReceivedClosingSignedBeforeSendingShutdown ->
            sprintf "received closing_signed before sending shutdown"
        | ReceivedClosingSignedBeforeSendingOrReceivingShutdown ->
            sprintf "received closing_signed before sending or receiving shutdown"
        | FundingTxNotGiven msg ->
            sprintf "Funding tx not given: %s" msg
        | CannotCloseChannel msg ->
            sprintf "Cannot close channel: %s" msg
        | NoUpdatesToSign ->
            "No updates to sign"
        | CannotSignCommitmentBeforeRevocation ->
            "Cannot sign commitment before previous commitment is revoked"
        | InsufficientConfirmations (requiredConfirmations, currentConfirmations) ->
            sprintf
                "Insufficient confirmations. %i required, only have %i"
                requiredConfirmations.Value
                currentConfirmations.Value
        | InvalidOperationAddHTLC invalidOperationAddHTLCError ->
            sprintf "Invalid operation (add htlc): %s" invalidOperationAddHTLCError.Message

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
and InvalidFundingLockedError ={
    NetworkMsg: FundingLockedMsg
}
    with
    member this.Message =
        "remote peer sent a second funding_locked message which does not match their first"

and InvalidOpenChannelError = {
    NetworkMsg: OpenChannelMsg
    Errors: string list
}
    with
    static member Create msg e = {
        NetworkMsg = msg
        Errors = e
    }
    member this.Message =
        String.concat "; " this.Errors
    
and InvalidAcceptChannelError = {
    NetworkMsg: AcceptChannelMsg
    Errors: string list
}
    with
    static member Create msg e = {
        NetworkMsg = msg
        Errors = e
    }
    member this.Message =
        String.concat "; " this.Errors
    
and InvalidUpdateAddHTLCError = {
    NetworkMsg: UpdateAddHTLCMsg
    Errors: string list
}
    with
    static member Create msg e = {
        NetworkMsg = msg
        Errors = e
    }
    member this.Message =
        String.concat "; " this.Errors

and InvalidRevokeAndACKError = {
    NetworkMsg: RevokeAndACKMsg
    Errors: string list
}
    with
    static member Create msg e = {
        NetworkMsg = msg
        Errors = e
    }
    member this.Message =
        String.concat "; " this.Errors

and InvalidUpdateFeeError = {
    NetworkMsg: UpdateFeeMsg
    Errors: string list
}
    with
    static member Create msg e = {
        NetworkMsg = msg
        Errors = e
    }
    member this.Message =
        String.concat "; " this.Errors

and InvalidOperationAddHTLCError = {
    Operation: OperationAddHTLC
    Errors: string list
}
    with
    static member Create op e = {
        Operation = op
        Errors = e
    }
    member this.Message =
        String.concat "; " this.Errors

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
        
    let inline htlcOriginNotKnown x =
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
        InvalidRevokeAndACKError.Create msg ([e]) |> InvalidRevokeAndACK
        
    let cannotCloseChannel msg =
        msg |> CannotCloseChannel

    let receivedShutdownWhenRemoteHasUnsignedOutgoingHTLCs msg =
        msg |> ReceivedShutdownWhenRemoteHasUnsignedOutgoingHTLCs |> Error
        
    let checkRemoteProposedHigherFeeThanBaseFee baseFee proposedFee =
        if (baseFee < proposedFee) then
            RemoteProposedHigherFeeThanBaseFee(baseFee, proposedFee) |> Error
        else
            Ok()

    let checkRemoteProposedFeeWithinNegotiatedRange (ourPreviousFeeOpt: Option<Money>)
                                                    (theirPreviousFeeOpt: Option<Money>)
                                                    (theirNextFee: Money) =
        match (ourPreviousFeeOpt, theirPreviousFeeOpt) with
        | (Some ourPreviousFee, Some theirPreviousFee) ->
            let feeWithinRange =
                ((theirNextFee < theirPreviousFee) && (theirNextFee >= ourPreviousFee)) ||
                ((theirNextFee <= ourPreviousFee) && (theirNextFee > theirPreviousFee))
            if feeWithinRange then
                Ok ()
            else
                RemoteProposedFeeOutOfNegotiatedRange(
                    ourPreviousFee,
                    theirPreviousFee,
                    theirNextFee
                ) |> Error
        | _ -> Ok ()

module internal OpenChannelMsgValidation =
    let checkMaxAcceptedHTLCs (msg: OpenChannelMsg) =
        if (msg.MaxAcceptedHTLCs < 1us) || (msg.MaxAcceptedHTLCs > 483us) then
            sprintf "max_accepted_htlcs must be in between %d and %d. But it was %d" 1us 483us msg.MaxAcceptedHTLCs
            |> Error
        else
            Ok()

    let checkFundingSatoshisLessThanMax (msg: OpenChannelMsg) (localParams: LocalParams) (isOurOpenChannelMsg: bool) =
        // If we are validating our own open message we make sure we are forcing support for option_support_large_channel on the other peer
        let featureType =
            match isOurOpenChannelMsg with
            | true ->
                Some FeaturesSupport.Mandatory
            | false ->
                None

        if msg.FundingSatoshis >= ChannelConstants.MAX_FUNDING_SATOSHIS && 
                not (localParams.Features.HasFeature (Feature.OptionSupportLargeChannel, ?featureType = featureType)) then
            sprintf 
                "funding_satoshis must be less than %A. It was %A, consider activating option_support_large_channel feature." 
                ChannelConstants.MAX_FUNDING_SATOSHIS 
                msg.FundingSatoshis
            |> Error
        else
            Ok()
    let checkChannelReserveSatohisLessThanFundingSatoshis (msg: OpenChannelMsg) =
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

    let checkFundingSatoshisLessThanDustLimitSatoshis (msg: OpenChannelMsg) =
        if (msg.DustLimitSatoshis > msg.FundingSatoshis) then
            sprintf
                "The dust limit (%A) is larger than the funding amount (%A)"
                msg.FundingSatoshis msg.DustLimitSatoshis
            |> Error
        else
            Ok()
            
    let checkRemoteFee (feeEstimator: IFeeEstimator)
                       (remoteFeeRatePerKw: FeeRatePerKw)
                       (maxFeeRateMismatchRatio: float) =
        let localFeeRatePerKw =
            feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Background)
        let diff = remoteFeeRatePerKw.MismatchRatio localFeeRatePerKw
        if (diff > maxFeeRateMismatchRatio) then
            sprintf
                "Peer's feerate (%A) was unacceptably far from the estimated fee rate of %A"
                remoteFeeRatePerKw localFeeRatePerKw
            |> Error
        else
            Ok()

    let checkToSelfDelayIsInAcceptableRange (msg: OpenChannelMsg) =
        if msg.ToSelfDelay > (MAX_LOCAL_BREAKDOWN_TIMEOUT) then
            sprintf "They wanted our payments to be delayed by a needlessly long period (%A) ." msg.ToSelfDelay
            |> Error
        else
            Ok()


    let checkConfigPermits (config: ChannelHandshakeLimits) (msg: OpenChannelMsg) =
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
    let checkChannelAnnouncementPreferenceAcceptable (channelHandshakeLimits: ChannelHandshakeLimits)
                                                     (announceChannel: bool)
                                                     (msg: OpenChannelMsg) =
        let theirAnnounce = msg.ChannelFlags.AnnounceChannel
        if (channelHandshakeLimits.ForceChannelAnnouncementPreference) && announceChannel <> theirAnnounce then
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
                "Funder's channel reserve (%A, dictated by the fundee) is less than the fundee's dust limit (%A). \
                The funder must use a larger amount to open a channel."
        let check2 =
            check
                msg.ChannelReserveSatoshis (<) ourDustLimit
                "Fundee's channel reserve (%A, dictated by the funder) is less than the fundee's dust limit (%A). \
                The funder must use a larger amount to open the channel, or require a smaller channel reserve."
        let check3 =
            check
                ourChannelReserve (<) msg.DustLimitSatoshis
                "Funder's channel reserve (%A, dictated by the fundee) is less than the funder's dust limit (%A)."
        Validation.ofResult(check1) *^> check2 *^> check3

    let checkFunderCanAffordFee (feeRate: FeeRatePerKw) (msg: OpenChannelMsg) =
        let fundersAmount = LNMoney.Satoshis(msg.FundingSatoshis.Satoshi) - msg.PushMSat
        let fee = feeRate.CalculateFeeFromWeight COMMITMENT_TX_BASE_WEIGHT
        if fundersAmount.ToMoney() < fee then
            sprintf
                "funding amount (%A) minus push amount (%A) does not cover commitment tx fee (%A)"
                msg.FundingSatoshis msg.PushMSat fee
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

    let checkMaxAcceptedHTLCs (msg: AcceptChannelMsg) =
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

    let checkChannelReserveSatoshis (fundingAmount: Money)
                                    (channelReserveAmount: Money)
                                    (dustLimit: Money)
                                    (acceptChannelMsg: AcceptChannelMsg) =
        if acceptChannelMsg.ChannelReserveSatoshis > fundingAmount then
            sprintf "bogus channel_reserve_satoshis %A . Must be larger than funding_satoshis %A" (acceptChannelMsg.ChannelReserveSatoshis) fundingAmount
            |> Error
        else if acceptChannelMsg.DustLimitSatoshis > channelReserveAmount then
            sprintf "Bogus channel_reserve and dust_limit. dust_limit: %A; channel_reserve %A" acceptChannelMsg.DustLimitSatoshis channelReserveAmount
            |> Error
        else if acceptChannelMsg.ChannelReserveSatoshis < dustLimit then
            sprintf "Peer never wants payout outputs? channel_reserve_satoshis are %A; dust_limit_satoshis in our last sent msg is %A" acceptChannelMsg.ChannelReserveSatoshis dustLimit
            |> Error
        else
            Ok()

    let checkDustLimitIsLargerThanOurChannelReserve (channelReserveAmount: Money)
                                                    (acceptChannelMsg: AcceptChannelMsg) =
        check
            acceptChannelMsg.DustLimitSatoshis (>) channelReserveAmount
            "dust limit (%A) is bigger than our channel reserve (%A)" 

    let checkMinimumHTLCValueIsAcceptable (fundingAmount: Money)
                                          (acceptChannelMsg: AcceptChannelMsg) =
        if acceptChannelMsg.HTLCMinimumMSat.ToMoney() >= (fundingAmount - acceptChannelMsg.ChannelReserveSatoshis) then
            sprintf "Minimum HTLC value is greater than full channel value HTLCMinimum %A satoshi; funding_satoshis %A; channel_reserve: %A" (acceptChannelMsg.HTLCMinimumMSat.ToMoney()) (fundingAmount) (acceptChannelMsg.ChannelReserveSatoshis)
            |> Error
        else
            Ok()

    let checkToSelfDelayIsAcceptable (msg) =
        if (msg.ToSelfDelay > MAX_LOCAL_BREAKDOWN_TIMEOUT) then
            sprintf "They wanted our payments to be delayed by a needlessly long period (%A)" msg.ToSelfDelay
            |> Error
        else
            Ok()

    let checkConfigPermits (config: ChannelHandshakeLimits) (msg: AcceptChannelMsg) =
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
        let checkIsToSoon = check (expiry) (<=) (current + MIN_CLTV_EXPIRY) "Operation_ADD_HTLC.Expiry was %A but it was too close to current height. Minimum is: %A"
        let checkIsToFar = check (expiry) (>=) (current + MAX_CLTV_EXPIRY) "Operation_ADD_HTLC.Expiry was %A but it was too far from current height. Maximum is: %A"
        Validation.ofResult(checkIsToSoon) *^> checkIsToFar

    let internal checkAmountIsLargerThanMinimum (htlcMinimum: LNMoney) (amount) =
        check (amount) (<) (htlcMinimum) "htlc value (%A) is too small. must be greater or equal to %A"

    
module internal UpdateAddHTLCValidationWithContext =
    let checkLessThanHTLCValueInFlightLimit (currentSpec: CommitmentSpec) (limit) (add: UpdateAddHTLCMsg) =
        let outgoingValue =
            currentSpec.OutgoingHTLCs
            |> Map.toSeq
            |> Seq.sumBy (fun (_, v) -> v.Amount)
        let incomingValue =
            currentSpec.IncomingHTLCs
            |> Map.toSeq
            |> Seq.sumBy (fun (_, v) -> v.Amount)
        let htlcValueInFlight = outgoingValue + incomingValue
        if (htlcValueInFlight > limit) then
            sprintf "Too much HTLC value is in flight. Current: %A. Limit: %A \n Could not add new one with value: %A"
                    htlcValueInFlight
                    limit
                    add.Amount
            |> Error
        else
            Ok()

    let checkLessThanMaxAcceptedHTLC (currentSpec: CommitmentSpec) (limit: uint16) =
        let acceptedHTLCs = currentSpec.IncomingHTLCs |> Map.count
        check acceptedHTLCs (>) (int limit) "We have much number of HTLCs (%A). Limit specified by remote is (%A). So not going to relay"

    let checkWeHaveSufficientFunds (staticChannelConfig: StaticChannelConfig) (currentSpec) =
        let fees =
            if staticChannelConfig.IsFunder then
                Transactions.commitTxFee
                    staticChannelConfig.RemoteParams.DustLimitSatoshis
                    currentSpec
            else
                Money.Zero
        let missing = currentSpec.ToRemote.ToMoney() - staticChannelConfig.RemoteParams.ChannelReserveSatoshis - fees
        if (missing < Money.Zero) then
            sprintf "We don't have sufficient funds to send HTLC. current to_remote amount is: %A. Remote Channel Reserve is: %A. and fee is %A"
                    (currentSpec.ToRemote.ToMoney())
                    (staticChannelConfig.RemoteParams.ChannelReserveSatoshis)
                    (fees)
            |> Error
        else
            Ok()
module internal UpdateFeeValidation =
    let checkFeeDiffTooHigh (msg: UpdateFeeMsg) (localFeeRatePerKw: FeeRatePerKw) (maxFeeRateMismatchRatio) =
        let remoteFeeRatePerKw = msg.FeeRatePerKw
        let diff = remoteFeeRatePerKw.MismatchRatio localFeeRatePerKw
        if (diff > maxFeeRateMismatchRatio) then
            (diff, maxFeeRateMismatchRatio)
            |> feeDeltaTooHigh msg
            else
                Ok ()
