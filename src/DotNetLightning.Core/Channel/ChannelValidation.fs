namespace DotNetLightning.Channel

open ResultUtils
open NBitcoin

open DotNetLightning.Chain
open DotNetLightning.Crypto
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Serialization.Msgs
open DotNetLightning.Transactions

exception ChannelException of ChannelError
module internal ChannelHelpers =

    let getFundingScriptCoin (ourFundingPubKey: FundingPubKey)
                             (theirFundingPubKey: FundingPubKey)
                             (TxId fundingTxId)
                             (TxOutIndex fundingOutputIndex)
                             (fundingSatoshis)
                                 : ScriptCoin =
        let redeem = Scripts.funding ourFundingPubKey theirFundingPubKey
        Coin(fundingTxId, uint32 fundingOutputIndex, fundingSatoshis, redeem.WitHash.ScriptPubKey)
        |> fun c -> ScriptCoin(c, redeem)

    let private makeFlags (isNode1: bool, enable: bool) =
        (if isNode1 then 1uy else 0uy) ||| ((if enable then 1uy else 0uy) <<< 1)

    let internal makeChannelUpdate (chainHash,
                                    nodeSecret: NodeSecret,
                                    remoteNodeId: NodeId,
                                    shortChannelId,
                                    cltvExpiryDelta,
                                    htlcMinimum,
                                    feeBase,
                                    feeProportionalMillionths,
                                    enabled: bool,
                                    timestamp
                                   ) =
        let timestamp = defaultArg timestamp ((System.DateTime.UtcNow.ToUnixTimestamp()) |> uint32)
        let isNodeOne = nodeSecret.NodeId() < remoteNodeId
        let unsignedChannelUpdate = {
            ChainHash = chainHash
            ShortChannelId = shortChannelId
            Timestamp = timestamp
            ChannelFlags = makeFlags (isNodeOne, enabled)
            MessageFlags = 0uy
            CLTVExpiryDelta = cltvExpiryDelta
            HTLCMinimumMSat = htlcMinimum
            FeeBaseMSat = feeBase
            FeeProportionalMillionths = feeProportionalMillionths
            HTLCMaximumMSat = None
        }
        let signature =
            unsignedChannelUpdate.ToBytes()
            |> Crypto.Hashes.SHA256
            |> uint256
            |> nodeSecret.RawKey().Sign
            |> LNECDSASignature
        {
            ChannelUpdateMsg.Contents = unsignedChannelUpdate
            Signature = signature
        }

    /// gets the fee we'd want to charge for adding an HTLC output to this channel
    let internal getOurFeeBaseMSat (feeEstimator: IFeeEstimator) (FeeRatePerKw feeRatePerKw) (isFunder: bool) =
        // for lack of a better metric, we calculate waht it would cost to consolidate the new HTLC
        // output value back into a transaction with the regular channel output:

        // the fee cost of the HTLC-success/HTLC-Timout transaction
        let mutable res = uint64 feeRatePerKw * (max (ChannelConstants.HTLC_TIMEOUT_TX_WEIGHT) (ChannelConstants.HTLC_TIMEOUT_TX_WEIGHT)) |> fun r -> r / 1000UL
        if (isFunder) then
            res <- res + uint64 feeRatePerKw * COMMITMENT_TX_WEIGHT_PER_HTLC / 1000UL

        //+ the marginal cost of an input which spends the HTLC-Success/HTLC-Timeout output:
        res <-
            res + (uint64 (feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Normal).Value) * SPENDING_INPUT_FOR_A_OUTPUT_WEIGHT) / 1000UL
        res |> LNMoney.Satoshis

    let makeFirstCommitTxs (localParams: LocalParams)
                          (remoteParams: RemoteParams)
                          (fundingSatoshis: Money)
                          (pushMSat: LNMoney)
                          (initialFeeRatePerKw: FeeRatePerKw)
                          (fundingOutputIndex: TxOutIndex)
                          (fundingTxId: TxId)
                          (localPerCommitmentPoint: PerCommitmentPoint)
                          (remotePerCommitmentPoint: PerCommitmentPoint)
                          (n: Network): Result<CommitmentSpec * CommitTx * CommitmentSpec * CommitTx, ChannelError> =
        let toLocal = if (localParams.IsFunder) then fundingSatoshis.ToLNMoney() - pushMSat else pushMSat
        let toRemote = if (localParams.IsFunder) then pushMSat else fundingSatoshis.ToLNMoney() - pushMSat
        let localChannelKeys = localParams.ChannelPubKeys
        let remoteChannelKeys = remoteParams.ChannelPubKeys
        let localSpec = CommitmentSpec.Create toLocal toRemote initialFeeRatePerKw
        let remoteSpec = CommitmentSpec.Create toRemote toLocal initialFeeRatePerKw
        let checkTheyCanAffordFee() =
            let toRemote = remoteSpec.ToLocal
            let fees = Transactions.commitTxFee remoteParams.DustLimitSatoshis remoteSpec
            let missing = toRemote.ToMoney() - localParams.ChannelReserveSatoshis - fees
            if missing < Money.Zero then
                theyCannotAffordFee(toRemote, fees, localParams.ChannelReserveSatoshis)
            else
                Ok()
        let makeFirstCommitTxCore() =
            let scriptCoin = getFundingScriptCoin localChannelKeys.FundingPubKey
                                                  remoteChannelKeys.FundingPubKey
                                                  fundingTxId
                                                  fundingOutputIndex
                                                  fundingSatoshis
            let localPubKeysForLocalCommitment = localPerCommitmentPoint.DeriveCommitmentPubKeys localChannelKeys
            let remotePubKeysForLocalCommitment = localPerCommitmentPoint.DeriveCommitmentPubKeys remoteChannelKeys

            let localCommitTx =
                Transactions.makeCommitTx scriptCoin
                                          CommitmentNumber.FirstCommitment
                                          localChannelKeys.PaymentBasepoint
                                          remoteChannelKeys.PaymentBasepoint
                                          localParams.IsFunder
                                          localParams.DustLimitSatoshis
                                          remotePubKeysForLocalCommitment.RevocationPubKey
                                          remoteParams.ToSelfDelay
                                          localPubKeysForLocalCommitment.DelayedPaymentPubKey
                                          remotePubKeysForLocalCommitment.PaymentPubKey
                                          localPubKeysForLocalCommitment.HtlcPubKey
                                          remotePubKeysForLocalCommitment.HtlcPubKey
                                          localSpec
                                          n

            let localPubKeysForRemoteCommitment = remotePerCommitmentPoint.DeriveCommitmentPubKeys localChannelKeys
            let remotePubKeysForRemoteCommitment = remotePerCommitmentPoint.DeriveCommitmentPubKeys remoteChannelKeys

            let remoteCommitTx =
                Transactions.makeCommitTx scriptCoin
                                          CommitmentNumber.FirstCommitment
                                          remoteChannelKeys.PaymentBasepoint
                                          localChannelKeys.PaymentBasepoint
                                          (not localParams.IsFunder)
                                          (remoteParams.DustLimitSatoshis)
                                          localPubKeysForRemoteCommitment.RevocationPubKey
                                          localParams.ToSelfDelay
                                          remotePubKeysForRemoteCommitment.DelayedPaymentPubKey
                                          localPubKeysForRemoteCommitment.PaymentPubKey
                                          remotePubKeysForRemoteCommitment.HtlcPubKey
                                          localPubKeysForRemoteCommitment.HtlcPubKey
                                          remoteSpec
                                          n

            (localSpec, localCommitTx, remoteSpec, remoteCommitTx) |> Ok

        if (not localParams.IsFunder) then
            result {
                do! checkTheyCanAffordFee()
                return! (makeFirstCommitTxCore())
            }
        else
            makeFirstCommitTxCore()


module internal Validation =

    open DotNetLightning.Channel
    let checkOurOpenChannelMsgAcceptable (_conf: ChannelConfig) (msg: OpenChannelMsg) =
        Validation.ofResult(OpenChannelMsgValidation.checkFundingSatoshisLessThanMax msg)
        *^> OpenChannelMsgValidation.checkChannelReserveSatohisLessThanFundingSatoshis msg
        *^> OpenChannelMsgValidation.checkPushMSatLesserThanFundingValue msg
        *^> OpenChannelMsgValidation.checkFundingSatoshisLessThanDustLimitSatoshis msg
        *^> OpenChannelMsgValidation.checkMaxAcceptedHTLCs msg
        *^> OpenChannelMsgValidation.checkFunderCanAffordFee (msg.FeeRatePerKw) msg
        |> Result.mapError((@)["open_channel msg is invalid"] >> InvalidOpenChannelError.Create msg >> InvalidOpenChannel)

    let internal checkOpenChannelMsgAcceptable (feeEstimator: IFeeEstimator) (conf: ChannelConfig) (msg: OpenChannelMsg) =
        let feeRate = feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Background)
        Validation.ofResult(OpenChannelMsgValidation.checkFundingSatoshisLessThanMax msg)
        *^> OpenChannelMsgValidation.checkChannelReserveSatohisLessThanFundingSatoshis msg
        *^> OpenChannelMsgValidation.checkPushMSatLesserThanFundingValue msg
        *^> OpenChannelMsgValidation.checkFundingSatoshisLessThanDustLimitSatoshis msg
        *^> OpenChannelMsgValidation.checkRemoteFee feeEstimator msg.FeeRatePerKw conf.ChannelOptions.MaxFeeRateMismatchRatio
        *^> OpenChannelMsgValidation.checkToSelfDelayIsInAcceptableRange msg
        *^> OpenChannelMsgValidation.checkMaxAcceptedHTLCs msg
        *> OpenChannelMsgValidation.checkConfigPermits conf.PeerChannelConfigLimits msg
        *^> OpenChannelMsgValidation.checkChannelAnnouncementPreferenceAcceptable conf msg
        *> OpenChannelMsgValidation.checkIsAcceptableByCurrentFeeRate feeEstimator msg
        *^> OpenChannelMsgValidation.checkFunderCanAffordFee feeRate msg
        |> Result.mapError((@)["rejected received open_channel msg"] >> InvalidOpenChannelError.Create msg >> InvalidOpenChannel)


    let internal checkAcceptChannelMsgAcceptable (conf: ChannelConfig) (state) (msg: AcceptChannelMsg) =
        Validation.ofResult(AcceptChannelMsgValidation.checkMaxAcceptedHTLCs msg)
        *^> AcceptChannelMsgValidation.checkDustLimit msg
        *^> (AcceptChannelMsgValidation.checkChannelReserveSatoshis state msg)
        *^> AcceptChannelMsgValidation.checkChannelReserveSatoshis state msg
        *^> AcceptChannelMsgValidation.checkDustLimitIsLargerThanOurChannelReserve state msg
        *^> AcceptChannelMsgValidation.checkMinimumHTLCValueIsAcceptable state msg
        *^> AcceptChannelMsgValidation.checkToSelfDelayIsAcceptable msg
        *> AcceptChannelMsgValidation.checkConfigPermits conf.PeerChannelConfigLimits msg
        |> Result.mapError(InvalidAcceptChannelError.Create msg >> InvalidAcceptChannel)

    let checkOurMonoHopUnidirectionalPaymentIsAcceptableWithCurrentSpec (currentSpec) (state: Commitments) (payment: MonoHopUnidirectionalPaymentMsg) =
        Validation.ofResult(MonoHopUnidirectionalPaymentValidationWithContext.checkWeHaveSufficientFunds state currentSpec)
        |> Result.mapError(fun errs -> InvalidMonoHopUnidirectionalPayment { NetworkMsg = payment; Errors = errs })

    let checkTheirMonoHopUnidirectionalPaymentIsAcceptableWithCurrentSpec (currentSpec) (state: Commitments) (payment: MonoHopUnidirectionalPaymentMsg) =
        Validation.ofResult(MonoHopUnidirectionalPaymentValidationWithContext.checkWeHaveSufficientFunds state currentSpec)
        |> Result.mapError(fun errs -> InvalidMonoHopUnidirectionalPayment { NetworkMsg = payment; Errors = errs })

    let checkOperationAddHTLC (state: NormalData) (op: OperationAddHTLC) =
        Validation.ofResult(UpdateAddHTLCValidation.checkExpiryIsNotPast op.CurrentHeight op.Expiry)
        *> UpdateAddHTLCValidation.checkExpiryIsInAcceptableRange op.CurrentHeight op.Expiry
        *^> UpdateAddHTLCValidation.checkAmountIsLargerThanMinimum state.Commitments.RemoteParams.HTLCMinimumMSat op.Amount
        |> Result.mapError(InvalidOperationAddHTLCError.Create op >> InvalidOperationAddHTLC)

    let checkOurUpdateAddHTLCIsAcceptableWithCurrentSpec (currentSpec) (state: Commitments) (add: UpdateAddHTLCMsg) =
        Validation.ofResult(UpdateAddHTLCValidationWithContext.checkLessThanHTLCValueInFlightLimit currentSpec state.RemoteParams.MaxHTLCValueInFlightMSat add)
        *^> UpdateAddHTLCValidationWithContext.checkLessThanMaxAcceptedHTLC currentSpec state.RemoteParams.MaxAcceptedHTLCs
        *^> UpdateAddHTLCValidationWithContext.checkWeHaveSufficientFunds state currentSpec
        |> Result.mapError(InvalidUpdateAddHTLCError.Create add >> InvalidUpdateAddHTLC)

    let checkTheirUpdateAddHTLCIsAcceptable (state: Commitments) (add: UpdateAddHTLCMsg) (currentHeight: BlockHeight) =
        Validation.ofResult(ValidationHelper.check add.HTLCId (<>) state.RemoteNextHTLCId "Received Unexpected HTLCId (%A). Must be (%A)")
            *^> UpdateAddHTLCValidation.checkExpiryIsNotPast currentHeight add.CLTVExpiry
            *> UpdateAddHTLCValidation.checkExpiryIsInAcceptableRange currentHeight add.CLTVExpiry
            *^> UpdateAddHTLCValidation.checkAmountIsLargerThanMinimum state.LocalParams.HTLCMinimumMSat add.Amount
            |> Result.mapError(InvalidUpdateAddHTLCError.Create add >> InvalidUpdateAddHTLC)

    let checkTheirUpdateAddHTLCIsAcceptableWithCurrentSpec (currentSpec) (state: Commitments) (add: UpdateAddHTLCMsg) =
        Validation.ofResult(UpdateAddHTLCValidationWithContext.checkLessThanHTLCValueInFlightLimit currentSpec state.LocalParams.MaxHTLCValueInFlightMSat add)
        *^> UpdateAddHTLCValidationWithContext.checkLessThanMaxAcceptedHTLC currentSpec state.LocalParams.MaxAcceptedHTLCs
        *^> UpdateAddHTLCValidationWithContext.checkWeHaveSufficientFunds state currentSpec
        |> Result.mapError(InvalidUpdateAddHTLCError.Create add >> InvalidUpdateAddHTLC)
