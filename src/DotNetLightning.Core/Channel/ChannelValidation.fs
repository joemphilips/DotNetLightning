namespace DotNetLightning.Channel

open ResultUtils
open NBitcoin

open DotNetLightning.Chain
open DotNetLightning.Crypto
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Transactions

exception ChannelException of ChannelError
module internal ChannelHelpers =

    let getFundingRedeemScript (ck: ChannelPubKeys) (theirFundingPubKey: PubKey): Script =
        let ourFundingKey = ck.FundingPubKey
        let pks = if ourFundingKey.ToBytes() < theirFundingPubKey.ToBytes() then
                      [| ourFundingKey; theirFundingPubKey |]
                  else
                      [| theirFundingPubKey; ourFundingKey |]
        PayToMultiSigTemplate.Instance.GenerateScriptPubKey(2, pks)

    let getFundingScriptCoin (ck: ChannelPubKeys) (theirFundingPubKey: PubKey) (TxId fundingTxId) (TxOutIndex fundingOutputIndex) (fundingSatoshis): ScriptCoin =
        let redeem = getFundingRedeemScript ck theirFundingPubKey
        Coin(fundingTxId, uint32 fundingOutputIndex, fundingSatoshis, redeem.WitHash.ScriptPubKey)
        |> fun c -> ScriptCoin(c, redeem)

    let private makeFlags (isNode1: bool, enable: bool) =
        (if isNode1 then 1uy else 0uy) ||| ((if enable then 1uy else 0uy) <<< 1)

    let internal makeChannelUpdate (chainHash, nodeSecret: Key, remoteNodeId: NodeId, shortChannelId, cltvExpiryDelta,
                                    htlcMinimum, feeBase, feeProportionalMillionths, enabled: bool, timestamp) =
        let timestamp = defaultArg timestamp ((System.DateTime.UtcNow.ToUnixTimestamp()) |> uint32)
        let isNodeOne = NodeId (nodeSecret.PubKey) < remoteNodeId
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
        let signature = unsignedChannelUpdate.ToBytes() |> Crypto.Hashes.SHA256 |> uint256 |> nodeSecret.Sign |> LNECDSASignature
        {
            ChannelUpdate.Contents = unsignedChannelUpdate
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
                          (localPerCommitmentPoint: PubKey)
                          (remotePerCommitmentPoint: PubKey)
                          (secpContext: ISecp256k1)
                          (n: Network): Result<CommitmentSpec * CommitTx * CommitmentSpec * CommitTx, ChannelError> =
        let toLocal = if (localParams.IsFunder) then fundingSatoshis.ToLNMoney() - pushMSat else pushMSat
        let toRemote = if (localParams.IsFunder) then pushMSat else fundingSatoshis.ToLNMoney() - pushMSat
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
            let scriptCoin = getFundingScriptCoin localParams.ChannelPubKeys
                                                  remoteParams.FundingPubKey
                                                  fundingTxId
                                                  fundingOutputIndex
                                                  fundingSatoshis
            let revPubKeyForLocal = Generators.revocationPubKey secpContext remoteParams.RevocationBasePoint localPerCommitmentPoint
            let delayedPubKeyForLocal = Generators.derivePubKey secpContext localParams.ChannelPubKeys.DelayedPaymentBasePubKey localPerCommitmentPoint
            let paymentPubKeyForLocal = Generators.derivePubKey secpContext remoteParams.PaymentBasePoint localPerCommitmentPoint
            let localCommitTx =
                Transactions.makeCommitTx scriptCoin
                                          0UL
                                          localParams.ChannelPubKeys.PaymentBasePubKey
                                          remoteParams.PaymentBasePoint
                                          localParams.IsFunder
                                          localParams.DustLimitSatoshis
                                          revPubKeyForLocal
                                          remoteParams.ToSelfDelay
                                          delayedPubKeyForLocal
                                          paymentPubKeyForLocal
                                          (localParams.ChannelPubKeys.HTLCBasePubKey)
                                          (remoteParams.HTLCBasePoint)
                                          localSpec
                                          n
            let revPubKeyForRemote = Generators.revocationPubKey secpContext localParams.ChannelPubKeys.RevocationBasePubKey remotePerCommitmentPoint
            let delayedPubKeyForRemote = Generators.derivePubKey secpContext remoteParams.DelayedPaymentBasePoint remotePerCommitmentPoint
            let paymentPubKeyForRemote = Generators.derivePubKey secpContext localParams.ChannelPubKeys.PaymentBasePubKey remotePerCommitmentPoint
            let remoteCommitTx =
                Transactions.makeCommitTx scriptCoin
                                          0UL
                                          remoteParams.PaymentBasePoint
                                          localParams.ChannelPubKeys.PaymentBasePubKey
                                          (not localParams.IsFunder)
                                          (remoteParams.DustLimitSatoshis)
                                          revPubKeyForRemote
                                          localParams.ToSelfDelay
                                          delayedPubKeyForRemote
                                          paymentPubKeyForRemote
                                          (remoteParams.HTLCBasePoint)
                                          (localParams.ChannelPubKeys.HTLCBasePubKey)
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
    let checkOurOpenChannelMsgAcceptable (conf: ChannelConfig) (msg: OpenChannel) =
        Validation.ofResult(OpenChannelMsgValidation.checkFundingSatoshisLessThanMax msg)
        *^> OpenChannelMsgValidation.checkChannelReserveSatohisLessThanFundingSatoshis msg
        *^> OpenChannelMsgValidation.checkPushMSatLesserThanFundingValue msg
        *^> OpenChannelMsgValidation.checkFundingSatoshisLessThanDustLimitSatoshis msg
        *^> OpenChannelMsgValidation.checkMaxAcceptedHTLCs msg
        *^> OpenChannelMsgValidation.checkFunderCanAffordFee (msg.FeeRatePerKw) msg
        |> Result.mapError((@)["our open_channel msg is invalid"] >> InvalidOpenChannelError.Create msg >> InvalidOpenChannel)

    let internal checkOpenChannelMsgAcceptable (feeEstimator: IFeeEstimator) (conf: ChannelConfig) (msg: OpenChannel) =
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
        |> Result.mapError((@)["their open_channel msg is invalid"] >> InvalidOpenChannelError.Create msg >> InvalidOpenChannel)


    let internal checkAcceptChannelMsgAcceptable (conf: ChannelConfig) (state) (msg: AcceptChannel) =
        Validation.ofResult(AcceptChannelMsgValidation.checkMaxAcceptedHTLCs msg)
        *^> AcceptChannelMsgValidation.checkDustLimit msg
        *^> (AcceptChannelMsgValidation.checkChannelReserveSatoshis state msg)
        *^> AcceptChannelMsgValidation.checkChannelReserveSatoshis state msg
        *^> AcceptChannelMsgValidation.checkDustLimitIsLargerThanOurChannelReserve state msg
        *^> AcceptChannelMsgValidation.checkMinimumHTLCValueIsAcceptable state msg
        *^> AcceptChannelMsgValidation.checkToSelfDelayIsAcceptable msg
        *> AcceptChannelMsgValidation.checkConfigPermits conf.PeerChannelConfigLimits msg
        |> Result.mapError(InvalidAcceptChannelError.Create msg >> InvalidAcceptChannel)


    let checkOperationAddHTLC (state: NormalData) (op: OperationAddHTLC) =
        Validation.ofResult(UpdateAddHTLCValidation.checkExpiryIsNotPast op.CurrentHeight op.Expiry)
        *> UpdateAddHTLCValidation.checkExpiryIsInAcceptableRange op.CurrentHeight op.Expiry
        *^> UpdateAddHTLCValidation.checkAmountIsLargerThanMinimum state.Commitments.RemoteParams.HTLCMinimumMSat op.Amount
        |> Result.mapError(InvalidOperationAddHTLCError.Create op >> InvalidOperationAddHTLC)

    let checkOurUpdateAddHTLCIsAcceptableWithCurrentSpec (currentSpec) (state: Commitments) (add: UpdateAddHTLC) =
        Validation.ofResult(UpdateAddHTLCValidationWithContext.checkLessThanHTLCValueInFlightLimit currentSpec state.RemoteParams.MaxHTLCValueInFlightMSat add)
        *^> UpdateAddHTLCValidationWithContext.checkLessThanMaxAcceptedHTLC currentSpec state.RemoteParams.MaxAcceptedHTLCs
        *^> UpdateAddHTLCValidationWithContext.checkWeHaveSufficientFunds state currentSpec
        |> Result.mapError(InvalidUpdateAddHTLCError.Create add >> InvalidUpdateAddHTLC)

    let checkTheirUpdateAddHTLCIsAcceptable (state: Commitments) (add: UpdateAddHTLC) (currentHeight: BlockHeight) =
        Validation.ofResult(ValidationHelper.check add.HTLCId (<>) state.RemoteNextHTLCId "Received Unexpected HTLCId (%A). Must be (%A)")
            *^> UpdateAddHTLCValidation.checkExpiryIsNotPast currentHeight add.CLTVExpiry
            *> UpdateAddHTLCValidation.checkExpiryIsInAcceptableRange currentHeight add.CLTVExpiry
            *^> UpdateAddHTLCValidation.checkAmountIsLargerThanMinimum state.LocalParams.HTLCMinimumMSat add.Amount
            |> Result.mapError(InvalidUpdateAddHTLCError.Create add >> InvalidUpdateAddHTLC)

    let checkTheirUpdateAddHTLCIsAcceptableWithCurrentSpec (currentSpec) (state: Commitments) (add: UpdateAddHTLC) =
        Validation.ofResult(UpdateAddHTLCValidationWithContext.checkLessThanHTLCValueInFlightLimit currentSpec state.LocalParams.MaxHTLCValueInFlightMSat add)
        *^> UpdateAddHTLCValidationWithContext.checkLessThanMaxAcceptedHTLC currentSpec state.LocalParams.MaxAcceptedHTLCs
        *^> UpdateAddHTLCValidationWithContext.checkWeHaveSufficientFunds state currentSpec
        |> Result.mapError(InvalidUpdateAddHTLCError.Create add >> InvalidUpdateAddHTLC)
