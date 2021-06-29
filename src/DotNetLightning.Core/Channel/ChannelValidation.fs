namespace DotNetLightning.Channel

open NBitcoin

open DotNetLightning.Chain
open DotNetLightning.Crypto
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Serialization.Msgs
open DotNetLightning.Transactions

open ResultUtils
open ResultUtils.Portability

exception ChannelException of ChannelError
module internal ChannelHelpers =

    let getFundingScriptCoin (ourFundingPubKey: FundingPubKey)
                             (theirFundingPubKey: FundingPubKey)
                             (TxId fundingTxId)
                             (TxOutIndex fundingOutputIndex)
                             (fundingAmount: Money)
                                 : ScriptCoin =
        let redeem = Scripts.funding ourFundingPubKey theirFundingPubKey
        Coin(fundingTxId, uint32 fundingOutputIndex, fundingAmount, redeem.WitHash.ScriptPubKey)
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
    let internal getOurFeeBase (feeEstimator: IFeeEstimator) (FeeRatePerKw feeRatePerKw) (isFunder: bool): LNMoney =
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

    let makeFirstCommitTxs (localIsFunder: bool)
                           (localChannelPubKeys: ChannelPubKeys)
                           (remoteChannelPubKeys: ChannelPubKeys)
                           (localParams: LocalParams)
                           (remoteParams: RemoteParams)
                           (fundingAmount: Money)
                           (pushAmount: LNMoney)
                           (initialFeeRatePerKw: FeeRatePerKw)
                           (fundingOutputIndex: TxOutIndex)
                           (fundingTxId: TxId)
                           (localPerCommitmentPoint: PerCommitmentPoint)
                           (remotePerCommitmentPoint: PerCommitmentPoint)
                           (network: Network)
                               : Result<CommitmentSpec * CommitTx * CommitmentSpec * CommitTx, ChannelError> =
        let toLocal =
            if localIsFunder then
                fundingAmount.ToLNMoney() - pushAmount
            else
                pushAmount
        let toRemote =
            if localIsFunder then
                pushAmount
            else
                fundingAmount.ToLNMoney() - pushAmount
        let localChannelKeys = localChannelPubKeys
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
                                                  remoteChannelPubKeys.FundingPubKey
                                                  fundingTxId
                                                  fundingOutputIndex
                                                  fundingAmount
            let localPubKeysForLocalCommitment = localPerCommitmentPoint.DeriveCommitmentPubKeys localChannelKeys
            let remotePubKeysForLocalCommitment = localPerCommitmentPoint.DeriveCommitmentPubKeys remoteChannelPubKeys

            let localCommitTx =
                Transactions.makeCommitTx scriptCoin
                                          CommitmentNumber.FirstCommitment
                                          localChannelKeys.PaymentBasepoint
                                          remoteChannelPubKeys.PaymentBasepoint
                                          localIsFunder
                                          localParams.DustLimitSatoshis
                                          remotePubKeysForLocalCommitment.RevocationPubKey
                                          remoteParams.ToSelfDelay
                                          localPubKeysForLocalCommitment.DelayedPaymentPubKey
                                          remotePubKeysForLocalCommitment.PaymentPubKey
                                          localPubKeysForLocalCommitment.HtlcPubKey
                                          remotePubKeysForLocalCommitment.HtlcPubKey
                                          localSpec
                                          network

            let localPubKeysForRemoteCommitment = remotePerCommitmentPoint.DeriveCommitmentPubKeys localChannelKeys
            let remotePubKeysForRemoteCommitment = remotePerCommitmentPoint.DeriveCommitmentPubKeys remoteChannelPubKeys

            let remoteCommitTx =
                Transactions.makeCommitTx scriptCoin
                                          CommitmentNumber.FirstCommitment
                                          remoteChannelPubKeys.PaymentBasepoint
                                          localChannelKeys.PaymentBasepoint
                                          (not localIsFunder)
                                          (remoteParams.DustLimitSatoshis)
                                          localPubKeysForRemoteCommitment.RevocationPubKey
                                          localParams.ToSelfDelay
                                          remotePubKeysForRemoteCommitment.DelayedPaymentPubKey
                                          localPubKeysForRemoteCommitment.PaymentPubKey
                                          remotePubKeysForRemoteCommitment.HtlcPubKey
                                          localPubKeysForRemoteCommitment.HtlcPubKey
                                          remoteSpec
                                          network

            (localSpec, localCommitTx, remoteSpec, remoteCommitTx) |> Ok

        if (not localIsFunder) then
            result {
                do! checkTheyCanAffordFee()
                return! (makeFirstCommitTxCore())
            }
        else
            makeFirstCommitTxCore()


module internal Validation =

    open DotNetLightning.Channel
    let checkOurOpenChannelMsgAcceptable (msg: OpenChannelMsg) (localParams: LocalParams) =
        Validation.ofResult(OpenChannelMsgValidation.checkFundingSatoshisLessThanMax msg localParams true)
        *^> OpenChannelMsgValidation.checkChannelReserveSatohisLessThanFundingSatoshis msg
        *^> OpenChannelMsgValidation.checkPushMSatLesserThanFundingValue msg
        *^> OpenChannelMsgValidation.checkFundingSatoshisLessThanDustLimitSatoshis msg
        *^> OpenChannelMsgValidation.checkMaxAcceptedHTLCs msg
        *^> OpenChannelMsgValidation.checkFunderCanAffordFee (msg.FeeRatePerKw) msg
        |> Result.mapError((@)["open_channel msg is invalid"] >> InvalidOpenChannelError.Create msg >> InvalidOpenChannel)

    let internal checkOpenChannelMsgAcceptable (channelHandshakeLimits: ChannelHandshakeLimits)
                                               (channelOptions: ChannelOptions)
                                               (announceChannel: bool)
                                               (msg: OpenChannelMsg)
                                               (localParams: LocalParams) =
        let feeRate = channelOptions.FeeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Background)
        Validation.ofResult(OpenChannelMsgValidation.checkFundingSatoshisLessThanMax msg localParams false)
        *^> OpenChannelMsgValidation.checkChannelReserveSatohisLessThanFundingSatoshis msg
        *^> OpenChannelMsgValidation.checkPushMSatLesserThanFundingValue msg
        *^> OpenChannelMsgValidation.checkFundingSatoshisLessThanDustLimitSatoshis msg
        *^> OpenChannelMsgValidation.checkRemoteFee channelOptions.FeeEstimator msg.FeeRatePerKw channelOptions.MaxFeeRateMismatchRatio
        *^> OpenChannelMsgValidation.checkToSelfDelayIsInAcceptableRange msg
        *^> OpenChannelMsgValidation.checkMaxAcceptedHTLCs msg
        *> OpenChannelMsgValidation.checkConfigPermits channelHandshakeLimits msg
        *^> OpenChannelMsgValidation.checkChannelAnnouncementPreferenceAcceptable channelHandshakeLimits announceChannel msg
        *> OpenChannelMsgValidation.checkIsAcceptableByCurrentFeeRate channelOptions.FeeEstimator msg
        *^> OpenChannelMsgValidation.checkFunderCanAffordFee feeRate msg
        |> Result.mapError((@)["rejected received open_channel msg"] >> InvalidOpenChannelError.Create msg >> InvalidOpenChannel)


    let internal checkAcceptChannelMsgAcceptable (channelHandshakeLimits: ChannelHandshakeLimits)
                                                 (fundingAmount: Money)
                                                 (channelReserveAmount: Money)
                                                 (dustLimit: Money)
                                                 (acceptChannelMsg: AcceptChannelMsg) =
        Validation.ofResult(AcceptChannelMsgValidation.checkMaxAcceptedHTLCs acceptChannelMsg)
        *^> AcceptChannelMsgValidation.checkDustLimit acceptChannelMsg
        *^> AcceptChannelMsgValidation.checkChannelReserveSatoshis fundingAmount channelReserveAmount dustLimit acceptChannelMsg
        *^> AcceptChannelMsgValidation.checkDustLimitIsLargerThanOurChannelReserve channelReserveAmount acceptChannelMsg
        *^> AcceptChannelMsgValidation.checkMinimumHTLCValueIsAcceptable fundingAmount acceptChannelMsg
        *^> AcceptChannelMsgValidation.checkToSelfDelayIsAcceptable acceptChannelMsg
        *> AcceptChannelMsgValidation.checkConfigPermits channelHandshakeLimits acceptChannelMsg
        |> Result.mapError(InvalidAcceptChannelError.Create acceptChannelMsg >> InvalidAcceptChannel)


    let checkOperationAddHTLC (remoteParams: RemoteParams) (op: OperationAddHTLC) =
        Validation.ofResult(UpdateAddHTLCValidation.checkExpiryIsNotPast op.CurrentHeight op.Expiry)
        *> UpdateAddHTLCValidation.checkExpiryIsInAcceptableRange op.CurrentHeight op.Expiry
        *^> UpdateAddHTLCValidation.checkAmountIsLargerThanMinimum remoteParams.HTLCMinimumMSat op.Amount
        |> Result.mapError(InvalidOperationAddHTLCError.Create op >> InvalidOperationAddHTLC)

    let checkOurUpdateAddHTLCIsAcceptableWithCurrentSpec (currentSpec)
                                                         (staticChannelConfig: StaticChannelConfig)
                                                         (add: UpdateAddHTLCMsg) =
        Validation.ofResult(
            UpdateAddHTLCValidationWithContext.checkLessThanHTLCValueInFlightLimit
                currentSpec
                staticChannelConfig.RemoteParams.MaxHTLCValueInFlightMSat
                add
        )
        *^> UpdateAddHTLCValidationWithContext.checkLessThanMaxAcceptedHTLC currentSpec staticChannelConfig.RemoteParams.MaxAcceptedHTLCs
        *^> UpdateAddHTLCValidationWithContext.checkWeHaveSufficientFunds staticChannelConfig currentSpec
        |> Result.mapError(InvalidUpdateAddHTLCError.Create add >> InvalidUpdateAddHTLC)

    let checkTheirUpdateAddHTLCIsAcceptable (state: Commitments)
                                            (localParams: LocalParams)
                                            (add: UpdateAddHTLCMsg)
                                            (currentHeight: BlockHeight) =
        Validation.ofResult(ValidationHelper.check add.HTLCId (<>) state.RemoteNextHTLCId "Received Unexpected HTLCId (%A). Must be (%A)")
            *^> UpdateAddHTLCValidation.checkExpiryIsNotPast currentHeight add.CLTVExpiry
            *> UpdateAddHTLCValidation.checkExpiryIsInAcceptableRange currentHeight add.CLTVExpiry
            *^> UpdateAddHTLCValidation.checkAmountIsLargerThanMinimum localParams.HTLCMinimumMSat add.Amount
            |> Result.mapError(InvalidUpdateAddHTLCError.Create add >> InvalidUpdateAddHTLC)

    let checkTheirUpdateAddHTLCIsAcceptableWithCurrentSpec (currentSpec)
                                                           (staticChannelConfig: StaticChannelConfig)
                                                           (add: UpdateAddHTLCMsg) =
        Validation.ofResult(UpdateAddHTLCValidationWithContext.checkLessThanHTLCValueInFlightLimit currentSpec staticChannelConfig.LocalParams.MaxHTLCValueInFlightMSat add)
        *^> UpdateAddHTLCValidationWithContext.checkLessThanMaxAcceptedHTLC currentSpec staticChannelConfig.LocalParams.MaxAcceptedHTLCs
        *^> UpdateAddHTLCValidationWithContext.checkWeHaveSufficientFunds staticChannelConfig currentSpec
        |> Result.mapError(InvalidUpdateAddHTLCError.Create add >> InvalidUpdateAddHTLC)

    let checkShutdownScriptPubKeyAcceptable (staticShutdownScriptPubKey: Option<ShutdownScriptPubKey>)
                                            (requestedShutdownScriptPubKey: ShutdownScriptPubKey)
                                                : Result<unit, ChannelError> =
        match staticShutdownScriptPubKey with
        | Some scriptPubKey when scriptPubKey <> requestedShutdownScriptPubKey ->
            Error <|
                cannotCloseChannel
                    "requested shutdown script does not match shutdown \
                    script in open/accept channel"
        | _ -> Ok ()
