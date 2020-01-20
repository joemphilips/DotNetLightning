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
    let deriveOurDustLimitSatoshis (feeEstimator: IFeeEstimator): Money =
        let (FeeRatePerKw atOpenBackGroundFee) = feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Background)
        (Money.Satoshis((uint64 atOpenBackGroundFee) * B_OUTPUT_PLUS_SPENDING_INPUT_WEIGHT / 1000UL), Money.Satoshis(546UL))
        |> Money.Max

    let getOurChannelReserve (channelValue: Money) =
        let q = channelValue / 100L
        Money.Min(channelValue, Money.Max(q, Money.Satoshis(1L)))

    let getFundingRedeemScript (ck: ChannelPubKeys) (theirFundingPubKey: PubKey): Script =
        let ourFundingKey = ck.FundingPubKey
        let pks = if ourFundingKey.ToBytes() < theirFundingPubKey.ToBytes() then
                      [| ourFundingKey; theirFundingPubKey |]
                  else
                      [| theirFundingPubKey; ourFundingKey |]
        PayToMultiSigTemplate.Instance.GenerateScriptPubKey(2, pks)

    let getFundingSCoin (ck: ChannelPubKeys) (theirFundingPubKey: PubKey) (TxId fundingTxId) (TxOutIndex fundingOutputIndex) (fundingSatoshis): ScriptCoin =
        let redeem = getFundingRedeemScript ck theirFundingPubKey
        Coin(fundingTxId, uint32 fundingOutputIndex, fundingSatoshis, redeem.WitHash.ScriptPubKey)
        |> fun c -> ScriptCoin(c, redeem)

    let private makeFlags (isNode1: bool, enable: bool) =
        (if isNode1 then 1us else 0us) ||| ((if enable then 1us else 0us) <<< 1)

    let internal makeChannelUpdate (chainHash, nodeSecret: Key, remoteNodeId: NodeId, shortChannelId, cltvExpiryDelta,
                                    htlcMinimum, feeBase, feeProportionalMillionths, enabled: bool, timestamp) =
        let timestamp = defaultArg timestamp ((System.DateTime.UtcNow.ToUnixTimestamp()) |> uint32)
        let isNodeOne = NodeId (nodeSecret.PubKey) < remoteNodeId
        let unsignedChannelUpdate = {
            ChainHash = chainHash
            ShortChannelId = shortChannelId
            Timestamp = timestamp
            Flags = makeFlags (isNodeOne, enabled)
            CLTVExpiryDelta = cltvExpiryDelta
            HTLCMinimumMSat = htlcMinimum
            FeeBaseMSat = feeBase
            FeeProportionalMillionths = feeProportionalMillionths
            ExcessData = [||]
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
            let sCoin = getFundingSCoin (localParams.ChannelPubKeys) (remoteParams.FundingPubKey) (fundingTxId) (fundingOutputIndex) (fundingSatoshis)
            let revPubKeyForLocal = Generators.revocationPubKey secpContext remoteParams.RevocationBasePoint localPerCommitmentPoint
            let delayedPubKeyForLocal = Generators.derivePubKey secpContext localParams.ChannelPubKeys.DelayedPaymentBasePubKey localPerCommitmentPoint
            let paymentPubKeyForLocal = Generators.derivePubKey secpContext remoteParams.PaymentBasePoint localPerCommitmentPoint
            let localCommitTx =
                Transactions.makeCommitTx sCoin
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
                Transactions.makeCommitTx sCoin
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
            checkTheyCanAffordFee() *> (makeFirstCommitTxCore())
        else
            makeFirstCommitTxCore()


module internal Validation =

    let internal RRApiE(e: APIError) =
        RResult.rbad (RBad.Object(e))

    let internal RRApiMisuse(msg: string) =
        msg |> APIMisuseError |> RRApiE

    /// Represents the error that something user can not control (e.g. peer has sent invalid msg).
    let internal RRChannelE(ex: ChannelError) =
        RResult.rexn (ChannelException(ex))

    let internal RRClose(msg: string) =
        RRChannelE(ChannelError.Close(msg))

    let internal RRIgnore(msg: string) =
        RRChannelE(ChannelError.Ignore(msg))
        
    let internal checkOrClose left predicate right msg =
        if predicate left right then
            sprintf msg left right |> RRClose
        else
            Good()

    let internal checkOrIgnore left predicate right msg =
        if predicate left right then
            sprintf msg left right |> RRIgnore
        else
            Good()

    let checkMaxAcceptedHTLCsInMeaningfulRange (maxAcceptedHTLCs: uint16) =
        let check1 =
            checkOrClose
                maxAcceptedHTLCs (<) 1us
                "max_accepted_htlcs was %A must be larger than %A"
        let check2 =
            checkOrClose
                maxAcceptedHTLCs (>) 483us
                "max_accepted_htlcs was (%A). But it must be less than %A"
        check1 *> check2

    module private OpenChannelRequest =
        let checkFundingSatoshisLessThanMax (msg: OpenChannel) =
            if (msg.FundingSatoshis >= ChannelConstants.MAX_FUNDING_SATOSHIS) then
                RRClose("Funding value > 2^24")
            else
                Good()

        let checkChannelReserveSatohisLessThanFundingSatoshis (msg: OpenChannel) =
            if (msg.ChannelReserveSatoshis > msg.FundingSatoshis) then
                RRClose("Bogus channel_reserve_satoshis. Must be bigger than funding_satoshis")
            else
                Good()

        let checkPushMSatLesserThanFundingValue msg =
            if (msg.PushMSat.ToMoney() > (msg.FundingSatoshis - msg.ChannelReserveSatoshis)) then
                RRClose("push_msat larger than funding value")
            else
                Good()

        let checkFundingSatoshisLessThanDustLimitSatoshis (msg: OpenChannel) =
            if (msg.DustLimitSatoshis > msg.FundingSatoshis) then
                RRClose (sprintf "Peer never wants payout outputs? dust_limit_satoshis: %A; funding_satoshi %A" msg.DustLimitSatoshis msg.FundingSatoshis)
            else
                Good()

        let checkRemoteFee (feeEstimator: IFeeEstimator) (feeRate: FeeRatePerKw) =
            let check1 =
                checkOrClose
                    feeRate (<) (feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Background))
                    "Peer's feerate much too low. it was %A but it must be higher than %A"
            let check2 =
                checkOrClose
                    feeRate (>) (feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority) * 2u)
                    "Peer's feerate much too high. it was %A but it must be lower than %A"
            check1 *> check2

        let checkToSelfDelayIsInAcceptableRange (msg: OpenChannel) =
            checkOrClose
                msg.ToSelfDelay (>) (MAX_LOCAL_BREAKDOWN_TIMEOUT)
                "They wanted our payments to be delayed by a needlessly long period %A . But our max %A"

        let checkConfigPermits (config: ChannelHandshakeLimits) (msg: OpenChannel) =

            let check1 =
                checkOrClose
                    msg.FundingSatoshis (<) config.MinFundingSatoshis
                    "funding satoshis is less than the user specified limit. received: %A; limit: %A"
            let check2 =
                checkOrClose
                    (msg.HTLCMinimumMsat.ToMoney()) (>) (config.MinFundingSatoshis)
                    "htlc minimum msat is higher than the users specified limit. received %A; limit: %A"
            let check3 =
                checkOrClose
                    msg.MaxHTLCValueInFlightMsat (<) config.MinMaxHTLCValueInFlightMSat
                    "max htlc value in light msat is less than the user specified limit. received: %A; limit %A"
            let check4 =
                checkOrClose
                    msg.ChannelReserveSatoshis (>) config.MaxChannelReserveSatoshis
                    "channel reserve satoshis is higher than the user specified limit. received %A; limit: %A"
            let check5 =
                checkOrClose
                    msg.MaxAcceptedHTLCs (<) config.MinMaxAcceptedHTLCs
                     "max accepted htlcs is less than the user specified limit. received: %A; limit: %A"
            let check6 =
                checkOrClose
                    msg.DustLimitSatoshis (<) config.MinDustLimitSatoshis
                    "dust_limit_satoshis is less than the user specified limit. received: %A; limit: %A"
            let check7 =
                checkOrClose
                    msg.DustLimitSatoshis (>) config.MaxDustLimitSatoshis
                    "dust_limit_satoshis is greater than the user specified limit. received: %A; limit: %A"
            check1 *> check2 *> check3 *> check4 *> check5 *> check6 *> check7

        let checkChannelAnnouncementPreferenceAcceptable (config: ChannelConfig) (msg) =
            let theirAnnounce = (msg.ChannelFlags &&& 1uy) = 1uy
            if (config.PeerChannelConfigLimits.ForceChannelAnnouncementPreference) && config.ChannelOptions.AnnounceChannel <> theirAnnounce then
                RRClose("Peer tried to open channel but their announcement preference is different from ours")
            else
                Good()

        let checkIsAcceptableByCurrentFeeRate (feeEstimator: IFeeEstimator) msg =
            let ourDustLimit = ChannelHelpers.deriveOurDustLimitSatoshis feeEstimator
            let ourChannelReserve = ChannelHelpers.getOurChannelReserve (msg.FundingSatoshis)
            let check1 =
                checkOrClose
                    ourChannelReserve (<) ourDustLimit
                    "Suitable channel reserve not found. Aborting. (our channel reserve was (%A). and our dust limit was(%A))"
            let check2 =
                checkOrClose
                    msg.ChannelReserveSatoshis (<) ourDustLimit
                    "channel_reserve_satoshis too small. It was: %A; dust_limit: %A"
            let check3 =
                checkOrClose
                    ourChannelReserve (<) msg.DustLimitSatoshis
                    "Dust limit too high for our channel reserve. our channel reserve: %A received dust limit: %A"
            check1 *> check2 *> check3

        let checkIfFundersAmountSufficient (feeEst: IFeeEstimator) msg =
            let fundersAmount = LNMoney.Satoshis(msg.FundingSatoshis.Satoshi) - msg.PushMSat
            let (backgroundFeeRate) = feeEst.GetEstSatPer1000Weight(ConfirmationTarget.Background)
            let backgroundFee = backgroundFeeRate.ToFee COMMITMENT_TX_BASE_WEIGHT
            if (fundersAmount.ToMoney() < backgroundFee) then
                RRClose (sprintf "Insufficient funding amount for initial commitment. BackgroundFee %A. funders amount %A" backgroundFee fundersAmount)
            else
                let ourChannelReserve = ChannelHelpers.getOurChannelReserve msg.FundingSatoshis
                let toLocalMSat = msg.PushMSat
                let toRemoteMSat = fundersAmount - backgroundFeeRate.ToFee(COMMITMENT_TX_BASE_WEIGHT).ToLNMoney()
                if (toLocalMSat <= (msg.ChannelReserveSatoshis.ToLNMoney()) && toRemoteMSat <= ourChannelReserve.ToLNMoney()) then
                    RRClose("Insufficient funding amount for initial commitment. ")
                else
                    Good()


    let internal checkOpenChannelMsgAcceptable (feeEstimator: IFeeEstimator) (conf: ChannelConfig) (msg: OpenChannel) =
        OpenChannelRequest.checkFundingSatoshisLessThanMax msg
        *> OpenChannelRequest.checkChannelReserveSatohisLessThanFundingSatoshis msg
        *> OpenChannelRequest.checkPushMSatLesserThanFundingValue msg
        *> OpenChannelRequest.checkFundingSatoshisLessThanDustLimitSatoshis msg
        *> OpenChannelRequest.checkRemoteFee feeEstimator msg.FeeRatePerKw
        *> OpenChannelRequest.checkToSelfDelayIsInAcceptableRange msg
        *> checkMaxAcceptedHTLCsInMeaningfulRange msg.MaxAcceptedHTLCs
        *> OpenChannelRequest.checkConfigPermits conf.PeerChannelConfigLimits msg
        *> OpenChannelRequest.checkChannelAnnouncementPreferenceAcceptable conf msg
        *> OpenChannelRequest.checkIsAcceptableByCurrentFeeRate feeEstimator msg

    module private AcceptChannelValidator =
        let checkDustLimit msg =
            if msg.DustLimitSatoshis > Money.Satoshis(21000000L * 100000L) then
                RRClose (sprintf "Peer never wants payout outputs? dust_limit_satoshis was: %A" msg.DustLimitSatoshis)
            else
                Good()

        let checkChannelReserveSatoshis (state: Data.WaitForAcceptChannelData) msg =
            if msg.ChannelReserveSatoshis > state.LastSent.FundingSatoshis then
                sprintf "bogus channel_reserve_satoshis %A . Must be larger than funding_satoshis %A" (msg.ChannelReserveSatoshis) (state.InputInitFunder.FundingSatoshis)
                |> RRClose
            else if msg.DustLimitSatoshis > state.LastSent.ChannelReserveSatoshis then
                sprintf "Bogus channel_reserve and dust_limit. dust_limit: %A; channel_reserve %A" msg.DustLimitSatoshis (state.LastSent.ChannelReserveSatoshis)
                |> RRClose
            else if msg.ChannelReserveSatoshis < state.LastSent.DustLimitSatoshis then
                sprintf "Peer never wants payout outputs? channel_reserve_satoshis are %A; dust_limit_satoshis in our last sent msg is %A" msg.ChannelReserveSatoshis (state.LastSent.DustLimitSatoshis)
                |> RRClose
            else
                Good()

        let checkDustLimitIsLargerThanOurChannelReserve (state: Data.WaitForAcceptChannelData) msg =
            let reserve = ChannelHelpers.getOurChannelReserve state.LastSent.FundingSatoshis
            checkOrClose
                msg.DustLimitSatoshis (>) reserve
                "dust limit (%A) is bigger than our channel reserve (%A)" 

        let checkMinimumHTLCValueIsAcceptable (state: Data.WaitForAcceptChannelData) (msg: AcceptChannel) =
            if (msg.HTLCMinimumMSat.ToMoney() >= (state.LastSent.FundingSatoshis - msg.ChannelReserveSatoshis)) then
                sprintf "Minimum HTLC value is greater than full channel value HTLCMinimum %A satoshi; funding_satoshis %A; channel_reserve: %A" (msg.HTLCMinimumMSat.ToMoney()) (state.LastSent.FundingSatoshis) (msg.ChannelReserveSatoshis)
                |> RRClose
            else
                Good()

        let checkToSelfDelayIsAcceptable (msg) =
            if (msg.ToSelfDelay > MAX_LOCAL_BREAKDOWN_TIMEOUT) then
                sprintf "They wanted our payments to be delayed by a needlessly long period (%A)" msg.ToSelfDelay
                |> RRClose
            else
                Good()

        let checkConfigPermits (config: ChannelHandshakeLimits) (msg: AcceptChannel) =
            let check1 = checkOrClose msg.HTLCMinimumMSat (>) config.MaxHTLCMinimumMSat "HTLC Minimum msat in accept_channel (%A) is higher than the user specified limit (%A)"
            let check2 = checkOrClose msg.MaxHTLCValueInFlightMsat (<) config.MinMaxHTLCValueInFlightMSat "max htlc value in flight msat (%A) is less than the user specified limit (%A)"
            let check3 = checkOrClose msg.ChannelReserveSatoshis (>) config.MaxChannelReserveSatoshis "max reserve_satoshis (%A) is higher than the user specified limit (%A)"
            let check4 = checkOrClose msg.MaxAcceptedHTLCs (<) config.MinMaxAcceptedHTLCs "max accepted htlcs (%A) is less than the user specified limit (%A)"
            let check5 = checkOrClose msg.DustLimitSatoshis (<) config.MinDustLimitSatoshis "dust limit satoshis (%A) is less then the user specified limit (%A)"
            let check6 = checkOrClose msg.DustLimitSatoshis (>) config.MaxDustLimitSatoshis "dust limit satoshis (%A) is greater then the user specified limit (%A)"
            let check7 = checkOrClose (msg.MinimumDepth.Value) (>) (config.MaxMinimumDepth.Value |> uint32) "We consider the minimum depth (%A) to be unreasonably large. Our max minimum depth is (%A)"

            check1 *> check2 *> check3 *> check4 *> check5 *> check6 *> check7

    let internal checkAcceptChannelMsgAcceptable (conf: ChannelConfig) (state) (msg: AcceptChannel) =
        AcceptChannelValidator.checkDustLimit msg
        *> AcceptChannelValidator.checkChannelReserveSatoshis state msg
        *> AcceptChannelValidator.checkDustLimitIsLargerThanOurChannelReserve state msg
        *> AcceptChannelValidator.checkMinimumHTLCValueIsAcceptable state msg
        *> AcceptChannelValidator.checkToSelfDelayIsAcceptable msg
        *> checkMaxAcceptedHTLCsInMeaningfulRange msg.MaxAcceptedHTLCs
        *> AcceptChannelValidator.checkConfigPermits conf.PeerChannelConfigLimits msg


    module private UpdateAddHTLCValidator =
        let internal checkExpiryIsNotPast (current: BlockHeight) (expiry) =
            checkOrIgnore (expiry) (<=) (current) "AddHTLC's Expiry was %A but it must be larger than current height %A"


        let internal checkExpiryIsInAcceptableRange (current: BlockHeight) (expiry) =
            let checkIsToSoon = checkOrIgnore (expiry) (<=) (current + MIN_CLTV_EXPIRY) "CMD_ADD_HTLC.Expiry was %A but it was too close to current height. Minimum is: %A"
            let checkIsToFar = checkOrIgnore (expiry) (>=) (current + MAX_CLTV_EXPIRY) "CMD_ADD_HTLC.Expiry was %A but it was too far from current height. Maximum is: %A"
            checkIsToSoon *> checkIsToFar

        let internal checkAmountIsLargerThanMinimum (htlcMinimum: LNMoney) (amount) =
            checkOrIgnore (amount) (<) (htlcMinimum) "htlc value (%A) is too small. must be greater or equal to %A"

        let checkLessThanHTLCValueInFlightLimit (currentSpec: CommitmentSpec) (limit) (add: UpdateAddHTLC) =
            let htlcValueInFlight = currentSpec.HTLCs |> Map.toSeq |> Seq.sumBy (fun (_, v) -> v.Add.AmountMSat)
            if (htlcValueInFlight > limit) then
                sprintf "Too much HTLC value is in flight. Current: %A. Limit: %A \n Could not add new one with value: %A"
                        htlcValueInFlight
                        limit
                        add.AmountMSat
                |> RRIgnore
            else
                Good()

        let checkLessThanMaxAcceptedHTLC (currentSpec: CommitmentSpec) (limit) =
            let acceptedHTLCs = currentSpec.HTLCs |> Map.toSeq |> Seq.filter (fun kv -> (snd kv).Direction = In) |> Seq.length
            checkOrIgnore acceptedHTLCs (>) (int limit) "We have much number of HTLCs (%A). Limit specified by remote is (%A). So not going to relay"

        let checkWeHaveSufficientFunds (state: Commitments) (currentSpec) =
            let fees = if (state.LocalParams.IsFunder) then (Transactions.commitTxFee (state.RemoteParams.DustLimitSatoshis) currentSpec) else Money.Zero
            let missing = currentSpec.ToRemote.ToMoney() - state.RemoteParams.ChannelReserveSatoshis - fees
            if (missing < Money.Zero) then
                sprintf "We don't have sufficient funds to send HTLC. current to_remote amount is: %A. Remote Channel Reserve is: %A. and fee is %A"
                        (currentSpec.ToRemote.ToMoney())
                        (state.RemoteParams.ChannelReserveSatoshis)
                        (fees)
                |> RRIgnore
            else
                Good()

    let checkCMDAddHTLC (state: NormalData) (cmd: CMDAddHTLC) =
        UpdateAddHTLCValidator.checkExpiryIsNotPast cmd.CurrentHeight cmd.Expiry
        *> UpdateAddHTLCValidator.checkExpiryIsInAcceptableRange cmd.CurrentHeight cmd.Expiry
        *> UpdateAddHTLCValidator.checkAmountIsLargerThanMinimum state.Commitments.RemoteParams.HTLCMinimumMSat cmd.AmountMSat

    let checkOurUpdateAddHTLCIsAcceptableWithCurrentSpec (currentSpec) (state: Commitments) (add: UpdateAddHTLC) =
        UpdateAddHTLCValidator.checkLessThanHTLCValueInFlightLimit currentSpec state.RemoteParams.MaxHTLCValueInFlightMSat add
        *> UpdateAddHTLCValidator.checkLessThanMaxAcceptedHTLC currentSpec state.RemoteParams.MaxAcceptedHTLCs
        *> UpdateAddHTLCValidator.checkWeHaveSufficientFunds state currentSpec

    let checkTheirUpdateAddHTLCIsAcceptable (state: Commitments) (add: UpdateAddHTLC) (currentHeight: BlockHeight) =
        checkOrClose add.HTLCId (<>) state.RemoteNextHTLCId "Received Unexpected HTLCId (%A). Must be (%A)"
        >>= fun _ ->
            UpdateAddHTLCValidator.checkExpiryIsNotPast currentHeight add.CLTVExpiry
            *> UpdateAddHTLCValidator.checkExpiryIsInAcceptableRange currentHeight add.CLTVExpiry
            *> UpdateAddHTLCValidator.checkAmountIsLargerThanMinimum state.LocalParams.HTLCMinimumMSat add.AmountMSat
            >>>= fun e -> RRClose(e.Describe())

    let checkTheirUpdateAddHTLCIsAcceptableWithCurrentSpec (currentSpec) (state: Commitments) (add: UpdateAddHTLC) =
        UpdateAddHTLCValidator.checkLessThanHTLCValueInFlightLimit currentSpec state.LocalParams.MaxHTLCValueInFlightMSat add
        *> UpdateAddHTLCValidator.checkLessThanMaxAcceptedHTLC currentSpec state.LocalParams.MaxAcceptedHTLCs
        *> UpdateAddHTLCValidator.checkWeHaveSufficientFunds state currentSpec
        >>>= fun e -> RRClose(e.Describe())
