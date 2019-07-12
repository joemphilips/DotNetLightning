namespace DotNetLightning.LN
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Chain
open DotNetLightning.Crypto
open DotNetLightning.Transactions
open DotNetLightning.Serialize.Msgs
open NBitcoin
open System.Linq
open System

type Channel = internal {
    Config: UserConfig
    UserId: UserId
    ChainListener: IChainListener
    KeysRepository: IKeysRepository
    FeeEstimator: IFeeEstimator
    FundingTxProvider: (IDestination * Money * FeeRatePerKw) -> RResult<FinalizedTx * TxOutIndex>
    Logger: Logger
    RemoteNodeId: NodeId
    LocalNodeSecret: Key
    State: ChannelState
    CurrentBlockHeight: BlockHeight
    Network: Network
}
    with
        static member Create (config, userId, logger, chainListener, keysRepo, feeEstimator, remoteNodeId, localNodeSecret, fundingTxProvider, n) =
            {
                Config = config
                UserId = userId
                ChainListener = chainListener
                KeysRepository = keysRepo
                FeeEstimator = feeEstimator
                Logger = logger
                FundingTxProvider = fundingTxProvider
                RemoteNodeId = remoteNodeId
                LocalNodeSecret = localNodeSecret
                State = WaitForInitInternal
                CurrentBlockHeight = BlockHeight.Zero
                Network = n
            }
        static member CreateCurried  = curry10 (Channel.Create)

module Channel =
    /// represents the user has than something wrong with this library
    let private RRApiE(e: APIError) =
        RResult.rbad(RBad.Object(e))

    /// Represents the error that something user can not control (e.g. peer has sent invalid msg).
    let private RRChannelE(ex: ChannelError) =
        RResult.rexn(ChannelException(ex))

    let private RRClose (msg: string) =
        RRChannelE(ChannelError.Close(msg))

    let private RRIgnore (msg: string) =
        RRChannelE(ChannelError.Ignore(msg))

    module internal Helpers =
        let deriveOurDustLimitSatoshis(feeEstimator: IFeeEstimator): Money =
            let (FeeRatePerKw atOpenBackGroundFee) = feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Background)
            (Money.Satoshis((uint64 atOpenBackGroundFee) * B_OUTPUT_PLUS_SPENDING_INPUT_WEIGHT / 1000UL), Money.Satoshis(546UL))
            |> Money.Max

        let getOurChannelReserve (channelValue: Money) =
            let q = channelValue / 100L
            Money.Min(channelValue, Money.Max(q, Money.Satoshis(1L)))

        let buildLocalCommitmentSecret (n) =
            failwith "not impl"

        let getFundingRedeemScript (ck: ChannelKeys) (theirFundingPubKey: PubKey) : Script =
            let ourFundingKey = ck.FundingKey.PubKey
            let pks = if ourFundingKey.ToBytes() < theirFundingPubKey.ToBytes() then
                          [| ourFundingKey; theirFundingPubKey |]
                      else
                          [| theirFundingPubKey; ourFundingKey |]
            PayToMultiSigTemplate.Instance.GenerateScriptPubKey(2, pks)


        let private makeFlags (isNode1: bool, enable: bool) =
            (if isNode1 then 1us else 0us) ||| ((if enable then 1us else 0us) <<< 1)

        let internal makeChannelUpdate(chainHash, nodeSecret: Key, remoteNodeId: NodeId, shortChannelId, cltvExpiryDelta,
                                       htlcMinimum, feeBase, feeProportionalMillionths, enabled: bool, timestamp) = 
            let timestamp = defaultArg timestamp ((System.DateTime.UtcNow.ToUnixTimestamp()) |> uint32)
            let isNodeOne = nodeSecret.PubKey < remoteNodeId.Value
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
            let signature = unsignedChannelUpdate.ToBytes() |> Crypto.Hashes.SHA256 |> uint256 |> nodeSecret.Sign
            {
                ChannelUpdate.Contents = unsignedChannelUpdate
                Signature = signature
            }

        /// gets the fee we'd want to charge for adding an HTLC output to this channel
        let internal getOurFeeBaseMSat (feeEstimator: IFeeEstimator) (FeeRatePerKw feeRatePerKw) (isFunder: bool) =
            // for lack of a better metric, we calculate waht it would cost to consolidate the new HTLC
            // output value back into a transaction with the regular channel output:

            // the fee cost of the HTLC-sucess/HTLC-Timout transaction
            let mutable res = uint64 feeRatePerKw * (max (ChannelConstants.HTLC_TIMEOUT_TX_WEIGHT) (ChannelConstants.HTLC_TIMEOUT_TX_WEIGHT)) |> fun r -> r / 1000UL
            if (isFunder) then
                res <- res + uint64 feeRatePerKw * COMMITMENT_TX_WEIGHT_PER_HTLC / 1000UL

            //+ the marginal cost of an input which spends the HTLC-Success/HTLC-Timeout output:
            res <-
                res + (uint64 (feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Normal).Value) * SPENDING_INPUT_FOR_A_OUTPUT_WEIGHT) / 1000UL
            res |> LNMoney.Satoshis

        let makeFirstCommitTxs(localParams: LocalParams)
                              (remoteParams: RemoteParams)
                              (fundingSatoshis: Money)
                              (pushMSat: LNMoney)
                              (initialFeeRatePerKw: FeeRatePerKw)
                              (fundingOutputIndex: TxOutIndex)
                              (fundingTxId: TxId)
                              (n: Network): RResult<(CommitmentSpec * CommitTx * CommitmentSpec * CommitTx)> =
            let toLocal = if (localParams.IsFunder) then fundingSatoshis.ToLNMoney() - pushMSat else pushMSat
            let toRemote = if (localParams.IsFunder) then pushMSat else fundingSatoshis.ToLNMoney() - pushMSat
            let localSpec = CommitmentSpec.Create toLocal toRemote initialFeeRatePerKw
            let remoteSpec = CommitmentSpec.Create toRemote toLocal initialFeeRatePerKw
            let checkTheyCanAffordFee () =
                let toRemote = remoteSpec.ToLocal
                let fees = Transactions.commitTxFee remoteParams.DustLimitSatoshis remoteSpec
                let missing = toRemote.ToMoney() - localParams.ChannelReserveSatoshis - fees
                if missing < Money.Zero then
                    RResult.rmsg (sprintf "they are funder but cannot affored there fee. to_remote output is: %A; actual fee is %A; channel_reserve_satoshis: %A" toRemote fees localParams.ChannelReserveSatoshis)
                else
                    Good()
            let makeFirstCommitTxCore () =
                let redeem = getFundingRedeemScript localParams.ChannelKeys remoteParams.FundingPubKey
                let scoin =
                    Coin(fundingTxId.Value, uint32 fundingOutputIndex.Value, fundingSatoshis, redeem.WitHash.ScriptPubKey)
                    |> fun c -> ScriptCoin(c, redeem)
                let localCommitTx =
                    Transactions.makeCommitTx scoin
                                              0UL
                                              localParams.ChannelKeys.PaymentBaseKey.PubKey 
                                              remoteParams.PaymentBasePoint
                                              localParams.IsFunder
                                              localParams.DustLimitSatoshis
                                              localParams.ChannelKeys.RevocationBaseKey.PubKey
                                              localParams.ToSelfDelay
                                              (localParams.ChannelKeys.DelayedPaymentBaseKey.PubKey)
                                              (remoteParams.PaymentBasePoint)
                                              (localParams.ChannelKeys.HTLCBaseKey.PubKey)
                                              (remoteParams.HTLCBasePoint)
                                              localSpec
                                              n
                let remoteCommitTx =
                    Transactions.makeCommitTx scoin
                                              0UL
                                              remoteParams.PaymentBasePoint
                                              localParams.ChannelKeys.PaymentBaseKey.PubKey
                                              (not localParams.IsFunder)
                                              (remoteParams.DustLimitSatoshis)
                                              (remoteParams.RevocationBasePoint)
                                              (remoteParams.ToSelfDelay)
                                              (remoteParams.DelayedPaymentBasePoint)
                                              (localParams.ChannelKeys.PaymentBaseKey.PubKey)
                                              (remoteParams.HTLCBasePoint)
                                              (localParams.ChannelKeys.HTLCBaseKey.PubKey)
                                              remoteSpec
                                              n

                (localSpec, localCommitTx, remoteSpec, remoteCommitTx) |> Good

            if (not localParams.IsFunder) then
                checkTheyCanAffordFee() *>  (makeFirstCommitTxCore())
            else
                makeFirstCommitTxCore()

    module internal Validation =
        let internal checkSigHash (expected: SigHash) (txSig: TransactionSignature) =
            if txSig.SigHash <> expected then
                RRClose(sprintf "tx sighash must be %A . but it was %A" expected txSig.SigHash)
            else
                Good (txSig.Signature)

        let checkOrClose left predicate right msg =
            if predicate left right then
                sprintf msg left right |> RRClose
            else
                Good ()

        let checkOrIgnore left predicate right msg =
            if predicate left right then
                sprintf msg left right |> RRIgnore
            else
                Good ()

        let checkMaxAcceptedHTLCsInMeaningfulRange (maxAcceptedHTLCs: uint16) =
            if (maxAcceptedHTLCs < 1us) then
                RRClose("max_accepted_htlcs must be not 0")
            else if (maxAcceptedHTLCs < 483us) then
                RRClose("max_accepted_htlcs must be less than 483")
            else
                Good ()

        module private OpenChannelRequest =
            let checkFundingSatoshisLessThanMax (msg: OpenChannel) =
                if (msg.FundingSatoshis >= ChannelConstants.MAX_FUNDING_SATOSHIS) then
                    RRClose("Funding value > 2^24")
                else
                    Good ()

            let checkChannelReserveSatohisLessThanFundingSatoshis (msg: OpenChannel) =
                if (msg.ChannelReserveSatoshis > msg.FundingSatoshis) then
                    RRClose("Bogus channel_reserve_satoshis. Must be bigger than funding_satoshis")
                else
                    Good()

            let checkPushMSatLesserThanFundingValue msg =
                if (msg.PushMSat.ToMoney() > (msg.FundingSatoshis - msg.ChannelReserveSatoshis)) then
                    RRClose("push_msat larger than funding value")
                else
                    Good ()

            let checkFundingSatoshisLessThanDustLimitSatoshis (msg: OpenChannel) =
                if (msg.DustLimitSatoshis > msg.FundingSatoshis) then
                    RRClose(sprintf "Peer never wants payout outputs? dust_limit_satoshis: %A; funding_satoshi %A" msg.DustLimitSatoshis msg.FundingSatoshis)
                else
                    Good ()

            let checkRemoteFee (feeEstimator: IFeeEstimator) (feeRate: FeeRatePerKw) =
                if (feeRate) < feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Background) then
                    RRClose("Peer's feerate much too low")
                else if (feeRate.Value > feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority).Value * 2u) then
                    RRClose("Peer's feerate much too high")
                else
                    Good ()

            let checkToSelfDelayIsInAcceptableRange (msg: OpenChannel) =
                if (msg.ToSelfDelay > MAX_LOCAL_BREAKDOWN_TIMEOUT) then
                    RRClose("They wanted our payments to be delayed by a needlessly long period")
                else
                    Good ()

            let checkConfigPermits (config: ChannelHandshakeLimits) (msg: OpenChannel) =
            
                if (msg.FundingSatoshis < config.MinFundingSatoshis) then
                    RRClose(sprintf "funding satoshis is less than the user specified limit. received: %A; limit: %A" msg.FundingSatoshis config.MinFundingSatoshis)
                else if (msg.HTLCMinimumMsat.ToMoney() > config.MinFundingSatoshis) then
                    RRClose(sprintf "htlc minimum msat is higher than the users specified limit. received %A; limit: %A" msg.FundingSatoshis config.MaxHTLCMinimumMSat)
                else if (msg.MaxHTLCValueInFlightMsat < config.MinMaxHTLCValueInFlightMSat) then
                    RRClose ("max htlc value in light msat is less than the user specified limit")
                else if  (msg.ChannelReserveSatoshis > config.MaxChannelReserveSatoshis) then
                    RRClose(sprintf "channel reserve satoshis is higher than the user specified limit. received %A; limit: %A" msg.ChannelReserveSatoshis msg.ChannelReserveSatoshis)
                else if (msg.MaxAcceptedHTLCs < config.MinMaxAcceptedHTLCs) then
                    RRClose(sprintf "max accepted htlcs is less than the user specified limit. received: %A; limit: %A" msg.MaxAcceptedHTLCs config.MinMaxAcceptedHTLCs)
                else if (msg.DustLimitSatoshis < config.MinDustLimitSatoshis) then
                    RRClose(sprintf "dust_limit_satoshis is less than the user specified limit. received: %A; limit: %A" msg.DustLimitSatoshis config.MinDustLimitSatoshis)
                else if (msg.DustLimitSatoshis > config.MaxDustLimitSatoshis) then 
                    RRClose(sprintf "dust_limit_satoshis is greater than the user specified limit. received: %A; limit: %A" msg.DustLimitSatoshis config.MaxDustLimitSatoshis)
                else
                    Good ()

            let checkChannelAnnouncementPreferenceAcceptable (config: UserConfig) (msg) =
                let theirAnnounce = (msg.ChannelFlags &&& 1uy) = 1uy
                if (config.PeerChannelConfigLimits.ForceAnnouncedChannelPreference) && config.ChannelOptions.AnnouncedChannel <> theirAnnounce then
                    RRClose("Peer tried to open channel but their announcement preference is different from ours")
                else
                    Good ()

            let checkIsAccpetableByCurrentFeeRate (feeEstimator: IFeeEstimator) msg = 
                let ourDustLimit = Helpers.deriveOurDustLimitSatoshis feeEstimator
                let ourChannelReserve = Helpers.getOurChannelReserve (msg.FundingSatoshis)
                if (ourChannelReserve < ourDustLimit) then
                    RRClose("Suitable channel reserve not found. Aborting")
                else if (msg.ChannelReserveSatoshis < ourDustLimit) then
                    RRClose (sprintf "channel_reserve_satoshis too small. It was: %A; dust_limit: %A" msg.ChannelReserveSatoshis ourDustLimit )
                else if (ourChannelReserve < msg.DustLimitSatoshis) then
                    RRClose(sprintf "Dust limit too high for our channel reserve. our channel reserve: %A received dust limit: %A" ourChannelReserve msg.DustLimitSatoshis)
                else
                    Good ()

            let checkIfFundersAmountSufficient (feeEst: IFeeEstimator) msg =
                let fundersAmount = LNMoney.Satoshis(msg.FundingSatoshis.Satoshi) - msg.PushMSat
                let (backgroundFeeRate) = feeEst.GetEstSatPer1000Weight (ConfirmationTarget.Background)
                let backgroundFee = backgroundFeeRate.ToFee COMMITMENT_TX_BASE_WEIGHT
                if (fundersAmount.ToMoney() < backgroundFee) then
                    RRClose(sprintf "Insufficient funding amount for initial commitment. BackgroundFee %A. funders amount %A" backgroundFee fundersAmount)
                else
                    let ourChannelReserve = Helpers.getOurChannelReserve msg.FundingSatoshis
                    let toLocalMSat = msg.PushMSat
                    let toRemoteMSat = fundersAmount - backgroundFeeRate.ToFee(COMMITMENT_TX_BASE_WEIGHT).ToLNMoney()
                    if (toLocalMSat <= (msg.ChannelReserveSatoshis.ToLNMoney()) && toRemoteMSat <= ourChannelReserve.ToLNMoney()) then
                        RRClose("Insufficient funding amount for iniial commitment. ")
                    else
                        Good ()


        let internal checkOpenChannelMsgAcceptable cs (msg: OpenChannel) =
            OpenChannelRequest.checkFundingSatoshisLessThanMax msg
            *> OpenChannelRequest.checkChannelReserveSatohisLessThanFundingSatoshis msg
            *> OpenChannelRequest.checkPushMSatLesserThanFundingValue msg
            *> OpenChannelRequest.checkFundingSatoshisLessThanDustLimitSatoshis msg
            *> OpenChannelRequest.checkRemoteFee cs.FeeEstimator msg.FeeRatePerKw
            *> OpenChannelRequest.checkToSelfDelayIsInAcceptableRange msg
            *> checkMaxAcceptedHTLCsInMeaningfulRange msg.MaxAcceptedHTLCs
            *> OpenChannelRequest.checkConfigPermits cs.Config.PeerChannelConfigLimits msg
            *> OpenChannelRequest.checkChannelAnnouncementPreferenceAcceptable cs.Config msg
            *> OpenChannelRequest.checkIsAccpetableByCurrentFeeRate cs.FeeEstimator msg

        module private AcceptChannelValidator =
            let checkDustLimit msg =
                if msg.DustLimitSatoshis > Money.Satoshis(21000000L * 100000L) then
                    RRClose(sprintf "Peer never wants payout outputs? dust_limit_satoshis was: %A" msg.DustLimitSatoshis)
                else
                    Good ()

            let checkChannelReserveSatoshis (c: Channel) (state: Data.WaitForAcceptChannelData) msg =
                if msg.ChannelReserveSatoshis > state.LastSent.FundingSatoshis then
                    sprintf "bogus channel_reserve_satoshis %A . Must be larger than funding_satoshis %A" (msg.ChannelReserveSatoshis) (state.InputInitFunder.FundingSatoshis)
                    |> RRClose
                else if msg.DustLimitSatoshis > state.LastSent.ChannelReserveSatoshis then
                    sprintf "Bogus channel_serserve and dust_limit. dust_limit: %A; channel_reserve %A" msg.DustLimitSatoshis (state.LastSent.ChannelReserveSatoshis)
                    |> RRClose
                else if msg.ChannelReserveSatoshis < state.LastSent.DustLimitSatoshis then
                    sprintf "Peer never wants payout outputs? channel_reserve_satoshis are %A; dust_limit_satoshis in our last sent msg is %A" msg.ChannelReserveSatoshis (state.LastSent.DustLimitSatoshis)
                    |> RRClose
                else
                    Good ()

            let checkDustLimitIsLargerThanOurChannelReserve (state: Data.WaitForAcceptChannelData) msg =
                let reserve = Helpers.getOurChannelReserve state.LastSent.FundingSatoshis
                if (msg.DustLimitSatoshis < reserve) then
                    sprintf "dust limit (%A) is bigger than our channel reserve (%A)" msg.DustLimitSatoshis reserve
                    |> RRClose
                else
                    Good ()

            let checkMinimumHTLCValueIsAcceptable (state: Data.WaitForAcceptChannelData) (msg: AcceptChannel) =
                if (msg.HTLCMinimumMSat.ToMoney() >= (state.LastSent.FundingSatoshis - msg.ChannelReserveSatoshis)) then
                    sprintf "Minimum HTLC value is greater than full channel value HTLCMinimum %A satoshi; funding_satoshis %A; channel_reserve: %A" (msg.HTLCMinimumMSat.ToMoney()) (state.LastSent.FundingSatoshis) (msg.ChannelReserveSatoshis)
                    |> RRClose
                else
                    Good ()

            let checkToSelfDelayIsAcceptable (msg) =
                if (msg.ToSelfDelay > MAX_LOCAL_BREAKDOWN_TIMEOUT) then
                    sprintf "They wanted our payments to be delayed by a needlessly long period (%A)" msg.ToSelfDelay
                    |> RRClose
                else
                    Good ()

            let checkConfigPermits (config: ChannelHandshakeLimits) (msg: AcceptChannel) =
                let check1 = checkOrClose msg.HTLCMinimumMSat (>) config.MaxHTLCMinimumMSat "HTLC Minimum msat in accept_channel (%A) is higher than the user specified limit (%A)"
                let check2 = checkOrClose msg.MaxHTLCValueInFlightMsat (<) config.MinMaxHTLCValueInFlightMSat  "max htlc value in flight msat (%A) is less than the user specified limit (%A)"
                let check3 = checkOrClose msg.ChannelReserveSatoshis (>) config.MaxChannelReserveSatoshis "max reserve_satoshis (%A) is higher than the user specified limit (%A)"
                let check4 = checkOrClose msg.MaxAcceptedHTLCs (<) config.MinMaxAcceptedHTLCs "max accpeted htlcs (%A) is less than the user specified limit (%A)"
                let check5 = checkOrClose msg.DustLimitSatoshis (<) config.MinDustLimitSatoshis "dust limit satoshis (%A) is less then the user specified limit (%A)"
                let check6 = checkOrClose msg.DustLimitSatoshis (>) config.MinDustLimitSatoshis "dust limit satoshis (%A) is greater then the user specified limit (%A)"
                let check7 = checkOrClose msg.MinimumDepth (>) config.MaxMinimumDepth "We consider the minimum depth (%A) to be unreasonably large. Our max minimum depth is (%A)"

                check1 *> check2 *> check3 *> check4 *> check5 *> check6 *> check7
        
        let internal checkAcceptChannelMsgAccpetable c (state) (msg: AcceptChannel) =
            AcceptChannelValidator.checkDustLimit msg
            *> AcceptChannelValidator.checkChannelReserveSatoshis c state msg
            *> AcceptChannelValidator.checkDustLimitIsLargerThanOurChannelReserve state msg
            *> AcceptChannelValidator.checkMinimumHTLCValueIsAcceptable state msg
            *> AcceptChannelValidator.checkToSelfDelayIsAcceptable msg
            *> checkMaxAcceptedHTLCsInMeaningfulRange msg.MaxAcceptedHTLCs
            *> AcceptChannelValidator.checkConfigPermits c.Config.PeerChannelConfigLimits msg


        module UpdateAddHTLCValidator =
            let internal checkExpiryIsNotPast (current: BlockHeight) (expiry) =
                checkOrIgnore (expiry) (<=) (current) "AddHTLC's Expiry was %A but it must be larger than current height %A"


            let internal checkExpiryIsInAcceptableRange (current: BlockHeight) (expiry) =
                let checkIsToSoon = checkOrIgnore (expiry) (<=) (current + MIN_CLTV_EXPIRY) "CMDADHTLC.Expiry was %A but it was too close to current height. Minimum is: %A"
                let checkIsToFar = checkOrIgnore (expiry) (>=) (current + MAX_CLTV_EXPIRY) "CMDADHTLC.Expiry was %A but it was too far from current height. Maximum is: %A"
                checkIsToSoon *> checkIsToFar

            let internal checkAmountIsLargerThanMinimum (htlcMinimum: LNMoney) (amount) =
                checkOrIgnore (amount) (<) (htlcMinimum) "htlc value (%A) is too small. must be greater or equal to %A"

            let checkLessThanHTLCValueInFlightLimit (currentSpec: CommitmentSpec) (limit) (add: UpdateAddHTLC) =
                let htlcValueInFlight = currentSpec.HTLCs |> Map.toSeq |> Seq.sumBy(fun (k ,v) -> v.Add.AmountMSat)
                if (htlcValueInFlight > limit) then
                    sprintf "Too much HTLC value is in flight. Current: %A. Limit: %A \n Could not add new one with value: %A"
                            htlcValueInFlight 
                            limit
                            add.AmountMSat
                    |> RRIgnore
                else
                    Good()

            let checkLessThanMaxAcceptedHTLC (currentSpec: CommitmentSpec) (limit) =
                let acceptedHTLCs = currentSpec.HTLCs |> Map.toSeq |> Seq.filter(fun kv -> (snd kv).Direction = In) |> Seq.length
                checkOrIgnore acceptedHTLCs (>) (int limit) "We have much number of HTLCs (%A). Limit specified by remote is (%A). So not going to relay"

            let checkWeHaveSufficientFunds (state: Commitments) (currentSpec) =
                let fees = if (state.LocalParams.IsFunder) then (Transactions.commitTxFee(state.RemoteParams.DustLimitSatoshis) currentSpec) else Money.Zero
                let missing = currentSpec.ToRemote.ToMoney() - state.RemoteParams.ChannelReserveSatoshis - fees
                if (missing < Money.Zero) then
                    sprintf "We don't have sufficient funds to send HTLC. current to_remote amount is: %A. Remote Channel Reserve is: %A. and fee is %A"
                            (currentSpec.ToRemote.ToMoney())
                            (state.RemoteParams.ChannelReserveSatoshis)
                            (fees)
                    |> RRIgnore
                else
                    Good()

        let checkCMDAddHTLC (c: Channel) (state: NormalData) (cmd: CMDAddHTLC) =
            UpdateAddHTLCValidator.checkExpiryIsNotPast c.CurrentBlockHeight cmd.Expiry
            *> UpdateAddHTLCValidator.checkExpiryIsInAcceptableRange c.CurrentBlockHeight cmd.Expiry
            *> UpdateAddHTLCValidator.checkAmountIsLargerThanMinimum state.Commitments.RemoteParams.HTLCMinimumMSat cmd.AmountMSat

        let checkOurUpdateAddHTLCIsAcceptableWithCurrentSpec (currentSpec) (state: Commitments) (add: UpdateAddHTLC) =
            UpdateAddHTLCValidator.checkLessThanHTLCValueInFlightLimit currentSpec state.RemoteParams.MaxHTLCValueInFlightMSat add
            *> UpdateAddHTLCValidator.checkLessThanMaxAcceptedHTLC  currentSpec state.RemoteParams.MaxAcceptedHTLCs
            *> UpdateAddHTLCValidator.checkWeHaveSufficientFunds state currentSpec

        let checkTheirUpdateAddHTLCIsAcceptable (c: Channel) (state: Commitments) (add: UpdateAddHTLC) =
            checkOrClose add.HTLCId (=) state.RemoteNextHTLCId "Received Unexpected HTLCId (%A). Must be (%A)"
            >>= fun _ ->
                UpdateAddHTLCValidator.checkExpiryIsNotPast c.CurrentBlockHeight add.CLTVExpiry
                *> UpdateAddHTLCValidator.checkExpiryIsInAcceptableRange c.CurrentBlockHeight add.CLTVExpiry
                *> UpdateAddHTLCValidator.checkAmountIsLargerThanMinimum state.LocalParams.HTLCMinimumMSat add.AmountMSat
                >>>= fun e -> RRClose(e.Describe())

        let checkTheirUpdateAddHTLCIsAcceptableWithCurrentSpec (currentSpec) (state: Commitments) (add: UpdateAddHTLC) =
            UpdateAddHTLCValidator.checkLessThanHTLCValueInFlightLimit currentSpec state.LocalParams.MaxHTLCValueInFlightMSat add
            *> UpdateAddHTLCValidator.checkLessThanMaxAcceptedHTLC currentSpec state.LocalParams.MaxAcceptedHTLCs
            *> UpdateAddHTLCValidator.checkWeHaveSufficientFunds state currentSpec
            >>>= fun e -> RRClose(e.Describe())

    let executeCommand (cs: Channel) (command: ChannelCommand): RResult<ChannelEvent list> =
        match cs.State, command with

        // --------------- open channel procedure: case we are fundee -------------
        | WaitForOpenChannel state, ApplyOpenChannel msg ->
            Validation.checkOpenChannelMsgAcceptable cs msg
            |>> fun _ ->
                let localParams = state.InitFundee.LocalParams
                let localCommitmentSecret = Helpers.buildLocalCommitmentSecret ()
                let channelKeys = cs.KeysRepository.GetChannelKeys(true)
                let acceptChannel = { AcceptChannel.TemporaryChannelId = msg.TemporaryChannelId
                                      DustLimitSatoshis = localParams.DustLimitSatoshis
                                      MaxHTLCValueInFlightMsat = localParams.MaxHTLCValueInFlightMSat
                                      ChannelReserveSatoshis = localParams.ChannelReserveSatoshis
                                      HTLCMinimumMSat = localParams.HTLCMinimumMSat
                                      MinimumDepth = cs.Config.OwnChannelConfig.MinimumDepth
                                      ToSelfDelay = localParams.ToSelfDelay
                                      MaxAcceptedHTLCs = localParams.MaxAcceptedHTLCs
                                      FundingPubKey = channelKeys.FundingKey.PubKey
                                      RevocationBasepoint = channelKeys.RevocationBaseKey.PubKey
                                      PaymentBasepoint = channelKeys.PaymentBaseKey.PubKey
                                      DelayedPaymentBasepoint = channelKeys.DelayedPaymentBaseKey.PubKey
                                      HTLCBasepoint = channelKeys.HTLCBaseKey.PubKey
                                      FirstPerCommitmentPoint = localCommitmentSecret.PubKey
                                      ShutdownScriptPubKey = None }

                let remoteParams = RemoteParams.FromOpenChannel cs.RemoteNodeId state.InitFundee.RemoteInit msg
                let data = Data.WaitForFundingCreatedData.Create localParams remoteParams msg acceptChannel
                [ WeAcceptedOpenChannel (acceptChannel, data) ]
        | WaitForOpenChannel state, Close spk ->  [ChannelEvent.Closed] |> Good

        | WaitForFundingCreated state, ApplyFundingCreated msg ->
            Helpers.makeFirstCommitTxs state.LocalParams
                                       state.RemoteParams
                                       state.FundingSatoshis
                                       state.PushMSat
                                       state.InitialFeeRatePerKw
                                       msg.FundingOutputIndex
                                       msg.FundingTxId
                                       cs.Network
            >>= fun (localSpec, CommitTx localCommitTx, remoteSpec, CommitTx remoteCommitTx) ->
                match (localCommitTx.IsReadyToSign()) with
                | false -> failwith "unreachable"
                | true ->
                    let finalLocalCommitTxRR =
                        let signedLocalCommitTx = localCommitTx.SignWithKeys(state.LocalParams.ChannelKeys.FundingKey)
                        let theirSigPair = (state.RemoteParams.FundingPubKey , TransactionSignature(msg.Signature, SigHash.All))
                        let sigPairs = seq [theirSigPair]
                        let redeem = Helpers.getFundingRedeemScript state.LocalParams.ChannelKeys state.RemoteParams.FundingPubKey
                        Transactions.checkTxFinalized (CommitTx signedLocalCommitTx) (msg.FundingOutputIndex) sigPairs redeem
                    finalLocalCommitTxRR
                    >>>= fun e -> RRClose(e.Describe())
                    >>= fun finalizedCommitTx ->
                        Validation.checkSigHash SigHash.All (cs.KeysRepository.GetSignature(remoteCommitTx))
                        |>> fun localSigOfRemoteCommit ->
                            let channelId = OutPoint(msg.FundingTxId.Value, uint32 msg.FundingOutputIndex.Value).ToChannelId()
                            let msgToSend: FundingSigned = { ChannelId = channelId; Signature = localSigOfRemoteCommit }
                            let commitments = { Commitments.LocalParams = state.LocalParams
                                                RemoteParams = state.RemoteParams
                                                ChannelFlags = state.ChannelFlags
                                                FundingTxOutIndex = msg.FundingOutputIndex
                                                LocalCommit = { LocalCommit.Index = 0u;
                                                                Spec = localSpec
                                                                PublishableTxs = { PublishableTxs.CommitTx = finalizedCommitTx;
                                                                                   HTLCTxs = [] } }
                                                RemoteCommit = { RemoteCommit.Index = 0u;
                                                                 Spec = remoteSpec
                                                                 TxId = remoteCommitTx.GetGlobalTransaction().GetTxId()
                                                                 RemotePerCommitmentPoint = state.RemoteFirstPerCommitmentPoint }
                                                LocalChanges = LocalChanges.Zero
                                                RemoteChanges = RemoteChanges.Zero
                                                LocalNextHTLCId = HTLCId.Zero
                                                RemoteNextHTLCId = HTLCId.Zero
                                                OriginChannels = Map.empty
                                                RemoteNextCommitInfo = DataEncoders.HexEncoder().DecodeData("0101010101010101010101010101010101010101010101010101010101010101") |> Key |> fun k -> k.PubKey |> Choice2Of2
                                                RemotePerCommitmentSecrets = ShaChain.Zero
                                                ChannelId = channelId }
                            let nextState = { WaitForFundingConfirmedData.Commitments = commitments
                                              Deferred = None
                                              LastSent = msgToSend |> Choice2Of2
                                              InitialFeeRatePerKw = state.InitialFeeRatePerKw }
                            [ WeAcceptedFundingCreated (msgToSend, nextState) ]

        // --------------- open channel procedure: case we are funder -------------
        | WaitForAcceptChannel state, ApplyAcceptChannel msg ->
            Validation.checkAcceptChannelMsgAccpetable cs state msg
            >>= fun _ ->
                let redeem = cs.KeysRepository.GetChannelKeys(false) |> Helpers.getFundingRedeemScript <| (msg.FundingPubKey)
                cs.FundingTxProvider(redeem.WitHash :> IDestination, state.InputInitFunder.FundingSatoshis, state.InputInitFunder.FundingTxFeeRatePerKw)
            >>= fun (fundingTx, outIndex) ->
                let remoteParams = RemoteParams.FromAcceptChannel cs.RemoteNodeId (state.InputInitFunder.RemoteInit) msg
                let localParams = state.InputInitFunder.LocalParams
                let commitmentSpec = state.InputInitFunder.DeriveCommitmentSpec()
                Helpers.makeFirstCommitTxs localParams
                                           remoteParams
                                           state.LastSent.FundingSatoshis
                                           state.LastSent.PushMSat
                                           state.LastSent.FeeRatePerKw
                                           outIndex
                                           (fundingTx.Value.GetHash() |> TxId)
                                           cs.Network
                >>= fun (localSpec, localCommitTx, remoteSpec, CommitTx remoteCommitTx) ->
                    Validation.checkSigHash SigHash.All (cs.KeysRepository.GetSignature(remoteCommitTx))
                    |>> fun (localSigOfRemoteCommit) ->
                        let nextMsg = { FundingCreated.TemporaryChannelId = msg.TemporaryChannelId
                                        FundingTxId = fundingTx.Value.GetTxId()
                                        FundingOutputIndex = outIndex
                                        Signature = localSigOfRemoteCommit }

                        let data = { Data.WaitForFundingSignedData.ChannelId = msg.TemporaryChannelId
                                     LocalParams = localParams
                                     RemoteParams = remoteParams
                                     Data.WaitForFundingSignedData.FundingTx = fundingTx
                                     Data.WaitForFundingSignedData.LocalSpec =  commitmentSpec
                                     LocalCommitTx = localCommitTx
                                     RemoteCommit = { RemoteCommit.Index = 0u;
                                                      Spec = remoteSpec
                                                      TxId = remoteCommitTx.GetGlobalTransaction().GetTxId()
                                                      RemotePerCommitmentPoint = msg.FirstPerCommitmentPoint }
                                     ChannelFlags = state.InputInitFunder.ChannelFlags
                                     LastSent = nextMsg
                                     InitialFeeRatePerKw = state.InputInitFunder.InitFeeRatePerKw }
                        [WeAcceptedAcceptChannel(nextMsg, data)]
        | WaitForFundingSigned state, ApplyFundingSigned msg ->
            let fundingOutIndex = state.LastSent.FundingOutputIndex
            let finalLocalCommitTxRR =
                let theirFundingPk = state.RemoteParams.FundingPubKey
                let localSigPairOfLocalTx = (theirFundingPk, TransactionSignature(state.LastSent.Signature, SigHash.All))
                let remoteSigPairOfLocalTx = (state.LocalParams.ChannelKeys.FundingKey.PubKey, TransactionSignature(msg.Signature, SigHash.All))
                let sigPairs = seq [ remoteSigPairOfLocalTx; localSigPairOfLocalTx ]
                let redeem = Helpers.getFundingRedeemScript state.LocalParams.ChannelKeys theirFundingPk
                Transactions.checkTxFinalized state.LocalCommitTx fundingOutIndex sigPairs (redeem)
            finalLocalCommitTxRR
            >>>= fun e -> RRClose(e.Describe())
            |>> fun finalizedLocalCommitTx ->
                let commitments = { Commitments.LocalParams = state.LocalParams
                                    RemoteParams = state.RemoteParams
                                    ChannelFlags = state.ChannelFlags
                                    FundingTxOutIndex = fundingOutIndex
                                    LocalCommit = { Index = 0u;
                                                    Spec = state.LocalSpec;
                                                    PublishableTxs = { PublishableTxs.CommitTx = finalizedLocalCommitTx
                                                                       HTLCTxs = [] } }
                                    RemoteCommit = state.RemoteCommit
                                    LocalChanges = LocalChanges.Zero
                                    RemoteChanges = RemoteChanges.Zero
                                    LocalNextHTLCId = HTLCId.Zero
                                    RemoteNextHTLCId = HTLCId.Zero
                                    OriginChannels = Map.empty
                                    // we will receive their next per-commitment point in the next msg, so we temporarily put a random byte array
                                    RemoteNextCommitInfo = DataEncoders.HexEncoder().DecodeData("0101010101010101010101010101010101010101010101010101010101010101") |> Key |> fun k -> k.PubKey |> Choice2Of2
                                    RemotePerCommitmentSecrets = ShaChain.Zero
                                    ChannelId =
                                        cs.Logger(sprintf "Channel id has been set to %A" msg.ChannelId, LogLevel.Info)
                                        msg.ChannelId }
                let nextState = { WaitForFundingConfirmedData.Commitments = commitments
                                  Deferred = None
                                  LastSent = Choice1Of2 state.LastSent
                                  InitialFeeRatePerKw = state.InitialFeeRatePerKw }
                [WeAcceptedFundingSigned (state.FundingTx, nextState)]
        | WaitForFundingConfirmed state, ApplyFundingLocked msg ->
            [ TheySentFundingLockedMsgBeforeUs msg ] |> Good
        | WaitForFundingConfirmed state, ApplyFundingConfirmedOnBC (height, txindex, depth) ->
            cs.Logger(sprintf "ChannelId %A was confirmed at blockheight %A; depth: %A" state.Commitments.ChannelId height.Value depth, LogLevel.Info)
            let nextPerCommitmentPoint =
                ChannelUtils.buildCommitmentSecret (state.Commitments.LocalParams.ChannelKeys.CommitmentSeed, 1UL)
                |> fun seed -> Key(seed.ToBytes()).PubKey
            let msgToSend: FundingLocked = { ChannelId = state.Commitments.ChannelId; NextPerCommitmentPoint = nextPerCommitmentPoint }

            // This is temporary channel id that we will use in our channel_update message, the goal is to be able to use our channel
            // as soon as it reaches NORMAL state, and before it is announced on the network
            // (this id might be updated when the funding tx gets deeplly buried, if there was a reorg in the meantime)
            // this is not specified in BOLT.
            let shortChannelId = { ShortChannelId.BlockHeight = height;
                                   BlockIndex = txindex
                                   TxOutIndex = state.Commitments.FundingTxOutIndex }
            let nextState = { Data.WaitForFundingLockedData.Commitments = state.Commitments
                              ShortChannelId = shortChannelId
                              OurMessage = msgToSend
                              TheirMessage = None
                              HaveWeSentFundingLocked = false
                              InitialFeeRatePerKw = state.InitialFeeRatePerKw }
            [ FundingConfirmed nextState ] |> Good
        | WaitForFundingLocked state, ApplyFundingConfirmedOnBC (height, txindex, depth) ->
            if (state.HaveWeSentFundingLocked) then
                if (cs.Config.OwnChannelConfig.MinimumDepth <= depth) then
                    [] |> Good 
                else
                    let msg = sprintf "once confirmed funding tx has become less confirmed than threashold %A! This is probably caused by reorg. current depth is: %A " height depth
                    cs.Logger(msg, LogLevel.Error)
                    RRClose(msg)
            else
                if (cs.Config.OwnChannelConfig.MinimumDepth <= depth) then
                    let nextPerCommitmentPoint =
                        ChannelUtils.buildCommitmentSecret (state.Commitments.LocalParams.ChannelKeys.CommitmentSeed, 1UL)
                        |> fun seed -> Key(seed.ToBytes()).PubKey
                    let msg: FundingLocked = { FundingLocked.ChannelId = state.Commitments.ChannelId
                                               NextPerCommitmentPoint = nextPerCommitmentPoint }
                    [ WeSentFundingLockedMsgBeforeThem msg ] |> Good
                else
                    [] |> Good 
        | WaitForFundingLocked state, ApplyFundingLocked msg ->
            if (state.HaveWeSentFundingLocked) then
                let initialCahnnelUpdate = failwith ""
                let nextState = { NormalData.Buried = true
                                  Commitments = { state.Commitments with RemoteNextCommitInfo = Choice2Of2(msg.NextPerCommitmentPoint) }
                                  ShortChannelId = state.ShortChannelId
                                  ChannelAnnouncement = None
                                  ChannelUpdate = initialCahnnelUpdate
                                  LocalShutdown = None
                                  RemoteShutdown = None }
                [ BothFundingLocked nextState ] |> Good
            else
                [ TheySentFundingLockedMsgBeforeUs msg ] |> Good

        // ---------- notmal operation ---------
        | ChannelState.Normal state, AddHTLC cmd when state.LocalShutdown.IsSome || state.RemoteShutdown.IsSome ->
            sprintf "Could not add new HTLC %A since shutdown is already in progress." cmd
            |> RRIgnore
        | ChannelState.Normal state, AddHTLC cmd ->
            Validation.checkCMDAddHTLC cs state cmd
            >>= fun _ ->
                let add: UpdateAddHTLC = { UpdateAddHTLC.ChannelId = state.Commitments.ChannelId
                                           HTLCId = state.Commitments.LocalNextHTLCId
                                           AmountMSat = cmd.AmountMSat
                                           PaymentHash = cmd.PaymentHash
                                           CLTVExpiry = cmd.Expiry
                                           OnionRoutingPacket = cmd.Onion}
                let commitments1 = { state.Commitments.AddLocalProposal(add)
                                        with LocalNextHTLCId = state.Commitments.LocalNextHTLCId + 1UL }
                                        |> fun commitments ->
                                            match cmd.Origin with
                                            | None -> commitments
                                            | Some o -> { commitments with OriginChannels = state.Commitments.OriginChannels |> Map.add add.HTLCId o }
                // we need to base the next current commitment on the last sig we sent, even if we didn't yet receive their revocation
                let remoteCommit1 =
                    match commitments1.RemoteNextCommitInfo with
                    | Choice1Of2 info -> info.NextRemoteCommit
                    | Choice2Of2 info -> commitments1.RemoteCommit
                remoteCommit1.Spec.Reduce(commitments1.RemoteChanges.ACKed, commitments1.LocalChanges.Proposed)
                >>= fun reduced ->
                    Validation.checkOurUpdateAddHTLCIsAcceptableWithCurrentSpec reduced commitments1 add
                    *> Good([ WeAcceptedCMDAddHTLC (add, commitments1)])
        | ChannelState.Normal state, ApplyUpdateAddHTLC msg ->
            Validation.checkTheirUpdateAddHTLCIsAcceptable cs state.Commitments msg
            >>= fun _ ->
                let commitments1 =  { state.Commitments.AddRemoteProposal(msg)
                                        with RemoteNextHTLCId = state.Commitments.LocalNextHTLCId + 1UL }
                commitments1.LocalCommit.Spec.Reduce(commitments1.LocalChanges.ACKed, commitments1.RemoteChanges.Proposed)
                >>= fun reduced ->
                    Validation.checkTheirUpdateAddHTLCIsAcceptableWithCurrentSpec reduced commitments1 msg
                    *> Good [ WeAcceptedUpdateAddHTLC commitments1 ]
        | ChannelState.Normal state, FulfillHTLC cmd ->
            failwith ""
        | ChannelState.Normal state, ApplyUpdateFulfillHLTC msg ->
            failwith ""
        | ChannelState.Normal state, ApplyUpdateFulfillHLTC msg ->
            failwith ""
        | ChannelState.Normal state, FailHTLC cmd ->
            failwith ""
        | ChannelState.Normal state, ApplyUpdateFailHTLC msg ->
            failwith ""
        | ChannelState.Normal state, FailMalformedHTLC cmd ->
            failwith ""
        | ChannelState.Normal state, ApplyUpdateFailMalformedHTLC msg ->
            failwith ""
        | ChannelState.Normal state, UpdateFee cmd ->
            failwith ""
        | ChannelState.Normal state, ApplyUpdateFee msg ->
            failwith ""
        | ChannelState.Normal state, SignCommitment ->
            failwith ""

    let applyEvent c (e: ChannelEvent): Channel =
        match e with
        // --------- init fundee -----
        | WeAcceptedOpenChannel (nextMsg, data) ->
            let state = WaitForFundingCreated data
            { c with State = state }
        | WeAcceptedFundingCreated(nextMsg, data) ->
            let state = WaitForFundingConfirmed data
            { c with State = state}

        // --------- init funder -----
        | WeAcceptedAcceptChannel (nextMsg, data) ->
            {c with State = WaitForFundingSigned data}
        | WeAcceptedFundingSigned (txToPublish, data) ->
            { c with State = WaitForFundingConfirmed data}

        // --------- init both ------
        | FundingConfirmed data ->
            { c with State = WaitForFundingLocked data}
        | TheySentFundingLockedMsgBeforeUs msg ->
            match c.State with
            | WaitForFundingConfirmed s -> { c with State = WaitForFundingConfirmed( { s with Deferred = Some(msg)} ) }
            | WaitForFundingLocked s ->
                let feeBase = Helpers.getOurFeeBaseMSat c.FeeEstimator s.InitialFeeRatePerKw s.Commitments.LocalParams.IsFunder
                let channelUpdate = Helpers.makeChannelUpdate (c.Network.GenesisHash,
                                                               c.LocalNodeSecret,
                                                               c.RemoteNodeId,
                                                               s.ShortChannelId,
                                                               s.Commitments.LocalParams.ToSelfDelay,
                                                               s.Commitments.RemoteParams.HTLCMinimumMSat,
                                                               feeBase,
                                                               c.Config.ChannelOptions.FeeProportionalMillionths,
                                                               true,
                                                               None)
                let nextState = { NormalData.Buried = false;
                                  Commitments = s.Commitments
                                  ShortChannelId = s.ShortChannelId
                                  ChannelAnnouncement = None
                                  ChannelUpdate = channelUpdate
                                  LocalShutdown = None
                                  RemoteShutdown = None }
                { c with State = ChannelState.Normal nextState }
            | _ -> failwith "Unreachable! "
        | WeSentFundingLockedMsgBeforeThem msg ->
            match c.State with
            | WaitForFundingLocked prevState ->
                { c with State = WaitForFundingLocked { prevState with OurMessage = msg; HaveWeSentFundingLocked = true } }
            | _ -> failwith "unreachabble!"
        | BothFundingLocked data ->
            match c.State with
            | WaitForFundingSigned s ->
                { c with State = ChannelState.Normal data }
            | _ -> failwith "unreachale!"


        // ----- normal operation --------
        | WeAcceptedCMDAddHTLC(msg, newCommitments) ->
            match c.State with
            | ChannelState.Normal d -> { c with State = ChannelState.Normal ({ d with Commitments = newCommitments})  }
            | _ -> failwith "Unreachable!"
        | WeAcceptedUpdateAddHTLC(newCommitments) ->
            match c.State with
            | ChannelState.Normal d -> { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }
            | _ -> failwith "Unreachable!"

        // ----- else -----
        | NewBlockVerified height ->
            { c with CurrentBlockHeight = height }