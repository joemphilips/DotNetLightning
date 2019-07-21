namespace DotNetLightning.LN
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Utils.Aether
open DotNetLightning.Utils.Aether.Operators
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

type ChannelError =
    | Ignore of string
    | Close of string

exception ChannelException of ChannelError

module Channel =
    /// represents the user has than something wrong with this library
    let private RRApiE(e: APIError) =
        RResult.rbad(RBad.Object(e))

    let private RRApiMisuse(msg: string) =
        msg |> APIMisuseError |> RRApiE

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

        let buildLocalCommitmentSecret (seed, n) =
            ChannelUtils.buildCommitmentSecret(seed, n)
            |> fun sec -> Key(sec.ToBytes())

        let buildLocalCommitmentPoint (seed, n) =
            buildLocalCommitmentSecret(seed, n) |> fun k -> k.PubKey

        let getFundingRedeemScript (ck: ChannelPubKeys) (theirFundingPubKey: PubKey) : Script =
            let ourFundingKey = ck.FundingPubKey
            let pks = if ourFundingKey.ToBytes() < theirFundingPubKey.ToBytes() then
                          [| ourFundingKey; theirFundingPubKey |]
                      else
                          [| theirFundingPubKey; ourFundingKey |]
            PayToMultiSigTemplate.Instance.GenerateScriptPubKey(2, pks)

        let getFundingSCoin(ck: ChannelPubKeys) (theirFundingPubKey: PubKey) (TxId fundingTxId) (TxOutIndex fundingOutputIndex) (fundingSatoshis) : ScriptCoin =
            let redeem = getFundingRedeemScript ck theirFundingPubKey
            Coin(fundingTxId, uint32 fundingOutputIndex, fundingSatoshis, redeem.WitHash.ScriptPubKey)
            |> fun c -> ScriptCoin(c, redeem)

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
                let scoin = getFundingSCoin (localParams.ChannelPubKeys) (remoteParams.FundingPubKey) (fundingTxId) (fundingOutputIndex) (fundingSatoshis)
                let localCommitTx =
                    Transactions.makeCommitTx scoin
                                              0UL
                                              localParams.ChannelPubKeys.PaymentBasePubKey
                                              remoteParams.PaymentBasePoint
                                              localParams.IsFunder
                                              localParams.DustLimitSatoshis
                                              localParams.ChannelPubKeys.RevocationBasePubKey
                                              localParams.ToSelfDelay
                                              (localParams.ChannelPubKeys.DelayedPaymentBasePubKey)
                                              (remoteParams.PaymentBasePoint)
                                              (localParams.ChannelPubKeys.HTLCBasePubKey)
                                              (remoteParams.HTLCBasePoint)
                                              localSpec
                                              n
                let remoteCommitTx =
                    Transactions.makeCommitTx scoin
                                              0UL
                                              remoteParams.PaymentBasePoint
                                              localParams.ChannelPubKeys.PaymentBasePubKey
                                              (not localParams.IsFunder)
                                              (remoteParams.DustLimitSatoshis)
                                              (remoteParams.RevocationBasePoint)
                                              (remoteParams.ToSelfDelay)
                                              (remoteParams.DelayedPaymentBasePoint)
                                              (localParams.ChannelPubKeys.PaymentBasePubKey)
                                              (remoteParams.HTLCBasePoint)
                                              (localParams.ChannelPubKeys.HTLCBasePubKey)
                                              remoteSpec
                                              n

                (localSpec, localCommitTx, remoteSpec, remoteCommitTx) |> Good

            if (not localParams.IsFunder) then
                checkTheyCanAffordFee() *>  (makeFirstCommitTxCore())
            else
                makeFirstCommitTxCore()

        let isAlreadySent (htlc: UpdateAddHTLC) (proposed: IUpdateMsg list) =
            proposed
            |> List.exists(fun p -> match p with
                                    | :? UpdateFulfillHTLC as u -> u.HTLCId = htlc.HTLCId
                                    | :? UpdateFailHTLC as u -> u.HTLCId = htlc.HTLCId
                                    | :? UpdateFailMalformedHTLC as u -> u.HTLCId = htlc.HTLCId
                                    | _ -> false)
        // facades
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

        let sortBothHTLCs (htlcTimeoutTxs: HTLCTimeoutTx list) (htlcSuccessTxs: HTLCSuccessTx list) =
            let timeoutTXsV = (htlcTimeoutTxs |> Seq.cast<IHTLCTx>)
            let successTXsV = (htlcSuccessTxs |> Seq.cast<IHTLCTx>)
            Seq.append timeoutTXsV successTXsV
            |> List.ofSeq
            |> List.sortBy(fun htlc -> htlc.Value.GetGlobalTransaction().Inputs.[htlc.WhichInput].PrevOut.N)

    module internal Validation =

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


        module private UpdateAddHTLCValidator =
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

        module private UpdateFeeValidator =
            let private feeRateMismatch (FeeRatePerKw remote, FeeRatePerKw local) =
                (2.0 * float (remote - local) / float (remote + local))
                |> abs
            let checkFeeDiffTooHigh (remoteFeeRatePerKw: FeeRatePerKw) (localFeeRatePerKw: FeeRatePerKw) (maxFeeRateMismatchRatio) =
                let diff = feeRateMismatch(remoteFeeRatePerKw, localFeeRatePerKw)
                if (diff > maxFeeRateMismatchRatio) then
                    sprintf "FeeReate Delta is too big. Local: %A ; remote %A ; So it will be %.2f%% higher. But it must be lower than %.2f%%"
                            (localFeeRatePerKw) (remoteFeeRatePerKw) (diff * 100.0) (maxFeeRateMismatchRatio * 100.0)
                    |> RRClose
                else
                    Good ()

        let checkUpdateFee (config: UserConfig) (msg: UpdateFee) (localFeeRate: FeeRatePerKw) =
            let maxMismatch = config.ChannelOptions.MaxFeeRateMismatchRatio
            UpdateFeeValidator.checkFeeDiffTooHigh (msg.FeeRatePerKw) (localFeeRate) (maxMismatch)

    let executeCommand (cs: Channel) (command: ChannelCommand): RResult<ChannelEvent list> =
        match cs.State, command with

        // --------------- open channel procedure: case we are fundee -------------
        | WaitForOpenChannel state, ApplyOpenChannel msg ->
            Validation.checkOpenChannelMsgAcceptable cs msg
            |>> fun _ ->
                let localParams = state.InitFundee.LocalParams
                let channelKeys = cs.KeysRepository.GetChannelKeys(true)
                let localCommitmentSecret = Helpers.buildLocalCommitmentSecret (channelKeys.CommitmentSeed, 0UL)
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
        | WaitForOpenChannel state, ChannelCommand.Close spk ->  [ChannelEvent.Closed] |> Good

        | WaitForFundingCreated state, ApplyFundingCreated msg ->
            Helpers.makeFirstCommitTxs state.LocalParams
                                       state.RemoteParams
                                       state.FundingSatoshis
                                       state.PushMSat
                                       state.InitialFeeRatePerKw
                                       msg.FundingOutputIndex
                                       msg.FundingTxId
                                       cs.Network
            >>= fun (localSpec, localCommitTx, remoteSpec, remoteCommitTx) ->
                match (localCommitTx.Value.IsReadyToSign()) with
                | false -> failwith "unreachable"
                | true ->
                    let s, signedLocalCommitTx =
                        cs.KeysRepository.GetSignatureFor(localCommitTx.Value, state.LocalParams.ChannelPubKeys.FundingPubKey)
                    let theirSigPair = (state.RemoteParams.FundingPubKey , TransactionSignature(msg.Signature, SigHash.All))
                    let sigPairs = seq [theirSigPair]
                    Transactions.checkTxFinalized (signedLocalCommitTx) (localCommitTx.WhichInput) sigPairs
                    >>>= fun e -> RRClose(e.Describe())
                    >>= fun finalizedCommitTx ->
                        let localSigOfRemoteCommit, _ = cs.KeysRepository.GetSignatureFor(remoteCommitTx.Value, state.LocalParams.ChannelPubKeys.FundingPubKey)
                        let channelId = OutPoint(msg.FundingTxId.Value, uint32 msg.FundingOutputIndex.Value).ToChannelId()
                        let msgToSend: FundingSigned = { ChannelId = channelId; Signature = localSigOfRemoteCommit.Signature }
                        let commitments = { Commitments.LocalParams = state.LocalParams
                                            RemoteParams = state.RemoteParams
                                            ChannelFlags = state.ChannelFlags
                                            FundingSCoin = Helpers.getFundingSCoin state.LocalParams.ChannelPubKeys state.RemoteParams.FundingPubKey msg.FundingTxId msg.FundingOutputIndex state.FundingSatoshis
                                            LocalCommit = { LocalCommit.Index = 0UL;
                                                            Spec = localSpec
                                                            PublishableTxs = { PublishableTxs.CommitTx = finalizedCommitTx;
                                                                               HTLCTxs = [] }
                                                            PendingHTLCSuccessTxs = []  }
                                            RemoteCommit = { RemoteCommit.Index = 0UL;
                                                             Spec = remoteSpec
                                                             TxId = remoteCommitTx.Value.GetGlobalTransaction().GetTxId()
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
                        |> Good

        // --------------- open channel procedure: case we are funder -------------
        | WaitForAcceptChannel state, ApplyAcceptChannel msg ->
            Validation.checkAcceptChannelMsgAccpetable cs state msg
            >>= fun _ ->
                let redeem = state.InputInitFunder.ChannelKeys.ToChannelPubKeys() |> Helpers.getFundingRedeemScript <| (msg.FundingPubKey)
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
                |>> fun (localSpec, localCommitTx, remoteSpec, remoteCommitTx) ->
                    let localSigOfRemoteCommit, _ = (cs.KeysRepository.GetSignatureFor(remoteCommitTx.Value, state.LastSent.FundingPubKey))
                    let nextMsg = { FundingCreated.TemporaryChannelId = msg.TemporaryChannelId
                                    FundingTxId = fundingTx.Value.GetTxId()
                                    FundingOutputIndex = outIndex
                                    Signature = localSigOfRemoteCommit.Signature }

                    let data = { Data.WaitForFundingSignedData.ChannelId = msg.TemporaryChannelId
                                 LocalParams = localParams
                                 RemoteParams = remoteParams
                                 Data.WaitForFundingSignedData.FundingTx = fundingTx
                                 Data.WaitForFundingSignedData.LocalSpec =  commitmentSpec
                                 LocalCommitTx = localCommitTx
                                 RemoteCommit = { RemoteCommit.Index = 0UL;
                                                  Spec = remoteSpec
                                                  TxId = remoteCommitTx.Value.GetGlobalTransaction().GetTxId()
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
                let remoteSigPairOfLocalTx = (state.LocalParams.ChannelPubKeys.FundingPubKey, TransactionSignature(msg.Signature, SigHash.All))
                let sigPairs = seq [ remoteSigPairOfLocalTx; localSigPairOfLocalTx ]
                Transactions.checkTxFinalized state.LocalCommitTx.Value state.LocalCommitTx.WhichInput sigPairs
            finalLocalCommitTxRR
            >>>= fun e -> RRClose(e.Describe())
            |>> fun finalizedLocalCommitTx ->
                let commitments = { Commitments.LocalParams = state.LocalParams
                                    RemoteParams = state.RemoteParams
                                    ChannelFlags = state.ChannelFlags
                                    FundingSCoin =
                                        let amount = state.FundingTx.Value.Outputs.[int state.LastSent.FundingOutputIndex.Value].Value
                                        Helpers.getFundingSCoin state.LocalParams.ChannelPubKeys
                                                                state.RemoteParams.FundingPubKey
                                                                state.LastSent.FundingTxId
                                                                state.LastSent.FundingOutputIndex
                                                                amount
                                    LocalCommit = { Index = 0UL;
                                                    Spec = state.LocalSpec;
                                                    PublishableTxs = { PublishableTxs.CommitTx = finalizedLocalCommitTx
                                                                       HTLCTxs = [] }
                                                    PendingHTLCSuccessTxs = []  }
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
                                        cs.Logger (LogLevel.Debug) (sprintf "Channel id has been set to %A" msg.ChannelId)
                                        msg.ChannelId }
                let nextState = { WaitForFundingConfirmedData.Commitments = commitments
                                  Deferred = None
                                  LastSent = Choice1Of2 state.LastSent
                                  InitialFeeRatePerKw = state.InitialFeeRatePerKw }
                [WeAcceptedFundingSigned (state.FundingTx, nextState)]
        | WaitForFundingConfirmed state, ApplyFundingLocked msg ->
            [ TheySentFundingLockedMsgBeforeUs msg ] |> Good
        | WaitForFundingConfirmed state, ApplyFundingConfirmedOnBC (height, txindex, depth) ->
            cs.Logger (LogLevel.Info) (sprintf "ChannelId %A was confirmed at blockheight %A; depth: %A" state.Commitments.ChannelId height.Value depth)
            let nextPerCommitmentPoint =
                Helpers.buildLocalCommitmentPoint (state.Commitments.LocalParams.ChannelPubKeys.CommitmentSeed, 1UL)
            let msgToSend: FundingLocked = { ChannelId = state.Commitments.ChannelId; NextPerCommitmentPoint = nextPerCommitmentPoint }

            // This is temporary channel id that we will use in our channel_update message, the goal is to be able to use our channel
            // as soon as it reaches NORMAL state, and before it is announced on the network
            // (this id might be updated when the funding tx gets deeplly buried, if there was a reorg in the meantime)
            // this is not specified in BOLT.
            let shortChannelId = { ShortChannelId.BlockHeight = height;
                                   BlockIndex = txindex
                                   TxOutIndex = state.Commitments.FundingSCoin.Outpoint.N |> uint16 |> TxOutIndex }
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
                    cs.Logger (LogLevel.Error) (msg)
                    RRClose(msg)
            else
                if (cs.Config.OwnChannelConfig.MinimumDepth <= depth) then
                    let nextPerCommitmentPoint =
                        ChannelUtils.buildCommitmentSecret (state.Commitments.LocalParams.ChannelPubKeys.CommitmentSeed, 1UL)
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
            match state.Commitments.GetHTLCCrossSigned(Direction.In, cmd.Id) with
            | Some htlc when (state.Commitments.LocalChanges.Proposed |> Helpers.isAlreadySent htlc) ->
                sprintf "We have already sent a fail/fulfill for this htlc: %A" htlc
                |> RRApiMisuse
            | Some htlc when (htlc.PaymentHash = cmd.PaymentPreimage.GetHash()) ->
                let msgToSend: UpdateFulfillHTLC = { ChannelId = state.Commitments.ChannelId; HTLCId = cmd.Id; PaymentPreimage = cmd.PaymentPreimage }
                let newCommitments = state.Commitments.AddLocalProposal(msgToSend)
                [ WeAcceptedCMDFulfillHTLC (msgToSend, newCommitments)] |> Good
            | Some htlc ->
                sprintf "Invalid HTLC PreImage %A. Hash (%A) does not match the one expected %A"
                        cmd.PaymentPreimage
                        (cmd.PaymentPreimage.GetHash())
                        (htlc.PaymentHash)
                |> RRApiMisuse
            | None ->
                sprintf "Unknown HTLCId (%A)" cmd.Id
                |> RRApiMisuse
        | ChannelState.Normal state, ApplyUpdateFulfillHLTC msg ->
            match state.Commitments.GetHTLCCrossSigned(Direction.Out, msg.HTLCId) with
            | Some htlc when htlc.PaymentHash = msg.PaymentPreimage.GetHash() ->
                let commitments = state.Commitments.AddRemoteProposal(msg)
                let origin = state.Commitments.OriginChannels |> Map.find(msg.HTLCId)
                [WeAcceptedFulfillHTLC(msg, origin, htlc, commitments)] |> Good
            | Some htlc ->
                sprintf "Invalid HTLC PreImage %A. Hash (%A) does not match the one expected %A"
                        msg.PaymentPreimage
                        (msg.PaymentPreimage.GetHash())
                        (htlc.PaymentHash)
                |> RRClose
            | None ->
                sprintf "Unknown HTLCId (%A)" msg.HTLCId
                |> RRClose
        | ChannelState.Normal state, FailHTLC cmd ->
            match state.Commitments.GetHTLCCrossSigned(Direction.In, cmd.Id) with
            | Some htlc when  (state.Commitments.LocalChanges.Proposed |> Helpers.isAlreadySent htlc) ->
                sprintf "We have already sent a fail/fulfill for this htlc: %A" htlc
                |> RRApiMisuse
            | Some htlc ->
                let localKey = cs.LocalNodeSecret
                let ad = htlc.PaymentHash.ToBytes()
                let rawPacket = htlc.OnionRoutingPacket.ToBytes()
                Sphinx.parsePacket localKey ad rawPacket
                >>= fun ({ SharedSecret = ss}) ->
                    let reason =
                        cmd.Reason
                        |> function Choice1Of2 b -> Sphinx.forwardErrorPacket(b, ss) | Choice2Of2 f -> Sphinx.ErrorPacket.Create(ss, f)
                    let f = { UpdateFailHTLC.ChannelId = state.Commitments.ChannelId
                              HTLCId = cmd.Id
                              Reason = { Data = reason } }
                    let nextComitments = state.Commitments.AddLocalProposal(f)
                    [ WeAcceptedCMDFailHTLC(f, nextComitments) ]
                    |> Good
                >>>= fun e -> RRApiMisuse(e.Describe())
            | None ->
                sprintf "Unknown HTLCId (%A)" cmd.Id
                |> RRApiMisuse
        | ChannelState.Normal state, FailMalformedHTLC cmd ->
            // BADONION bit must be set in failure code
            if ((cmd.FailureCode.Value &&& Error.BADONION) = 0us) then
                sprintf "invalid failure code %A" (cmd.FailureCode.GetOnionErrorDescription())
                |> RRApiMisuse
            else
                match state.Commitments.GetHTLCCrossSigned(Direction.In, cmd.Id) with
                | Some htlc when (state.Commitments.LocalChanges.Proposed |> Helpers.isAlreadySent htlc) ->
                    sprintf "We have already sent a fail/fulfill for this htlc: %A" htlc
                    |> RRApiMisuse
                | Some htlc ->
                    let msg = { UpdateFailMalformedHTLC.ChannelId = state.Commitments.ChannelId
                                HTLCId = cmd.Id
                                Sha256OfOnion = cmd.Sha256OfOnion
                                FailureCode = cmd.FailureCode }
                    let nextCommitments = state.Commitments.AddLocalProposal(msg)
                    [ WeAcceptedCMDFailMalformedHTLC(msg, nextCommitments) ]
                    |> Good
                | None ->
                    sprintf "Unknown HTLCId (%A)" cmd.Id |> RRClose
        | ChannelState.Normal state, ApplyUpdateFailHTLC msg ->
            match state.Commitments.GetHTLCCrossSigned(Direction.Out, msg.HTLCId) with
            | Some htlc ->
                match state.Commitments.OriginChannels.TryGetValue(msg.HTLCId) with
                | true, origin -> Good origin
                | false, _ -> sprintf "Invalid HTLCId, Unknown Origin. HTLCId (%A)" msg.HTLCId |> RRClose
                >>= fun o ->
                    let nextC = state.Commitments.AddRemoteProposal(msg)
                    [WeAcceptedFailHTLC(o, htlc, nextC)]
                    |> Good
            | None ->
                sprintf "Unknown HTLCId (%A)" msg.HTLCId |> RRClose
        | ChannelState.Normal state, ApplyUpdateFailMalformedHTLC msg ->
            if msg.FailureCode.Value &&& Error.BADONION = 0us then
                sprintf "invalid failure code %A" (msg.FailureCode.GetOnionErrorDescription()) |> RRClose
            else
                match state.Commitments.GetHTLCCrossSigned(Direction.Out, msg.HTLCId) with
                | Some htlc ->
                    match state.Commitments.OriginChannels.TryGetValue(msg.HTLCId) with
                    | true, o -> Good o
                    | false, _ -> sprintf "Invalid HTLCId, Unknown Origin. HTLCId(%A)" msg.HTLCId |> RRClose
                    >>= fun o ->
                        let nextC = state.Commitments.AddRemoteProposal(msg)
                        [WeAcceptedFailMalformedHTLC(o, htlc, nextC)]
                        |> Good
                | None ->
                    sprintf "Unknown HTLCId (%A)" msg.HTLCId |> RRClose
        | ChannelState.Normal state, UpdateFee cmd ->
            if (not state.Commitments.LocalParams.IsFunder) then
                "Local is Fundee so it cannot send update fee" |> RRApiMisuse
            else
                let fee = { UpdateFee.ChannelId = state.Commitments.ChannelId
                            FeeRatePerKw = cmd.FeeRatePerKw }
                let c1 = state.Commitments.AddLocalProposal(fee)
                c1.RemoteCommit.Spec.Reduce(c1.RemoteChanges.ACKed, c1.LocalChanges.Proposed)
                >>= fun reduced ->
                    // A node cannot spend pending incoming htlcs, and need to keep funds above the reserve required by
                    // the counterparty, after paying the fee, we look from remote'S point of view, so if local is funder
                    // remote doesn'T pay the fees.
                    let fees = Transactions.commitTxFee(c1.RemoteParams.DustLimitSatoshis) reduced
                    let missing = reduced.ToRemote.ToMoney() - c1.RemoteParams.ChannelReserveSatoshis - fees
                    if (missing < Money.Zero) then
                        sprintf "Cannot affored fees. Missing Satoshis are: %A . Reserve Satoshis are %A . Fees are %A" (-1 * missing) (c1.LocalParams.ChannelReserveSatoshis) (fees)
                        |> RRIgnore
                    else
                        [ WeAcceptedCMDUpdateFee(fee, c1) ]
                        |> Good
        | ChannelState.Normal state, ApplyUpdateFee msg ->
            if (state.Commitments.LocalParams.IsFunder) then
                "Remote is Fundee so it cannot send update fee" |> RRClose
            else
                let localFeerate = cs.FeeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority)
                Validation.checkUpdateFee (cs.Config) (msg) (localFeerate)
                >>= fun _ ->
                    let c1 = state.Commitments.AddRemoteProposal(msg)
                    c1.LocalCommit.Spec.Reduce(c1.LocalChanges.ACKed, c1.RemoteChanges.Proposed)
                    >>= fun reduced ->
                        let fees = Transactions.commitTxFee(c1.RemoteParams.DustLimitSatoshis) reduced
                        let missing = reduced.ToRemote.ToMoney() - c1.RemoteParams.ChannelReserveSatoshis - fees
                        if (missing < Money.Zero) then
                            sprintf "Cannot affored fees. Missing Satoshis are: %A . Reserve Satoshis are %A . Fees are %A" (-1 * missing) (c1.LocalParams.ChannelReserveSatoshis) (fees)
                            |> RRClose
                        else
                            [ WeAcceptedUpdateFee msg ]
                            |> Good
        | ChannelState.Normal state, SignCommitment ->
            let cm = state.Commitments
            match cm.RemoteNextCommitInfo with
            | _ when (cm.LocalHasChanges() |> not) ->
                sprintf "Ignoring SignCommitment Command (nothing to sign)" |> RRIgnore
            | Choice2Of2 _ ->
                match cm.RemoteNextCommitInfo with
                | Choice2Of2 remoteNextPerCommitmentPoint ->
                    // remote commitment will include all local changes + remote acked changes
                    cm.RemoteCommit.Spec.Reduce(cm.RemoteChanges.ACKed, cm.LocalChanges.Proposed)
                    >>= fun spec ->
                        let localKeys = cs.KeysRepository.GetChannelKeys(not cm.LocalParams.IsFunder)
                        Helpers.makeRemoteTxs (localKeys)
                                              (cm.RemoteCommit.Index + 1UL)
                                              (cm.LocalParams)
                                              (cm.RemoteParams)
                                              (cm.FundingSCoin)
                                              (remoteNextPerCommitmentPoint)
                                              (spec) cs.Network
                        >>= fun (remoteCommitTx, htlcTimeoutTxs, htlcSuccessTxs) ->
                            let signature,_ = cs.KeysRepository.GetSignatureFor(remoteCommitTx.Value, cm.LocalParams.ChannelPubKeys.FundingPubKey)
                            let sortedHTLCTXs = Helpers.sortBothHTLCs htlcTimeoutTxs htlcSuccessTxs
                            let htlcSigs =
                                sortedHTLCTXs
                                |> List.map(
                                        (fun htlc -> cs.KeysRepository.GenerateKeyFromBasePointAndSign(htlc.Value, cm.LocalParams.ChannelPubKeys.HTLCBasePubKey, remoteNextPerCommitmentPoint))
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
                                                             ReAsignASAP = false }

                                { cm with RemoteNextCommitInfo = Choice1Of2(nextRemoteCommitInfo)
                                          LocalChanges = { cm.LocalChanges with Proposed = []; Signed = cm.LocalChanges.Proposed }
                                          RemoteChanges = { cm.RemoteChanges with ACKed = []; Signed = cm.RemoteChanges.ACKed } }
                            [ WeAcceptedCMDSign (msg, nextCommitments) ] |> Good

                | Choice1Of2 _ ->
                    "Can not sign before Revocation"
                    |> RRApiMisuse
            | Choice1Of2 _ ->
                sprintf "Already in the process of signing."
                |> RRIgnore

        | ChannelState.Normal state, ApplyCommitmentSigned msg ->
            let cm = state.Commitments
            if cm.RemoteHasChanges() |> not then
                sprintf "Remote has sent commitment_signed but we have no pending changes" |> RRClose
            else
                let chanKeys = cm.LocalParams.ChannelPubKeys
                let nextI = cm.LocalCommit.Index + 1UL
                cm.LocalCommit.Spec.Reduce(cm.LocalChanges.ACKed, cm.RemoteChanges.Proposed)
                >>= fun spec ->
                    let localPerCommitmentPoint = Helpers.buildLocalCommitmentPoint (chanKeys.CommitmentSeed, nextI)
                    Helpers.makeLocalTXs chanKeys (nextI) (cm.LocalParams) (cm.RemoteParams) (cm.FundingSCoin) (localPerCommitmentPoint) spec cs.Network
                    >>= fun (localCommitTx, htlcTimeoutTxs, htlcSuccessTxs) ->
                        let signature, signedCommitTx = cs.KeysRepository.GetSignatureFor (localCommitTx.Value, chanKeys.FundingPubKey)

                        let sigPair =
                            let localSigPair = seq [(chanKeys.FundingPubKey, signature)]
                            let remoteSigPair = seq[ (cm.RemoteParams.FundingPubKey, TransactionSignature(msg.Signature, SigHash.All)) ]
                            Seq.append localSigPair remoteSigPair
                        Transactions.checkTxFinalized signedCommitTx localCommitTx.WhichInput sigPair
                        >>= fun finalizedCommitTx ->
                            let sortedHTLCTXs = Helpers.sortBothHTLCs htlcTimeoutTxs htlcSuccessTxs
                            Validation.checkOrClose sortedHTLCTXs.Length (<>) msg.HTLCSignatures.Length "Number of signatures went from the remote (%A) does not match the number expected (%A)"
                            >>= fun _ ->
                                let localHTLCSigs, sortedHTLCTXs =
                                    let localHtlcSigsAndHTLCTxs = sortedHTLCTXs |> List.map(fun htlc -> cs.KeysRepository.GenerateKeyFromBasePointAndSign(htlc.Value, cm.LocalParams.ChannelPubKeys.HTLCBasePubKey, localPerCommitmentPoint))
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
                                        Helpers.buildLocalCommitmentSecret(cm.LocalParams.ChannelPubKeys.CommitmentSeed, cm.LocalCommit.Index)
                                    let localNextPerCommitmentPoint =
                                        Helpers.buildLocalCommitmentPoint(cm.LocalParams.ChannelPubKeys.CommitmentSeed, cm.LocalCommit.Index + 2UL)

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
                            >>>= fun e -> RRClose("Something unexpected happend while handling commitment_signed from remote" + e.Describe())

        | ChannelState.Normal state, ApplyRevokeAndACK msg ->
            let cm = state.Commitments
            match cm.RemoteNextCommitInfo with
            | Choice1Of2 _ when (msg.PerCommitmentSecret.ToPubKey() <> cm.RemoteCommit.RemotePerCommitmentPoint) ->
                sprintf "Invalid revoke_and_ack %A; must be %A" msg.PerCommitmentSecret cm.RemoteCommit.RemotePerCommitmentPoint
                |> RRClose
            | Choice1Of2 ({ NextRemoteCommit = theirNextCommit }) ->
                let commitments1 = { cm  with LocalChanges = { cm.LocalChanges with Signed = []; ACKed = cm.LocalChanges.ACKed @ cm.LocalChanges.Signed }
                                              RemoteChanges = { cm.RemoteChanges with Signed = [] }
                                              RemoteCommit = theirNextCommit
                                              RemoteNextCommitInfo = Choice2Of2(msg.NextPerCommitmentPoint)
                                              RemotePerCommitmentSecrets = cm.RemotePerCommitmentSecrets.AddHash(msg.PerCommitmentSecret.ToBytes(), 0xffffffffffffUL - cm.RemoteCommit.Index )}
                [ WeAcceptedRevokeAndACK (commitments1) ] |> Good
            | Choice2Of2 _ ->
                sprintf "Unexpected revocation"
                |> RRClose








    let applyEvent c (e: ChannelEvent): Channel =
        match e, c.State with
        // --------- init fundee -----
        | WeAcceptedOpenChannel (nextMsg, data), WaitForOpenChannel _ ->
            let state = WaitForFundingCreated data
            { c with State = state }
        | WeAcceptedFundingCreated(nextMsg, data), WaitForFundingCreated _ ->
            let state = WaitForFundingConfirmed data
            { c with State = state}

        // --------- init funder -----
        | WeAcceptedAcceptChannel (nextMsg, data), WaitForAcceptChannel _ ->
            {c with State = WaitForFundingSigned data}
        | WeAcceptedFundingSigned (txToPublish, data), WaitForFundingSigned _ ->
            { c with State = WaitForFundingConfirmed data}

        // --------- init both ------
        | FundingConfirmed data, WaitForFundingConfirmed _ ->
            { c with State = WaitForFundingLocked data}
        | TheySentFundingLockedMsgBeforeUs msg, WaitForFundingConfirmed s ->
            { c with State = WaitForFundingConfirmed( { s with Deferred = Some(msg)} ) }
        | TheySentFundingLockedMsgBeforeUs msg, WaitForFundingLocked s ->
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
        | WeSentFundingLockedMsgBeforeThem msg, WaitForFundingLocked prevState ->
            { c with State = WaitForFundingLocked { prevState with OurMessage = msg; HaveWeSentFundingLocked = true } }
        | BothFundingLocked data, WaitForFundingSigned s ->
            { c with State = ChannelState.Normal data }

        // ----- normal operation --------
        | WeAcceptedCMDAddHTLC(msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal ({ d with Commitments = newCommitments})  }
        | WeAcceptedUpdateAddHTLC(newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal ({ d with Commitments = newCommitments})  }

        | WeAcceptedCMDFulfillHTLC (msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }
        | WeAcceptedFulfillHTLC (msg, origin, htlc, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }

        | WeAcceptedCMDFailHTLC (msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }
        | WeAcceptedFailHTLC (origin, msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments })}
        
        | WeAcceptedCMDFailMalformedHTLC(msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }
        | WeAcceptedFailMalformedHTLC(origin, msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }

        | WeAcceptedCMDUpdateFee(msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }
        | WeAcceptedUpdateFee(msg), ChannelState.Normal d -> c

        | WeAcceptedCMDSign(msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }
        | WeAcceptedCommitmentSigned(msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }

        | WeAcceptedRevokeAndACK(newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }

        // ----- else -----
        | NewBlockVerified height, _ ->
            { c with CurrentBlockHeight = height }