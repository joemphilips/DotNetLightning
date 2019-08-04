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
open Secp256k1Net

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
    Secp256k1Context: Secp256k1
}
    with
        static member Create (ctx, config, userId, logger, chainListener, keysRepo, feeEstimator, remoteNodeId, localNodeSecret, fundingTxProvider, n) =
            {
                Secp256k1Context = ctx
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
        static member CreateCurried  = curry11 (Channel.Create)

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

    let private hex = NBitcoin.DataEncoders.HexEncoder()
    let private ascii = System.Text.ASCIIEncoding.ASCII
    let private dummyPrivKey = Key(hex.DecodeData("0101010101010101010101010101010101010101010101010101010101010101"))
    let private dummyPubKey = dummyPrivKey.PubKey
    let private dummySig =
        "01010101010101010101010101010101" |> ascii.GetBytes
        |> uint256
        |> fun m -> dummyPrivKey.SignCompact(m ,false)
        |> fun d -> Crypto.ECDSASignature.FromBytesCompact(d, true)
        |> fun ecdsaSig -> TransactionSignature(ecdsaSig, SigHash.All)


    let private checkOrClose left predicate right msg =
        if predicate left right then
            sprintf msg left right |> RRClose
        else
            Good ()

    let private checkOrIgnore left predicate right msg =
        if predicate left right then
            sprintf msg left right |> RRIgnore
        else
            Good ()


    module internal Helpers =
        let deriveOurDustLimitSatoshis(feeEstimator: IFeeEstimator): Money =
            let (FeeRatePerKw atOpenBackGroundFee) = feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Background)
            (Money.Satoshis((uint64 atOpenBackGroundFee) * B_OUTPUT_PLUS_SPENDING_INPUT_WEIGHT / 1000UL), Money.Satoshis(546UL))
            |> Money.Max

        let getOurChannelReserve (channelValue: Money) =
            let q = channelValue / 100L
            Money.Min(channelValue, Money.Max(q, Money.Satoshis(1L)))

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

        // facades

        module Closing =
            let makeClosingTx(keyRepo: IKeysRepository, cm: Commitments, localSpk: Script, remoteSpk: Script, closingFee: Money, localFundingPk, n) =
                assert (Scripts.isValidFinalScriptPubKey(remoteSpk) && Scripts.isValidFinalScriptPubKey(localSpk))
                let dustLimitSatoshis = Money.Max(cm.LocalParams.DustLimitSatoshis, cm.RemoteParams.DustLimitSatoshis)
                Transactions.makeClosingTx(cm.FundingSCoin) (localSpk) (remoteSpk) (cm.LocalParams.IsFunder) (dustLimitSatoshis) (closingFee) (cm.LocalCommit.Spec) n
                |>> fun closingTx ->
                    let localSignature, psbtUpdated = keyRepo.GetSignatureFor(closingTx.Value, localFundingPk)
                    let msg = { ClosingSigned.ChannelId = cm.ChannelId
                                FeeSatoshis = closingFee
                                Signature = localSignature.Signature }
                    (ClosingTx psbtUpdated, msg)

            let firstClosingFee(cm: Commitments, localSpk: Script, remoteSpk: Script, feeEst: IFeeEstimator, n) =
                Transactions.makeClosingTx cm.FundingSCoin localSpk remoteSpk cm.LocalParams.IsFunder Money.Zero Money.Zero cm.LocalCommit.Spec n
                |>> fun dummyClosingTx ->
                    let tx = dummyClosingTx.Value.GetGlobalTransaction()
                    tx.Inputs.[0].WitScript <-
                        let witness = seq [dummySig.ToBytes(); dummySig.ToBytes(); dummyClosingTx.Value.Inputs.[0].WitnessScript.ToBytes()] |> Array.concat
                        Script(witness).ToWitScript()
                    let feeRatePerKw = FeeRatePerKw.Max(feeEst.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority), cm.LocalCommit.Spec.FeeRatePerKw)
                    let vsize = tx.GetVirtualSize()
                    feeRatePerKw.ToFee(uint64 vsize)

            let makeFirstClosingTx(keyRepo, commitments, localSpk, remoteSpk, feeEst, localFundingPk, n) = 
                firstClosingFee(commitments, localSpk, remoteSpk, feeEst ,n)
                >>= fun closingFee ->
                    makeClosingTx(keyRepo, commitments, localSpk, remoteSpk, closingFee, localFundingPk, n)

            let nextClosingFee(localClosingFee: Money, remoteClosingFee: Money) =
                ((localClosingFee.Satoshi + remoteClosingFee.Satoshi) / 4L) * 2L
                |> Money.Satoshis

            let handleMutualClose (closingTx: FinalizedTx, d: Choice<NegotiatingData, ClosingData>) =
                let nextData =
                    match d with
                    | Choice1Of2 negotiating -> 
                        ClosingData.Create(negotiating.ChannelId, negotiating.Commitments, None, DateTime.Now, (negotiating.ClosingTxProposed |> List.collect id |> List.map(fun tx -> tx.UnsignedTx)), closingTx::[])
                    | Choice2Of2 closing -> { closing with MutualClosePublished  = closingTx::closing.MutualClosePublished }
                [ MutualClosePerformed nextData ]
                |> Good

            let claimCurrentLocalCommitTxOutputs(keyRepo: IKeysRepository, channelPubKeys: ChannelPubKeys, commitments: Commitments, commitTx: CommitTx) =
                checkOrClose (commitments.LocalCommit.PublishableTxs.CommitTx.Value.GetTxId()) (=) (commitTx.Value.GetTxId()) "txid mismatch. provided txid (%A) does not match current local commit tx (%A)"
                >>= fun _ ->
                    let localPerCommitmentPoint = ChannelUtils.buildCommitmentPoint (channelPubKeys.CommitmentSeed, commitments.LocalCommit.Index)
                    let localRevocationPubKey = Generators.revocationPubKey
                    failwith ""

    module internal Validation =

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


    let executeCommand (cs: Channel) (command: ChannelCommand): RResult<ChannelEvent list> =
        match cs.State, command with

        // --------------- open channel procedure: case we are fundee -------------
        | WaitForOpenChannel state, ApplyOpenChannel msg ->
            Validation.checkOpenChannelMsgAcceptable cs msg
            |>> fun _ ->
                let localParams = state.InitFundee.LocalParams
                let channelKeys = cs.KeysRepository.GetChannelKeys(true)
                let localCommitmentSecret = ChannelUtils.buildCommitmentSecret (channelKeys.CommitmentSeed, 0UL)
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
                                          InitialFeeRatePerKw = state.InitialFeeRatePerKw
                                          ChannelId = channelId }
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
                                  InitialFeeRatePerKw = state.InitialFeeRatePerKw
                                  ChannelId = msg.ChannelId}
                [WeAcceptedFundingSigned (state.FundingTx, nextState)]
        | WaitForFundingConfirmed state, ApplyFundingLocked msg ->
            [ TheySentFundingLockedMsgBeforeUs msg ] |> Good
        | WaitForFundingConfirmed state, ApplyFundingConfirmedOnBC (height, txindex, depth) ->
            cs.Logger (LogLevel.Info) (sprintf "ChannelId %A was confirmed at blockheight %A; depth: %A" state.Commitments.ChannelId height.Value depth)
            let nextPerCommitmentPoint =
                ChannelUtils.buildCommitmentPoint (state.Commitments.LocalParams.ChannelPubKeys.CommitmentSeed, 1UL)
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
                              InitialFeeRatePerKw = state.InitialFeeRatePerKw
                              ChannelId = state.Commitments.ChannelId }
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
                                  RemoteShutdown = None
                                  ChannelId = state.ChannelId }
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
            state.Commitments |> Commitments.sendFulfill (cmd)
            |>> fun t -> [ WeAcceptedCMDFulfillHTLC (t) ]
            >>>= fun e -> RRApiMisuse (e.Describe())

        | ChannelState.Normal state, ChannelCommand.ApplyUpdateFulfillHTLC msg ->
            state.Commitments |> Commitments.receiveFulfill msg
            >>>= fun e -> RRClose(e.Describe())

        | ChannelState.Normal state, FailHTLC cmd ->
            state.Commitments |> Commitments.sendFail cs.LocalNodeSecret cmd
            >>>= fun e -> RRApiMisuse(e.Describe())

        | ChannelState.Normal state, FailMalformedHTLC cmd ->
            state.Commitments |> Commitments.sendFailMalformed cmd
            >>>= fun e -> RRApiMisuse(e.Describe())

        | ChannelState.Normal state, ApplyUpdateFailHTLC msg ->
            state.Commitments |> Commitments.receiveFail msg
            >>>= fun e -> RRClose(e.Describe())

        | ChannelState.Normal state, ApplyUpdateFailMalformedHTLC msg ->
            state.Commitments |> Commitments.receiveFailMalformed msg
            >>>= fun e -> RRClose(e.Describe())

        | ChannelState.Normal state, UpdateFee cmd ->
            state.Commitments |> Commitments.sendFee cmd
            >>>= fun e -> e.Describe() |> RRApiMisuse
        | ChannelState.Normal state, ApplyUpdateFee msg ->
            let localFeerate = cs.FeeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority)
            state.Commitments |> Commitments.receiveFee cs.Config localFeerate msg
            >>>= fun e -> e.Describe() |> RRClose

        | ChannelState.Normal state, SignCommitment ->
            let cm = state.Commitments
            match cm.RemoteNextCommitInfo with
            | _ when (cm.LocalHasChanges() |> not) ->
                sprintf "Ignoring SignCommitment Command (nothing to sign)" |> RRIgnore
            | Choice2Of2 _ ->
                cm |> Commitments.sendCommit (cs.Secp256k1Context) (cs.KeysRepository) (cs.Network)
                >>>= fun e -> e.Describe() |> RRApiMisuse
            | Choice1Of2 _ ->
                sprintf "Already in the process of signing."
                |> RRIgnore

        | ChannelState.Normal state, ApplyCommitmentSigned msg ->
            state.Commitments |> Commitments.receiveCommit (cs.Secp256k1Context) cs.KeysRepository msg cs.Network
            >>>= fun e -> RRClose("Something unexpected happend while handling commitment_signed from remote" + e.Describe())

        | ChannelState.Normal state, ApplyRevokeAndACK msg ->
            let cm = state.Commitments
            match cm.RemoteNextCommitInfo with
            | Choice1Of2 _ when (msg.PerCommitmentSecret.ToPubKey() <> cm.RemoteCommit.RemotePerCommitmentPoint) ->
                sprintf "Invalid revoke_and_ack %A; must be %A" msg.PerCommitmentSecret cm.RemoteCommit.RemotePerCommitmentPoint
                |> RRClose
            | Choice2Of2 _ ->
                sprintf "Unexpected revocation"
                |> RRClose
            | Choice1Of2 ({ NextRemoteCommit = theirNextCommit }) ->
                let commitments1 = { cm  with LocalChanges = { cm.LocalChanges with Signed = []; ACKed = cm.LocalChanges.ACKed @ cm.LocalChanges.Signed }
                                              RemoteChanges = { cm.RemoteChanges with Signed = [] }
                                              RemoteCommit = theirNextCommit
                                              RemoteNextCommitInfo = Choice2Of2(msg.NextPerCommitmentPoint)
                                              RemotePerCommitmentSecrets = cm.RemotePerCommitmentSecrets.AddHash(msg.PerCommitmentSecret.ToBytes(), 0xffffffffffffUL - cm.RemoteCommit.Index )}
                let result = [ WeAcceptedRevokeAndACK (commitments1) ]
                result |> Good
                failwith "needs update"


        | ChannelState.Normal state, ChannelCommand.Close cmd ->
            let localSPK = cmd.ScriptPubKey |> Option.defaultValue (state.Commitments.LocalParams.DefaultFinalScriptPubKey)
            if (not <| Scripts.isValidFinalScriptPubKey(localSPK)) then
                sprintf "Invalid local final ScriptPubKey %O" (localSPK)
                |> RRIgnore
            else if (state.LocalShutdown.IsSome) then
                RRIgnore "shutdown is already in progress"
            else if (state.Commitments.LocalHasUnsignedOutgoingHTLCs()) then
                RRIgnore "Cannot close with unsigned outgoing htlcs"
            else
                let shutDown = { Shutdown.ChannelId = state.ChannelId
                                 ScriptPubKey = localSPK }
                [ AcceptedShutdownCMD shutDown ]
                |> Good
        | ChannelState.Normal state, RemoteShutdown msg ->
            let cm = state.Commitments
            // They have pending unsigned htlcs => they violated the spec, close the channel
            // they don't have pending unsigned htlcs
            //      We have pending unsigned htlcs
            //          We already sent a shutdown msg => spec violation (we can't send htlcs after having sent shutdown)
            //          We did not send a shutdown msg
            //              We are ready to sign => we stop sending further htlcs, we initiate a signature
            //              We are waiting for a rev => we stop sending further htlcs, we wait for their revocation, will resign immediately after, and then we will send our shutdown msg
            //      We have no pending unsigned htlcs
            //          we already sent a shutwodn msg
            //              There are pensing signed htlcs => send our shutdown msg, go to SHUTDOWN state
            //              there are no htlcs => send our shutdown msg, goto NEGOTIATING state
            //          We did not send a shutdown msg
            //              There are pending signed htlcs => go to SHUTDOWN state
            //              there are no HTLCs => go to NEGOTIATING state
            if (not <| Scripts.isValidFinalScriptPubKey(msg.ScriptPubKey)) then
                sprintf "Invalid remote final ScriptPubKey %O" (msg.ScriptPubKey.ToString())
                |> RRClose
            else if (cm.RemoteHasUnsignedOutgoingHTLCs()) then
                sprintf "They sent shutdown msg (%A) while they have pending unsigned HTLCs, this is protocol violation" msg
                |> RRClose
            // Do we have Unsigned Outgoing HTLCs?
            else if (cm.LocalHasUnsignedOutgoingHTLCs()) then
                if (state.LocalShutdown.IsSome) then
                    "can't have pending unsigned outgoing htlcs after having sent Shutdown" |> RRClose
                else
                    // Are we in the middle of a signature?
                    match cm.RemoteNextCommitInfo with
                    // yes.
                    | Choice1Of2 waitingForRevocation ->
                        let nextCommitments = { state.Commitments with
                                                    RemoteNextCommitInfo = Choice1Of2({ waitingForRevocation with ReSignASAP = true }) }
                        [ AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs(msg, nextCommitments) ]
                        |> Good
                    // No. let's sign right away.
                    | Choice2Of2 _ ->
                        [ ChannelStateRequestedSignCommitment; AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs(msg, cm) ] |> Good
            else
                let (localShutdown, sendList) = match state.LocalShutdown with
                                                | Some localShutdown -> (localShutdown, [])
                                                | None ->
                                                    let localShutdown = { Shutdown.ChannelId = state.ChannelId
                                                                          ScriptPubKey = cm.LocalParams.DefaultFinalScriptPubKey }
                                                    (localShutdown, [localShutdown])
                if (cm.HasNoPendingHTLCs()) then
                    // we have to send first closing_signed msg iif we are the funder
                    if (cm.LocalParams.IsFunder) then
                        Helpers.Closing.makeFirstClosingTx(cs.KeysRepository, cm, localShutdown.ScriptPubKey, msg.ScriptPubKey, cs.FeeEstimator, cm.LocalParams.ChannelPubKeys.FundingPubKey, cs.Network)
                        |>> fun (closingTx, closingSignedMsg) ->
                            let nextState = { NegotiatingData.ChannelId = cm.ChannelId
                                              Commitments = cm
                                              LocalShutdown = localShutdown
                                              RemoteShutdown = msg
                                              ClosingTxProposed = [[{ ClosingTxProposed.UnsignedTx = closingTx; LocalClosingSigned = closingSignedMsg }]]
                                              MaybeBestUnpublishedTx = None }
                            [ AcceptedShutdownWhenNoPendingHTLCs(closingSignedMsg |> Some, nextState) ]
                    else
                        let nextState = { NegotiatingData.ChannelId = cm.ChannelId
                                          Commitments = cm
                                          LocalShutdown = localShutdown
                                          RemoteShutdown = msg
                                          ClosingTxProposed = [[]]
                                          MaybeBestUnpublishedTx = None }
                        [ AcceptedShutdownWhenNoPendingHTLCs(None, nextState) ] |> Good
                else
                    let nextState = { ShutdownData.Commitments = cm
                                      LocalShutdown = localShutdown
                                      RemoteShutdown = msg
                                      ChannelId = cm.ChannelId }
                    [ AcceptedShutdownWhenWeHavePendingHTLCs(nextState) ]
                    |> Good

        // ----------- closing ---------
        | Shutdown state, FulfillHTLC cmd ->
            state.Commitments |> Commitments.sendFulfill cmd
            |>> fun t -> [ WeAcceptedCMDFulfillHTLC (t) ]
            >>>= fun e -> e.Describe() |> RRApiMisuse
        | Shutdown state, ApplyUpdateFulfillHTLC msg ->
            state.Commitments |> Commitments.receiveFulfill msg
            >>>= fun e -> e.Describe() |> RRClose
        | Shutdown state, FailHTLC cmd ->
            state.Commitments |> Commitments.sendFail (cs.LocalNodeSecret) cmd
            >>>= fun e -> e.Describe() |> RRApiMisuse
        | Shutdown state, FailMalformedHTLC cmd ->
            state.Commitments |> Commitments.sendFailMalformed cmd
            >>>= fun e -> e.Describe() |> RRApiMisuse
        | Shutdown state, ApplyUpdateFailMalformedHTLC msg ->
            state.Commitments |> Commitments.receiveFailMalformed msg
            >>>= fun e -> e.Describe() |> RRClose
        | Shutdown state, UpdateFee cmd ->
            state.Commitments |> Commitments.sendFee cmd
            >>>= fun e -> e.Describe() |> RRApiMisuse
        | Shutdown state, ApplyUpdateFee  msg ->
            let localFeerate = cs.FeeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority)
            state.Commitments |> Commitments.receiveFee cs.Config localFeerate msg
            >>>= fun e -> e.Describe() |> RRClose
        | Shutdown state, SignCommitment ->
            let cm = state.Commitments
            match cm.RemoteNextCommitInfo with
            | _ when (not <| cm.LocalHasChanges()) ->
                sprintf "nothing to sign" |> RRIgnore
            | Choice2Of2 _ ->
                cm |> Commitments.sendCommit (cs.Secp256k1Context) (cs.KeysRepository) (cs.Network)
                >>>= fun e -> e.Describe() |> RRClose
            | Choice1Of2 waitForRevocation ->
                sprintf "Already in the process of signing."
                |> RRIgnore
        | Shutdown state, ApplyCommitmentSigned msg ->
            state.Commitments |> Commitments.receiveCommit (cs.Secp256k1Context) (cs.KeysRepository) msg cs.Network
            >>>= fun e -> e.Describe() |> RRClose
        | Shutdown state, ApplyRevokeAndACK msg ->
            failwith "not implemented"

        | Negotiating state, ApplyClosingSigned msg ->
            let cm = state.Commitments
            let lastCommitFeeSatoshi =
                cm.FundingSCoin.TxOut.Value - (cm.LocalCommit.PublishableTxs.CommitTx.Value.TotalOut)
            checkOrClose msg.FeeSatoshis (>) lastCommitFeeSatoshi "remote proposed a commit fee higher than the last commitment fee. remoteClosingFee=%A; localCommitTxFee=%A;" 
            >>= fun _ ->
                Helpers.Closing.makeClosingTx(cs.KeysRepository, cm, state.LocalShutdown.ScriptPubKey, state.RemoteShutdown.ScriptPubKey, msg.FeeSatoshis, cm.LocalParams.ChannelPubKeys.FundingPubKey, cs.Network)
                >>= fun (closingTx, closingSignedMsg) ->
                    Transactions.checkTxFinalized closingTx.Value (closingTx.WhichInput) (seq [cm.RemoteParams.FundingPubKey, TransactionSignature(msg.Signature, SigHash.All)])
                    >>= fun finalizedTx ->
                        let maybeLocalFee =
                            state.ClosingTxProposed
                            |> List.tryHead
                            |> Option.bind (List.tryHead)
                            |> Option.map(fun v -> v.LocalClosingSigned.FeeSatoshis)
                        let areWeInDeal = Some (msg.FeeSatoshis) = maybeLocalFee
                        let hasTooManyNegotiationDone =
                            (state.ClosingTxProposed |> List.collect(id) |> List.length) >= MAX_NEGOTIATION_ITERATIONS
                        if (areWeInDeal || hasTooManyNegotiationDone) then
                            Helpers.Closing.handleMutualClose(finalizedTx, Choice1Of2({ state with MaybeBestUnpublishedTx = Some(finalizedTx) }))
                        else
                            let lastLocalClosingFee = state.ClosingTxProposed |> List.tryHead |> Option.bind(List.tryHead) |> Option.map(fun txp -> txp.LocalClosingSigned.FeeSatoshis)
                            let nextClosingFeeRR =
                                match lastLocalClosingFee with Some v -> Good v | None -> (Helpers.Closing.firstClosingFee(state.Commitments, state.LocalShutdown.ScriptPubKey, state.RemoteShutdown.ScriptPubKey, cs.FeeEstimator, cs.Network))
                                |>> fun localF ->
                                    Helpers.Closing.nextClosingFee(localF, msg.FeeSatoshis)
                            nextClosingFeeRR
                            >>= fun nextClosingFee ->
                                if (Some nextClosingFee = lastLocalClosingFee) then
                                    Helpers.Closing.handleMutualClose(finalizedTx, Choice1Of2({ state with MaybeBestUnpublishedTx = Some(finalizedTx) }))
                                else if (nextClosingFee = msg.FeeSatoshis) then
                                    // we have reached on agreement!
                                    let closingTxProposed1 =
                                        let newProposed = [{ ClosingTxProposed.UnsignedTx = closingTx
                                                             LocalClosingSigned = closingSignedMsg }]
                                        newProposed::state.ClosingTxProposed
                                    let negoData = { state with ClosingTxProposed = closingTxProposed1
                                                                MaybeBestUnpublishedTx = Some(finalizedTx) }
                                    Helpers.Closing.handleMutualClose (finalizedTx, Choice1Of2(negoData))
                                else
                                    let closingTxProposed1 =
                                        let newProposed = [{ ClosingTxProposed.UnsignedTx = closingTx
                                                             LocalClosingSigned = closingSignedMsg }]
                                        newProposed::state.ClosingTxProposed
                                    let nextState = { state with ClosingTxProposed = closingTxProposed1; MaybeBestUnpublishedTx = Some (finalizedTx) }
                                    [ WeProposedNewClosingSigned(closingSignedMsg, nextState) ]
                                    |> Good
            >>>= fun e -> e.Describe() |> RRClose
        | Closing state, FulfillHTLC cmd ->
            cs.Logger(LogLevel.Info) (sprintf "got valid payment preimage, recalculating txs to redeem the corresponding htlc on-chain")
            let cm = state.Commitments
            cm |> Commitments.sendFulfill cmd
            >>= fun (msgToSend, newCommitments) ->
                let localCommitPublished  =
                    state.LocalCommitPublished
                    |> Option.map (fun localCommitPublished -> Helpers.Closing.claimCurrentLocalCommitTxOutputs (cs.KeysRepository, newCommitments.LocalParams.ChannelPubKeys, newCommitments, localCommitPublished.CommitTx))
                failwith ""

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
                              RemoteShutdown = None
                              ChannelId = msg.ChannelId }
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

        // -----  closing ------
        | AcceptedShutdownCMD msg, ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with LocalShutdown = Some msg }) }
        | AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs(remoteShutdown, nextCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with RemoteShutdown = Some remoteShutdown; Commitments = nextCommitments })}
        | AcceptedShutdownWhenNoPendingHTLCs (maybeMsg, nextState), ChannelState.Normal d ->
            { c with State = Negotiating nextState }
        | AcceptedShutdownWhenWeHavePendingHTLCs(nextState), ChannelState.Normal d ->
            { c with State = Shutdown nextState }
        | MutualClosePerformed nextState, ChannelState.Negotiating d ->
            { c with State = Closing nextState }
        | WeProposedNewClosingSigned (msg, nextState), ChannelState.Negotiating d ->
            { c with State = Negotiating (nextState) }
        // ----- else -----
        | NewBlockVerified height, _ ->
            { c with CurrentBlockHeight = height }