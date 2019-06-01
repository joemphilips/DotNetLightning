namespace DotNetLightning.LN
open DotNetLightning.Utils
open NBitcoin
open System
open System.Text
open System.Linq
open DotNetLightning
open BTCPayServer.Lightning
open Microsoft.Extensions.Logging
open DotNetLightning.Chain
open DotNetLightning.Utils.RResult
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Utils.NBitcoinExtensions
open NBitcoin.Crypto

type ChannelValueStat = {
    ValueToSelf: LightMoney;
    ChannelValue: LightMoney;
    ChannelReserve: LightMoney;
    PendingOutboundHTLCsAmount: LightMoney;
    PendingInBoundHTLCsAmount: LightMoney;
    HoldingCellOutBoundAmount: LightMoney;
}

type InboundHTLCRemovalReason =
    | FailRelay of OnionErrorPacket
    | FailMalformed of uint256 * uint16
    | Fullfill of PaymentPreimage

type InboundHTLCState =
    // added by remote, to be included in next local commitment tx
    | RemoteAnnounced of PendingHTLCStatus
    // Included in a received commitment_signed message (implying we've revoke_and_ack'd it), but
    // the remote side hasn't et revoked their previous state.
    | AwaitingRemoteRevokeToAnnounce of PendingHTLCStatus
    | AwaitingAnnouncedRemoteRevoke of PendingHTLCStatus
    | LocalRemoved of InboundHTLCRemovalReason
    | Commited

type InboundHTLCOutput = {
    HTLCId: HTLCId
    Amount: LightMoney
    CLTVExpiry: uint32
    PaymentHash: PaymentHash
    State: InboundHTLCState
}

type OutboundHTLCState =
    | LocalAnnounced of OnionPacket
    | Commited
    | RemoteRemoved of HTLCFailReason option
    | AwaitingRemoteRevokeToRemove of HTLCFailReason option
    | AwaitingRemovedRemoteRevoke of HTLCFailReason option

type OutboundHTLCOutput = {
    HTLCId: HTLCId
    Amount: LightMoney
    CLTVExpiry: uint32
    PaymentHash: PaymentHash
    State: OutboundHTLCState
    Source: HTLCSource
}

type HTLCOutPut =
    | Inbound of InboundHTLCOutput
    | OutBound of OutboundHTLCOutput

type AddHTLCRecord = {
    AmountMSat : uint64
    CTLVExpiry: uint32
    PaymantHash: PaymentHash
    Source: HTLCSource
    OnionRoutingPacket: OnionPacket
}
type ClaimHTLCRecord = {
    PaymentPreimage: PaymentPreimage
    HTLCId: HTLCId
}

type FailHTLCRecord = {
    HTLCId: HTLCId
    ErrPacket: OnionErrorPacket
}

type HTLCUpdateAwaitingACK =
    | AddHTLC of AddHTLCRecord
    | ClaimHTLC of  ClaimHTLCRecord
    | FailHTLC of FailHTLCRecord

[<Flags>]
type ChannelState =
    | Zero = 0u
    | OurInitSent =        1u
    | TheirInitSent =      2u
    | FundingCreated =     4u
    | FundingSent =        8u
    | TheirFundingLocked = 16u
    | OurFundingLocked =   32u
    | ChannelFunded =      64u
    | PeerDisconnected =   128u
    | MonitorUpdateFailed = 256u
    | AwaitingRemoteRevoke = 512u
    | RemoteShutdownSent = 1024u
    | LocalShutdownSent = 2048u
    | ShutdownComplete = 4096u

[<AutoOpen>]
module ChannelConstants =
    [<Literal>]
    let BOTH_SIDES_SHUTDOWN_MASK = ChannelState.LocalShutdownSent ||| ChannelState.RemoteShutdownSent
    [<Literal>]
    let MULTI_STATE_FLAGS = BOTH_SIDES_SHUTDOWN_MASK ||| ChannelState.PeerDisconnected ||| ChannelState.MonitorUpdateFailed

    [<Literal>]
    let INITIAL_COMMITMENT_NUMBER = 281474976710655UL // (1 << 48 - 1)

    [<Literal>]
    let OUR_MAX_HTLCs = 50us

    [<Literal>]
    let UNCONF_THRESHOLD = 6u

    let BREAKDOWN_TIMEOUT = BlockHeightOffset(6us * 24us * 7us)
    let MAX_LOCAL_BREAKDOWN_TIMEOUT = BlockHeightOffset(6us * 24us * 14us)

    [<Literal>]
    let COMMITMENT_TX_BASE_WEIGHT = 724UL
    [<Literal>]
    let COMMITMENT_TX_WEIGHT_PER_HTLC = 172UL

    // prevout: 36, nSequence: 4, script len: 1, witness lengths: (3+1)/4, sig: 73/4, if-selector: 1, redeemScript: (6 ops + 2*33 pubkeys + 1*2 delay)/4
    [<Literal>]
    let SPENDING_INPUT_FOR_A_OUTPUT_WEIGHT = 79UL 
    // prevout: 40, nSequence: 4, script len: 1, witness lengths: 3/4, sig: 73/4, pubkey: 33/4, output: 31
    [<Literal>]
    let B_OUTPUT_PLUS_SPENDING_INPUT_WEIGHT = 104UL

    // Specified in BOLT #2
    let MAX_FUNDING_SATOSHIS = Money.Satoshis(16777216m) // (1 << 24)

    [<Literal>]
    let ACCEPTED_HTLC_SCRIPT_WEIGHT = 139uy
    [<Literal>]
    let OFFERED_HTLC_SCRIPT_WEIGHT = 133uy

type Channel = {
    Config: ChannelConfig
    UserId: UserId
    ChannelId: ChannelId
    ChannelState: ChannelState
    ChannelOutbound: bool
    ChannelValueSatoshis: Money
    LocalKeys: ChannelKeys
    ShutdownPubKey: PubKey
    CurrentLocalCommitmentTxNumber: uint64
    CurrentRemoteCommitmentTxNumber: uint64
    ValueToSelf: LightMoney
    PendingInboundHTLCs: InboundHTLCOutput list
    PendingOutboundHTLCs: OutboundHTLCOutput list
    HoldingCellHTLCUpdates: HTLCUpdateAwaitingACK list
    /// RAA stands for "revoke_and_ack"
    ResendOrder: RAACommitmentOrder
    MonitorPendingRevokeAndAck: bool
    MonitorPendingCommitmentSigned: bool
    MonitorPendingForwards: (PendingForwardHTLCInfo * uint64) list
    MonitorPendingFailures: (HTLCSource * PaymentHash * HTLCFailReason) list
    PendingUpdateFee: LightMoney option
    HoldingCellUpdateFee: LightMoney option
    NextLocalHTLCId: HTLCId
    NextRemoteHTLCId: HTLCId
    ChannelUpdateCount: uint32
    FeeRatePerKw: FeeRatePerKw
    MaxCommitmentTxOutputLocal: (LightMoney * LightMoney)
    MaxCommitmentTxOutputRemote: (LightMoney * LightMoney)
    LastLocalCommitmentTxn: Transaction list
    LastSentClosingFee: (uint64 * uint64) option
    FundingTxConfirmedIn: TxId option
    ShortChannelId: ShortChannelId option

    LastBlockConnected: BlockId
    FundingTxConfirmations: uint64

    TheirDustLimit: Money
    OurDustLimit: Money
    TheirMaxHTLCValueInFlight: LightMoney

    TheirChannelReserve: Money
    TheirHTLCMinimum: LightMoney
    OurHTLCMinimum: LightMoney
    TheirToSelfDelay: BlockHeightOffset
    TheirMaxAcceptedHTLCs: uint16
    MinimumDepth: uint32

    TheirFundingPubKey: PubKey option
    TheirRevocationBasePoint: PubKey option
    TheirPaymentBasePoint: PubKey option
    TheirDelayedPaymentBasePoint: PubKey option
    TheirHTLCBasePoint: PubKey option
    TheirCurrentCommitmentPoint: PubKey option
    TheirPreviousCommitmentBasePoint: PubKey option
    TheirNodeId: PubKey
    TheirShutdownScriptPubKey: PubKey option
    ChannelMonitor: ChannelMonitor
    Logger: ILogger
}

type ChannelError =
    | Ignore of string
    | Close of string

exception ChannelException of ChannelError

module Channel =
    let getOurMaxHTLCValueInFlight (channelValue: Money) =
        channelValue * 1000L / 10L

    let getOurChannelReserve (channelValue: Money) =
        let q = channelValue / 100L
        Money.Min(channelValue, Money.Max(q, Money.Satoshis(1L)))

    /// TODO:
    let deriveOurDustLimitSatoshis(FeeRatePerKw atOpenBackGroundFee): Money =
        failwith "Not implemnted"

    /// TODO:
    let deriveOurHTLCMinimum(FeeRatePerKw atOpenBackGroundFee): LightMoney =
        LightMoney.Satoshis(1000UL)

    let private getChannel (userId: UserId)
                           (config: UserConfig)
                           (channKeys)
                           (theirNodeId)
                           (keysProvider: IKeysRepository)
                           (logger: ILogger)
                           (channelMonitor)
                           (backGroundFeeRate)
                           (channelValue: Money)
                           (pushMSat)
                           (fee: FeeRatePerKw): Channel =
        {
            UserId = userId
            Config = config.ChannelOptions
            ChannelId = keysProvider.GetChannelId()
            ChannelState = ChannelState.OurInitSent
            ChannelOutbound = true
            ChannelValueSatoshis = channelValue
            LocalKeys = channKeys
            ShutdownPubKey = keysProvider.GetShutdownPubKey()
            CurrentLocalCommitmentTxNumber = INITIAL_COMMITMENT_NUMBER
            CurrentRemoteCommitmentTxNumber = INITIAL_COMMITMENT_NUMBER
            ValueToSelf = LightMoney.MilliSatoshis(channelValue.Satoshi * 1000L) - pushMSat
            PendingInboundHTLCs = List.empty
            PendingOutboundHTLCs = List.empty
            HoldingCellHTLCUpdates = List.empty
            PendingUpdateFee = None
            HoldingCellUpdateFee = None
            NextLocalHTLCId = HTLCId.Zero
            NextRemoteHTLCId = HTLCId.Zero
            ChannelUpdateCount = 1u
            ResendOrder = RAACommitmentOrder.CommitmentFirst

            MonitorPendingRevokeAndAck = false
            MonitorPendingCommitmentSigned = false
            MonitorPendingForwards = List.empty
            MonitorPendingFailures = List.empty

            MaxCommitmentTxOutputLocal = (LightMoney.MilliSatoshis(channelValue.Satoshi * (1000L)) - pushMSat, pushMSat)
            MaxCommitmentTxOutputRemote = (LightMoney.MilliSatoshis(channelValue.Satoshi * (1000L)) - pushMSat, pushMSat)

            LastLocalCommitmentTxn = List.empty

            LastSentClosingFee = None
            FundingTxConfirmedIn = None
            ShortChannelId = None
            LastBlockConnected = BlockId(uint256.Zero)
            FundingTxConfirmations = 0UL

            FeeRatePerKw = fee
            TheirDustLimit = Money.Zero
            OurDustLimit = deriveOurDustLimitSatoshis(backGroundFeeRate)
            TheirMaxHTLCValueInFlight = LightMoney.Zero
            TheirChannelReserve = Money.Zero
            TheirHTLCMinimum = LightMoney.Zero
            OurHTLCMinimum = deriveOurHTLCMinimum(fee)
            TheirToSelfDelay = BlockHeightOffset 0us
            TheirMaxAcceptedHTLCs = 0us
            MinimumDepth = 0u
            TheirFundingPubKey = None
            TheirRevocationBasePoint = None
            TheirPaymentBasePoint = None
            TheirDelayedPaymentBasePoint = None
            TheirHTLCBasePoint = None
            TheirCurrentCommitmentPoint = None
            TheirPreviousCommitmentBasePoint = None
            TheirNodeId = theirNodeId
            TheirShutdownScriptPubKey = None
            ChannelMonitor = channelMonitor
            Logger = logger
        }

    // ----- constructors -------
    let public newOutBound(feeEstimator: IFeeEstimator,
                           keyProvider: IKeysRepository,
                           theirNodeId: PubKey,
                           channelValue: Money,
                           pushMSat: LightMoney,
                           userId: UserId,
                           logger: ILogger,
                           config: UserConfig): RResult<Channel>  =

        /// ------ Validators -----
        let checkSmallerThenMaxPossible(channelValue) =
            if (channelValue >= MAX_FUNDING_SATOSHIS) then
                RResult.Bad(RBadTree.Leaf(RBad.Message("Funding value. 2^24")))
            else
                Good(channelValue)

        let checkPushValueLessThanChannelValue (pushMSat: LightMoney) (channelValue: Money) =
            if pushMSat.ToUnit(LightMoneyUnit.Satoshi) >= (channelValue.ToDecimal(MoneyUnit.Satoshi)) then
                RResult.Bad(RBadTree.Leaf(RBad.Message("push value > channel value")))
            else
                Good(pushMSat)

        let checkBackgroundFeeRate (estimator: IFeeEstimator) (channelValue) =
            let backgroundFeeRate = estimator.GetEstSatPer1000Weight(ConfirmationTarget.Background)
            if (getOurChannelReserve(channelValue).Satoshi < deriveOurDustLimitSatoshis(backgroundFeeRate).Satoshi * 1000L) then
                RResult.Bad(RBadTree.Leaf(RBad.Message(sprintf "Not eonugh reserve above dust limit can be found at current fee rate(%O)" backgroundFeeRate)))
            else
                Good(backgroundFeeRate)

        let feeRate =
            feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Normal)

        let channelKeys = keyProvider.GetChannelKeys()
        let channelMonitor = ChannelMonitor.create(channelKeys.RevocationBaseKey,
                                                   channelKeys.DelayedPaymentBaseKey,
                                                   channelKeys.HTLCBaseKey,
                                                   channelKeys.PaymentBaseKey,
                                                   keyProvider.GetShutdownPubKey(),
                                                   BREAKDOWN_TIMEOUT,
                                                   keyProvider.GetDestinationScript(),
                                                   logger)

        let getChannelCurried = getChannel (userId)
                                           (config)
                                           (channelKeys)
                                           (theirNodeId)
                                           (keyProvider)
                                           (logger)
                                           (channelMonitor)
                                           (feeRate)
                                           // (channelValue)
                                           // (pushMSat)
                                           // (backGroundFeeRate)

        RResult.Good(getChannelCurried)
            <*> checkSmallerThenMaxPossible(channelValue)
            <*> checkPushValueLessThanChannelValue pushMSat channelValue
            <*> checkBackgroundFeeRate feeEstimator channelValue

    let public checkRemoteFee(feeEstimator: IFeeEstimator, feeRatePerKw: FeeRatePerKw) =
        if (feeRatePerKw < feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Background) ) then
            RResult.Bad(!> ChannelException(Close("Peer's Feerate much to low")))
        else if (feeRatePerKw.Value > feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority).Value * 2) then
            RResult.Bad(!> ChannelException(Close("Peer's feerate much too high")))
        else
            Good()

    /// Creates a new channel from a remote sides' request for one.
    /// Assumes chain_hash has already been checked and corresponds with what we expect!
    let public newFromReq(feeEstimator: IFeeEstimator,
                          keysProvider: IKeysRepository,
                          theirNodeId: NodeId,
                          msg: OpenChannel,
                          userId: UserId,
                          logger: ILogger,
                          config: UserConfig) =
        let chanKeys = keysProvider.GetChannelKeys()
        let localConfig = config.ChannelOptions

        let checkMsg1 msg =
            if (msg.FundingSatoshis >= MAX_FUNDING_SATOSHIS) then
                Bad(!> ChannelException(Close("Funding value > 2^24")))
            else if (msg.ChannelReserveSatoshis > msg.FundingSatoshis) then
                Bad(!> ChannelException(Close("Bogus ChannelReserveSatoshis")))
            else if (msg.PushMSat.MilliSatoshi > (msg.FundingSatoshis - msg.ChannelReserveSatoshis).Satoshi * 1000L) then
                Bad(!> ChannelException(Close("PushMsat larger than funding value")))
            else if (msg.DustLimitSatoshis > msg.FundingSatoshis) then
                Bad(!> ChannelException(Close("Peer neve rwants payout outputs?")))
            else if (msg.DustLimitSatoshis > msg.ChannelReserveSatoshis) then
                Bad(!> ChannelException(Close("Bogus; Channel reserve is less than dust limit")))
            else if (msg.HTLCMinimumMsat.MilliSatoshi >= (msg.FundingSatoshis - msg.ChannelReserveSatoshis).Satoshi * 1000L) then
                Bad(!> ChannelException(Close("Minimum HTLC value is full channel value")))
            else
                Good()

        let checkMsg2 msg =
            if (msg.ToSelfDelay > MAX_LOCAL_BREAKDOWN_TIMEOUT) then
                Bad(!> ChannelException(Close("They wanted our payments to be delayed by a needlessly long period")))
            else if (msg.MaxAcceptedHTLCs < 1us) then
                Bad(!> ChannelException(Close("0 max_accepted_htlcs makes for a useless channel")))
            else if (msg.MaxAcceptedHTLCs > 483us) then
                Bad(!> ChannelException(Close("max_accepted_htlcs > 483")))
            else
                Good()

        let checkMsgAgainstConfig config msg =
            let theirAnnounce = (msg.ChannelFlags &&& 1uy) = 1uy
            if (msg.FundingSatoshis < config.PeerChannelConfigLimits.MinFundingSatoshis) then
                Bad(!> ChannelException(Close("dust limit satoshis is less than the user specified limit")))
            else if (msg.HTLCMinimumMsat > config.PeerChannelConfigLimits.MaxHTLCMinimumMSat) then
                Bad(!> ChannelException(Close("HTLC minimum msat is higher than the user specified limit")))
            else if (msg.MaxHTLCValueInFlightMsat < config.PeerChannelConfigLimits.MinMaxHTLCValueInFlightMSat) then
                Bad(!> ChannelException(Close("Max htlc value in flight msat is less than the user specified limit")))
            else if (msg.ChannelReserveSatoshis > config.PeerChannelConfigLimits.MaxChannelReserveSatoshis) then
                Bad(!> ChannelException(Close("Channel resere satoshis is higher than the user specified limit")))
            else if (msg.MaxAcceptedHTLCs < config.PeerChannelConfigLimits.MinMaxAcceptedHTLCs) then
                Bad(!> ChannelException(Close("dust limit satoshis is less than the user specified limit")))
            else if (msg.DustLimitSatoshis < config.PeerChannelConfigLimits.MinDustLimitSatoshis ) then
                Bad(!> ChannelException(Close("dust limit satoshis is less than the user specified limit")))
            else if (msg.DustLimitSatoshis > config.PeerChannelConfigLimits.MaxDustLimitSatoshis) then
                Bad(!> ChannelException(Close("dust limit satoshis is greater than the user specified limit")))
            else if (config.PeerChannelConfigLimits.ForceAnnouncedChannelPreference && localConfig.AnnouncedChannel <> theirAnnounce) then
                Bad(!> ChannelException(Close("Peer tried to open channel but their announcement is different from ours")))
            else
                Good()

        let prepareDustLimit (feeEstimator: IFeeEstimator) (msg: OpenChannel) =
            let backgroundFeeRate = feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Background)
            let ourDustLimitSatoshis = deriveOurDustLimitSatoshis(backgroundFeeRate)
            let ourChannelReserveSatoshis = getOurChannelReserve(msg.ChannelReserveSatoshis)
            // check if the funder's amount for the initial commitment tx is sufficient for fee payment
            let fundersAmountMSat = LightMoney.MilliSatoshis(msg.FundingSatoshis.Satoshi * 1000L - msg.PushMSat.MilliSatoshi)

            let toLocal = msg.PushMSat
            let toRemote = fundersAmountMSat - backgroundFeeRate.Value * !> COMMITMENT_TX_BASE_WEIGHT
            if (ourChannelReserveSatoshis < ourDustLimitSatoshis) then
                Bad(!> ChannelException(Close("Suitable channel reserve not found. aborting")))
            else if (msg.ChannelReserveSatoshis < ourDustLimitSatoshis) then
                Bad(!> ChannelException(Close("channel_reserve_satoshis too small")))
            else if (ourChannelReserveSatoshis < msg.DustLimitSatoshis) then
                Bad(!> ChannelException(Close("Dust limit too high for our channel reserve")))
            else if fundersAmountMSat < backgroundFeeRate.Value * !> COMMITMENT_TX_BASE_WEIGHT then
                Bad(!> ChannelException(Close("Insufficient funding amount for initial commitment")))
            else if toLocal.MilliSatoshi <= msg.ChannelReserveSatoshis.Satoshi * 1000L && toRemote.MilliSatoshi <= ourChannelReserveSatoshis.Satoshi * 1000L then
                Bad(!> ChannelException(Close("Insufficient funding amount for initial commitment")))
            else
                Good()

        let channelMonitorTmp = ChannelMonitor.create(chanKeys.RevocationBaseKey,
                                                   chanKeys.DelayedPaymentBaseKey,
                                                   chanKeys.HTLCBaseKey,
                                                   chanKeys.PaymentBaseKey,
                                                   keysProvider.GetShutdownPubKey(),
                                                   BREAKDOWN_TIMEOUT,
                                                   keysProvider.GetDestinationScript(),
                                                   logger)

        let channelMonitor = { channelMonitorTmp with
                                 TheirHTLCBaseKey = Some(msg.HTLCBasepoint)
                                 TheirDelayedPaymentBaseKey = Some(msg.DelayedPaymentBasepoint)
                                 TheirToSelfDelay = Some(msg.ToSelfDelay)
                                 }

        let getChannel (userId)
                       (localConfig)
                       (channelValue: Money)
                       (pushMSat)
                       (ourDustLimitSatoshis)
                       (theirNodeId)
                       (msg: OpenChannel) =
            {
                UserId = userId
                Config = localConfig
                ChannelId = msg.TemporaryChannelId
                ChannelState = (ChannelState.OurInitSent ||| ChannelState.TheirInitSent)
                ChannelOutbound = false
                LocalKeys = chanKeys
                ShutdownPubKey = keysProvider.GetShutdownPubKey()
                CurrentLocalCommitmentTxNumber = INITIAL_COMMITMENT_NUMBER
                CurrentRemoteCommitmentTxNumber = INITIAL_COMMITMENT_NUMBER
                ValueToSelf = pushMSat
                PendingInboundHTLCs = List.empty
                PendingOutboundHTLCs = List.empty
                HoldingCellHTLCUpdates = List.empty
                PendingUpdateFee = None
                HoldingCellUpdateFee = None
                NextLocalHTLCId = HTLCId.Zero
                NextRemoteHTLCId = HTLCId.Zero
                ChannelUpdateCount = 1u
                ResendOrder = RAACommitmentOrder.CommitmentFirst

                MonitorPendingRevokeAndAck = false
                MonitorPendingCommitmentSigned = false
                MonitorPendingForwards = List.empty
                MonitorPendingFailures = List.empty

                MaxCommitmentTxOutputLocal = (pushMSat, LightMoney.MilliSatoshis(channelValue.Satoshi * (1000L)) - pushMSat)
                MaxCommitmentTxOutputRemote = (pushMSat, LightMoney.MilliSatoshis(channelValue.Satoshi * (1000L)) - pushMSat)

                LastLocalCommitmentTxn = List.empty

                LastSentClosingFee = None
                FundingTxConfirmedIn = None
                ShortChannelId = None
                LastBlockConnected = BlockId(uint256.Zero)
                FundingTxConfirmations = 0UL

                FeeRatePerKw = msg.FeeRatePerKw
                ChannelValueSatoshis = msg.FundingSatoshis
                TheirDustLimit = msg.DustLimitSatoshis
                OurDustLimit = ourDustLimitSatoshis
                TheirMaxHTLCValueInFlight = LightMoney.Min(msg.MaxHTLCValueInFlightMsat, !> msg.FundingSatoshis)
                TheirChannelReserve = msg.ChannelReserveSatoshis
                TheirHTLCMinimum = msg.HTLCMinimumMsat
                OurHTLCMinimum = deriveOurHTLCMinimum(msg.FeeRatePerKw)
                TheirToSelfDelay = msg.ToSelfDelay
                TheirMaxAcceptedHTLCs = msg.MaxAcceptedHTLCs
                MinimumDepth = config.OwnChannelConfig.MinimumDepth
                TheirFundingPubKey = Some(msg.FundingPubKey)
                TheirRevocationBasePoint = Some(msg.RevocationBasepoint)
                TheirPaymentBasePoint = Some(msg.PaymentBasepoint)
                TheirDelayedPaymentBasePoint = Some(msg.DelayedPaymentBasepoint)
                TheirHTLCBasePoint = Some(msg.HTLCBasepoint)
                TheirCurrentCommitmentPoint = Some(msg.FirstPerCommitmentPoint)
                TheirPreviousCommitmentBasePoint = None
                TheirNodeId = theirNodeId
                TheirShutdownScriptPubKey = None

                ChannelMonitor = channelMonitor
                Logger = logger
            }

        checkMsg1 msg
            >>= fun _ -> checkRemoteFee(feeEstimator, msg.FeeRatePerKw)

    let buildLocalCommitmentSecret index c : Key =
        let res = ChannelUtils.buildCommitmentSecret(c.LocalKeys.CommitmentSeed, index)
        Key(res.ToBytes())

    let getCommitmentTxNumberObscureFactor(c: Channel) =
        let ourPaymentBasepoint = c.LocalKeys.PaymentBaseKey.PubKey
        let mutable res: byte[] = null
        if (c.ChannelOutbound) then
            res <- Hashes.SHA256(Array.concat[| ourPaymentBasepoint.ToBytes(); c.TheirPaymentBasePoint.Value.ToBytes() |])
        else
            res <- Hashes.SHA256(Array.concat[| c.TheirPaymentBasePoint.Value.ToBytes(); ourPaymentBasepoint.ToBytes() |])
        (uint64 (res.[26]) <<< 5*8 |||
         uint64 (res.[27]) <<< 4*8 |||
         uint64 (res.[28]) <<< 3*8 |||
         uint64 (res.[29]) <<< 2*8 |||
         uint64 (res.[30]) <<< 1*8 |||
         uint64 (res.[31]) <<< 0*8)

    let private getHTLCInCommitment h o =
        {
            Offered = o
            Amount = h.Amount
            CLTVExpiry = h.CLTVExpiry
            PaymentHash = h.PaymentHash
            TransactionOutputIndex = None
        }

    let private addHTLCOutput (htlc: HTLCOutPut, offered, source, stateName) =
        match htlc with
        | OutBound h ->
            let htlcInTx = getHTLCInCommitment h true
            failwith "not impl"
        | Inbound h ->
            failwith "not impl"

    /// Transaction nomencclature is somewhat confusing here as there are many different cases - a
    /// transaction is referred to as "a's transaction" implying that a will be able to broadcast the
    /// transaction. Thus, b will generally be spending a signature over such a trasnaction to a,
    /// and a can revoke the transaction by providing b the relevant per_commtment_secret. As
    /// such, a transaction is generally the result of b increasing the amount paid to a (or adding
    /// an HTLC to a).
    /// 
    /// <param name="local">
    ///     Is used only to convert relevant internal structures which refer to remote vs local
    ///     to decide value of outputs and direction of HTLCs.
    /// </param>
    /// <param name="generatedByLocal">
    ///     Is used to determine *which* HTLCs to include - noting that the HTLC state may indicate
    ///     that one peer has informed the other that they'd like to add an HTLC but
    ///     have not yet committed it. Such HTLCs will only be included in transactions which are being
    ///     generated by the peer which proposed adding the HTLCs, and thus we need to understand both
    ///     which peer generated this transaction and "to whom" this transaction flows.
    ///     Returns (the transaction built, the number of HTLC outputs which were present in the
    ///     transaction, the list of HTLCs which were not ignored when building the transaction).
    ///     Note that below-dust HTLCs are included in the third return value, but not the second, and
    ///     sources are provided only for outbound HTLCs in the third return value.
    /// </param>

    let buildCommitmentTransaction (commitmentN: uint64)
                                   (keys: TxCreationKeys)
                                   (local: bool)
                                   (generatedByLocal: bool)
                                   (feeRatePerKW: FeeRatePerKw)
                                   (c: Channel): (Transaction * uint32 * (HTLCOutputInCommitment * HTLCSource option) list) =
        let obscuredCommitmentTxN = getCommitmentTxNumberObscureFactor(c) ^^^ (INITIAL_COMMITMENT_NUMBER - commitmentN)
        let txin = TxIn()
        txin.PrevOut <- c.ChannelMonitor.GetFundingTxo().Value
        txin.Sequence <- !> ((0x80u <<< 8 * 3) ||| ((uint32 obscuredCommitmentTxN) >>> 3 * 8))
        let mutable txOuts: (TxOut * (HTLCOutputInCommitment * HTLCSource option) option) list = List.empty
        let mutable includedDustHTLCs: (HTLCOutputInCommitment * HTLCSource option) list = List.empty
        let dustLimitSatoshis = if local then c.OurDustLimit else c.TheirDustLimit
        let mutable remoteHTLCTotalMSat = LightMoney.Zero
        let mutable localHTLCTotalMSat = LightMoney.Zero
        let mutable valueToSelfMSatOffset = LightMoney.Zero

        c.Logger.LogDebug(sprintf "building commitment tx number %d for %s generated by %s with fee %O"
                                  commitmentN
                                  (if local then "us" else "remote")
                                  (if generatedByLocal then "us" else "remote")
                                  (feeRatePerKW))

        for htlc in c.PendingInboundHTLCs do
            let (includeThis, stateName) =
                match htlc.State with
                | RemoteAnnounced _ -> (not generatedByLocal, "RemoteAnnounced")
                | AwaitingRemoteRevokeToAnnounce _ -> (not generatedByLocal, "AwaitingRemoteRevokeAnnounced")
                | AwaitingAnnouncedRemoteRevoke _ -> (true, "AwaitingAnnouncedRemoteRevoke")
                | LocalRemoved _ -> (not generatedByLocal, "LocalRemoved")
                | _ -> (true, "Commited")
            if includeThis then
                addHTLCOutput(htlc, false, None, stateName)
                remoteHTLCTotalMSat <- htlc.Amount + remoteHTLCTotalMSat
            else
                c.Logger.LogDebug(sprintf "  ...not including inbound HTLC %A (Hash: %A) with value %O due to state (%s)"
                                   htlc.HTLCId htlc.PaymentHash htlc.Amount stateName)
                match htlc.State with
                | LocalRemoved reason when (generatedByLocal) ->
                    match reason with
                    | Fullfill _ ->
                        valueToSelfMSatOffset <- htlc.Amount + valueToSelfMSatOffset
                    | _ -> ()
                | _ -> ()
        for htlc in c.PendingOutboundHTLCs do
            let (includeThis, stateName) =
                match htlc.State with
                | LocalAnnounced _ -> (generatedByLocal, "LocalAnnounced")
                | Commited _ -> (true, "Committed")
                | RemoteRemoved _ -> (generatedByLocal, "RemoteRemoved")
                | AwaitingRemoteRevokeToRemove _ -> (generatedByLocal, "AwaitingRemoteRevokeToRemove")
                | AwaitingRemovedRemoteRevoke _ -> (false, "AwaitingRemovedRemoteRevoke")
            if includeThis then
                addHTLCOutput(htlc, true, Some(htlc.Source), stateName)
                localHTLCTotalMSat <- htlc.Amount + localHTLCTotalMSat
            else
                match htlc.State with
                | AwaitingRemoteRevokeToRemove (None )
                | AwaitingRemovedRemoteRevoke (None )
                | RemoteRemoved (None ) when (not generatedByLocal) ->
                    valueToSelfMSatOffset <- valueToSelfMSatOffset - htlc.Amount
                | _ -> ()

        let valueToSelfMSat = c.ValueToSelf - localHTLCTotalMSat + valueToSelfMSatOffset
        assert (valueToSelfMSat >= LightMoney.Zero)

        let valueToRemoteMSat = LightMoney(!> c.ChannelValueSatoshis * 1000L - !> c.ValueToSelf -
                                           !> remoteHTLCTotalMSat - !> valueToSelfMSatOffset)
        
        let totalFee = feeRatePerKW.Value * ((COMMITMENT_TX_BASE_WEIGHT + (!> txOuts.Length) * COMMITMENT_TX_WEIGHT_PER_HTLC) / 1000UL)
        let (valueToSelfSat, valueToRemoteSat) =
            if c.ChannelOutBound then
                (valueToSelfMSat / 1000L - totalFee), (valueToRemote / 1000)
            else
                (valueToSelfMSat / 1000L), (valueToRemoteMSat / 1000 - totalFee)
        let valutToASat = if local then valueToSelfSat else valueToRemoteSat
        let valutToBSat = if local then valueToRemoteSat else valueToSelfSat
        if valueToA >= dustLimitSatoshis.Satoshis then
            let txOut = TxOut(Money.Satoshi(valueToBSat), keys.BPaymentKey.Hash.ScriptPubKey)
            txOuts.Add(txOut, None)
        failwith "Not implemented"

    let getClosingScriptPubKey (c: Channel): Script =
        c.ShutdownPubKey.Hash.ScriptPubKey
        
    let getClosingTransactionWeight (aScriptPubKey: Script, bScriptPubKey: Script): uint64 =
        uint64 (4 + 1 + 36 + 4 + 1 + 1 + 2 * (8+1) + 4 + aScriptPubKey.Length + bScriptPubKey.Length * 4 + 2 + 1 + 1 + 2 * (1 + 72))

    let buildClosingTransaction (propsedTotalFeeSatoshis: Money, skipRemoteOutput: bool, n: Network) (c: Channel): (Transaction * int64) =
        let txin = TxIn()
        txin.PrevOut <- c.ChannelMonitor.GetFundingTxo().Value
        txin.Sequence <- Sequence.Final
        assert (c.PendingInboundHTLCs.IsEmpty)
        assert (c.PendingOutboundHTLCs.IsEmpty)

        let mutable totalFeeSatoshis = propsedTotalFeeSatoshis
        let valueToSelf = Money.Satoshis(c.ValueToSelf.MilliSatoshi / 1000L) - if c.ChannelOutbound then totalFeeSatoshis else Money.Zero
        let valueToRemote = Money.Satoshis((c.ChannelValueSatoshis.Satoshi * 1000L - c.ValueToSelf.MilliSatoshi) / 1000L) -
                            if c.ChannelOutbound then Money.Zero else totalFeeSatoshis
        if valueToSelf < Money.Zero then
            assert (c.ChannelOutbound)
            totalFeeSatoshis <- totalFeeSatoshis - valueToSelf
        else if valueToRemote < Money.Zero then
            assert (c.ChannelOutbound)
            totalFeeSatoshis <- totalFeeSatoshis - valueToRemote

        let mutable txOuts = List.empty
        if (not skipRemoteOutput && valueToRemote > c.OurDustLimit) then
            let txO = TxOut(valueToRemote, c.TheirShutdownScriptPubKey.Value)
            txOuts <- txO::txOuts

        if (valueToSelf > c.OurDustLimit) then
            let txO = TxOut(valueToSelf, getClosingScriptPubKey c)
            txOuts <- txO::txOuts

        let outputs = txOuts |> TransactionUtils.sortOutputs
        let tx = n.CreateTransaction()
        for o in outputs do
            tx.Outputs.Add(o) |> ignore
        tx.Inputs.Add(txin) |> ignore
        tx.LockTime <- !> 0
        tx.Version <- 2u
        (tx, totalFeeSatoshis.Satoshi)

    /// Creates a set of keys for buildCommitmentTx to generate a transaction which our counterparty
    /// will sign (ie DO NOT send signatures over a transaction created by this to our counterparty!)
    /// The result is a transaction which we can revoke ownership of (ie a "local" transaction)
    let buildLocalTransactionKeys (commitmentN: uint64) (c: Channel): TxCreationKeys =
        let perCommitmentPoint = (buildLocalCommitmentSecret commitmentN c).PubKey
        let delayedPaymentBase = c.LocalKeys.DelayedPaymentBaseKey.PubKey
        let htlcBasePoint = c.LocalKeys.HTLCBaseKey.PubKey
        {
            PerCommitmentPoint = perCommitmentPoint
            RevocationKey = delayedPaymentBase
            AHTLCKey = htlcBasePoint
            BHTLCKey = c.TheirRevocationBasePoint.Value
            ADelayedPaymentKey = c.TheirPaymentBasePoint.Value
            BPaymentKey = c.TheirHTLCBasePoint.Value
        }

    /// Creates a set of keys for build_commitment_transaction to generate a transaction which we
    /// will sign and send to our counterpaty.
    /// If an Err is returned, it is a ChannelError.Create (for PgetOutboundFundingCreated`)
    let buildRemoteTransactionKeys (c: Channel) =
        {
            PerCommitmentPoint = c.TheirCurrentCommitmentPoint.Value

            RevocationKey = c.TheirDelayedPaymentBasePoint.Value
            AHTLCKey = c.TheirHTLCBasePoint.Value
            BHTLCKey = c.LocalKeys.RevocationBaseKey.PubKey
            ADelayedPaymentKey = c.LocalKeys.PaymentBaseKey.PubKey
            BPaymentKey = c.LocalKeys.HTLCBaseKey.PubKey
        }

    /// Gets the redeem script for the funding transaction output
    let getFundingRedeemScript (c: Channel): Script =
        let ourFundingKey = c.LocalKeys.FundingKey.PubKey
        let theirFundingKey = match c.TheirFundingPubKey with
                              | Some pk -> pk
                              | None -> failwith "getFundingRedeemScript only allowed after accept_channel"
        let pks = if ourFundingKey.ToBytes() < theirFundingKey.ToBytes() then
                      [| ourFundingKey; theirFundingKey |]
                  else
                      [| theirFundingKey; ourFundingKey |]

        PayToMultiSigTemplate.Instance.GenerateScriptPubKey(2, pks)


    let signCommitmentTransaction (tx: Transaction, theirSig: TransactionSignature, n: Network) (c: Channel) =
        if tx.Inputs.Count <> 1 then
            Bad(!> "Tried to sign commitment transaction that had input count != 1")
        else if not (isNull tx.Inputs.[0].WitScript) || tx.Inputs.[0].WitScript <> WitScript.Empty then
            Bad(!> "Tried to re-sign commitment tx")
        else
            let fundingRedeemScript = getFundingRedeemScript c
            let coin = ScriptCoin(tx.Inputs.[0].PrevOut,
                                  TxOut(c.ChannelValueSatoshis, fundingRedeemScript.WitHash.ScriptPubKey),
                                  fundingRedeemScript)
            let b = n.CreateTransactionBuilder()
            Good(b.AddCoins(coin)
                .AddKeys(c.LocalKeys.FundingKey)
                .SignTransaction(tx))

    /// Builds the HTLC-success or htlc-timeout tx which spends a given HTLC output
    let buildHTLCTransaction (prevHash: TxId)
                             (htlc: HTLCOutputInCommitment)
                             (local: bool)
                             (keys: TxCreationKeys)
                             (feeRate: FeeRatePerKw)
                             (c: Channel) =
        ChannelUtils.buildHTLCTransaction (prevHash)
                                          (feeRate)
                                          (if local then c.TheirToSelfDelay else BREAKDOWN_TIMEOUT)
                                          (htlc)
                                          (keys.ADelayedPaymentKey)
                                          (keys.RevocationKey)

    let createHTLCTxSignature (tx: Transaction)
                              (htlc: HTLCOutputInCommitment)
                              (keys: TxCreationKeys)
                              (n: Network)
                              (c: Channel): RResult<(Script * TransactionSignature * bool)> =

        if tx.Inputs.Count <> 1 then
            Bad(!> "Tried to sign HTLC transaction that had input count != 1")
        else
            let htlcRedeemScript = getHTLCRedeemScript htlc keys
            let ourHTLCKey = derivePrivateKey keys.PerCommitmentPoint c.LocalKeys.HTLCBaseKey
            let amount = Money.Satoshis(htlc.Amount.MilliSatoshi / 1000L)
            let p = n.CreateTransactionBuilder()
                     .AddKeys(ourHTLCKey)
                     .AddCoins(ScriptCoin(tx.Inputs.[0].PrevOut,
                                          TxOut(amount, htlcRedeemScript.WitHash.ScriptPubKey),
                                          htlcRedeemScript))
                     .BuildPSBT(true)
            let signature = p.Inputs.[0].PartialSigs.Values.First()
            let isLocalTx = ourHTLCKey.PubKey = keys.AHTLCKey
            Good(htlcRedeemScript, signature, isLocalTx)

    /// Signs a transaction created by buildHTLCTransaction. IF an transaction is an
    /// HTLC-Success transaction (i.e. htlc.offered is false), preimage must be set
    let signHTLCTransaction (tx: Transaction)
                            (theirSig: ECDSASignature)
                            (preImage: PaymentPreimage option)
                            (htlc: HTLCOutputInCommitment) 
                            (keys: TxCreationKeys)
                            (n: Network)
                            (c: Channel): RResult<TransactionSignature> =
        if tx.Inputs.Count <> 1 then
            Bad(!> "Tried to sign HTLC transaction that had input count != 1")
        else if not (isNull tx.Inputs.[0].WitScript) || tx.Inputs.[0].WitScript <> WitScript.Empty then
            Bad(!> "Tried to re-sign HTLC transaction")
        else 
            match createHTLCTxSignature tx htlc keys n c with
            | Bad b -> Bad b
            | Good (htlcRedeemScript, ourSig, localTx) ->
                let amount = Money.Satoshis(htlc.Amount.MilliSatoshi / 1000L)
                let p = n.CreateTransactionBuilder()
                         .AddCoins(ScriptCoin(tx.Inputs.[0].PrevOut,
                                              TxOut(amount, htlcRedeemScript.WitHash.ScriptPubKey),
                                              htlcRedeemScript))
                let sb = StringBuilder()
                sb.Append("0") |> ignore
                
                if localTx then
                    let pks = htlcRedeemScript.GetAllPubKeys()
                    sb.AppendFormat(" {0} {1}", TransactionSignature(theirSig, SigHash.All), ourSig) |> ignore
                else
                    let pks = htlcRedeemScript.GetAllPubKeys()
                    sb.AppendFormat(" {0} {1}", ourSig, TransactionSignature(theirSig, SigHash.All)) |> ignore

                (if htlc.Offered then sb.Append(" 0") else sb.AppendFormat(" {0}", preImage.Value.Value)) |> ignore
                sb.AppendFormat(" {0}", htlcRedeemScript) |> ignore
                tx.Inputs.[0].WitScript <- WitScript(sb.ToString())
                Good(ourSig)

    /// Per HTLC, only one get_update_fail_htlc or get_update_fulfill_htlc call may be made.
    /// In such cases we debug_assert!(false) and return an IgnoreError. Thus, will always return
    /// Ok(_) if debug assertions are turned on and preconditions are met.
    let getUpdateFullFillHTLC(htlcIdArg: HTLCId)
                             (PaymentPreimage paymentPreimageArg)
                             (c: Channel): RResult<(UpdateFulfillHTLC * Option<ChannelMonitor>)> =
        if (c.ChannelState &&& ChannelState.ChannelFunded) <> (ChannelState.ChannelFunded) then
            failwith "Was asked to fulfill an HTLC when channel was not in an operational state"
        else
            assert (c.ChannelState &&& ChannelState.ShutdownComplete = ChannelState.Zero)
            let paymentHashCalc = PaymentHash(uint256(Hashes.SHA256(paymentPreimageArg.ToBytes()), false))

            /// ChannelManager may generate duplicate claims/fails due to HTLC update events from
            /// On-chain ChannelMonitors during block rescan. Ideally we'd figure out a way to drop
            /// These, but for now we just have to treat them as normal.
            let pendingIdx: RResult<int32> =
                c.PendingInboundHTLCs
                |> List.filter(fun htlc -> htlc.HTLCId = htlcIdArg)
                |> List.mapi(fun i htlc -> 
                    match htlc.State with
                    | InboundHTLCState.Commited -> i
                    | LocalRemoved reason ->
                        match reason with
                        | InboundHTLCRemovalReason.Fullfill _ ->
                            i
                        | _ ->
                            let msg = sprintf "Have preimage and want to fulfill HTLC with payment hash %A we already failed against channel %A"
                                              (htlc.PaymentHash)
                                              (c.ChannelId)
                            c.Logger.LogWarning(msg)
                            i
                    | _ -> failwith "Have an inbound HTLC we tried to claim before it was fully committed to!"
                )
                |> List.tryLast
                |> function | Some g -> Good g
                            | None -> Bad (!> ChannelException(Ignore("Unable to find a pending HTLC which match the given HTLC ID")))


            if (c.ChannelState &&& (ChannelState.AwaitingRemoteRevoke ||| ChannelState.PeerDisconnected ||| ChannelState.MonitorUpdateFailed) <> ChannelState.Zero) then
                c.HoldingCellHTLCUpdates
                failwith ""
            else
                failwith ""
            failwith ""