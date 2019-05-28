namespace DotNetLightning.LN
open DotNetLightning.Utils
open NBitcoin
open System
open DotNetLightning
open BTCPayServer.Lightning
open Microsoft.Extensions.Logging
open DotNetLightning.Chain
open DotNetLightning.Utils.RResult
open DotNetLightning.Serialize.Msgs

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
    | OurInitSent =        1u
    | TheirInitSent =      2u
    | FundingCreated =     4u
    | FundingSent =        16u
    | TheirFundingLocked = 32u
    | OurFundingLocked =   64u
    | ChannelFunded =      128u
    | PeerDisconnected =   256u
    | MonitorUpdateFailed = 512u
    | AwaitingRemoteRevoke = 1024u
    | RemoteShutdownSent = 2048u
    | LocalShutdownSent = 4096u

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

    let BREAKDOWN_TIMEOUT = DateTime(0, 0, 7, 24, 60, 0)
    let MAX_LOCAL_BREAKDOWN_TIMEOUT = DateTime(0, 0, 14, 24, 60, 0)

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
    UserId: uint64
    ChannelId: uint256
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
    FeeRatePerKw: FeeRate
    MaxCommitmentTxOutputLocal: (LightMoney * LightMoney)
    MaxCommitmentTxOutputRemote: (LightMoney * LightMoney)
    LastLocalCommitmentTxn: Transaction list
    LastSentClosingFee: (uint64 * uint64) option
    FundingTxConfirmedIn: TxId option
    ShortChannelId: ShortChannelId option

    LastBlockConnected: BlockId
    FundingTxConfirmations: uint64

    TheirDustLimit: LightMoney
    OurDustLimit: LightMoney
    TheirMaxHTLCValueInFlight: LightMoney

    TheirChannelReserve: LightMoney
    TheirHTLCMinimum: LightMoney
    OurHTLCMinimum: LightMoney
    TheirToSelfDelay: uint16
    TheirMaxAcceptedHTLCs: uint16
    MinimumDepth: uint16

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
    let deriveOurDustLimitSatoshis(atOpenBackGroundFeeRate: FeeRate): LightMoney =
        failwith "Not implemnted"

    /// TODO:
    let deriveOurHTLCMinimum(atOpenBackGroundFeeRate: FeeRate): LightMoney =
        LightMoney.Satoshis(1000UL)

    let private getChannel (UserId userId)
                           (config: UserConfig)
                           (channKeys)
                           (theirNodeId)
                           (keysProvider: IKeysRepository)
                           (logger: ILogger)
                           (channelMonitor)
                           (backGroundFeeRate)
                           (channelValue: Money)
                           (pushMSat)
                           (fee: FeeRate): Channel =
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
            TheirDustLimit = LightMoney.Zero
            OurDustLimit = deriveOurDustLimitSatoshis(backGroundFeeRate)
            TheirMaxHTLCValueInFlight = LightMoney.Zero
            TheirChannelReserve = LightMoney.Zero
            TheirHTLCMinimum = LightMoney.Zero
            OurHTLCMinimum = deriveOurHTLCMinimum(fee)
            TheirToSelfDelay = 0us
            TheirMaxAcceptedHTLCs = 0us
            MinimumDepth = 0us
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
            if (getOurChannelReserve(channelValue).Satoshi < deriveOurDustLimitSatoshis(backgroundFeeRate).MilliSatoshi * 1000L) then
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

    let public checkRemoteFee(feeEstimator: IFeeEstimator, feeRatePerKw: FeeRate) =
        if (feeRatePerKw < feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Background) ) then
            RResult.Bad(!> ChannelException(Close("Peer's Feerate much to low")))
        else if (feeRatePerKw.FeePerK > feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority).FeePerK * 2) then
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
            else
                Good()

        checkMsg1 msg
            >>= fun _ -> checkRemoteFee(feeEstimator, FeeRate(msg.FeeRatePerKw))
