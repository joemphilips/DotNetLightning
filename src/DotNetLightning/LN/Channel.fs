namespace DotNetLightning.LN
open DotNetLightning.Utils
open NBitcoin
open System
open System.Text
open System.Linq
open DotNetLightning
open Microsoft.Extensions.Logging
open DotNetLightning.Chain
open DotNetLightning.Utils.RResult
open DotNetLightning.Utils.Aether
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Transactions
open DotNetLightning.Utils.NBitcoinExtensions
open NBitcoin.Crypto

type ChannelValueStat = internal {
    ValueToSelf: LNMoney;
    ChannelValue: LNMoney;
    ChannelReserve: LNMoney;
    PendingOutboundHTLCsAmount: LNMoney;
    PendingInBoundHTLCsAmount: LNMoney;
    HoldingCellOutBoundAmount: LNMoney;
}

type InboundHTLCRemovalReason =
    internal
    | FailRelay of OnionErrorPacket
    | FailMalformed of uint256 * uint16
    | Fullfill of PaymentPreimage

type InboundHTLCState =
    internal
    /// added by remote, to be included in next local commitment tx
    | RemoteAnnounced of PendingHTLCStatus
    /// Included in a received commitment_signed message (implying we've revoke_and_ack'd it), but
    /// the remote side hasn't et revoked their previous state, which we need them to do before we
    /// acept this htlc. Implies AwaitingRemoteRevoke.
    /// We also have not yet included this HTLC in a commitment_signed message, and are waiting on
    /// a remote revoke_and_ack on a previous state before we can do so.
    | AwaitingRemoteRevokeToAnnounce of PendingHTLCStatus
    /// Included in a received commitment_signed message (implying we've revoke_and_ack'ed it), but
    /// the remote side hasn't yet revoked their previous state, which we need them to do before we
    /// accept this HTLC. Implies AwaitingRemoteRevoke.
    /// We have included this HTLC in our latest commitment_signed and are now just waiting on a
    /// revoke_and_ack.
    | AwaitingAnnouncedRemoteRevoke of PendingHTLCStatus
    | Commited
    /// Removed by us and a new commitment_signed was sent (if we were AwaitingRemoteRevoke when we
    /// created it we would have put it in the holding cell instead). When they next revoke_and_ack
    /// we'll drop it.
    /// Note that we have to keep eye on the HTLC until we've revoked HTLCs that we can't claim
    /// it before the timeout (obviously doesn't apply to revoked HTLCs that we can't claim anyway).
    /// That said, ChannelMonitor does this for us (see ChannelMonitor.WouldBroadcastAtHeight)
    /// so we actually remove the HTLC from our own local state before then, once we're sure that
    /// the next commitment_signed and ChannelMonitor.ProvideLatestLocalCommitmentTxInfo will not
    /// included this HTLC.
    | LocalRemoved of InboundHTLCRemovalReason

type InboundHTLCOutput = internal {
    HTLCId: Primitives.HTLCId
    Amount: LNMoney
    CLTVExpiry: uint32
    PaymentHash: PaymentHash
    State: InboundHTLCState
}

type OutboundHTLCState =
    internal
    /// Added by us and include in a commitment_signed (if we were AwaitingRemoteRevoke when we
    /// created it we would have put it in the holding cell instead). When they next revoke_and_ack
    /// we will promite to Commitmed (note that they may not accept it until the next time we
    /// revoke, but we don't really care about that:
    ///  * they've revoked, so worst case we can announce an old state and get our (option on)
    ///    money back (though we won't), and,
    ///  * we'll send them a revoke when they send a commitment_signed, and since only they're
    ///    allowed to remove it, the "can only be removed once commited on both sides" requirement
    ///    doesn't matter to us and it's up to them to enforce it, worst-case they jump ahead but
    ///    we'll never get out of sync).
    /// Note that we Box the OnionPacket as it's rather large and we don't want to blow up
    /// OutboundHTLCOutput's size just for a temporary bit
    | LocalAnnounced of OnionPacket
    | Commited
    /// Remote removed this (outbound) HTLC. We're waiting on their commitment_signed to finalize
    /// the change (though they'll need to revoke before we fail the payment).
    | RemoteRemoved of HTLCFailReason option
    /// Remote removed this and sent a commitment_signed (implying we've revoke_and_ack'ed it), but
    /// the remote side hasn't yet revoked their previous state, which we need them to do before we
    /// can do any backwards failing. Implies AwaitingRemoteRevoke.
    /// We also have not yet remove this HTLC in a commitment_signed message, and are waiting on a
    /// remote revoke_and_ack on a previous state before we can do so.
    | AwaitingRemoteRevokeToRemove of HTLCFailReason option
    /// Remote removed this and sent a commitment_signed (implying we've revoke_and_ack'ed it), but
    /// the remote side hasn't yet revoked their previous state, which we we need them to do before we
    /// can do any backwards failing. Implies AwaitingRemoteRevoke.
    /// We have removed this HTLC in our latest commitment_signed and are now just waiting on a 
    /// revoke_and_ack to drop completely.
    | AwaitingRemovedRemoteRevoke of HTLCFailReason option

type OutboundHTLCOutput = {
    HTLCId: HTLCId
    Amount: LNMoney
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

/// There are a few "states" and then a number of flags which can be applied:
/// We first move through init with OurInitSent -> TheirInitSent -> FundingCreated -> FundingSent.
/// TheirFundingLocked and OurFundingLocked then get set on FundingSent, and when both are set we
/// move on to ChannelFunded.
/// Note that PeerDisconnected can be set on both ChannelFunded and FundingSent.
/// ChannelFunded can then get all remaining flags set on it, until we finish shutdown, then we
/// move on to ShutdownComplete, at which point most calls into this channel are disallowed
[<Flags>]
type ChannelState =
    /// Should not used in real situaion. Because we always send (openchannel/acceptc_channel)
    /// when initalize a channel
    | Zero =                 0u
    /// Implies we have (or are prepared to) send our open_channel_accept_channel message
    | OurInitSent =          1u
    /// Implies we have received their open_channel/accept_channel message
    | TheirInitSent =        2u
    /// We have sent funding_created and are awaiting a funding_signed to advance to FundingSent.
    /// Note that this is nonsense for an inbound channel as we immediately generate funding_signed
    /// upon receipt of funding_created, so simply skip this state.
    | FundingCreated =       4u
    /// Set when we have received/sent funding_created and funding_signed and are thus now waiting
    /// on the funding transaction to confirm. The FundingLocked flags are set to indicate when we
    /// and our counterparty consider the funding transaction confirmed.
    | FundingSent =          8u
    /// Flags which can be set on FundingSent to indicate they sent us a funding_locked message.
    /// Once both TheirFundingLocked and OurFundingLocked are set, state moves to ChannelFunded.
    | TheirFundingLocked =   16u
    /// Flag which can be set on FundingSent to indicate we sent them a funding_locked message.
    /// Once both TheirFundingLocked and OurFundingLocked are set, state moves on to ChannelFunded.
    | OurFundingLocked =     32u
    | ChannelFunded =        64u
    /// Flags which is set on ChannelFunded and FundingSent indicating remote side is considered
    /// "disconnected" and no updates are allowed until after we've done a channel_reestablish
    /// dance.
    | PeerDisconnected =     128u
    /// Flag which is set on ChannelFunded and FundingSent indicating the user has told us they
    /// failed to upate our ChannelMonitor somewhere and we should pause sending any outbound
    /// messages until they've managed to do so.
    | MonitorUpdateFailed =  256u
    /// Flag which implies that we have sent a commitment_signed but are awaiting the responding
    /// revoke_and_ack message. During this time period, we can't generate new commitment_signed
    /// messages as then we will be unable to determine which HTLCs they included in their
    /// revoke_and_ack implicit ACK, so instead we have to hold them away temporarily to be sent
    /// later.
    /// Flag is set on ChannelFunded.
    | AwaitingRemoteRevoke = 512u
    /// Flag which is set on ChannelFunded or FundingSent after receiving a shutdown message from
    /// the remote end. If set, they may not add any new HTLCs to the channel, and we are expected
    /// to respond with our own shutdown message when possible.
    | RemoteShutdownSent =   1024u
    /// Flag which is set on ChannelFunded or FundingSent after sending a shotdown message. At this
    /// point, we may not add any new HTLCs to the channel.
    /// TODO: Investigate some kind of timeout mechanism by which point the remote end must provide
    /// us their shutdown.
    | LocalShutdownSent =    2048u
    /// We've successfully negotiated a closing_signed dance. At this point ChannelManager is about
    /// to drop us, but we store this anyway.
    | ShutdownComplete =     4096u

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
    /// see refs: https://github.com/lightningnetwork/lightning-rfc/blob/master/07-routing-gossip.md#requirements
    let UNCONF_THRESHOLD = 6u

    /// The amount of time we require our counterparty wait to claim their money (i.e. time between when
    /// we, or our watchtower, mush check for them having a broadcast a theft transaction).
    let BREAKDOWN_TIMEOUT = BlockHeightOffset(6us * 24us * 7us) // one week
    let MAX_LOCAL_BREAKDOWN_TIMEOUT = BlockHeightOffset(6us * 24us * 14us) // two weeks

    /// Specified in BOLT 11
    let MIN_CLTV_EXPIRY = 9us |> BlockHeightOffset

    let MAX_CLTV_EXPIRY = BREAKDOWN_TIMEOUT

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

    /// Specified in BOLT #2
    let MAX_FUNDING_SATOSHIS = Money.Satoshis(16777216m) // (1 << 24)


    [<Literal>]
    let ACCEPTED_HTLC_SCRIPT_WEIGHT = 139uy
    [<Literal>]
    let OFFERED_HTLC_SCRIPT_WEIGHT = 133uy

type Channel = internal {
    Config: ChannelConfig
    UserId: UserId
    ChannelId: ChannelId
    ChannelState: ChannelState
    ChannelOutbound: bool
    ChannelValue: Money
    LocalKeys: ChannelKeys
    ShutdownPubKey: PubKey
    CurrentLocalCommitmentTxNumber: uint64
    CurrentRemoteCommitmentTxNumber: uint64
    ValueToSelf: LNMoney
    PendingInboundHTLCs: InboundHTLCOutput list
    PendingOutboundHTLCs: OutboundHTLCOutput list
    HoldingCellHTLCUpdates: HTLCUpdateAwaitingACK list
    /// RAA stands for "revoke_and_ack"
    ResendOrder: RAACommitmentOrder
    MonitorPendingRevokeAndAck: bool
    MonitorPendingCommitmentSigned: bool
    MonitorPendingForwards: (PendingForwardHTLCInfo * uint64) list
    MonitorPendingFailures: (HTLCSource * PaymentHash * HTLCFailReason) list
    PendingUpdateFee: LNMoney option
    HoldingCellUpdateFee: LNMoney option
    NextLocalHTLCId: HTLCId
    NextRemoteHTLCId: HTLCId
    ChannelUpdateCount: uint32
    FeeRatePerKw: FeeRatePerKw
    MaxCommitmentTxOutputLocal: (LNMoney * LNMoney)
    MaxCommitmentTxOutputRemote: (LNMoney * LNMoney)
    LastLocalCommitmentTxn: Transaction list
    LastSentClosingFee: (uint64 * uint64) option
    FundingTxConfirmedIn: TxId option
    ShortChannelId: ShortChannelId option

    LastBlockConnected: BlockId
    FundingTxConfirmations: uint64

    TheirDustLimit: Money
    OurDustLimit: Money
    TheirMaxHTLCValueInFlight: LNMoney

    TheirChannelReserve: Money
    TheirHTLCMinimum: LNMoney
    OurHTLCMinimum: LNMoney
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
    Network: Network
}
    with

        static member internal Config_ =
            (fun c -> c.ChannelState), (fun s c -> { c with ChannelState = s})

        static member internal LocalKeys_ =
            (fun c -> c.LocalKeys), (fun s c -> { c with LocalKeys = s})

        static member internal PendingInboundHTLCs_ =
            (fun c -> c.PendingInboundHTLCs), (fun v c -> { c with PendingInboundHTLCs = v })

        static member internal PendingOutboundHTLCs_ =
            (fun c -> c.PendingOutboundHTLCs), (fun v c -> { c with PendingOutboundHTLCs = v })

        static member internal PendingUpdateFee_: Prism<Channel, LNMoney> =
            (fun c -> c.PendingUpdateFee), (fun pendingUpdateFee c -> { c with PendingUpdateFee = Some pendingUpdateFee})

        static member internal HoldingCellUpdateFee_ =
            (fun c -> c.HoldingCellUpdateFee), (fun holdingCellUpdateFee c -> { c with HoldingCellUpdateFee = Some holdingCellUpdateFee })

        static member internal TheirToSelfDelay_: Lens<_,_> =
            (fun c -> c.TheirToSelfDelay),
            (fun v c -> { c with TheirToSelfDelay = v})
        
        static member internal OurDustLimit_ : Lens<_,_> =
            (fun c -> c.OurDustLimit),
            (fun v c -> { c with OurDustLimit = v})

        static member internal ChannelMonitor_ : Lens<_,_> =
            (fun c -> c.ChannelMonitor),
            (fun v c -> { c with ChannelMonitor = v})

        static member internal TheirFundingPubKey_: Prism<_, _> =
            (fun c -> c.TheirFundingPubKey),
            (fun v c -> { c with TheirFundingPubKey = Some v})


type ChannelError =
    | Ignore of string
    | Close of string

exception ChannelException of ChannelError


[<RequireQualifiedAccess>]
module Channel =
    let private RREx(ex: ChannelError) =
        RResult.rexn(ChannelException(ex))

    let private RRApiE(err: APIError) =
        RResult.rbad(RBad.Object(err))

    let getOurMaxHTLCValueInFlight (channelValue: Money) =
        channelValue * 1000L / 10L

    let getOurChannelReserve (channelValue: Money) =
        let q = channelValue / 100L
        Money.Min(channelValue, Money.Max(q, Money.Satoshis(1L)))

    /// TODO:
    let deriveOurDustLimitSatoshis(FeeRatePerKw atOpenBackGroundFee): Money =
        (Money.Satoshis((uint64 atOpenBackGroundFee) * B_OUTPUT_PLUS_SPENDING_INPUT_WEIGHT / 1000UL), Money.Satoshis(546UL))
        |> Money.Max


    /// TODO:
    let deriveOurHTLCMinimum(FeeRatePerKw atOpenBackGroundFee): LNMoney =
        LNMoney.Satoshis(1000UL)

    let private getChannel (userId: UserId)
                           (config: UserConfig)
                           (network: Network)
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
            ChannelValue = channelValue
            LocalKeys = channKeys
            ShutdownPubKey = keysProvider.GetShutdownPubKey()
            CurrentLocalCommitmentTxNumber = INITIAL_COMMITMENT_NUMBER
            CurrentRemoteCommitmentTxNumber = INITIAL_COMMITMENT_NUMBER
            ValueToSelf = LNMoney.MilliSatoshis(channelValue.Satoshi * 1000L) - pushMSat
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

            MaxCommitmentTxOutputLocal = (LNMoney.MilliSatoshis(channelValue.Satoshi * (1000L)) - pushMSat, pushMSat)
            MaxCommitmentTxOutputRemote = (LNMoney.MilliSatoshis(channelValue.Satoshi * (1000L)) - pushMSat, pushMSat)

            LastLocalCommitmentTxn = List.empty

            LastSentClosingFee = None
            FundingTxConfirmedIn = None
            ShortChannelId = None
            LastBlockConnected = BlockId(uint256.Zero)
            FundingTxConfirmations = 0UL

            FeeRatePerKw = fee
            TheirDustLimit = Money.Zero
            OurDustLimit = deriveOurDustLimitSatoshis(backGroundFeeRate)
            TheirMaxHTLCValueInFlight = LNMoney.Zero
            TheirChannelReserve = Money.Zero
            TheirHTLCMinimum = LNMoney.Zero
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
            Network = network
        }

    // ----- constructors -------
    let public newOutBound(feeEstimator: IFeeEstimator,
                           keyProvider: IKeysRepository,
                           NodeId theirNodeId,
                           network: Network,
                           channelValue: Money,
                           pushMSat: LNMoney,
                           userId: UserId,
                           logger: ILogger,
                           config: UserConfig): RResult<Channel>  =

        /// ------ Validators -----
        let checkSmallerThenMaxPossible(channelValue) =
            if (channelValue >= MAX_FUNDING_SATOSHIS) then
                RRApiE(APIMisuseError "Funding value. 2^24")
            else
                Good(channelValue)

        let checkPushValueLessThanChannelValue (pushMSat: LNMoney) (channelValue: Money) =
            if pushMSat.Satoshi >= (channelValue.Satoshi) then
                RRApiE(APIMisuseError("push value > channel value"))
            else
                Good(pushMSat)

        let checkBackgroundFeeRate (estimator: IFeeEstimator) (channelValue) =
            let backgroundFeeRate = estimator.GetEstSatPer1000Weight(ConfirmationTarget.Background)
            let cR = getOurChannelReserve(channelValue)
            let dustLimit = deriveOurDustLimitSatoshis(backgroundFeeRate)
            if (cR < dustLimit) then
                RRApiE(FeeRateTooHigh({ Msg = (sprintf "Not enough reserve above dust limit can be found at current fee rate(%O)" backgroundFeeRate); FeeRate = backgroundFeeRate }))
            else
                Good(backgroundFeeRate)

        let feeRate =
            feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Normal)

        let channelKeys = keyProvider.GetChannelKeys(false)
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
                                           (network)
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
            RREx(Close("Peer's Feerate much to low"))
        else if (feeRatePerKw.Value > feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority).Value * 2u) then
            RREx(Close("Peer's feerate much too high"))
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
        let chanKeys = keysProvider.GetChannelKeys(true)
        let localConfig = config.ChannelOptions

        let checkMsg1 msg =
            if (msg.FundingSatoshis >= MAX_FUNDING_SATOSHIS) then
                RREx(Close("Funding value > 2^24"))
            else if (msg.ChannelReserveSatoshis > msg.FundingSatoshis) then
                RREx(Close("Bogus ChannelReserveSatoshis"))
            else if (msg.PushMSat.MilliSatoshi > (msg.FundingSatoshis - msg.ChannelReserveSatoshis).Satoshi * 1000L) then
                RREx(Close("PushMsat larger than funding value"))
            else if (msg.DustLimitSatoshis > msg.FundingSatoshis) then
                RREx(Close("Peer neve rwants payout outputs?"))
            else if (msg.DustLimitSatoshis > msg.ChannelReserveSatoshis) then
                RREx(Close("Bogus; Channel reserve is less than dust limit"))
            else if (msg.HTLCMinimumMsat.MilliSatoshi >= (msg.FundingSatoshis - msg.ChannelReserveSatoshis).Satoshi * 1000L) then
                RREx(Close("Minimum HTLC value is full channel value"))
            else
                Good()

        let checkMsg2 msg =
            if (msg.ToSelfDelay > MAX_LOCAL_BREAKDOWN_TIMEOUT) then
                RREx(Close("They wanted our payments to be delayed by a needlessly long period"))
            else if (msg.MaxAcceptedHTLCs < 1us) then
                RREx(Close("0 max_accepted_htlcs makes for a useless channel"))
            else if (msg.MaxAcceptedHTLCs > 483us) then
                RREx(Close("max_accepted_htlcs > 483"))
            else
                Good()

        let checkMsgAgainstConfig config msg =
            let theirAnnounce = (msg.ChannelFlags &&& 1uy) = 1uy
            if (msg.FundingSatoshis < config.PeerChannelConfigLimits.MinFundingSatoshis) then
                RREx(Close("dust limit satoshis is less than the user specified limit"))
            else if (msg.HTLCMinimumMsat > config.PeerChannelConfigLimits.MaxHTLCMinimumMSat) then
                RREx(Close("HTLC minimum msat is higher than the user specified limit"))
            else if (msg.MaxHTLCValueInFlightMsat < config.PeerChannelConfigLimits.MinMaxHTLCValueInFlightMSat) then
                RREx(Close("Max htlc value in flight msat is less than the user specified limit"))
            else if (msg.ChannelReserveSatoshis > config.PeerChannelConfigLimits.MaxChannelReserveSatoshis) then
                RREx(Close("Channel resere satoshis is higher than the user specified limit"))
            else if (msg.MaxAcceptedHTLCs < config.PeerChannelConfigLimits.MinMaxAcceptedHTLCs) then
                RREx(Close("dust limit satoshis is less than the user specified limit"))
            else if (msg.DustLimitSatoshis < config.PeerChannelConfigLimits.MinDustLimitSatoshis ) then
                RREx(Close("dust limit satoshis is less than the user specified limit"))
            else if (msg.DustLimitSatoshis > config.PeerChannelConfigLimits.MaxDustLimitSatoshis) then
                RREx(Close("dust limit satoshis is greater than the user specified limit"))
            else if (config.PeerChannelConfigLimits.ForceAnnouncedChannelPreference && localConfig.AnnouncedChannel <> theirAnnounce) then
                RREx(Close("Peer tried to open channel but their announcement is different from ours"))
            else
                Good()

        let prepareDustLimit (feeEstimator: IFeeEstimator) (msg: OpenChannel) =
            let backgroundFeeRate = feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Background)
            let ourDustLimitSatoshis = deriveOurDustLimitSatoshis(backgroundFeeRate)
            let ourChannelReserveSatoshis = getOurChannelReserve(msg.ChannelReserveSatoshis)
            // check if the funder's amount for the initial commitment tx is sufficient for fee payment
            let fundersAmountMSat = LNMoney.MilliSatoshis(msg.FundingSatoshis.Satoshi * 1000L - msg.PushMSat.MilliSatoshi)

            let toLocal = msg.PushMSat
            let toRemote = fundersAmountMSat.MilliSatoshi - (int64 backgroundFeeRate.Value) * (int64 COMMITMENT_TX_BASE_WEIGHT)
            if (ourChannelReserveSatoshis < ourDustLimitSatoshis) then
                RREx(Close("Suitable channel reserve not found. aborting"))
            else if (msg.ChannelReserveSatoshis < ourDustLimitSatoshis) then
                RREx(Close("channel_reserve_satoshis too small"))
            else if (ourChannelReserveSatoshis < msg.DustLimitSatoshis) then
                RREx(Close("Dust limit too high for our channel reserve"))
            else if fundersAmountMSat.MilliSatoshi < (int64 backgroundFeeRate.Value) * (int64 COMMITMENT_TX_BASE_WEIGHT) then
                RREx(Close("Insufficient funding amount for initial commitment"))
            else if toLocal.MilliSatoshi <= msg.ChannelReserveSatoshis.Satoshi * 1000L && toRemote <= ourChannelReserveSatoshis.Satoshi * 1000L then
                RREx(Close("Insufficient funding amount for initial commitment"))
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
                       (network: Network)
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

                MaxCommitmentTxOutputLocal = (pushMSat, LNMoney.MilliSatoshis(channelValue.Satoshi * (1000L)) - pushMSat)
                MaxCommitmentTxOutputRemote = (pushMSat, LNMoney.MilliSatoshis(channelValue.Satoshi * (1000L)) - pushMSat)

                LastLocalCommitmentTxn = List.empty

                LastSentClosingFee = None
                FundingTxConfirmedIn = None
                ShortChannelId = None
                LastBlockConnected = BlockId(uint256.Zero)
                FundingTxConfirmations = 0UL

                FeeRatePerKw = msg.FeeRatePerKw
                ChannelValue = msg.FundingSatoshis
                TheirDustLimit = msg.DustLimitSatoshis
                OurDustLimit = ourDustLimitSatoshis
                TheirMaxHTLCValueInFlight = LNMoney(Math.Min(msg.MaxHTLCValueInFlightMsat.MilliSatoshi, msg.FundingSatoshis.Satoshi))
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
                Network = network
            }

        checkMsg1 msg
            >>= fun _ -> checkRemoteFee(feeEstimator, msg.FeeRatePerKw)

    let buildLocalCommitmentSecret index c : Key =
        let res = ChannelUtils.buildCommitmentSecret(c.LocalKeys.CommitmentSeed, index)
        Key(res.ToBytes())


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

    /// Transaction nomenclature is somewhat confusing here as there are many different cases - a
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
                                   (localIsFunder: bool)
                                   (generatedByLocal: bool)
                                   (feeRatePerKW: FeeRatePerKw)
                                   (c: Channel) = //(Transaction * uint32 * (HTLCOutputInCommitment * HTLCSource option) list) =
        let fundingTxCoin = c.ChannelMonitor.GetFundingCoin().Value
        Transactions.makeCommitTx
            fundingTxCoin
            commitmentN
            (c.LocalKeys.PaymentBaseKey.PubKey)
            (c.TheirPaymentBasePoint.Value)
            (localIsFunder)
            (c.OurDustLimit)
            (c.LocalKeys.RevocationBaseKey.PubKey)
            (c.TheirToSelfDelay)
            (c.TheirDelayedPaymentBasePoint.Value)
            (c.TheirPaymentBasePoint.Value)
            (c.LocalKeys.HTLCBaseKey.PubKey)
            (c.TheirHTLCBasePoint.Value)
        (*
        let obscuredCommitmentTxN =
            let factor = (Transactions.getCommitmentTxNumberObscureFactor(c.ChannelOutbound) (c.LocalKeys.PaymentBaseKey.PubKey) (c.TheirPaymentBasePoint.Value))
            factor ^^^ (INITIAL_COMMITMENT_NUMBER - commitmentN)
        let txins =
            let txin = TxIn()
            txin.PrevOut <- c.ChannelMonitor.GetFundingTxo().Value
            txin.Sequence <- Sequence(((0x80u <<< 8 * 3) ||| ((uint32 obscuredCommitmentTxN) >>> 3 * 8)))
            [txin]
        let mutable txOuts: (TxOut * (HTLCOutputInCommitment * HTLCSource option) option) list = List.empty
        let mutable includedDustHTLCs: (HTLCOutputInCommitment * HTLCSource option) list = List.empty
        let dustLimitSatoshis = if local then c.OurDustLimit else c.TheirDustLimit
        let mutable remoteHTLCTotalMSat = LNMoney.Zero
        let mutable localHTLCTotalMSat = LNMoney.Zero
        let mutable valueToSelfMSatOffset = LNMoney.Zero

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
                addHTLCOutput(HTLCOutPut.Inbound(htlc), false, None, stateName)
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
                addHTLCOutput(HTLCOutPut.OutBound(htlc), true, Some(htlc.Source), stateName)
                localHTLCTotalMSat <- htlc.Amount + localHTLCTotalMSat
            else
                match htlc.State with
                | AwaitingRemoteRevokeToRemove (None )
                | AwaitingRemovedRemoteRevoke (None )
                | RemoteRemoved (None ) when (not generatedByLocal) ->
                    valueToSelfMSatOffset <- valueToSelfMSatOffset - htlc.Amount
                | _ -> ()

        let valueToSelfMSat = c.ValueToSelf - localHTLCTotalMSat + valueToSelfMSatOffset
        assert (valueToSelfMSat >= LNMoney.Zero)

        let valueToRemoteMSat = LNMoney(c.ChannelValue.Satoshi * 1000L - c.ValueToSelf.MilliSatoshi -
                                           remoteHTLCTotalMSat.MilliSatoshi - valueToSelfMSatOffset.MilliSatoshi)
        
        let totalFee = int64((uint64 feeRatePerKW.Value) * ((COMMITMENT_TX_BASE_WEIGHT + (uint64 txOuts.Length) * COMMITMENT_TX_WEIGHT_PER_HTLC) / 1000UL))
        let (valueToSelfSat, valueToRemoteSat) =
            if c.ChannelOutbound then
                (valueToSelfMSat.MilliSatoshi / 1000L - totalFee), (valueToRemoteMSat.MilliSatoshi / 1000L)
            else
                (valueToSelfMSat.MilliSatoshi / 1000L), (valueToRemoteMSat.MilliSatoshi / 1000L - totalFee)
        let valueToASat = (if local then valueToSelfSat else valueToRemoteSat) |> Money.Satoshis
        let valueToBSat = (if local then valueToRemoteSat else valueToSelfSat) |> Money.Satoshis

        if valueToASat >= dustLimitSatoshis then
            txOuts <-
                let redeem = ChannelUtils.getRevokableRedeemScript(keys.RevocationKey)
                                                                   (if (local) then c.TheirToSelfDelay else BREAKDOWN_TIMEOUT)
                                                                   (keys.ADelayedPaymentKey)
                let txOut = TxOut(valueToASat, redeem.WitHash.ScriptPubKey)
                txOuts @ [txOut, None]
        if (valueToBSat >= dustLimitSatoshis) then
            txOuts <-
                let spk = keys.BPaymentKey.WitHash.ScriptPubKey
                let txOut = TxOut(valueToBSat, spk)
                txOuts @ [txOut, None]

        let htlcsInclued = txOuts
                           |> List.mapi(fun idx o ->
                            let metadata = snd o
                            if metadata.IsNone then
                                None
                            else
                                let (htlc, sourceOption) = metadata.Value
                                Some ({ htlc with TransactionOutputIndex = Some (uint32 idx) }, sourceOption)
                            )
                           |> List.choose id
        let nonDustHTLCCount = htlcsInclued.Length |> uint32
        let resultTx =
            let tx = c.Network.CreateTransaction()
            tx.Version <- 2u
            tx.LockTime <- !> ((0x20u <<< 8*3) ||| (uint32 (obscuredCommitmentTxN &&& 0xffffffUL)))
            tx.Inputs.AddRange(txins)
            tx.Outputs.AddRange( txOuts |> Seq.map fst )
            tx
        resultTx, nonDustHTLCCount, htlcsInclued
        *)

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
        let valueToRemote = Money.Satoshis((c.ChannelValue.Satoshi * 1000L - c.ValueToSelf.MilliSatoshi) / 1000L) -
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


    let signCommitmentTransaction (tx: Transaction, theirSig: ECDSASignature, n: Network, forceLowR: bool option) (c: Channel) =
        let forceLowR = defaultArg forceLowR true
        if tx.Inputs.Count <> 1 then
            RResult.rmsg("Tried to sign commitment transaction that had input count != 1")
        else if not (isNull tx.Inputs.[0].WitScript) || tx.Inputs.[0].WitScript <> WitScript.Empty then
            RResult.rmsg("Tried to re-sign commitment tx")
        else
            let fundingRedeemScript = getFundingRedeemScript c
            let coin = ScriptCoin(tx.Inputs.[0].PrevOut,
                                  TxOut(c.ChannelValue, fundingRedeemScript.WitHash.ScriptPubKey),
                                  fundingRedeemScript)
            let b = n.CreateTransactionBuilder()
            b.UseLowR <- forceLowR
            Good(b.AddCoins(coin)
                .AddKeys(c.LocalKeys.FundingKey)
                .SignTransaction(tx, SigHash.All))

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
            Bad(RBadTree.Leaf(RBad.Message"Tried to sign HTLC transaction that had input count != 1"))
        else
            let htlcRedeemScript = ChannelUtils.getHTLCRedeemScript htlc keys
            let ourHTLCKey = ChannelUtils.derivePrivateKey keys.PerCommitmentPoint c.LocalKeys.HTLCBaseKey
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
                            (c: Channel): RResult<Transaction> =
        if tx.Inputs.Count <> 1 then
            RResult.rmsg("Tried to sign HTLC transaction that had input count != 1")
        else if not (isNull tx.Inputs.[0].WitScript) || tx.Inputs.[0].WitScript <> WitScript.Empty then
            RResult.rmsg("Tried to re-sign HTLC transaction")
        else 
            match createHTLCTxSignature tx htlc keys n c with
            | Bad b -> Bad b
            | Good (htlcRedeemScript, ourSig, localTx) ->
                let amount = Money.Satoshis(htlc.Amount.MilliSatoshi / 1000L)
                let p = n.CreateTransactionBuilder()
                         .AddCoins(ScriptCoin(tx.Inputs.[0].PrevOut,
                                              TxOut(amount, htlcRedeemScript.WitHash.ScriptPubKey),
                                              htlcRedeemScript))
                let scriptStr =
                    let sb = StringBuilder()
                    // multisig dummy
                    sb.Append("0") |> ignore
                    
                    if localTx then
                        let pks = htlcRedeemScript.GetAllPubKeys()
                        sb.AppendFormat(" {0} {1}", TransactionSignature(theirSig, SigHash.All), ourSig) |> ignore
                    else
                        let pks = htlcRedeemScript.GetAllPubKeys()
                        sb.AppendFormat(" {0} {1}", ourSig, TransactionSignature(theirSig, SigHash.All)) |> ignore

                    (if htlc.Offered then sb.Append(" 0") else sb.AppendFormat(" {0}", preImage.Value.Value)) |> ignore
                    sb.AppendFormat(" {0}", htlcRedeemScript) |> ignore
                    sb.ToString()

                tx.Inputs.[0].WitScript <- WitScript(scriptStr)
                Good(tx)

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
                            | None -> RREx(Ignore("Unable to find a pending HTLC which match the given HTLC ID"))


            if (c.ChannelState &&& (ChannelState.AwaitingRemoteRevoke ||| ChannelState.PeerDisconnected ||| ChannelState.MonitorUpdateFailed) <> ChannelState.Zero) then
                failwith ""
            else
                failwith ""
            failwith ""
