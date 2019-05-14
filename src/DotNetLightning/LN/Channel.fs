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
    | OutinitSent =        1u
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
    ChanelSate: ChannelState
    ChannelOutbound: bool
    ChannelValueSatoshis: uint64
    LocalKeys: ChannelKeys
    ShotdownPubKey: PubKey
    CurrentLocalCommitmentTxNumber: uint64
    CurrentRemoteCommitmentTxNumber: uint64
    ValueToSelf: LightMoney
    PendingInboundHTLCs: InboundHTLCOutput list
    PendingOutboundHTLCs: OutboundHTLCOutput list
    HoldingCellHTLCUpdates: HTLCUpdateAwaitingACK list
    ResendOrder: RAACommitmentOrder
    MonitorPendingRevokdAndAck: bool
    MonitorPendingCommitmentSigned: bool
    MonitorPendingForwards: (PendingForwardHTLCInfo * uint64) list
    MonitorPendingFailures: (HTLCSource * PaymentHash * HTLCFailReason) list
    PendingUpdateFee: LightMoney option
    HoldingCellUpdateFee: LightMoney option
    NextLocalHTLCId: HTLCId
    NextRemoteHTLCId: HTLCId
    ChannelUpdateCount: uint32
    FeeRatePerKw: FeeRate
    MaxCommitmentTxOutputLocal: (uint64 * uint64)
    MaxCommitmentTxOutputRemote: (uint64 * uint64)
    LastLocalCommitmentTxn: Transaction list
    LastSentClosingFee: (uint64 * uint64) option
    FundingTxConfirmedIn: TxId
    TheirDustLimit: LightMoney
    OurDustLimit: LightMoney
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
    ThierHTLCBasePoint: PubKey option
    TheirCurrentCommitmentPoint: PubKey option
    TheirPreviousCommitmentBasePoint: PubKey option
    TheirNodeId: PubKey
    TheirShutdownScriptPubKey: PubKey
    ChannelMonitor: ChannelMonitor
    Logger: ILogger
}

module Channel =
    /// TODO:
    let getOurMaxHTLCValueInFlight (channelValue: Money) =
        channelValue * 1000L / 10L

    /// TODO:
    let getOurChannelReserve (channelValue: Money) =
        let q = channelValue / 100L
        Money.Min(channelValue, Money.Max(q, Money.Satoshis(1L)))

    /// TODO:
    let deriveOurDustLimitSatoshis(atOpenBackGroundFeeRate: FeeRate) =
        failwith "Not implemnted"

    /// TODO:
    let deriveOurHTLCMinimum(atOpenBackGroundFeeRate: FeeRate) =
        1000UL

    let private getChannel(fee: FeeRate): RResult<Channel> =
        failwith ""

    // ----- constructors -------
    let public newOutBound(
        feeEstimator: IFeeEstimator,
        keyProvider: IKeysRepository,
        theirNodeId: PubKey,
        channelValue: Money,
        pushMSat: LightMoney,
        userId: UserId,
        logger: ILogger,
        config: UserConfig
        ): RResult<Channel>  =
        let checkSmallerThenMaxPossible(channelValue) =
            if (channelValue >= MAX_FUNDING_SATOSHIS) then
                RResult.Bad(RBadTree.Leaf(RBad.Message("Funding value. 2^24")))
            else
                Good()

        let checkPushValueLessThanChannelValue (pushMSat: LightMoney) (channelValue: Money) () =
            if pushMSat.ToUnit(LightMoneyUnit.Satoshi) >= (channelValue.ToDecimal(MoneyUnit.Satoshi)) then
                RResult.Bad(RBadTree.Leaf(RBad.Message("push value > channel value")))
            else
                Good()

        let checkFeeRate (estimator: IFeeEstimator) (channelValue) () =
            let backgroundFeeRate = estimator.GetEstSatPer1000Weight(ConfirmationTarget.Background)
            if (getOurChannelReserve(channelValue) < deriveOurDustLimitSatoshis(backgroundFeeRate)) then
                RResult.Bad(RBadTree.Leaf(RBad.Message(sprintf "Not eonugh reserve above dust limit can be found at current fee rate(%O)" backgroundFeeRate)))
            else
                Good()

        let getFeeRate (feeEstimator: IFeeEstimator) () =
            Good(feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Normal))

        // let channelMonitor = {CommitmentTxNumberObscureFactor}

        checkSmallerThenMaxPossible(channelValue) >>=
            checkPushValueLessThanChannelValue pushMSat channelValue >>=
            checkFeeRate feeEstimator channelValue >>=
            getFeeRate feeEstimator >>=
            getChannel


type ChannelError =
    | Ignore of string
    | Close of string

