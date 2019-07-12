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

[<AutoOpen>]
module ChannelConstants =
    [<Literal>]
    let INITIAL_COMMITMENT_NUMBER = 281474976710655UL // (1 << 48 - 1)

    [<Literal>]
    let OUR_MAX_HTLCs = 50us

    /// Specified in BOLT #2
    let MAX_FUNDING_SATOSHIS = Money.Satoshis(16777216m) // (1 << 24)

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

    // ------------ weights ----------

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


    [<Literal>]
    let ACCEPTED_HTLC_SCRIPT_WEIGHT = 139uy
    [<Literal>]
    let OFFERED_HTLC_SCRIPT_WEIGHT = 133uy

    [<Literal>]
    let HTLC_SUCCESS_TX_WEIGHT = 703UL

    [<Literal>]
    let HTLC_TIMEOUT_TX_WEIGHT = 663UL