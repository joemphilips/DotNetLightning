namespace DotNetLightning.Channel
open DotNetLightning.Utils
open NBitcoin
type ChannelValueStat = internal {
    ValueToSelf: LNMoney;
    ChannelValue: LNMoney;
    ChannelReserve: LNMoney;
    PendingOutboundHTLCsAmount: LNMoney;
    PendingInBoundHTLCsAmount: LNMoney;
    HoldingCellOutBoundAmount: LNMoney;
}

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
    let BREAKDOWN_TIMEOUT = BlockHeightOffset16(6us * 24us * 7us) // one week
    let MAX_LOCAL_BREAKDOWN_TIMEOUT = BlockHeightOffset16(6us * 24us * 14us) // two weeks

    /// Specified in BOLT 11
    let MIN_CLTV_EXPIRY = 9us |> BlockHeightOffset16

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