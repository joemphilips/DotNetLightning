namespace DotNetLightning.Utils

open System
open NBitcoin

/// Optional Channel limits which are applied during channel creation.
/// These limits are only applied to our counterparty's limits, not our own
type ChannelHandshakeLimits =
    {
        MinFundingSatoshis: Money
        MaxHTLCMinimumMSat: LNMoney
        /// The remote node sets a limit on the maximum value of pending HTLCs to them at any given time
        /// to limit their funds exposure to HTLCs. This allows you to set a minimum such value.
        MinMaxHTLCValueInFlightMSat: LNMoney
        /// The remote node will require we keep a certain amount in direct payment to ourselves at all
        /// time, ensuring that we are able to be punished if we broadcast an old state. This allows to
        /// you limit the amount which we will have to keep to ourselves (and cannot use for HTLCs).
        MaxChannelReserveSatoshis: Money
        /// The remote node sets a limit on the maximum number of pending HTLCs to them at any given
        /// time. This allows you to set a minimum such value.
        MinMaxAcceptedHTLCs: uint16
        /// HTLCs below this amount plus HTLC transaction fees are not enforceable on-chain.
        /// This settings allows you to set a minimum dust limit for their commitment TXs,
        /// Defaults to 546 , or the current dust limit on the Bitcoin network.
        MinDustLimitSatoshis: Money

        /// Maximum allowed threshold above which outputs will not be generated in their commitment
        /// Transactions.
        /// HTLCs below this amount plus HTLC tx fees are not enforceable on-chain.
        MaxDustLimitSatoshis: Money
        /// before a channel is usable the funding TX will need to be confirmed by at least a certain number
        /// of blocks, specified by the node which is not the funder (as the funder can assume they aren't
        /// going to double-spend themselves).
        /// This config allows you to set a limit on the maximum amount of time to wait. Defaults to 144
        /// blocks or roughly one day and only applies to outbound channels.
        MaxMinimumDepth: BlockHeightOffset32
        /// Set to force the incoming channel to match our announced channel preference in ChannelConfig.
        /// Defaults to true to make the default that no announced channels are possible (which is
        /// appropriate for any nodes which are not online very reliably)
        ForceChannelAnnouncementPreference: bool
        MaxToSelfDelay: BlockHeightOffset16
    }

    /// Returns default value
    static member Zero =
        {
            MinFundingSatoshis = Money.Satoshis(1000m)
            MaxHTLCMinimumMSat = LNMoney.Coins(21_000_000m)
            MinMaxHTLCValueInFlightMSat = LNMoney.Zero
            MaxChannelReserveSatoshis = Money.Zero
            MinMaxAcceptedHTLCs = 0us
            MinDustLimitSatoshis = Money.Satoshis(546m)
            MaxDustLimitSatoshis = Money.Coins(21_000_000m)
            MaxMinimumDepth = 144u |> BlockHeightOffset32
            ForceChannelAnnouncementPreference = true
            MaxToSelfDelay = BlockHeightOffset16(6us * 24us * 14us) // two weeks
        }


/// Configuration for router module.
type RouterConfig() =
    member val RandomizeRouteSelection: bool = true with get, set

    member val ChannelExcludeDuration: DateTimeOffset =
        Unchecked.defaultof<DateTimeOffset> with get, set

    member val RouterBroadcastInterval: DateTimeOffset =
        Unchecked.defaultof<DateTimeOffset> with get, set

    member val NetworkStatsRefreshInterval = Unchecked.defaultof<TimeSpan>
    member val RequestNodeAnnouncement = false

    member val ChannelRangeChunkSize = 0
    member val SearchMaxRouteLength: int = 6 with get, set
    // max acceptable cltv expiry fo the payment (1008 ~ 1 week)
    member val SearchMaxCLTV: int = 1008 with get, set
    // if fee is below this value we skip the MaxFeePct check
    member val SearchMaxFeeBaseSat: int = 21 with get, set
    // route will be discarded if fee is above this value
    member val SearchMaxFeePct: float = 0.03 with get, set
    // channel 'weight' is computed with the following formula
    // channelFee * (cltvDelta * ratio-cltv + channelAge + channelCapacity * ratio-channel-capacity)
    // following parameters can be used to ask the router to use heuristics to find i.e: 'cltv-optimized' routes,
    // **the sum of the three ratios must be > 0 and <= 1 **
    member val SearchHeuristicsEnabled: bool = true with get, set
    member val SearchRatioCLTV: float = 0.15 with get, set
    member val SearchRatioChannelAge: float = 0.35 with get, set
    member val SearchRatioChannelCapacity: float = 0.5 with get, set
