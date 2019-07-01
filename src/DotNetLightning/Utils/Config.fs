namespace DotNetLightning.Utils
open NBitcoin
open Aether

type ChannelHandshakeConfig = {
    /// Confirmations we will wait for before considering the channel locked in.
    /// Applied only for inbound channels (see `ChannelHandshakeLimits.MaxMinimumDepth` for the
    /// equivalent limit applied to outbound channel) 
    MinimumDepth: uint32
}
    with
        static member Zero =
            {
                MinimumDepth = 6u
            }

/// Optional Channel limits which are applied during channel creation.
/// These limits are only applied to our counterpaty's limits, not our own
type ChannelHandshakeLimits = {
    MinFundingSatoshis: Money
    MaxHTLCMinimumMSat: LNMoney
    /// The remote node sets a limit on the maximum value of pending HTLCs to them at any given time
    /// to limit their funds exposure to HTLCs. This allows you to set a minimum such value.
    MinMaxHTLCValueInFlightMSat: LNMoney
    /// The remote node will require we keep a certain amount in direct payment to ourselves at all
    /// time, ensuring that we are able to be pusnished if we broadcast an old state. This allows to
    /// you limit the amount which we will have to keep to ourselves (and cannot use for HTLCs).
    MaxChannelReserveSatoshis: Money
    /// The remote node sets a limit on the maximum number of pending HTLCs to them at any given
    /// time. This allows you to set a minimum such value.
    MinMaxAcceptedHTLCs: uint16
    /// HTLCs below tthis amount plus HTLC transaction fees are not enforceable on-chain.
    /// This settings allows you to set a minimum dust limit for their commitment TXs,
    /// Defaults to 546 , or the current dust limit on the Bitcoin network.
    MinDustLimitSatoshis: Money

    /// Maximum allowed threshold above which outputs will not be generated in their commitment
    /// Transactions.
    /// HTLCs below this amount plus HTLC tx fees are not enforceable on-chain.
    MaxDustLimitSatoshis: Money
    /// before a channel is usable the funding TX will need to be confirmed by at least a cetain number
    /// of blocks, specified by the node which is not the funder (as the funder can assume they aren't
    /// going to double-spend themselves).
    /// This config allwos you to set a limit on the maximum amount of time to wait. Defaults to 144
    /// blocks or roughly one day and only applies to outbound channels.
    MaxMinimumDepth: uint32
    /// Set to force the incoming channel to match our announced channel preference in ChannelConfig.
    /// Defaults to true to make the default that no announced channels are possible (which is
    /// appropriate for any nodes which are not online very reliably)
    ForceAnnouncedChannelPreference: bool
}
    with
        static member Zero =
            {
                MinFundingSatoshis = Money.Zero
                MaxHTLCMinimumMSat = LNMoney.Coins(21_000_000m)
                MinMaxHTLCValueInFlightMSat = LNMoney.Zero
                MaxChannelReserveSatoshis = Money.Zero
                MinMaxAcceptedHTLCs = 0us
                MinDustLimitSatoshis = Money.Satoshis(546m)
                MaxDustLimitSatoshis = Money.Coins(21_000_000m)
                MaxMinimumDepth = 144u
                ForceAnnouncedChannelPreference = true
            }


type UserConfig = {
    OwnChannelConfig: ChannelHandshakeConfig
    PeerChannelConfigLimits: ChannelHandshakeLimits
    ChannelOptions: ChannelConfig
}
    with
        static member Zero =
            {
                OwnChannelConfig = ChannelHandshakeConfig.Zero
                PeerChannelConfigLimits = ChannelHandshakeLimits.Zero
                ChannelOptions = ChannelConfig.Zero
            }

        static member OwnChannelConfig_: Lens<_, _> =
            (fun uc -> uc.OwnChannelConfig),
            (fun v uc -> { uc with OwnChannelConfig = v })

        static member PeerCahnnelConfigLimits_ : Lens<_,_> =
            (fun uc -> uc.PeerChannelConfigLimits),
            (fun v uc -> { uc with PeerChannelConfigLimits = v })

        static member ChannelOptions_: Lens<_,_> =
            (fun uc -> uc.ChannelOptions),
            (fun v uc -> { uc with ChannelOptions = v })

and ChannelConfig = {
    // Amount (in millionth of a satoshi) the channel will change per transfered satoshi.
    // This may be allowed to change at runtime in a later update, however doing so must result in
    // update mesages sent to notify all nodes of our updated relay fee.
    FeeProportionalMillionths: uint32
    // Set to announce the channel publicly and notify all nodes that they can route via this channel.
    // This should only be set to true for nodes which expect to be online reliably.
    // As the node which funds a cahnnel picks this value this will only pply for new outbound channels unless
    // `ChannleHandshaekLimits.ForceAnnnoucedChannelPreferences` is set.
    AnnouncedChannel: bool
}
    with
        static member Zero =
            {
                FeeProportionalMillionths = 0u
                AnnouncedChannel = false
            }

        static member FeeProportionalMillionths_: Lens<_, _> =
            (fun cc -> cc.FeeProportionalMillionths),
            (fun v cc -> { cc with FeeProportionalMillionths = v })

        static member AnnouncedChannel_ : Lens<_, _> =
            (fun cc -> cc.AnnouncedChannel),
            (fun v cc -> { cc with AnnouncedChannel = v })