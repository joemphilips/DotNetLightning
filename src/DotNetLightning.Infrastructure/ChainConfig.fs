namespace DotNetLightning.Infrastructure

open System

open ResultUtils

open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Channel

open DotNetLightning.Serialize
open System.Collections.Generic
open DotNetLightning.Transactions
open DotNetLightning.Transactions
open NBitcoin

[<AutoOpen>]
module SupportedTypes =
    type SupportedDBType =
        | Null
        /// Only for testing
        | InMemory
        | MongoDB of connectionString: string

    type SupportedChainWatcherType =
        | BitcoindPolling
    
    type SupportedKeyRepositoryTypes =
        /// Only for testing
        | InMemory
        | FlatFile of path: string

/// configuration specific to particular chain
type ChainConfig() =
    member val Alias: string = "" with get, set
    // default color is the same with github's `F#`
    // refs: https://github.com/ozh/github-colors
    member val Color: RGB = { RGB.Red = 184uy; Green = 69uy; Blue = 252uy } with get, set
    member val DataDirPath: string = null with get, set
    
    member val Network = DotNetLightningNetwork() with get, set
    // ---- infrastructure types -----
    member val ChainWatcherType: SupportedChainWatcherType = BitcoindPolling with get, set
    member val DBType: SupportedDBType = SupportedDBType.Null with get, set
    member val KeyRepoType: SupportedKeyRepositoryTypes = SupportedKeyRepositoryTypes.FlatFile(Constants.homePath) with get, set

    member val PublicAddresses: System.Net.IPEndPoint list = [] with get, set

    // ---- channel parameters ---
    member val Features: FeatureBit = defaultFeatureBits with get, set
    
    /// `dust_limit_satoshis` we are going to send.
    member val DustLimitSatoshis: Money = Money.Satoshis(546L) with get, set
    
    /// We won't accept the channel open if other node's `dust_limit_satoshis` is lower than this.
    member val MinDustLimitSatoshis: Money = Money.Satoshis(400L) with get, set
    /// We won't accept the channel open if other node's `dust_limit_satoshis` is higher than this.
    member val MaxDustLimitSatoshis: Money = Money.Satoshis(16000L) with get, set
    
    /// value for the `max_htlc_value_in_flight_msat` which we are going to send with our `open_channel` message
    member val MaxHTLCValueInFlightMSat: LNMoney = LNMoney.MilliSatoshis(5000000000L) with get, set
    
    /// If other nodes `max_htlc_value_in_flight_msat` is smaller than this. We wont open channel.
    member val MinMaxHTLCValueInFlightMSat: LNMoney = LNMoney.MilliSatoshis(1000L) with get, set
    
    /// `max_accepted_htlcs` field we are going to send by `open_channel` or `accept_channel`
    member val MaxAcceptedHTLCs: uint16 = 30us with get, set
    
    /// If `max_accepted_htlcs` field from other node's `open_channel` or `accept_channel` is
    /// smaller than this, we won't open channel.
    member val MinMaxAcceptedHTLCs: uint16 = 5us with get, set
    member val ExpiryDeltaBlocks: BlockHeight = BlockHeight 144u with get, set
    
    /// `htlc_minimum_msat` which we are going to send in `open_channel` message
    member val HTLCMinimumMSat: LNMoney = LNMoney.MilliSatoshis(1L) with get, set
    
    /// if `htlc_minimum_msat` sent by other nodes `open_channel` is larger than this.
    /// re will refuse to open channel.
    member val MaxHTLCMinimumMSat: LNMoney = LNMoney.MilliSatoshis(100L) with get, set
    member val ToRemoteDelayBlocks: BlockHeightOffset16 = BlockHeightOffset16(720us) with get, set
    member val MaxToLocalDelayBlocks: BlockHeightOffset16 = BlockHeightOffset16(2016us) with get, set
    
    /// The number of confirmation when we consider the funding tx is locked.
    member val MinimumDepth = BlockHeightOffset32(3u) with get, set
    /// We do not accept channel if other node wants to wait more than this (in block number) before funding tx is locked.
    member val MaxMinimumDepth  = BlockHeightOffset32(144u) with get, set
    
    // member val SmartFeeNBlocks: BlockHeight
    member val FeeBaseMSat: LNMoney = LNMoney.MilliSatoshis(1000L) with get, set
    member val FeeProportionalMillionths: uint32 = 100u with get, set
    // Recommended  in bolt 2
    member val ReserveToFundingRatio: float = 0.01 with get, set
    
    /// If other node's `channel_reserve_satoshis` in open_channel` or `accept_channel` is larger
    /// than this. We won't open channel
    member val MaxChannelReserveSatoshis = Money.Satoshis(1000000L) with get, set
    member val RevocationTimeout: DateTimeOffset = Unchecked.defaultof<DateTimeOffset> with get, set
    member val PingInterval: DateTimeOffset= Unchecked.defaultof<DateTimeOffset> with get, set
    member val PingTimeout: DateTimeOffset= Unchecked.defaultof<DateTimeOffset> with get, set
    member val PingDisconnect: DateTimeOffset= Unchecked.defaultof<DateTimeOffset> with get, set
    member val MaxFeeRateMismatch: float = 1.56 with get, set
    member val UpdateFeeMinDiffRatio: float = 0.1 with get, set
    member val AutoReconnect: bool = true with get, set
    member val InitialRandomReconnectDelay: DateTimeOffset= Unchecked.defaultof<DateTimeOffset> with get, set
    member val MaxReconnectionInterval: DateTimeOffset= Unchecked.defaultof<DateTimeOffset> with get, set
    member val ChannelFlags: uint8 = 1uy
    
    /// We don't exchange more than this many signatures when negotiating the closing fee
    member val MaxClosingNegotiationIterations = 20 with get, set
    
    /// If true, other node's channel announcement must match ours
    member val ForceChannelAnnouncementPreference = false with get, set
    member val PaymentRequestExpiry: DateTimeOffset= Unchecked.defaultof<DateTimeOffset> with get, set
    member val MinFundingSatoshis: Money = Money.Satoshis(100000L)
    member val SocksProxy: Socks5Params option = None
    member val MaxPaymentAttempts: int = 5
    
    
    /// We are going to send funds to this ScriptPubKey.
    /// If None is given, we will call `IKeysRepository.GetShutDownPubKey()` and
    /// send it to its P2WPKH
    member val ShutdownScriptPubKey: Script option = None
    member val RouterConfig = RouterConfig() with get
    with
    member this.GetChannelConfig() =
        {
            ChannelConfig.ChannelHandshakeConfig = { MinimumDepth = this.MinimumDepth }
            PeerChannelConfigLimits = { ChannelHandshakeLimits.MaxMinimumDepth = this.MaxMinimumDepth
                                        MinFundingSatoshis = this.MinFundingSatoshis
                                        MaxHTLCMinimumMSat = this.MaxHTLCMinimumMSat
                                        MinMaxHTLCValueInFlightMSat = this.MinMaxHTLCValueInFlightMSat
                                        MaxChannelReserveSatoshis = this.MaxChannelReserveSatoshis
                                        MinMaxAcceptedHTLCs = this.MinMaxAcceptedHTLCs
                                        MinDustLimitSatoshis = this.MinDustLimitSatoshis
                                        MaxDustLimitSatoshis = this.MaxDustLimitSatoshis
                                        ForceChannelAnnouncementPreference = this.ForceChannelAnnouncementPreference
                                        MaxClosingNegotiationIterations = this.MaxClosingNegotiationIterations }
            ChannelOptions = { ChannelOptions.AnnounceChannel = (this.ChannelFlags &&& 1uy) = 1uy
                               MaxFeeRateMismatchRatio = this.MaxFeeRateMismatch
                               FeeProportionalMillionths = this.FeeProportionalMillionths
                               ShutdownScriptPubKey = this.ShutdownScriptPubKey }
        }
        
    member this.MakeLocalParams(ourNodeId, channelPubKeys, defaultFinalScriptPubKey: Script, isFunder: bool, fundingSatoshis: Money) =
        {
            LocalParams.NodeId = ourNodeId
            ChannelPubKeys = channelPubKeys
            DustLimitSatoshis = this.DustLimitSatoshis
            MaxHTLCValueInFlightMSat = this.MaxHTLCValueInFlightMSat
            ChannelReserveSatoshis = (this.ReserveToFundingRatio * (float fundingSatoshis.Satoshi)) |> int64 |> Money.Satoshis
            HTLCMinimumMSat = this.HTLCMinimumMSat
            ToSelfDelay = this.ToRemoteDelayBlocks
            MaxAcceptedHTLCs = this.MaxAcceptedHTLCs
            IsFunder = isFunder
            DefaultFinalScriptPubKey = defaultFinalScriptPubKey
            Features = this.Features
        }

module private NodeParamsValidation =
    let checkDustLimitSatoshisIsToHigh(np: ChainConfig) =
        let limit = Money.Satoshis(100000L)
        if (np.DustLimitSatoshis > limit) then
            sprintf "Dust limit satoshis is absurdly high. The limit is (%A)" limit
            |> Error
        else
            Ok ()
 
    /// Currently no needs for check
    let checkMaxHTLCValueInFlightMSat _ =
        Ok ()

type ChainConfig with
    member this.Validate(): Result<unit, string list> =
        Validation.ofResult(NodeParamsValidation.checkDustLimitSatoshisIsToHigh this)
        *> NodeParamsValidation.checkMaxHTLCValueInFlightMSat this
        *^> (this.ShutdownScriptPubKey |> function None -> Ok() | Some spk -> Scripts.checkIsValidFinalScriptPubKey spk)
