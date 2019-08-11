namespace DotNetLightning.Infrastructure

open DotNetLightning.Chain
open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open Microsoft.Extensions.Configuration
open NBitcoin
open System

type RouterConfig() =
    member val ChannelExcludeDuration: DateTimeOffset = Unchecked.defaultof<DateTimeOffset> with get, set
    member val RouterBroadcastInterval: DateTimeOffset = Unchecked.defaultof<DateTimeOffset> with get, set
    member val RandomizeRouteSelection: bool = true with get, set
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

module private Helpers =
    let getChainHash chain =
        match chain with
        | "" ->
            failwith ""
        | _ -> failwith ""

/// Our own node's parameter
type NodeParams() as this =
    member val DataDirPath: string = Constants.homePath with get, set
    member val Alias: string = "" with get, set
    // default color will be the same with github's `F#`
    // refs: https://github.com/ozh/github-colors
    member val Color: RGB = { RGB.Red = 184uy; Green = 69uy; Blue = 252uy } with get, set

    // ---- infrastructure types -----
    member val WatcherType: SupportedChainWatcherType = Bitcoind(RPC.RPCClient(this.ChainNetwork))
    member val DBType: SupportedDBType = SupportedDBType.FlatFile(this.DataDirPath)
    member val KeyRepoType: SupportedKeyRepositoryTypes = SupportedKeyRepositoryTypes.FlatFile(this.DataDirPath)

    // ---- channel parameters ---
    member val PublicAddresses: System.Net.IPEndPoint list = [] with get, set
    member val LocalFeatures: LocalFeatures = LocalFeatures.Flags [||] with get, set
    member val GlobalFeatures: GlobalFeatures = GlobalFeatures.Flags[||] with get, set
    member val DustLimitSatoshis: Money = Money.Satoshis(546L) with get, set
    member val MaxHTLCValueInFlightMSat: LNMoney = LNMoney.MilliSatoshis(5000000000L) with get, set
    member val MaxAcceptedHTLCs: uint16 = 30us with get, set
    member val ExpirtyDeltaBlocks: BlockHeight = BlockHeight 144u with get, set
    member val HTLCMinimumMSat: LNMoney = LNMoney.MilliSatoshis(1L) with get, set
    member val ToRemoteDelayBlocks: BlockHeight = BlockHeight (720u) with get, set
    member val MaxToLocalDelayBlocks: BlockHeight = BlockHeight(2016u) with get, set
    member val MinDepthBlocks: BlockHeight = BlockHeight(3u) with get, set
    // member val SmartFeeNBlocks: BlockHeight
    member val FeeBaseMSat: LNMoney = LNMoney.MilliSatoshis(1000L) with get, set
    member val FeeProportionalMillionths: uint32 = 100u with get, set
    // Recommended  in bolt 2
    member val ReserveToFundingRatio: float = -0.01 with get, set
    member val RevocationTimeout: DateTimeOffset = Unchecked.defaultof<DateTimeOffset> with get, set
    member val PingInterval: DateTimeOffset= Unchecked.defaultof<DateTimeOffset> with get, set
    member val PingTimeout: DateTimeOffset= Unchecked.defaultof<DateTimeOffset> with get, set
    member val PingDisconnect: DateTimeOffset= Unchecked.defaultof<DateTimeOffset> with get, set
    member val MaxFeeRateMismatch: float = 1.56 with get, set
    member val UpdateFeeMinDiffRatio: float = 0.1 with get, set
    member val AutoReconnect: bool = true with get, set
    member val InitialRandomReconnectDelay: DateTimeOffset= Unchecked.defaultof<DateTimeOffset> with get, set
    member val MaxReconnectionInterval: DateTimeOffset= Unchecked.defaultof<DateTimeOffset> with get, set
    member val ChainNetwork: Network = Network.RegTest
    member val ChannelFlags: uint8 = 1uy
    member val PaymentRequestExpiry: DateTimeOffset= Unchecked.defaultof<DateTimeOffset> with get, set
    member val MinFundingSatoshis: Money = Money.Satoshis(100000L)
    member val RouterConf: RouterConfig = RouterConfig()
    member val SocksProxy: Socks5Params option = None
    member val MaxPaymentAttempts: int = 5
    with
    member this.GetChannelConfig() =
        {
            ChannelConfig.ChannelHandshakeConfig = failwith ""
            PeerChannelConfigLimits = failwith "Not Implemented"
            ChannelOptions = failwith "Not Implemented"
        }

module private Validation =
    let checkDustLimitSatoshisIsToHigh(np: NodeParams) =
        let limit = Money.Satoshis(100000L)
        if (np.DustLimitSatoshis > limit) then
            sprintf "Dust limit satoshis is absurdly high. The limit is (%A)" limit
            |> RResult.rmsg 
        else
            Good ()
 
    /// Currently no needs for check
    let checkMaxHTLCValueInFlightMSat np =
        Good ()

type NodeParams with
    member this.Validate(): RResult<unit> =
        Validation.checkDustLimitSatoshisIsToHigh this
        *> Validation.checkMaxHTLCValueInFlightMSat this