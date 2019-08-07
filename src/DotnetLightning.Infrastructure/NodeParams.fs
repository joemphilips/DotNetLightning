namespace DotNetLightning.Infrastructure

open DotNetLightning.Chain
open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open Microsoft.Extensions.Configuration
open NBitcoin
open System

type RouterConfig = {
    ChannelExcludeDuration: DateTimeOffset
    RouterBroadcastInterval: DateTimeOffset
    RandomizeRouteSelection: bool
    SearchMaxRouteLength: int
    SearchMaxCLTV: int
    SearchMaxFeeBaseSat: int
    SearchMaxFeePct: float
    SearchHeuristicsEnabled: bool
    SearchRatioCLTV: float
    SearchRatioChannelAge: float
    SearchRatioChannelCapacity: float
}

module private Helpers =
    let getChainHash chain =
        match chain with
        | "" ->
            failwith ""
        | _ -> failwith ""

/// Our own node's parameter
[<CLIMutable;NoComparison;NoEquality>] // make it CLIMutable for easy binding to the configuraiton object
type NodeParams = {
    mutable KeysRepo: IKeysRepository
    mutable Alias: string
    mutable Color: RGB
    mutable PublicAddresses: System.Net.IPAddress list
    mutable LocalFeatures: LocalFeatures
    mutable GlobalFeatures: GlobalFeatures
    mutable DustLimitSatoshis: Money
    mutable MaxHTLCValueInFlightMSat: LNMoney
    mutable MaxAcceptedHTLCs: uint16
    mutable ExpirtyDeltaBlocks: BlockHeight
    mutable HTLCMinimumMSat: LNMoney
    mutable ToRemoteDelayBlocks: BlockHeight
    mutable MaxToLocalDelayBlocks: BlockHeight
    mutable MinDepthBlocks: BlockHeight
    mutable SmartFeeNBlocks: BlockHeight
    mutable FeeBaseMSat: LNMoney
    mutable FeeProportionalMillionths: uint32
    mutable ReserveToFundingRatio: float
    mutable DB: SupportedDB
    mutable RevocationTimeout: DateTimeOffset
    mutable PingInterval: DateTimeOffset
    mutable PingTimeout: DateTimeOffset
    mutable PingDisconnect: DateTimeOffset
    mutable MaxFeeRateMismatch: float
    mutable UpdateFeeMinDiffRatio: float
    mutable AutoReconnect: bool
    mutable InitialRandomReconnectDelay: DateTimeOffset
    mutable MaxReconnectionInterval: DateTimeOffset
    mutable ChainChash: uint256
    mutable ChannelFlags: uint8
    mutable WatcherType: IChainListener
    mutable PaymentRequestExpiry: DateTimeOffset
    mutable MinFundingSatoshis: Money
    mutable RouterConf: RouterConfig
    mutable SocksProxy: Socks5Params option
    mutable MaxPaymentAttempts: int
}
    with
    static member FromConfigurationRoot(c: IConfigurationRoot) =
        let chain = c.["chain"]
        let chainHash = Helpers.getChainHash chain
        failwith ""
    member this.GetChannelConfig() =
        {
            ChannelConfig.ChannelHandshakeConfig = failwith ""
            PeerChannelConfigLimits = failwith "Not Implemented"
            ChannelOptions = failwith "Not Implemented"
        }