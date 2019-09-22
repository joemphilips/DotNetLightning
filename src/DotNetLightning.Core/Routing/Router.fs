namespace DotNetLightning.Core.Routing

open System
open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open NBitcoin

type NetworkEvent =
    | NodeDiscovered of msg: NodeAnnouncement
    | NodeUpdated of msg: NodeAnnouncement
    | NodeLost of nodeId: NodeId
    | ChannelDiscovered of msg: ChannelAnnouncement * capacity : Money
    | ChannelUpdateReceived of msg: ChannelUpdate


type RouterConf = {
    RandomizeRouterSelection: bool
    ChannelExcludeDuration: TimeSpan
    RouterBroadcastInterval: TimeSpan
    SearchMaxFeeBase: Money
    SearchMaxRouteLength: int
    SearchMaxCLTV: BlockHeightOffset
    SearchHeuristicsEnabled: bool
    SearchRatioCLTV: double
    SearchRatioChannelAge: double
    SearchRatioChannelCapacity: double
}

type Hop = {
    NodeId: NodeId
    NextNodeId: NodeId
    LastUpdate: ChannelUpdate
}

type RouteParams = {
    Randomize: bool
    MaxFeeBase: LNMoney
    MaxFeePCT: double
    RouteMaxLength: int
    RouteMaxCLTV: int
    Ratios: WeightRatios option
}
