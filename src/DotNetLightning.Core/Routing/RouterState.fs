namespace DotNetLightning.Routing

open System.Collections.Generic
open DotNetLightning.Payment
open DotNetLightning.Serialization.Msgs
open DotNetLightning.Utils
open Graph

open ResultUtils
open ResultUtils.Portability

type RouteParams = {
    Randomize: bool
    MaxFeeBase: LNMoney
    MaxFeePCT: double
    RouteMaxLength: int
    RouteMaxCLTV: BlockHeightOffset16
    Ratios: WeightRatios option
}
    with
    static member GetDefault(conf: RouterConf) =
        {
            Randomize = conf.RandomizeRouterSelection
            MaxFeeBase = conf.SearchMaxFeeBase
            MaxFeePCT = conf.SearchMaxFeePct
            RouteMaxLength = conf.SearchMaxRouteLength
            RouteMaxCLTV = conf.SearchMaxCLTV
            Ratios =
                match conf.SearchHeuristicsEnabled with
                | false -> None
                | true ->
                    match WeightRatios.TryCreate(conf.SearchRatioCLTV, conf.SearchRatioChannelAge, conf.SearchRatioChannelCapacity) with
                    | Ok s -> Some s
                    | Error _ -> None
        }

type RouteRequest = private {
    Source: NodeId
    Target: NodeId
    Amount: LNMoney
    AssistedRoutes: ExtraHop seq seq
    IgnoredNodes: Set<NodeId>
    IgnoredChannels: Set<ChannelDesc>
    RouteParams: RouteParams
}
    with
    static member Create(source, target, amount, routeParams, ?assistedRoutes, ?ignoredNodes, ?ignoredChannels) =
        let a = Option.defaultValue [] assistedRoutes
        let iN = Option.defaultValue Set.empty ignoredNodes
        let iC = Option.defaultValue Set.empty ignoredChannels
        {
            Source = source
            Target = target
            Amount = amount
            AssistedRoutes = a
            IgnoredNodes = iN
            IgnoredChannels = iC
            RouteParams = routeParams
        }
        
type FinalizeRoute = private {
    Hops: NodeId seq
    AssistedRoutes: ExtraHop seq seq
}
    with
    static member Create(hops, ?assistedRoutes) =
        {
            Hops = hops
            AssistedRoutes = Option.defaultValue [] assistedRoutes
        }

type RouteResponse = private {
    Hops: ChannelHop seq
    IgnoredNodes: Set<NodeId>
    IgnoredChannels: Set<ChannelDesc>
}
    with
    static member TryCreate (hops: ChannelHop seq, ignoredNodes, ignoredChannels, ?allowEmpty) =
        let allowEmpty = Option.defaultValue false allowEmpty
        if allowEmpty || (hops |> Seq.isEmpty |> not) then Error("Route cannot be empty") else
        {
            Hops = hops
            IgnoredNodes = ignoredNodes
            IgnoredChannels = ignoredChannels
        } |> Ok

type RoutingState = {
    Channels: PublicChannel seq
    Nodes: NodeAnnouncementMsg seq
}

type GossipOrigin =
    | Remote of PeerId
    | Local
type Stash = {
    Updates: Map<ChannelUpdateMsg, Set<GossipOrigin>>
    Nodes: Map<NodeAnnouncementMsg, Set<GossipOrigin>>
}
type ReBroadcast = {
    Channels: Map<ChannelAnnouncementMsg, Set<GossipOrigin>>
    Updates: Map<ChannelUpdateMsg, Set<GossipOrigin>>
    Nodes: Map<NodeAnnouncementMsg, Set<GossipOrigin>>
}

type Sync = {
    Pending: IRoutingMsg seq
    Total: int
}

type RouterData = private {
    Nodes: Map<NodeId, NodeAnnouncementMsg>
    Channels: SortedDictionary<ShortChannelId, PublicChannel>
    Stats: NetworkStats
    ReBroadcast: ReBroadcast
    Awaiting: Map<ChannelAnnouncementMsg, seq<PeerId>>
    PrivateChannels: Map<ShortChannelId, PrivateChannel>
    ExcludedChannels: Set<ChannelDesc>
    Graph: DirectedLNGraph
    Sync: Map<NodeId, Sync>
    CurrentBlockHeight: BlockHeight
}
    with
    member this.NetworkStats = this.Stats
    member this.RoutingState =
        { RoutingState.Channels = this.Channels.Values
          Nodes = this.Nodes |> Seq.map(fun kvp -> kvp.Value) }
    
type RouterState = Normal of RouterData
