namespace DotNetLightning.Routing

open ResultUtils
open System.Collections.Generic
open DotNetLightning.Payment
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Utils
open Graph

        
type RouteParams = {
    Randomize: bool
    MaxFeeBase: LNMoney
    MaxFeePCT: double
    RouteMaxLength: int
    RouteMaxCLTV: BlockHeightOffset
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
    Nodes: NodeAnnouncement seq
}

type GossipOrigin =
    | Remote of PeerId
    | Local
type Stash = {
    Updates: Map<ChannelUpdate, Set<GossipOrigin>>
    Nodes: Map<NodeAnnouncement, Set<GossipOrigin>>
}
type ReBroadcast = {
    Channels: Map<ChannelAnnouncement, Set<GossipOrigin>>
    Updates: Map<ChannelUpdate, Set<GossipOrigin>>
    Nodes: Map<NodeAnnouncement, Set<GossipOrigin>>
}

type Sync = {
    Pending: IRoutingMsg seq
    Total: int
}

type RouterData = private {
    Nodes: Map<NodeId, NodeAnnouncement>
    Channels: SortedDictionary<ShortChannelId, PublicChannel>
    Stats: NetworkStats
    ReBroadcast: ReBroadcast
    Awaiting: Map<ChannelAnnouncement, seq<PeerId>>
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
