namespace DotNetLightning.Routing

open DotNetLightning.Channel
open DotNetLightning.Payment
open DotNetLightning.Routing.Graph
open System
open System.Collections.Generic
open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open NBitcoin
open ResultUtils


type NetworkEvent =
    | NodeDiscovered of msg: NodeAnnouncement
    | NodeUpdated of msg: NodeAnnouncement
    | NodeLost of nodeId: NodeId
    | ChannelDiscovered of msg: ChannelAnnouncement * capacity : Money
    | ChannelUpdateReceived of msg: ChannelUpdate
    // when funding tx on the blockchain has been spent, we must consider that the channel is closed in a graph view
    | FundingTxSpent of ShortChannelId

type NetworkCommand =
    | SendChannelQuery of remoteNodeId: NodeId
    /// This is used when we get TemporaryChannelFailure, to give time for the
    /// channel to recover (note that exclusions are directed)
    | ExcludeChannel of desc: ChannelDesc
    | CalculateRoute of RouteRequest
    
    
type RouterError =
    | RouteFindingError of string
[<AutoOpen>]
module internal RouterError =
    let routeFindingError msg = RouteFindingError msg |> Error
type RouterCommand =
    | ChannelEvent of ChannelEvent
    | NetworkEvent of NetworkEvent
    | NetworkCommand of NetworkCommand
    
