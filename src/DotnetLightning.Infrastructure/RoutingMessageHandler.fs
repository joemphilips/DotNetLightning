namespace DotNetLightning.Infrastructure

open System.Threading.Tasks

open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open NBitcoin

type IRoutingMessageHandler =
    abstract member HandleMsg: theirNodeId: NodeId * msg:IRoutingMsg -> ValueTask
    //abstract member GetNextChannelAnnouncements: startingPoint: ShortChannelId * batchAmount: uint8 -> (ChannelAnnouncement * ChannelUpdate * ChannelUpdate) list
    //abstract member GetNextNodeAnnouncements: startingPoint: PubKey option * batchAMount: uint8 -> NodeAnnouncement list