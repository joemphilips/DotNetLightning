namespace DotNetLightning.Infrastructure

open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open NBitcoin

type IRoutingMessageHandler =
    abstract member HandleNodeAnnouncement: msg:NodeAnnouncement -> RResult<bool>
    abstract member HandleChannelAnnouncement: msg:ChannelAnnouncement -> RResult<bool>
    abstract member HandleChannelUpdate: msg:ChannelUpdate -> RResult<bool>
    abstract member HandleHTLCFailChannelUpdate: msg:HTLCFailChannelUpdate -> RResult<unit>
    abstract member GetNextChannelAnnouncements: startingPoint: ShortChannelId * batchAmount: uint8 -> (ChannelAnnouncement * ChannelUpdate * ChannelUpdate) list
    abstract member GetNextNodeAnnouncements: startingPoint: PubKey option * batchAMount: uint8 -> NodeAnnouncement list