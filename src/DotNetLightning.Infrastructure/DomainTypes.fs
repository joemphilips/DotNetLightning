namespace DotNetLightning.Infrastructure

open System

open System.Threading.Tasks
open DotNetLightning.LN
open DotNetLightning.Utils.Primitives

type ChannelEventWithContext = {
    NodeId: NodeId
    ChannelEvent: ChannelEvent
}

type ChannelCommandWithContext = {
    NodeId: NodeId
    ChannelCommand: ChannelCommand
}

type PeerEventWithContext = {
    NodeId: NodeId
    PeerEvent: PeerEvent
}

type PeerCommandWithContext = {
    PeerId: PeerId
    PeerCommand: PeerCommand
}


type IActor<'TCommand> =
    inherit IDisposable
    abstract member StartAsync: System.Threading.Channels.Channel<'TCommand> -> Task
