namespace DotNetLightning.Infrastructure

open DotNetLightning.Utils
open DotNetLightning.Peer
open DotNetLightning.Channel

type ChannelEventWithContext = {
    NodeId: NodeId
    ChannelEvent: ChannelEvent
}
type ChannelCommandWithContext = {
    NodeId: NodeId
    ChannelCommand: ChannelCommand
}

type PeerEventWithContext = {
    PeerId: PeerId
    /// Only when we are handling new inbound connection, this will be none.
    /// because there is no way for us to know their node id before handshake completes
    NodeId: NodeId option
    PeerEvent: PeerEvent
}

type PeerCommandWithContext = {
    PeerId: PeerId
    PeerCommand: PeerCommand
}

type Aggregate<'TState, 'TCommand, 'TEvent> = {
    InitialState: 'TState
    ExecuteCommand: 'TState -> 'TCommand -> RResult<'TEvent list>
    ApplyEvent: 'TState -> 'TEvent -> 'TState
}

[<AutoOpen>]
module ChannelDomain =
    let CreateChannelAggregate(c: Channel): Aggregate<Channel, ChannelCommand, ChannelEvent> = {
        InitialState = c
        ExecuteCommand = Channel.executeCommand
        ApplyEvent = Channel.applyEvent
    }
  
module PeerDomain =
    let CreatePeerAggregate(p: Peer): Aggregate<Peer, PeerCommand, PeerEvent> = {
        InitialState = p
        ExecuteCommand = Peer.executeCommand
        ApplyEvent = Peer.applyEvent
    }

