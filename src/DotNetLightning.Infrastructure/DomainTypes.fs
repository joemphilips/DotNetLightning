namespace DotNetLightning.Infrastructure

open System

open System.Threading.Channels
open System.Threading.Tasks
open DotNetLightning.Utils
open DotNetLightning.LN
open FSharp.Control.Tasks


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

type IActor =
    inherit IDisposable
    abstract member StartAsync: unit -> Task
    
/// Simple actor model agent utilizing System.Threading.Channels.
/// All inputs to this should be given through CommunicationChannel (To ensure only one change will take place at the time.)
/// And all outputs to other services will go through PublishEvent (typically using EventAggregator)
[<AbstractClass>]
type Actor<'TState, 'TCommand, 'TEvent>(aggregate: Aggregate<'TState, 'TCommand, 'TEvent>, ?capacity: int) =
    let mutable disposed = false
    let capacity = defaultArg capacity 600
    member val State = aggregate.InitialState with get, set
    member val CommunicationChannel =
        let options = BoundedChannelOptions(capacity)
        options.SingleReader <- true
        options.SingleWriter <- false
        System.Threading.Channels.Channel.CreateBounded<'TCommand>(options) with get, set
    abstract member PublishEvent: e: 'TEvent -> Task
    abstract member HandleError: RBad -> Task
    interface IActor with
        member this.StartAsync() = unitTask {
            let mutable nonFinished = true
            while nonFinished && (not disposed) do
                let! cont = this.CommunicationChannel.Reader.WaitToReadAsync()
                nonFinished <- cont
                if nonFinished && (not disposed) then
                    match (this.CommunicationChannel.Reader.TryRead()) with
                    | true, cmd ->
                        match aggregate.ExecuteCommand this.State cmd with
                        | Good events ->
                            this.State <- events |> List.fold aggregate.ApplyEvent this.State
                            for e in events do
                                do! this.PublishEvent e
                        | Bad ex ->
                            let ex = ex.Flatten()
                            ex |> Array.map (this.HandleError) |> ignore
                    | false, _ ->
                        ()
            return ()
        }
        
        member this.Dispose() =
            disposed <- true
            ()
