namespace DotNetLightning.Infrastructure

open Microsoft.Extensions.Options
open Microsoft.Extensions.Logging

open FSharp.Control.Tasks

open CustomEventAggregator
open DotNetLightning.Utils
open DotNetLightning.Chain
open DotNetLightning.LN

open DotNetLightning.Infrastructure


/// Actor for specific channel.
/// All inputs to this should be done through CommunicationChannel (To ensure only one change will take place at the time.)
/// And all outputs to other services will go through EventAggregator
type IChannelActor =
    inherit DotNetLightning.Infrastructure.IActor<ChannelCommand>
    abstract member CommunicationChannel: System.Threading.Channels.Channel<ChannelCommand>
    /// Start accepting message from the CommunicationChannel

type ChannelActor(nodeParams: IOptions<NodeParams>,
                  log: ILogger<ChannelActor>,
                  eventAggregator: IEventAggregator,
                  channel: Channel,
                  channelEventRepo: IChannelEventRepository,
                  keysRepository: IKeysRepository) as this =
    
    
    let _nodeParams = nodeParams.Value
    let mutable disposed = false
    member val Channel = channel with get, set
    
    member val CommunicationChannel: System.Threading.Channels.Channel<ChannelCommand> = null with get, set
        
    member private this.HandleChannelError (b: RBad) = unitTask {
            match b with
            | RBad.Exception(ChannelException(ChannelError.Close(msg))) ->
                let closeCMD =
                    let spk = _nodeParams.ShutdownScriptPubKey |> Option.defaultValue (keysRepository.GetShutdownPubKey().WitHash.ScriptPubKey)
                    ChannelCommand.Close({ CMDClose.ScriptPubKey = Some spk })
                log.LogError(sprintf "Closing a channel for a node (%A) due to a following error. \n %s" (this.Channel.RemoteNodeId) msg)
                log.LogError(sprintf "%s" msg)
                return! this.CommunicationChannel.Writer.WriteAsync(closeCMD)
            | RBad.Exception(ChannelException(ChannelError.Ignore(msg))) ->
                log.LogWarning("Observed a following error in a channel. But ignoring")
                log.LogWarning(msg)
            | RBad.Object(o) ->
                match o with
                | :? APIError as apiError ->
                    match apiError with
                    | APIMisuseError msg ->
                        log.LogWarning(sprintf "Channel returned api misuse error. ignoring. %s" msg)
                    | e ->
                        log.LogCritical(sprintf "Unreachable %A" e)
                | e -> sprintf "unreachable %A" e |> log.LogCritical
            | o -> sprintf "Observed a following error in a channel %A" o |> log.LogError
        }
    
    member this.StartAsync(jobQueue: System.Threading.Channels.Channel<ChannelCommand>) = unitTask {
        this.CommunicationChannel <- jobQueue
        let mutable nonFinished = true
        while nonFinished do
            let! cont = jobQueue.Reader.WaitToReadAsync()
            nonFinished <- cont
            if nonFinished && (not disposed) then
                match (jobQueue.Reader.TryRead()) with
                | true, cmd ->
                    match Channel.executeCommand this.Channel cmd with
                    | Good events ->
                        this.Channel <- events |> List.fold Channel.applyEvent this.Channel
                        let contextEvents =
                            events |> List.map(fun e -> { ChannelEventWithContext.ChannelEvent = e; NodeId = channel.RemoteNodeId })
                        do! channelEventRepo.SetEventsAsync(contextEvents)
                        contextEvents
                        |> List.iter
                            eventAggregator.Publish<ChannelEventWithContext>
                        ()
                    | Bad ex ->
                        let ex = ex.Flatten()
                        ex |> Array.map (this.HandleChannelError) |> ignore
                        ()
                | false, _ ->
                    ()
    }

    interface IChannelActor with
        member this.StartAsync jobQueue = this.StartAsync jobQueue
        member this.CommunicationChannel = this.CommunicationChannel
        member this.Dispose() =
            disposed <- true
            ()
        
