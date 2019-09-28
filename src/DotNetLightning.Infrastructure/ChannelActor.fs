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
type IChannelActor = Actor<Channel, ChannelCommand, ChannelEvent>

type ChannelActor(nodeParams: IOptions<NodeParams>,
                  log: ILogger,
                  eventAggregator: IEventAggregator,
                  channel: Channel,
                  channelEventRepo: IChannelEventRepository,
                  keysRepository: IKeysRepository) as this =
    
    inherit Actor<Channel, ChannelCommand, ChannelEvent>( CreateChannelAggregate(channel))
    let _nodeParams = nodeParams.Value
    
    member val Channel = channel with get, set
    
    override this.HandleError (b: RBad) = unitTask {
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
    
    override this.PublishEvent(e) = unitTask {
            let contextEvents =
                { ChannelEventWithContext.ChannelEvent = e; NodeId = this.Channel.RemoteNodeId }
            do! channelEventRepo.SetEventsAsync([contextEvents])
            eventAggregator.Publish<ChannelEventWithContext> contextEvents
        }

        
