namespace DotNetLightning.Infrastructure.Actors

open FSharp.Control.Tasks
open CustomEventAggregator
open ResultUtils

open DotNetLightning.Chain
open DotNetLightning.Channel

open DotNetLightning.Infrastructure
open DotNetLightning.Infrastructure.Interfaces

open Microsoft.Extensions.Logging

/// Actor for specific channel.
type ChannelActor(nodeParams: ChainConfig,
                  log: ILogger,
                  eventAggregator: IEventAggregator,
                  channel: Channel,
                  channelEventRepo: IChannelEventStream,
                  keysRepository: IKeysRepository) as this =
    
    inherit Actor<Channel, ChannelCommand, ChannelEvent, ChannelError>(CreateChannelAggregate(channel), log)
    
    member val ChannelState = channel
    
    override this.HandleError (b: ChannelError) = unitTask {
            match b.RecommendedAction with
            | ChannelConsumerAction.Close ->
                let closeCMD =
                    nodeParams.ShutdownScriptPubKey
                    |> Option.defaultValue (keysRepository.GetShutdownPubKey().WitHash.ScriptPubKey)
                    |> CMDClose.Create
                    // should never return Error in here. That means nodeParams has never done validation
                    // or, keysRepository is bogus
                    |> Result.deref
                    |> ChannelCommand.Close
                log.LogError(sprintf "Closing a channel for a node (%A) due to a following error. \n" (this.ChannelState.RemoteNodeId))
                log.LogError((b.ToString()))
                return! (this :> IActor<_>).Put(closeCMD)
            | Ignore ->
                log.LogWarning("Observed a following error in a channel. But ignoring")
                log.LogWarning(b.ToString())
            | ChannelConsumerAction.DistrustPeer ->
                log.LogWarning("Distrusting Peer for the following error")
                log.LogWarning(b.ToString())
            | ReportAndCrash ->
                log.LogCritical("Observed following critical error in a channel")
                log.LogCritical(b.ToString())
                log.LogCritical("This should never happen and it may implies a bug. Please report an issue with a stack trace")
                failwith (b.ToString())
        }
    
    override this.PublishEvent(e) = unitTask {
            let contextEvents =
                { ChannelEventWithContext.ChannelEvent = e; NodeId = this.ChannelState.RemoteNodeId }
            do! channelEventRepo.SetEventsAsync([contextEvents])
            eventAggregator.Publish<ChannelEventWithContext> contextEvents
        }

        
