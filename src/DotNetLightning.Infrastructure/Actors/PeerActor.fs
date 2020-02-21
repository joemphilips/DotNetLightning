namespace DotNetLightning.Infrastructure.Actors


open System.IO.Pipelines

open FSharp.Control.Tasks

open Microsoft.Extensions.Logging

open CustomEventAggregator

open DotNetLightning.Utils
open DotNetLightning.Chain
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Peer

open DotNetLightning.Infrastructure

type IPeerActor = Actor<Peer, PeerCommand, PeerEvent, PeerError>

type PeerActor(peer: Peer,
               peerId: PeerId,
               keyRepo: IKeysRepository,
               log: ILogger,
               pipeWriter: PipeWriter,
               eventAggregator: IEventAggregator) as this =
    inherit Actor<Peer, PeerCommand, PeerEvent, PeerError>(PeerDomain.CreatePeerAggregate peer, log)
    
    let _ourNodeId = keyRepo.GetNodeSecret().PubKey |> NodeId

    override this.HandleError (b: PeerError) =
        unitTask {
            log.LogError(sprintf "Critical error during processing against peer: %A" peerId)
            log.LogError(sprintf "%A" b)
            (*
            let handleObj (o: obj) = 
                unitVtask {
                    match o with
                    | :? HandleError as he ->
                        sprintf "Got Error when handling message"
                        |> log.LogTrace
                        match he.Action with
                        | Some(DisconnectPeer _) ->
                            sprintf "disconnecting peer because %A" he.Error
                            |> log.LogInformation
                        | Some(IgnoreError) ->
                            sprintf "ignoring the error because %A" he.Error
                            |> log.LogDebug
                        | Some(SendErrorMessage msg) ->
                            sprintf "sending error message because %A" he.Error
                            |> log.LogDebug
                            eventAggregator.Publish(PeerCommand.EncodeMsg msg)
                            return ()
                        | None ->
                            sprintf "Got error when handling message, action not yet filled in %A" he.Error
                            |> log.LogDebug
                    | _ ->
                        log.LogCritical(sprintf "Unknown Error object %A" o)
                }
            let! _ =
                unitTask {
                    match b with
                    | RBad.Exception ex -> log.LogError(ex.StackTrace)
                    | RBad.Message msg -> log.LogError(msg) 
                    | RBad.DescribedObject (msg, obj) ->
                        log.LogError(msg)
                        do! handleObj obj
                    | RBad.Object obj ->
                        do! handleObj obj
                }
            *)
            return ()
        }
        
    override this.PublishEvent e = unitTask {
        let eCon = { PeerEvent = e; PeerId = peerId; NodeId = this.State.TheirNodeId }
        eventAggregator.Publish<PeerEventWithContext>(eCon)
        match e with
        | ActOneProcessed (actTwo, _) ->
            do! pipeWriter.WriteAsync(actTwo)
        | ActTwoProcessed ((actThree, _theirNodeId), _) ->
            do! pipeWriter.WriteAsync(actThree)
            ()
        | ActThreeProcessed(_, _) ->
            ()
        | MsgEncoded (msg, _) ->
            do! pipeWriter.WriteAsync(msg)
        | _ -> ()
    }

        
