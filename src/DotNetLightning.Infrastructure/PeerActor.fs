namespace DotNetLightning.Infrastructure


open System.IO.Pipelines
open System.Collections.Concurrent
open System.Threading.Tasks

open FSharp.Control.Tasks

open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Chain
open DotNetLightning.Serialize.Msgs
open DotNetLightning.LN

open CustomEventAggregator
open DotNetLightning.Utils.Aether
open FSharp.Control.Reactive


type PeerError =
    | DuplicateConnection of PeerId
    | UnexpectedByteLength of expected: int * actual: int
    | EncryptorError of string

type IPeerActor = Actor<Peer, PeerCommand, PeerEvent>

type PeerActor(peer: Peer,
               peerId: PeerId,
               keyRepo: IKeysRepository,
               log: ILogger,
               pipeWriter: PipeWriter,
               nodeParams: IOptions<NodeParams>,
               eventAggregator: IEventAggregator) as this =
    inherit Actor<Peer, PeerCommand, PeerEvent>(PeerDomain.CreatePeerAggregate peer, log)
    let nodeParams = nodeParams.Value
    
    let ourNodeId = keyRepo.GetNodeSecret().PubKey |> NodeId

    override this.HandleError (b: RBad) =
        unitTask {
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

        
