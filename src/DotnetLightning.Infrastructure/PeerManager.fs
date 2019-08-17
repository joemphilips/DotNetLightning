namespace DotNetLightning.Infrastructure


open System
open System.IO.Pipelines
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading.Tasks

open FSharp.Control.Tasks

open Microsoft.Extensions.Logging

open NBitcoin

open DotNetLightning.Utils
open DotNetLightning.Chain
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Crypto
open DotNetLightning.LN


type PeerError =
    | DuplicateConnection of PeerId
    | UnexpectedByteLength of expected: int * actual: int

type IPeerManager =
    abstract member HandleData: PeerId * byte[] -> Task

type PeerManager(keyRepo: IKeysRepository, logger: ILogger<PeerManager>) =
    let _logger = logger
    member val KnownPeers = ConcurrentDictionary<PeerId, Peer>() with get, set
    member val OpenedPeers = ConcurrentDictionary<PeerId, Peer>() with get, set
    member val NodeIdToDescriptor = ConcurrentDictionary<NodeId, PeerId>() with get, set

    member this.NewOutBoundConnection (theirNodeId: NodeId, peerId: PeerId): Result<byte[], PeerError> =
        let act1, peerEncryptor =
            PeerChannelEncryptor.newOutBound(theirNodeId)
            |> PeerChannelEncryptor.getActOne
        let newPeer = {
                            ChannelEncryptor = peerEncryptor
                            IsOutBound = true
                            TheirNodeId = None
                            TheirGlobalFeatures = None
                            TheirLocalFeatures = None
                            SyncStatus = InitSyncTracker.NoSyncRequested
                            PeerId = peerId
                            GetOurNodeSecret = keyRepo.GetNodeSecret
                      }
        match this.KnownPeers.TryAdd(peerId, newPeer) with
        | true ->
            peerId
            |> DuplicateConnection
            |> Result.Error
        | false ->
            Ok(act1)

    member this.NewInboundConnection(peerId: PeerId, actOne: byte[]): RResult<byte[]> =
        if (actOne.Length <> 50) then RResult.rbad (RBad.Object(UnexpectedByteLength(50, actOne.Length))) else
        let secret = keyRepo.GetNodeSecret()
        let peerEncryptor = PeerChannelEncryptor.newInBound(secret)
        PeerChannelEncryptor.processActOneWithKey actOne secret peerEncryptor
        >>= fun (actTwo, pce) ->
            let newPeer = {
                ChannelEncryptor = pce
                IsOutBound = false
                TheirNodeId = None
                TheirGlobalFeatures = None
                TheirLocalFeatures = None

                SyncStatus = InitSyncTracker.NoSyncRequested
                PeerId = peerId
                GetOurNodeSecret = keyRepo.GetNodeSecret
            }
            match this.KnownPeers.TryAdd(peerId, newPeer) with
            | true ->
                peerId
                |> DuplicateConnection
                |> box
                |> RBad.Object
                |> RResult.rbad
            | false ->
                actTwo
                |> RResult.Good

    member private this.UpdatePeerWith(peerId, newPeer: Peer) =
        ignore <| this.KnownPeers.AddOrUpdate(peerId, newPeer, (fun pId exisintgPeer -> newPeer))

    member private this.EncodeAndSendMsg(peerId: PeerId, transport: IDuplexPipe) (msg: ILightningMsg) =
        unitVtask {
            match this.OpenedPeers.TryGetValue(peerId) with
            | true, peer ->
                sprintf "Encoding and sending message of type %A to %A "  msg (peer.TheirNodeId)
                |> _logger.LogTrace
                let msgEncrypted, newPCE =
                    peer.ChannelEncryptor |> PeerChannelEncryptor.encryptMessage (_logger.LogTrace) (msg.ToBytes())
                this.UpdatePeerWith(peerId, { peer with ChannelEncryptor = newPCE })
                let! _ = transport.Output.WriteAsync(ReadOnlyMemory(msgEncrypted))
                return ()
            | false, _ ->
                sprintf "peerId %A is not in opened peers" peerId
                |> _logger.LogCritical
        }

    member inline private this.TryPotentialHandleError (peerId, transport: IDuplexPipe) (b: RBad) =
        unitTask {
            let handleObj (o: obj) = 
                unitVtask {
                    match o with
                    | :? HandleError as he ->
                        sprintf "Got Error when handling message"
                        |> _logger.LogTrace
                        match he.Action with
                        | Some(DisconnectPeer _) ->
                            sprintf "disconnecting peer because %A" he.Error
                            |> _logger.LogTrace
                        | Some(IgnoreError) ->
                            sprintf "ignoring the error because %A" he.Error
                            |> _logger.LogTrace
                        | Some(SendErrorMessage msg) ->
                            sprintf "sending error message because %A" he.Error
                            |> _logger.LogTrace
                            let! _ = this.EncodeAndSendMsg(peerId, transport) msg
                            return ()
                        | None ->
                            sprintf "Got error when handling message, action not yet filled in %A" he.Error
                            |> _logger.LogDebug
                    | _ ->
                        _logger.LogCritical(sprintf "Unknown Error object %A" o)
                }
            let! _ =
                unitTask {
                    match b with
                    | RBad.Exception ex -> _logger.LogError(ex.StackTrace)
                    | RBad.Message msg -> _logger.LogError(msg) 
                    | RBad.DescribedObject (msg, obj) ->
                        _logger.LogError(msg)
                        do! handleObj obj
                    | RBad.Object obj ->
                        do! handleObj obj
                }
            return ()
        }

    member private this.InsertNodeId(nodeId: NodeId, peerId) =
        match this.NodeIdToDescriptor.TryAdd(nodeId, peerId) with
        | true ->
            _logger.LogDebug(sprintf "Got second connection with %A , closing." nodeId)
            RResult.rbad(RBad.Object { HandleError.Action = Some IgnoreError ; Error = sprintf "We already have connection with %A. nodeid: is %A" peerId nodeId })
        | false ->
            _logger.LogTrace(sprintf "Finished noise handshake for connection with %A" nodeId)
            Good ()

    member this.ReadAsync(peerId: PeerId, pipe: IDuplexPipe) =
        let errorHandler: RBad -> Task = this.TryPotentialHandleError(peerId, pipe)
        unitVtask {
            let! r = this.ReadAsyncCore(peerId, pipe)
            let r: RResult<_> =  r // compiler complains about type annotation without this
            do! r.RBadIterAsync(errorHandler)
            return ()
        }
        

    // I wanted to use a bind (>>=) as we do in Channel, but we had to prepare monad-transformer.
    // so instead decided to just use match expression.
    member private this.ReadAsyncCore(peerId: PeerId, pipe: IDuplexPipe) =
        vtask {
            match this.KnownPeers.TryGetValue(peerId) with
            | true, peer ->
                match peer.ChannelEncryptor.GetNoiseStep() with
                | NextNoiseStep.ActOne ->
                    let! actOne = pipe.Input.ReadExactAsync(50)
                    match peer.ChannelEncryptor |> PeerChannelEncryptor.processActOneWithKey actOne (keyRepo.GetNodeSecret()) with
                    | Bad rbad -> return Bad rbad
                    | Good (actTwo, nextPCE) ->
                        this.UpdatePeerWith(peerId, {peer with ChannelEncryptor = nextPCE})
                        do! pipe.Output.WriteAsync(actTwo)
                        return Good ()
                | ActTwo ->
                    let! actTwo = pipe.Input.ReadExactAsync(50)
                    match peer.ChannelEncryptor |> PeerChannelEncryptor.processActTwo(actTwo) (keyRepo.GetNodeSecret()) with
                    | Bad rbad -> return Bad rbad
                    | Good ((actThree, theirNodeId), newPCE) ->
                        do! pipe.Output.WriteAsync(actThree)
                        this.UpdatePeerWith(peerId, { peer with TheirNodeId = Some theirNodeId })
                        match this.InsertNodeId(theirNodeId, peerId) with
                        | Bad rbad -> return Bad rbad
                        | Good _ -> 
                            let localFeatures = LocalFeatures.Flags [||]
                            return Good ()
            | false, _ ->
                return RResult.rmsg (sprintf "unknown peer %A" peerId)
        }
    member this.DoAttemptWriteData (connId: ConnectionId) (peer: Peer) (pm: PeerManager) =
        failwith ""