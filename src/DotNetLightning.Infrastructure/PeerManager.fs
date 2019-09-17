namespace DotNetLightning.Infrastructure


open System.IO.Pipelines
open System.Collections.Concurrent
open System.Threading.Tasks
open System.Reactive.Linq


open FSharp.Control.Tasks

open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Chain
open DotNetLightning.Serialize.Msgs
open DotNetLightning.LN

open System
open System.Threading
open CustomEventAggregator
open DotNetLightning.Utils.Aether
open FSharp.Control.Reactive


type PeerError =
    | DuplicateConnection of PeerId
    | UnexpectedByteLength of expected: int * actual: int
    | EncryptorError of string

type IPeerManager =
    abstract member ProcessMessageAsync: PeerId * IDuplexPipe -> ValueTask
    abstract member AcceptCommand : PeerCommand -> ValueTask
    abstract member OurNodeId: NodeId

type PeerManager(keyRepo: IKeysRepository,
                 logger: ILogger<PeerManager>,
                 nodeParams: IOptions<NodeParams>,
                 eventAggregator: IEventAggregator,
                 chainWatcher: IChainWatcher,
                 broadCaster: IBroadCaster) as this =
    let _logger = logger
    let _nodeParams = nodeParams.Value
    let ascii = System.Text.ASCIIEncoding.ASCII
    
    let _chainWatcher = chainWatcher
    let _broadCaster = broadCaster
    
    let _ourNodeId = keyRepo.GetNodeSecret().PubKey |> NodeId
    
    let _keyRepo = keyRepo
    
    let _channelEventObservable =
        eventAggregator.GetObservable<ChannelEventWithContext>()
        |> Observable.flatmapAsync(this.ChannelEventListener)
        |> Observable.subscribeWithError
               (id)
               ((sprintf "PeerManager got error while Observing ChannelEvent: %A") >> _logger.LogCritical)
        
    member val KnownPeers = ConcurrentDictionary<PeerId, Peer>() with get, set
    
    member val OurNodeId = _ourNodeId with get
    
    member val OpenedPeers = ConcurrentDictionary<PeerId, Peer>() with get, set
    member val PeerIdToTransport = ConcurrentDictionary<PeerId, PipeWriter>() with get

    member val NodeIdToPeerId = ConcurrentDictionary<NodeId, PeerId>() with get, set
    
    member val EventAggregator: IEventAggregator = eventAggregator with get
    member private this.ChannelEventListener (contextEvent): Async<unit> =
        let vt = vtask {
            let nodeId = contextEvent.NodeId
            let peerId = this.NodeIdToPeerId.TryGet(nodeId)
            let transport = this.PeerIdToTransport.TryGet(peerId)
            match contextEvent.ChannelEvent with
            // ---- channel init: funder -----
            | ChannelEvent.NewOutboundChannelStarted(msg, _) ->
                return! this.EncodeAndSendMsg(peerId, transport) (msg)
            | ChannelEvent.WeAcceptedAcceptChannel(msg, _) ->
                return! this.EncodeAndSendMsg(peerId, transport) (msg)
            | ChannelEvent.WeAcceptedFundingSigned(finalizedFundingTx, _) ->
                let! r = _broadCaster.BroadCastTransaction(finalizedFundingTx.Value) |> Async.Catch |> Async.StartAsTask
                match r with
                | Choice1Of2 txId ->
                    let spk1 = finalizedFundingTx.Value.Outputs.[0].ScriptPubKey
                    let spk2 = finalizedFundingTx.Value.Outputs.[1].ScriptPubKey
                    match ([spk1; spk2]
                          |> List.map(fun spk -> _chainWatcher.InstallWatchTx(txId, spk))
                          |> List.forall(id)) with
                    | true ->
                        _logger.LogInformation(sprintf "Waiting for funding tx (%A) to get confirmed..." txId )
                        return ()
                    | false ->
                        _logger.LogCritical(sprintf "Failed to install watching tx (%A)" txId)
                        this.EventAggregator.Publish<PeerEvent>(FailedToBroadcastTransaction(nodeId, finalizedFundingTx.Value))
                        return ()
                | Choice2Of2 ex ->
                    ex.Message |> _logger.LogCritical
                    this.EventAggregator.Publish<PeerEvent>(FailedToBroadcastTransaction(nodeId, finalizedFundingTx.Value))
                    return ()
            // ---- channel init: fundee -----
            | ChannelEvent.NewInboundChannelStarted _ ->
                ()
            | ChannelEvent.WeAcceptedOpenChannel(msg, _) ->
                return! this.EncodeAndSendMsg(peerId, transport) msg
            | ChannelEvent.WeAcceptedFundingCreated(msg, _) ->
                return! this.EncodeAndSendMsg(peerId, transport) msg
                
            // ---- else ----
            | FundingConfirmed _  -> ()
            | TheySentFundingLockedMsgBeforeUs _msg -> ()
            | WeSentFundingLockedMsgBeforeThem fundingLockedMsg ->
                return! this.EncodeAndSendMsg(peerId, transport) fundingLockedMsg
            | BothFundingLocked _ -> ()
            | WeAcceptedUpdateAddHTLC _
            | WeAcceptedFulfillHTLC _
            | WeAcceptedFailHTLC _ -> ()
            | WeAcceptedFailMalformedHTLC _ -> ()
            | WeAcceptedUpdateFee _ -> ()
            | WeAcceptedCommitmentSigned  _ -> failwith "TODO: route"
            | WeAcceptedRevokeAndACK _ -> failwith "TODO: route"
            // The one which includes `CMD` in its names is the one started by us.
            // So there are no need to think about routing, just send it to specified peer.
            | ChannelEvent.WeAcceptedCMDAddHTLC (msg, _) ->
                return! this.EncodeAndSendMsg(peerId, transport) (msg)
            | ChannelEvent.WeAcceptedCMDFulfillHTLC (msg, _) ->
                return! this.EncodeAndSendMsg(peerId, transport) (msg)
            | ChannelEvent.WeAcceptedCMDFailHTLC(msg, _) ->
                return! this.EncodeAndSendMsg(peerId, transport) (msg)
            | ChannelEvent.WeAcceptedCMDFailMalformedHTLC(msg, _) ->
                return! this.EncodeAndSendMsg(peerId, transport) (msg)
            | ChannelEvent.WeAcceptedCMDUpdateFee(msg, _) ->
                return! this.EncodeAndSendMsg(peerId, transport) (msg)
            | ChannelEvent.WeAcceptedCMDSign (msg, _) ->
                return! this.EncodeAndSendMsg(peerId, transport) (msg)
            | AcceptedShutdownCMD (_) ->
                failwith "TODO "
            | AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs _ ->
                failwith "TODO "
            /// We have to send closing_signed to initiate the negotiation only when if we are the funder
            | AcceptedShutdownWhenNoPendingHTLCs _ ->
                failwith "TODO"
            | AcceptedShutdownWhenWeHavePendingHTLCs _ ->
                failwith "TODO"
            | MutualClosePerformed _ -> failwith "todo"
            | WeProposedNewClosingSigned _ -> failwith "todo"
            | ChannelEvent.Closed _ -> failwith "TODO"
            | Disconnected _ -> failwith "TODO"
            | ChannelStateRequestedSignCommitment _ -> failwith "TODO"
        }
        vt.AsTask() |> Async.AwaitTask
            
    member private this.RememberPeersTransport(peerId: PeerId, pipe: PipeWriter) =
        match this.PeerIdToTransport.TryAdd(peerId, pipe) with
        | true -> ()
        | false ->
            sprintf "failed to remember peerId(%A) 's Transport. This should never happen" peerId
            |> _logger.LogCritical
            ()

    /// Initiate Handshake with peer by sending noise act-one
    /// `ie` is required only for testing. BOLT specifies specific ephemeral key for handshake
    member this.NewOutBoundConnection (theirNodeId: NodeId,
                                       peerId: PeerId,
                                       pipeWriter: PipeWriter,
                                       ?ie: Key) = vtask {
        let act1, peerEncryptor =
            PeerChannelEncryptor.newOutBound(theirNodeId)
            |> fun pce -> if (ie.IsNone) then pce else (Optic.set PeerChannelEncryptor.OutBoundIE_ ie.Value pce)
            |> PeerChannelEncryptor.getActOne
        sprintf "Going to create outbound peer for %A" peerId
        |> _logger.LogTrace
        let newPeer = {
                            ChannelEncryptor = peerEncryptor
                            IsOutBound = true
                            TheirNodeId = None
                            TheirGlobalFeatures = None
                            TheirLocalFeatures = None
                            SyncStatus = InitSyncTracker.NoSyncRequested
                            PeerId = peerId
                            GetOurNodeSecret = _keyRepo.GetNodeSecret
                      }
        match this.KnownPeers.TryAdd(peerId, newPeer) with
        | false ->
            return peerId
                   |> DuplicateConnection
                   |> Result.Error
        | true ->
            this.RememberPeersTransport(peerId, pipeWriter)
            // send act1
            let! _ = pipeWriter.WriteAsync(act1)
            let! _ = pipeWriter.FlushAsync()
            return Ok()
        }

    member this.NewInboundConnection(theirPeerId: PeerId, actOne: byte[], pipeWriter: PipeWriter, ?ourEphemeral) = vtask {
        if (actOne.Length <> 50) then return (UnexpectedByteLength(50, actOne.Length) |> Result.Error) else
        let secret = _keyRepo.GetNodeSecret()
        let peerEncryptor = PeerChannelEncryptor.newInBound(secret)
        let r =
            if (ourEphemeral.IsSome) then
                (PeerChannelEncryptor.processActOneWithEphemeralKey actOne secret ourEphemeral.Value peerEncryptor)
            else
                (PeerChannelEncryptor.processActOneWithKey actOne secret peerEncryptor)
        match r with
        | Bad b -> return (b.Describe() |> PeerError.EncryptorError |> Result.Error)
        | Good (actTwo, pce) ->
            let newPeer = {
                ChannelEncryptor = pce
                IsOutBound = false
                TheirNodeId = None
                TheirGlobalFeatures = None
                TheirLocalFeatures = None

                SyncStatus = InitSyncTracker.NoSyncRequested
                PeerId = theirPeerId
                GetOurNodeSecret = _keyRepo.GetNodeSecret
            }
            match this.KnownPeers.TryAdd(theirPeerId, newPeer) with
            | false ->
                sprintf "duplicate connection with peer %A" theirPeerId
                |> _logger.LogInformation
                return theirPeerId
                |> DuplicateConnection
                |> Result.Error
            | true ->
                this.RememberPeersTransport(theirPeerId, pipeWriter)
                let! _ = pipeWriter.WriteAsync(actTwo)
                let! _ = pipeWriter.FlushAsync()
                return Ok ()
        }

    member private this.UpdatePeerWith(theirPeerId, newPeer: Peer) =
        this.KnownPeers.AddOrUpdate(theirPeerId, newPeer, (fun pId (oldPeer: Peer) -> newPeer))
        |> ignore
        if newPeer.ChannelEncryptor.IsReadyForEncryption() then
            this.OpenedPeers.AddOrUpdate(theirPeerId, newPeer, (fun pId (oldPeer: Peer) -> newPeer))
            |> ignore
    member private this.SetFeaturesToPeer(theirPeerId, gf: GlobalFeatures, lf: LocalFeatures) =
        let dummy = fun _ ->
            let msg = sprintf "Unknown peer. id (%A)" theirPeerId
            _logger.LogError(msg); failwith msg
        this.OpenedPeers.AddOrUpdate(theirPeerId, dummy, fun pId (oldPeer: Peer) -> { oldPeer with
                                                                                          TheirGlobalFeatures = Some gf
                                                                                          TheirLocalFeatures = Some lf })
    member private this.EncodeAndSendMsg(theirPeerId: PeerId, pipeWriter: PipeWriter) (msg: ILightningMsg) =
        _logger.LogTrace(sprintf "encoding and sending msg %A to peer %A" (msg.GetType()) theirPeerId)
        unitVtask {
                match this.OpenedPeers.TryGetValue(theirPeerId) with
                | true, peer ->
                    let msgEncrypted, newPCE =
                        peer.ChannelEncryptor |> PeerChannelEncryptor.encryptMessage (_logger.LogTrace) (msg.ToBytes())
                    this.UpdatePeerWith(theirPeerId, { peer with ChannelEncryptor = newPCE }) |> ignore
                    do! pipeWriter.WriteAsync(msgEncrypted)
                    return ()
                | false, _ ->
                    sprintf "peerId %A is not in opened peers" theirPeerId
                    |> _logger.LogCritical
        }

    /// wrapper to work on PeerChannelEncryptor in atomic way.
    /// i.e. retrieve old peer, operate on with it, and set updated peer to dict again.
    member private this.AtomicEncryptorOperation(peerId, f: Peer -> RResult<('a * PeerChannelEncryptor)>) =
        match this.KnownPeers.TryGetValue peerId with
        | false, _ ->
            sprintf "Unknown Peer %A. Failed to operate by Encryptor" peerId
            |> RResult.rmsg
        | true, peer ->
            match f(peer) with
            | Good(result, newPCE) ->
                this.UpdatePeerWith(peerId, { peer with ChannelEncryptor = newPCE })
                Good result
            | Bad e -> Bad e
    /// Atomic version of PeerChannelEncryptor.processActOneWithKey
    member private this.ProcessActOneWithKey(peerId, actOne: byte[], nodeSecret: Key) =
        _logger.LogTrace("processing act one")
        let innerOp peer = peer.ChannelEncryptor |> PeerChannelEncryptor.processActOneWithKey actOne nodeSecret
        this.AtomicEncryptorOperation(peerId, innerOp)

    /// Atomic version of PeerChannelEncryptor.processActTwo
    member private this.ProcessActTwo(peerId, actTwo, nodeSecret: Key) =
        _logger.LogTrace("processing act two")
        let innerOp = fun p -> p.ChannelEncryptor |> PeerChannelEncryptor.processActTwo actTwo nodeSecret
        this.AtomicEncryptorOperation(peerId, innerOp)
        
    /// Atomic version of PeerChannelEncryptor.processActThree
    member private this.ProcessActThree(peerId, actThree) =
        _logger.LogTrace("processing act three")
        let innerOp peer = peer.ChannelEncryptor |> PeerChannelEncryptor.processActThree actThree
        this.AtomicEncryptorOperation(peerId, innerOp)
    member private this.DecryptLengthHeader(peerId, headerCypherText) =
        let innerOp peer = peer.ChannelEncryptor |> PeerChannelEncryptor.decryptLengthHeader (_logger.LogTrace) headerCypherText
        this.AtomicEncryptorOperation(peerId, innerOp)
        
    member private this.DecryptMessage(peerId, cypherText) =
        let innerOp peer = peer.ChannelEncryptor |> PeerChannelEncryptor.decryptMessage (_logger.LogTrace) cypherText
        this.AtomicEncryptorOperation(peerId, innerOp)
        
    member private this.SetTheirNodeIdToPeer(peerId: PeerId, theirNodeId: NodeId) =
        let dum = fun x -> failwithf "Unknown Peer Id %A" peerId
        this.KnownPeers.AddOrUpdate(peerId, dum, fun v (oldPeer: Peer) -> { oldPeer with TheirNodeId = Some theirNodeId })
    
    member private this.HandleSetupMsgAsync(peerId, msg: ISetupMsg, pipe: IDuplexPipe) =
        vtask {
            let ok, peer = this.OpenedPeers.TryGetValue(peerId)
            assert ok
            match msg with
            | :? Init as init ->
                if (init.GlobalFeatures.RequiresUnknownBits()) then
                    _logger.LogInformation("Peer global features required unknown version bits")
                    return RResult.rbad(RBad.Object({ PeerHandleError.NoConnectionPossible = true }))
                else if (init.LocalFeatures.RequiresUnknownBits()) then
                    _logger.LogInformation("Peer local features required unknown version bits")
                    return RResult.rbad(RBad.Object({ PeerHandleError.NoConnectionPossible = true }))
                else if (peer.TheirGlobalFeatures.IsSome) then
                    return RResult.rbad(RBad.Object({ PeerHandleError.NoConnectionPossible = false }))
                else
                    sprintf "Received peer Init message: data_loss_protect: %s, initial_routing_sync: %s , upfront_shutdown_script: %s, unknown local flags: %s, unknown global flags %s" 
                        (if init.LocalFeatures.SupportsDataLossProect() then "supported" else "not supported")
                        (if init.LocalFeatures.InitialRoutingSync() then "supported" else "not supported")
                        (if init.LocalFeatures.SupportsUpfrontShutdownScript() then "supported" else "not supported")
                        (if init.LocalFeatures.SupportsUnknownBits() then "present" else "not present")
                        (if init.GlobalFeatures.SupportsUnknownBits() then "present" else "not present")
                        |> _logger.LogInformation
                    let theirNodeId = if peer.TheirNodeId.IsSome then peer.TheirNodeId.Value else
                                        let msg = "peer node id is not set. This should never happen"
                                        _logger.LogError msg
                                        failwith msg
                    this.EventAggregator.Publish<PeerEvent>(ReceivedInit (theirNodeId, init))
                    this.SetFeaturesToPeer(peerId, init.GlobalFeatures, init.LocalFeatures) |> ignore
                    if (not peer.IsOutBound) then
                        let lf = LocalFeatures.Flags([||]).SetInitialRoutingSync()
                        do! this.EncodeAndSendMsg(peer.PeerId, pipe.Output) ({ Init.GlobalFeatures = GlobalFeatures.Flags([||]); Init.LocalFeatures = lf })
                        return Good ()
                    else
                        this.EventAggregator.Publish<PeerEvent>(Connected theirNodeId)
                        return Good ()
            | :? ErrorMessage as e ->
                let isDataPrintable = e.Data |> Array.exists(fun b -> b < 32uy || b > 126uy) |> not
                do
                    if isDataPrintable then
                        sprintf "Got error message from %A:%A" (peer.TheirNodeId.Value) (ascii.GetString(e.Data))
                        |> _logger.LogDebug
                    else
                        sprintf "Got error message from %A with non-ASCII error message" (peer.TheirNodeId.Value)
                        |> _logger.LogDebug
                if (e.ChannelId = WhichChannel.All) then
                    return
                        { PeerHandleError.NoConnectionPossible = true }
                        |> box
                        |> RBad.Object
                        |> RResult.rbad
                else
                    this.EventAggregator.Publish<PeerEvent>(ReceivedError(peer.TheirNodeId.Value, e))
                    return Good ()
            | :? Ping as ping ->
                this.EventAggregator.Publish<PeerEvent>(ReceivedPing (peer.TheirNodeId.Value, ping))
                sprintf "Received ping from %A" peer.TheirNodeId.Value
                |> _logger.LogDebug
                if (ping.PongLen < 65532us) then
                    let pong = { Pong.BytesLen = ping.PongLen }
                    do! this.EncodeAndSendMsg(peer.PeerId, pipe.Output) (pong)
                    return Good()
                else
                    return Good()
            | :? Pong as pong ->
                sprintf "Received pong from %A"  peer.TheirNodeId.Value |> _logger.LogDebug
                this.EventAggregator.Publish<PeerEvent>(ReceivedPong (peer.TheirNodeId.Value, pong))
                return Good ()
            | _ -> return failwithf "Unknown setup message %A This should never happen" msg
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
                            |> _logger.LogInformation
                        | Some(IgnoreError) ->
                            sprintf "ignoring the error because %A" he.Error
                            |> _logger.LogDebug
                        | Some(SendErrorMessage msg) ->
                            sprintf "sending error message because %A" he.Error
                            |> _logger.LogDebug
                            let! _ = this.EncodeAndSendMsg(peerId, transport.Output) msg
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

    member private this.InsertNodeId(theirNodeId: NodeId, theirPeerId) =
        match this.NodeIdToPeerId.TryAdd(theirNodeId, theirPeerId) with
        | false ->
            _logger.LogDebug(sprintf "Got second connection with %A , closing." theirNodeId)
            RResult.rbad(RBad.Object { HandleError.Action = Some IgnoreError ; Error = sprintf "We already have connection with %A. nodeid: is %A" theirPeerId theirNodeId })
        | true ->
            _logger.LogTrace(sprintf "Finished noise handshake for connection with %A" theirNodeId)
            match this.KnownPeers.TryGetValue(theirPeerId) with
            | true, peer ->
                this.UpdatePeerWith(theirPeerId, { peer with TheirNodeId = Some theirNodeId })
                Good ()
            | false, _ ->
                RResult.rmsg("Failed to get from known peers")

    member this.ProcessMessageAsync(peerId: PeerId, pipe: IDuplexPipe) =
        this.ProcessMessageAsync(peerId, pipe, Key())
        
    /// read from pipe, If the handshake is not completed. proceed with handshaking process
    /// automatically. If it has been completed. This patch to message handlers
    /// <param name="ourEphemeral"> Used only for test. usually ephemeral keys can be generated randomly </param>
    member this.ProcessMessageAsync(theirPeerId: PeerId, pipe: IDuplexPipe, ourEphemeral: Key) =
        let errorHandler: RBad -> Task = this.TryPotentialHandleError(theirPeerId, pipe)
        unitVtask {
            let! r = this.ReadAsyncCore(theirPeerId, pipe, ourEphemeral)
            let r: RResult<unit> =  r // compiler complains about type annotation without this
            do! r.RBadIterAsync(errorHandler)
            return ()
        }
        
        
    // I wanted to use a bind (>>=) as we do in Channel, but we had to prepare monad-transformer.
    // so instead decided to just use match expression.
    member private this.ReadAsyncCore(peerId: PeerId, pipe: IDuplexPipe, ourEphemeral) =
        vtask {
            match this.KnownPeers.TryGetValue(peerId) with
            | false, _ ->
                sprintf "Going to create new inbound peer against %A" (peerId)
                |>  _logger.LogTrace 
                let! actOne = pipe.Input.ReadExactAsync(50, true)
                let! r = this.NewInboundConnection(peerId, actOne, pipe.Output, ourEphemeral)
                match r with
                | Ok p -> return Good ()
                | Result.Error e -> return (e |> box |> RBad.Object |> RResult.rbad)
            // This case might be unnecessary. Since probably there is no way these are both true
            // 1. We know the peer
            // 2. peer has not sent act1 yet.
            // TODO: Remove?
            | true, _peer when _peer.ChannelEncryptor.GetNoiseStep() = ActOne ->
                let! actOne = pipe.Input.ReadExactAsync(50)
                match this.ProcessActOneWithKey(peerId, actOne, (_keyRepo.GetNodeSecret())) with
                | Bad rbad -> return Bad rbad
                | Good (actTwo) ->
                    do! pipe.Output.WriteAsync(actTwo)
                    return Good ()
            | true, _peer when _peer.ChannelEncryptor.GetNoiseStep() = ActTwo ->
                let! actTwo = pipe.Input.ReadExactAsync(50)
                let r = this.ProcessActTwo(peerId, actTwo, (_keyRepo.GetNodeSecret()))
                match r with
                | Bad rbad -> return Bad rbad
                | Good(actThree, theirNodeId) ->
                    do! pipe.Output.WriteAsync(actThree)
                    match this.InsertNodeId(theirNodeId, peerId) with
                    | Bad rbad -> return Bad rbad
                    | Good _ -> 
                        let localFeatures = 
                            let lf = (LocalFeatures.Flags [||])
                            if _nodeParams.RequireInitialRoutingSync then
                                lf.SetInitialRoutingSync()
                            else
                                lf
                        do!
                            this.EncodeAndSendMsg (peerId, pipe.Output)
                                ({ Init.GlobalFeatures = GlobalFeatures.Flags [||]; LocalFeatures = localFeatures })
                        return Good ()
            | true, _peer when _peer.ChannelEncryptor.GetNoiseStep() = ActThree ->
                let! actThree = pipe.Input.ReadExactAsync(66)
                match this.ProcessActThree(peerId, actThree) with
                | Bad rbad -> return Bad rbad
                | Good (theirNodeId) ->
                    match this.InsertNodeId(theirNodeId, peerId) with
                    | Bad b -> return Bad b
                    | Good _ ->
                        this.EventAggregator.Publish<PeerEvent>(Connected theirNodeId)
                        this.SetTheirNodeIdToPeer(peerId, theirNodeId) |> ignore
                        return Good ()
            | true, peer when peer.ChannelEncryptor.GetNoiseStep() = NoiseComplete ->
                let! lengthHeader = pipe.Input.ReadExactAsync(18)
                match this.DecryptLengthHeader(peerId, lengthHeader) with
                | Bad b -> return Bad b
                | Good (length) -> 
                    let! cipherTextWithMAC = pipe.Input.ReadExactAsync(int length + 16)
                    match this.DecryptMessage(peerId,cipherTextWithMAC) with
                    | Bad b -> return Bad b
                    | Good (data) ->
                        match LightningMsg.fromBytes data with
                        | Bad b -> return Bad b
                        | Good msg ->
                            _logger.LogTrace(sprintf "Peer Decrypted msg %A" (msg.GetType()))
                            match msg with
                            | :? ISetupMsg as setupMsg ->
                                return! this.HandleSetupMsgAsync (peerId, setupMsg, pipe)
                            | :? IRoutingMsg as routingMsg ->
                                this.EventAggregator.Publish<PeerEvent>(ReceivedRoutingMsg (peer.TheirNodeId.Value, routingMsg))
                                return Good ()
                            | :? IChannelMsg as channelMsg ->
                                this.EventAggregator.Publish<PeerEvent>(ReceivedChannelMsg (peer.TheirNodeId.Value, channelMsg))
                                return Good ()
                            | msg -> return failwithf "Unknown type of message (%A), this should never happen" msg
            | _ -> return failwith "unreachable"
        }
    member this.AcceptCommand(cmd: PeerCommand) = unitVtask {
        match cmd with
        | Connect (peerId, nodeId) ->
            let pw = this.PeerIdToTransport.TryGet(peerId)
            match! this.NewOutBoundConnection(nodeId, peerId, pw) with
            | Ok _ -> return ()
            | Result.Error e ->
                sprintf "Failed to create outbound connection to the peer (%A) (%A)" peerId e |> _logger.LogError
                return ()
        | SendPing (peerId, ping) ->
            let pw = this.PeerIdToTransport.TryGet(peerId)
            do! this.EncodeAndSendMsg (peerId, pw) (ping)
            return ()
    }
    
    member this.MakeLocalParams(channelPubKeys, defaultFinalScriptPubKey: Script, isFunder: bool, fundingSatoshis: Money) =
        _nodeParams.MakeLocalParams(this.OurNodeId, channelPubKeys, defaultFinalScriptPubKey, isFunder, fundingSatoshis)
        
    interface IPeerManager with
        member this.ProcessMessageAsync(peerId, pipe) = this.ProcessMessageAsync(peerId, pipe)
        member this.AcceptCommand(cmd) = this.AcceptCommand(cmd)
        member this.OurNodeId = this.OurNodeId