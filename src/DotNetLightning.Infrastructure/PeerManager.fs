namespace DotNetLightning.Infrastructure

open DotNetLightning.Utils
open DotNetLightning.Infrastructure

open CustomEventAggregator

open NBitcoin
open DotNetLightning.Chain
open DotNetLightning.LN

open DotNetLightning.Serialize.Msgs
open DotNetLightning.Utils.Aether
open DotNetLightning.Utils.Aether.Operators
open System.Collections.Concurrent
open System.IO.Pipelines
open System.Threading.Tasks
open FSharp.Control.Reactive
open FSharp.Control.Tasks
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options

type IPeerManager =
    abstract member ReadAsync: PeerId * IDuplexPipe -> ValueTask
    abstract member AcceptCommand: cmd: PeerCommandWithContext -> ValueTask
    abstract member OurNodeId: NodeId
type PeerManager(eventAggregator: IEventAggregator,
                 log: ILogger<PeerManager>,
                 loggerFactory: ILoggerFactory,
                 keyRepo: IKeysRepository,
                 nodeParams: IOptions<NodeParams>,
                 chainWatcher: IChainWatcher,
                 broadCaster: IBroadCaster
                 ) as this =
    
    let nodeParamsValue = nodeParams.Value
    let _channelEventObservable =
        eventAggregator.GetObservable<ChannelEventWithContext>()
        |> Observable.flatmapAsync(this.ChannelEventListener)
        |> Observable.subscribeWithError
               (id)
               ((sprintf "PeerManager got error while Observing ChannelEvent: %A") >> log.LogCritical)
    let _peerEventObservable =
        eventAggregator.GetObservable<PeerEventWithContext>()
        |> Observable.flatmapAsync(this.PeerEventListener)
        |> Observable.subscribeWithError
            id
           ((sprintf "PeerManager got error while Observing PeerEvent: %A") >> log.LogCritical)
    let ascii = System.Text.ASCIIEncoding.ASCII
    
    let ourNodeId = keyRepo.GetNodeSecret().PubKey |> NodeId

    member val KnownPeers: ConcurrentDictionary<_,_> = ConcurrentDictionary<PeerId, PeerActor>() with get, set
    member val NodeIdToPeerId: ConcurrentDictionary<_,_> = ConcurrentDictionary<NodeId, PeerId>() with get, set
    member val PeerIdToTransport: ConcurrentDictionary<_,_> = ConcurrentDictionary<PeerId, PipeWriter>() with get
    
    
    /// event which should not be handled by the peer (e.g. ReceivedRoutingMsg) will be ignored
    member private this.PeerEventListener e =
        let peerId = this.NodeIdToPeerId.[e.NodeId]
        let vt = vtask {
            match e.PeerEvent with
            | ReceivedError (error, _) ->
                let isDataPrintable = error.Data |> Array.exists(fun b -> b < 32uy || b > 126uy) |> not
                do
                    if isDataPrintable then
                        sprintf "Got error message from %A:%A" (e.NodeId) (ascii.GetString(error.Data))
                        |> log.LogDebug
                    else
                        sprintf "Got error message from %A with non-ASCII error message" (e.NodeId)
                        |> log.LogDebug
                if (error.ChannelId = WhichChannel.All) then
                    log.LogError (sprintf "Got error message which does not specify channel %A" e)
                    do! this.KnownPeers.[peerId].CommunicationChannel.Writer.WriteAsync(Disconnect(e.NodeId))
            | ReceivedPing (ping, _) ->
                sprintf "Received ping from %A" e.NodeId
                |> log.LogDebug
                if (ping.PongLen < 65532us) then
                    let pong = { Pong.BytesLen = ping.PongLen }
                    do! this.KnownPeers.[peerId].CommunicationChannel.Writer.WriteAsync(EncodeMsg pong)
                else
                    log.LogError(sprintf "PongLen is too long %A" ping)
            | ReceivedPong (_pong, _) ->
                sprintf "Received pong from %A"  e.NodeId.Value |> log.LogDebug
            | ReceivedInit (init, _) ->
                let peerActor = this.KnownPeers.[peerId]
                let peer = peerActor.State
                if (init.GlobalFeatures.RequiresUnknownBits()) then
                    log.LogError("Peer global features required unknown version bits")
                    do! peerActor.CommunicationChannel.Writer.WriteAsync(Disconnect(e.NodeId))
                else if (init.LocalFeatures.RequiresUnknownBits()) then
                    log.LogError("Peer local features required unknown version bits")
                    do! peerActor.CommunicationChannel.Writer.WriteAsync(Disconnect(e.NodeId))
                else
                    sprintf "Received peer Init message: data_loss_protect: %s, initial_routing_sync: %s , upfront_shutdown_script: %s, unknown local flags: %s, unknown global flags %s" 
                        (if init.LocalFeatures.SupportsDataLossProect() then "supported" else "not supported")
                        (if init.LocalFeatures.InitialRoutingSync() then "supported" else "not supported")
                        (if init.LocalFeatures.SupportsUpfrontShutdownScript() then "supported" else "not supported")
                        (if init.LocalFeatures.SupportsUnknownBits() then "present" else "not present")
                        (if init.GlobalFeatures.SupportsUnknownBits() then "present" else "not present")
                        |> log.LogInformation
                    let theirNodeId = if peer.TheirNodeId.IsSome then peer.TheirNodeId.Value else
                                        let msg = "peer node id is not set. This should never happen"
                                        log.LogError msg
                                        failwith msg
                    if (not peer.IsOutBound) then
                        let lf = LocalFeatures.Flags([||]).SetInitialRoutingSync()
                        do! peerActor.CommunicationChannel.Writer.WriteAsync(EncodeMsg { Init.GlobalFeatures = GlobalFeatures.Flags([||]); Init.LocalFeatures = lf })
            | _ -> ()
        }
        vt.AsTask() |> Async.AwaitTask
    member private this.RememberPeersTransport(peerId: PeerId, pipe: PipeWriter) =
        match this.PeerIdToTransport.TryAdd(peerId, pipe) with
        | true -> ()
        | false ->
            sprintf "failed to remember peerId(%A) 's Transport. This should never happen" peerId
            |> log.LogCritical
            ()
        
    member private this.ChannelEventListener (contextEvent): Async<unit> =
        let vt = vtask {
            let nodeId = contextEvent.NodeId
            let peerId = this.NodeIdToPeerId.TryGet(nodeId)
            match contextEvent.ChannelEvent with
            // ---- channel init: funder -----
            | ChannelEvent.NewOutboundChannelStarted(msg, _) ->
                return! this.KnownPeers.[peerId].CommunicationChannel.Writer.WriteAsync(EncodeMsg msg)
            | ChannelEvent.WeAcceptedAcceptChannel(msg, _) ->
                return! this.KnownPeers.[peerId].CommunicationChannel.Writer.WriteAsync(EncodeMsg msg)
            | ChannelEvent.WeAcceptedFundingSigned(finalizedFundingTx, _) ->
                let! r = broadCaster.BroadCastTransaction(finalizedFundingTx.Value) |> Async.Catch |> Async.StartAsTask
                match r with
                | Choice1Of2 txId ->
                    let spk1 = finalizedFundingTx.Value.Outputs.[0].ScriptPubKey
                    let spk2 = finalizedFundingTx.Value.Outputs.[1].ScriptPubKey
                    match ([spk1; spk2]
                          |> List.map(fun spk -> chainWatcher.InstallWatchTx(txId, spk))
                          |> List.forall(id)) with
                    | true ->
                        log.LogInformation(sprintf "Waiting for funding tx (%A) to get confirmed..." txId )
                        return ()
                    | false ->
                        log.LogCritical(sprintf "Failed to install watching tx (%A)" txId)
                        eventAggregator.Publish<PeerEventWithContext>({ PeerEvent = FailedToBroadcastTransaction(finalizedFundingTx.Value); NodeId = nodeId})
                        return ()
                | Choice2Of2 ex ->
                    ex.Message |> log.LogCritical
                    eventAggregator.Publish<PeerEventWithContext>({ PeerEvent = FailedToBroadcastTransaction(finalizedFundingTx.Value); NodeId = nodeId})
                    return ()
            // ---- channel init: fundee -----
            | ChannelEvent.NewInboundChannelStarted _ ->
                ()
            | ChannelEvent.WeAcceptedOpenChannel(msg, _) ->
                return! this.KnownPeers.[peerId].CommunicationChannel.Writer.WriteAsync(EncodeMsg msg)
            | ChannelEvent.WeAcceptedFundingCreated(msg, _) ->
                return! this.KnownPeers.[peerId].CommunicationChannel.Writer.WriteAsync(EncodeMsg msg)
                
            // ---- else ----
            | FundingConfirmed _  -> ()
            | TheySentFundingLocked _msg -> ()
            | ChannelEvent.WeResumedDelayedFundingLocked _ -> ()
            | WeSentFundingLocked fundingLockedMsg ->
                return! this.KnownPeers.[peerId].CommunicationChannel.Writer.WriteAsync(EncodeMsg fundingLockedMsg)
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
                return! this.KnownPeers.[peerId].CommunicationChannel.Writer.WriteAsync(EncodeMsg msg)
            | ChannelEvent.WeAcceptedCMDFulfillHTLC (msg, _) ->
                return! this.KnownPeers.[peerId].CommunicationChannel.Writer.WriteAsync(EncodeMsg msg)
            | ChannelEvent.WeAcceptedCMDFailHTLC(msg, _) ->
                return! this.KnownPeers.[peerId].CommunicationChannel.Writer.WriteAsync(EncodeMsg msg)
            | ChannelEvent.WeAcceptedCMDFailMalformedHTLC(msg, _) ->
                return! this.KnownPeers.[peerId].CommunicationChannel.Writer.WriteAsync(EncodeMsg msg)
            | ChannelEvent.WeAcceptedCMDUpdateFee(msg, _) ->
                return! this.KnownPeers.[peerId].CommunicationChannel.Writer.WriteAsync(EncodeMsg msg)
            | ChannelEvent.WeAcceptedCMDSign (msg, _) ->
                return! this.KnownPeers.[peerId].CommunicationChannel.Writer.WriteAsync(EncodeMsg msg)
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
        

    member this.NewInboundConnection(theirPeerId: PeerId, actOne: byte[], pipeWriter: PipeWriter, ?ourEphemeral) = vtask {
        if (actOne.Length <> 50) then return (UnexpectedByteLength(50, actOne.Length) |> Result.Error) else
        let secret = keyRepo.GetNodeSecret()
        let newPeer = Peer.CreateInbound(theirPeerId, secret)
        let r =
            if (ourEphemeral.IsSome) then
                (PeerChannelEncryptor.processActOneWithEphemeralKey actOne secret ourEphemeral.Value newPeer.ChannelEncryptor)
            else
                (PeerChannelEncryptor.processActOneWithKey actOne secret newPeer.ChannelEncryptor)
        match r with
        | Bad b -> return (b.Describe() |> PeerError.EncryptorError |> Result.Error)
        | Good (actTwo, pce) ->
            let newPeer = { newPeer with ChannelEncryptor = pce }
            let log = loggerFactory.CreateLogger(sprintf "PeerActor (%A)" theirPeerId)
            let peerActor = new PeerActor (newPeer, keyRepo, log, pipeWriter, nodeParams, eventAggregator)
            match this.KnownPeers.TryAdd(theirPeerId, peerActor) with
            | false ->
                sprintf "duplicate connection with peer %A" theirPeerId
                |> log.LogInformation
                return theirPeerId
                |> DuplicateConnection
                |> Result.Error
            | true ->
                this.RememberPeersTransport(theirPeerId, pipeWriter)
                let! _ = pipeWriter.WriteAsync(actTwo)
                let! _ = pipeWriter.FlushAsync()
                return Ok ()
        }
    
    /// Initiate Handshake with peer by sending noise act-one
    /// `ie` is required only for testing. BOLT specifies specific ephemeral key for handshake
    member this.NewOutBoundConnection (theirNodeId: NodeId,
                                       peerId: PeerId,
                                       pipeWriter: PipeWriter,
                                       ?ie: Key) = vtask {
        let newPeer = Peer.CreateOutbound(peerId, theirNodeId)
        let act1, peerEncryptor =
            newPeer
            |> fun p -> if (ie.IsNone) then p
                          else (Optic.set (Peer.ChannelEncryptor_ >-> PeerChannelEncryptor.OutBoundIE_) ie.Value p)
            |> fun p -> PeerChannelEncryptor.getActOne p.ChannelEncryptor
        let newPeer = { newPeer with ChannelEncryptor = peerEncryptor }
        sprintf "Going to create outbound peer for %A" peerId
        |> log.LogTrace
        let log = loggerFactory.CreateLogger(sprintf "PeerActor (%A)" peerId)
        let peerActor = new PeerActor (newPeer, keyRepo, log, pipeWriter, nodeParams, eventAggregator)
        match this.KnownPeers.TryAdd(peerId, peerActor) with
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

        
    member this.ReadAsync(peerId: PeerId, pipe: IDuplexPipe) =
        this.ReadAsync(peerId, pipe, Key())
        
        
    member this.ReadAsync(peerId: PeerId, pipe: IDuplexPipe, ourEphemeral): ValueTask =
        unitVtask {
            match this.KnownPeers.TryGetValue(peerId) with
            | false, _ ->
                sprintf "Going to create new inbound peer against %A" (peerId)
                |>  log.LogTrace 
                let! actOne = pipe.Input.ReadExactAsync(50, true)
                let! r = this.NewInboundConnection(peerId, actOne, pipe.Output, ourEphemeral)
                do! this.KnownPeers.[peerId].CommunicationChannel.Writer.WriteAsync(ProcessActOne(actOne, (keyRepo.GetNodeSecret())))
            // This case might be unnecessary. Since probably there is no way these are both true
            // 1. We know the peer
            // 2. peer has not sent act1 yet.
            // TODO: Remove?
            | true, peer when peer.State.ChannelEncryptor.GetNoiseStep() = ActOne ->
                let! actOne = pipe.Input.ReadExactAsync(50)
                do! this.KnownPeers.[peerId].CommunicationChannel.Writer.WriteAsync(ProcessActOne(actOne, (keyRepo.GetNodeSecret())))
            | true, peer when peer.State.ChannelEncryptor.GetNoiseStep() = ActTwo ->
                let! actTwo = pipe.Input.ReadExactAsync(50)
                do! this.KnownPeers.[peerId].CommunicationChannel.Writer.WriteAsync(ProcessActTwo(actTwo, (keyRepo.GetNodeSecret())))
                
            | true, peer when peer.State.ChannelEncryptor.GetNoiseStep() = ActThree ->
                let! actThree = pipe.Input.ReadExactAsync(66)
                do! this.KnownPeers.[peerId].CommunicationChannel.Writer.WriteAsync(ProcessActThree(actThree))
            | true, peer when peer.State.ChannelEncryptor.GetNoiseStep() = NoiseComplete && (peer.State.LengthDecoded.IsNone) ->
                let! lengthHeader = pipe.Input.ReadExactAsync(18)
                do! this.KnownPeers.[peerId].CommunicationChannel.Writer.WriteAsync(PeerCommand.DecodeLength lengthHeader)
            | true, peer when peer.State.ChannelEncryptor.GetNoiseStep() = NoiseComplete && (peer.State.LengthDecoded.IsSome) ->
                let len = peer.State.LengthDecoded.Value
                let! packet = pipe.Input.ReadExactAsync(int len)
                do! this.KnownPeers.[peerId].CommunicationChannel.Writer.WriteAsync(PeerCommand.DecodeCipherPacket packet)
            | _ -> return failwith "unreachable"
        }
    member this.AcceptCommand(cmd: PeerCommandWithContext) = unitVtask {
        match this.KnownPeers.TryGetValue(cmd.PeerId) with
        | true, peerActor ->
            do! peerActor.CommunicationChannel.Writer.WriteAsync(cmd.PeerCommand)
        | _ ->
            match cmd with
            | { PeerCommand = Connect nodeId; PeerId = peerId } ->
                let pw = this.PeerIdToTransport.TryGet(peerId)
                match! this.NewOutBoundConnection(nodeId, peerId, pw) with
                | Ok _ -> return ()
                | Result.Error e ->
                    sprintf "Failed to create outbound connection to the peer (%A) (%A)" peerId e |> log.LogError
            | _ ->
                log.LogError(sprintf "failed to get peer %A" cmd.PeerId)
                ()
    }
    
    member this.MakeLocalParams(channelPubKeys, defaultFinalScriptPubKey: Script, isFunder: bool, fundingSatoshis: Money) =
        nodeParamsValue.MakeLocalParams(ourNodeId, channelPubKeys, defaultFinalScriptPubKey, isFunder, fundingSatoshis)
    
    interface IPeerManager with
        member this.OurNodeId = ourNodeId
        member this.ReadAsync(peerId, pipe) = this.ReadAsync(peerId, pipe)
        member this.AcceptCommand(cmd) = this.AcceptCommand cmd

