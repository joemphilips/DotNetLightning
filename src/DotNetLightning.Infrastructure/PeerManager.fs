namespace DotNetLightning.Infrastructure

open DotNetLightning.Utils
open DotNetLightning.Infrastructure

open CustomEventAggregator

open NBitcoin
open DotNetLightning.Chain
open DotNetLightning.LN
open DotNetLightning.Channel

open System
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Utils.Aether
open DotNetLightning.Utils.Aether.Operators
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO.Pipelines
open System.Threading.Tasks
open FSharp.Control.Reactive
open FSharp.Control.Tasks
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options

type IPeerManager =
    abstract member ReadAsync: PeerId * IDuplexPipe -> ValueTask
    abstract member AcceptCommand: cmd: PeerCommandWithContext -> ValueTask
    abstract member NewOutBoundConnection: theirNodeId: NodeId *
                                           peerId: PeerId *
                                           pipeWriter: PipeWriter *
                                           ?ie: Key -> ValueTask
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
        |> Observable.filter(fun cec -> this.NodeIdToPeerId.ContainsKey(cec.NodeId))
        |> Observable.flatmapAsync(this.ChannelEventListener)
        |> Observable.subscribeWithError
               (id)
               ((sprintf "PeerManager got error while Observing ChannelEvent: %A") >> log.LogCritical)
    let peerEventObservable =
        eventAggregator.GetObservable<PeerEventWithContext>()
    
    let _ =
        peerEventObservable
        |> Observable.map(fun e -> sprintf "peer manager observed peer event %A" (e.PeerEvent.GetType()) |> log.LogInformation; e)
        |> Observable.flatmapAsync(this.PeerEventListener)
        |> Observable.subscribeWithError
            id
           ((sprintf "PeerManager got error while Observing PeerEvent: %A") >> log.LogCritical)
    let ascii = System.Text.ASCIIEncoding.ASCII
    
    let ourNodeId = keyRepo.GetNodeSecret().PubKey |> NodeId

    member val KnownPeers: ConcurrentDictionary<_,_> = ConcurrentDictionary<PeerId, PeerActor>() with get, set
    member val NodeIdToPeerId: ConcurrentDictionary<_,_> = ConcurrentDictionary<NodeId, PeerId>() with get, set
    
    member private this.Disconnect(peerId) =
        log.LogInformation(sprintf "disconnecting peer %A" peerId)
        (this.KnownPeers.[peerId] :> IDisposable).Dispose()
        this.KnownPeers.Remove(peerId) |> ignore
        
    /// event which should not be handled by the peer (e.g. ReceivedRoutingMsg) will be ignored
    member private this.PeerEventListener e =
        let vt = vtask {
            match e.PeerEvent with
            // --- initialization ---
            | ActTwoProcessed((_, nodeId), _) ->
                match this.NodeIdToPeerId.TryAdd(nodeId, e.PeerId) with
                | true -> ()
                | _ -> failwithf "Duplicate Connection with %A" nodeId
                let localFeatures = 
                    let lf = (LocalFeatures.Flags [||])
                    if nodeParamsValue.RequireInitialRoutingSync then
                        lf.SetInitialRoutingSync()
                    else
                        lf
                let init = { Init.GlobalFeatures = GlobalFeatures.Flags [||]; LocalFeatures = localFeatures }
                log.LogTrace "sending init"
                do! (this.KnownPeers.[e.PeerId] :> IActor<_>). Put(PeerCommand.EncodeMsg (init))
            | ActThreeProcessed(nodeId, _) ->
                match this.NodeIdToPeerId.TryAdd(nodeId, e.PeerId) with
                | true -> ()
                | _ -> failwithf "Duplicate Connection with %A" nodeId
            
            // --- received something ---
            | ReceivedError (error, _) ->
                let peerId = e.PeerId
                let isDataPrintable = error.Data |> Array.exists(fun b -> b < 32uy || b > 126uy) |> not
                do
                    if isDataPrintable then
                        sprintf "Got error message from %A:%A" (e.PeerId) (ascii.GetString(error.Data))
                        |> log.LogDebug
                    else
                        sprintf "Got error message from %A with non-ASCII error message" (e.PeerId)
                        |> log.LogDebug
                if (error.ChannelId = WhichChannel.All) then
                    log.LogError (sprintf "Got error message which does not specify channel. Disconnect %A" e)
                    this.Disconnect(peerId)
            | ReceivedPing (ping, _) ->
                sprintf "Received ping from %A" e.PeerId
                |> log.LogDebug
                if (ping.PongLen < 65532us) then
                    let pong = { Pong.BytesLen = ping.PongLen }
                    do! (this.KnownPeers.[e.PeerId] :> IActor<_>). Put(EncodeMsg pong)
                else
                    log.LogError(sprintf "PongLen is too long %A" ping)
            | ReceivedPong (_pong, _) ->
                sprintf "Received pong from %A"  e.PeerId |> log.LogDebug
            | ReceivedInit (init, _) ->
                let peerId = e.PeerId
                let peerActor = this.KnownPeers.[peerId]
                let peer = peerActor.State
                if (init.GlobalFeatures.RequiresUnknownBits()) then
                    log.LogError("Peer global features required unknown version bits")
                    this.Disconnect(peerId)
                else if (init.LocalFeatures.RequiresUnknownBits()) then
                    log.LogError("Peer local features required unknown version bits")
                    this.Disconnect(peerId)
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
                        do! (peerActor :> IActor<_>). Put(EncodeMsg { Init.GlobalFeatures = GlobalFeatures.Flags([||]); Init.LocalFeatures = lf })
            | _ -> ()
        }
        vt.AsTask() |> Async.AwaitTask
    member private this.ChannelEventListener (contextEvent): Async<unit> =
        let vt = vtask {
            let nodeId = contextEvent.NodeId
            match this.NodeIdToPeerId.TryGetValue(nodeId) with
            | false, _ -> ()
            | true, peerId ->
                match contextEvent.ChannelEvent with
                // ---- channel init: funder -----
                | ChannelEvent.NewOutboundChannelStarted(msg, _) ->
                    return! (this.KnownPeers.[peerId] :> IActor<_>). Put(EncodeMsg msg)
                | ChannelEvent.WeAcceptedAcceptChannel(msg, _) ->
                    return! (this.KnownPeers.[peerId] :> IActor<_>).Put(EncodeMsg msg)
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
                            eventAggregator.Publish<PeerEventWithContext>({ PeerEvent = FailedToBroadcastTransaction(finalizedFundingTx.Value);
                                                                            PeerId = peerId; NodeId = Some nodeId})
                            return ()
                    | Choice2Of2 ex ->
                        ex.Message |> log.LogCritical
                        eventAggregator.Publish<PeerEventWithContext>({ PeerEvent = FailedToBroadcastTransaction(finalizedFundingTx.Value);
                                                                        PeerId = peerId; NodeId = Some nodeId})
                        return ()
                // ---- channel init: fundee -----
                | ChannelEvent.NewInboundChannelStarted _ ->
                    ()
                | ChannelEvent.WeAcceptedOpenChannel(msg, _) ->
                    return! (this.KnownPeers.[peerId] :> IActor<_>).Put(EncodeMsg msg)
                | ChannelEvent.WeAcceptedFundingCreated(msg, _) ->
                    return! (this.KnownPeers.[peerId] :> IActor<_>).Put(EncodeMsg msg)
                    
                // ---- else ----
                | FundingConfirmed _  -> ()
                | TheySentFundingLocked _msg -> ()
                | ChannelEvent.WeResumedDelayedFundingLocked _ -> ()
                | WeSentFundingLocked fundingLockedMsg ->
                    return! (this.KnownPeers.[peerId] :> IActor<_>).Put(EncodeMsg fundingLockedMsg)
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
                    return! (this.KnownPeers.[peerId] :> IActor<_>).Put(EncodeMsg msg)
                | ChannelEvent.WeAcceptedCMDFulfillHTLC (msg, _) ->
                    return! (this.KnownPeers.[peerId] :> IActor<_>).Put(EncodeMsg msg)
                | ChannelEvent.WeAcceptedCMDFailHTLC(msg, _) ->
                    return! (this.KnownPeers.[peerId] :> IActor<_>).Put(EncodeMsg msg)
                | ChannelEvent.WeAcceptedCMDFailMalformedHTLC(msg, _) ->
                    return! (this.KnownPeers.[peerId] :> IActor<_>).Put(EncodeMsg msg)
                | ChannelEvent.WeAcceptedCMDUpdateFee(msg, _) ->
                    return! (this.KnownPeers.[peerId] :> IActor<_>).Put(EncodeMsg msg)
                | ChannelEvent.WeAcceptedCMDSign (msg, _) ->
                    return! (this.KnownPeers.[peerId] :> IActor<_>).Put(EncodeMsg msg)
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
            let peerActor = new PeerActor (newPeer, theirPeerId, keyRepo, log, pipeWriter, nodeParams, eventAggregator)
            (peerActor :> IActor<_>).StartAsync() |> ignore
            match this.KnownPeers.TryAdd(theirPeerId, peerActor) with
            | false ->
                sprintf "duplicate connection with peer %A" theirPeerId
                |> log.LogInformation
                return theirPeerId
                |> DuplicateConnection
                |> Result.Error
            | true ->
                let! _ = pipeWriter.WriteAsync(actTwo)
                let! _ = pipeWriter.FlushAsync()
                return Ok ()
        }
    
    /// Initiate Handshake with peer by sending noise act-one
    /// `ie` is required only for testing. BOLT specifies specific ephemeral key for handshake
    member this.NewOutBoundConnection (theirNodeId: NodeId,
                                       peerId: PeerId,
                                       pipeWriter: PipeWriter,
                                       ?ie: Key) =
        unitVtask {
            let newPeer = Peer.CreateOutbound(peerId, theirNodeId)
            let act1, peerEncryptor =
                newPeer
                |> fun p -> if (ie.IsNone) then p
                              else (Optic.set (Peer.ChannelEncryptor_ >-> PeerChannelEncryptor.OutBoundIE_) ie.Value p)
                |> fun p -> PeerChannelEncryptor.getActOne p.ChannelEncryptor
            let newPeer = { newPeer with ChannelEncryptor = peerEncryptor }
            sprintf "Going to create outbound peer for %A" peerId
            |> log.LogTrace
            let peerActor =
                let log = loggerFactory.CreateLogger(sprintf "PeerActor (%A)" peerId)
                new PeerActor (newPeer, peerId, keyRepo, log, pipeWriter, nodeParams, eventAggregator)
            (peerActor :> IActor<_>).StartAsync() |> ignore
            match this.KnownPeers.TryAdd(peerId, peerActor) with
            | false ->
                sprintf "peer (%A) (%A) is already connected" peerId theirNodeId |> log.LogError
            | true ->
                // send act1
                let! _ = pipeWriter.WriteAsync(act1)
                let! _ = pipeWriter.FlushAsync()
                ()
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
                let! _r = this.NewInboundConnection(peerId, actOne, pipe.Output, ourEphemeral)
                ()
            | true, peer when peer.State.ChannelEncryptor.GetNoiseStep() = ActTwo ->
                let! actTwo = pipe.Input.ReadExactAsync(50)
                do! (this.KnownPeers.[peerId] :> IActor<_>).PutAndWaitProcess(ProcessActTwo(actTwo, (keyRepo.GetNodeSecret())))
            | true, peer when peer.State.ChannelEncryptor.GetNoiseStep() = ActThree ->
                let! actThree = pipe.Input.ReadExactAsync(66)
                do! (this.KnownPeers.[peerId] :> IActor<_>).PutAndWaitProcess(ProcessActThree(actThree))
            | true, peer when peer.State.ChannelEncryptor.GetNoiseStep() = NoiseComplete ->
                let! l = pipe.Input.ReadExactAsync(18)
                let reader = fun l -> (pipe.Input.ReadExactAsync (l)).GetAwaiter().GetResult()
                do! (this.KnownPeers.[peerId] :> IActor<_>).PutAndWaitProcess(DecodeCipherPacket (l, reader))
            | _ -> return failwith "unreachable"
        }
    member this.AcceptCommand(cmd: PeerCommandWithContext) = unitVtask {
        match this.KnownPeers.TryGetValue(cmd.PeerId) with
        | true, peerActor ->
            do! (peerActor :> IActor<_>).Put(cmd.PeerCommand)
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
        member this.NewOutBoundConnection (nodeId, peerId, pipeWriter, ?key) =
            match key with
            | Some key ->
                this.NewOutBoundConnection(nodeId, peerId, pipeWriter, key)
            | None ->
                this.NewOutBoundConnection(nodeId, peerId, pipeWriter)

