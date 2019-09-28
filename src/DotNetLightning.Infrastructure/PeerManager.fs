namespace DotnetLightning.Infrastructure

open DotNetLightning.Utils
open DotNetLightning.Infrastructure

open CustomEventAggregator

open NBitcoin
open System.IO.Pipelines
open System.Threading.Tasks
open FSharp.Control.Reactive
open FSharp.Control.Tasks
open Microsoft.Extensions.Logging

type IPeerManager =
    abstract member ProcessMessageAsync: PeerId * IDuplexPipe -> ValueTask
    abstract member OurNodeId: NodeId
type PeerManager(eventAggregator: IEventAggregator,
                 log: ILogger<PeerManager>
                 ) as this =
    let _channelEventObservable =
        eventAggregator.GetObservable<ChannelEventWithContext>()
        |> Observable.flatmapAsync(this.ChannelEventListener)
        |> Observable.subscribeWithError
               (id)
               ((sprintf "PeerManager got error while Observing ChannelEvent: %A") >> log.LogCritical)
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
                        this.EventAggregator.Publish<PeerEventWithContext>({ PeerEvent = FailedToBroadcastTransaction(finalizedFundingTx.Value); NodeId = nodeId})
                        return ()
                | Choice2Of2 ex ->
                    ex.Message |> log.LogCritical
                    this.EventAggregator.Publish<PeerEventWithContext>({ PeerEvent = FailedToBroadcastTransaction(finalizedFundingTx.Value); NodeId = nodeId})
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
            | TheySentFundingLocked _msg -> ()
            | ChannelEvent.WeResumedDelayedFundingLocked _ -> ()
            | WeSentFundingLocked fundingLockedMsg ->
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
                |>  log.LogTrace 
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
                match this.ProcessActOneWithKey(peerId, actOne, (keyRepo.GetNodeSecret())) with
                | Bad rbad -> return Bad rbad
                | Good (actTwo) ->
                    do! pipe.Output.WriteAsync(actTwo)
                    return Good ()
            | true, _peer when _peer.ChannelEncryptor.GetNoiseStep() = ActTwo ->
                let! actTwo = pipe.Input.ReadExactAsync(50)
                let r = this.ProcessActTwo(peerId, actTwo, (keyRepo.GetNodeSecret()))
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
                        eventAggregator.Publish<PeerEventWithContext>({ PeerEvent = Connected; NodeId = theirNodeId })
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
                            log.LogTrace(sprintf "Peer Decrypted msg %A" (msg.GetType()))
                            match msg with
                            | :? ISetupMsg as setupMsg ->
                                return! this.HandleSetupMsgAsync (peerId, setupMsg, pipe)
                            | :? IRoutingMsg as routingMsg ->
                                eventAggregator.Publish<PeerEventWithContext>({ PeerEvent = ReceivedRoutingMsg routingMsg; NodeId = peer.TheirNodeId.Value })
                                return Good ()
                            | :? IChannelMsg as channelMsg ->
                                eventAggregator.Publish<PeerEventWithContext>({ PeerEvent = ReceivedChannelMsg (channelMsg); NodeId = peer.TheirNodeId.Value })
                                return Good ()
                            | msg -> return failwithf "Unknown type of message (%A), this should never happen" msg
            | _ -> return failwith "unreachable"
        }

    interface IPeerManager with
        member this.ProcessMessageAsync(i, e) =
            this.ProcessMessageAsync(i, e)
