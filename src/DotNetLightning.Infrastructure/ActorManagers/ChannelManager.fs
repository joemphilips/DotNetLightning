namespace DotNetLightning.Infrastructure.ActorManagers

open System.Collections.Concurrent
open System.Threading.Tasks

open FSharp.Control.Tasks
open FSharp.Control.Reactive

open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Utils.Primitives
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Chain
open DotNetLightning.Peer
open DotNetLightning.Channel

open DotNetLightning.Infrastructure
open DotNetLightning.Infrastructure.Interfaces
open DotNetLightning.Infrastructure.Actors

open CustomEventAggregator

open Microsoft.Extensions.Logging

type IChannelManager =
    inherit IActorManager<ChannelCommandWithContext>
    abstract KeysRepository : IKeysRepository with get
    abstract GetPendingChannels: unit -> ChannelState seq
    abstract GetActiveChannels: unit -> ChannelState seq
    abstract GetInactiveChannels: unit -> ChannelState seq

type ChannelManager(log: ILogger<ChannelManager>,
                    loggerProvider: ILoggerFactory,
                    eventAggregator: IEventAggregator,
                    keysRepository: IKeysRepository,
                    channelEventRepo: IChannelEventStream,
                    feeEstimator: IFeeEstimator,
                    fundingTxProvider: IFundingTxProvider,
                    chainConfig: ChainConfig) as this =
    let _chainConfig = chainConfig
    
    let _ourNodeId = keysRepository.GetNodeSecret().PubKey |> NodeId
    
    let createChannel nodeId =
        let config = _chainConfig.GetChannelConfig()
        let c =
            Channel.CreateCurried
                config
                keysRepository
                feeEstimator
                (keysRepository.GetNodeSecret())
                (fundingTxProvider.ProvideFundingTx)
                _chainConfig.Network.NBitcoinNetwork
                nodeId
        let logger = loggerProvider.CreateLogger(sprintf "%A" nodeId)
        let channelActor = new ChannelActor(chainConfig, logger, eventAggregator, c, channelEventRepo, keysRepository)
        (channelActor :> IActor<_>).StartAsync() |> ignore
        match this.Actors.TryAdd(nodeId, channelActor) with
        | true ->
            channelActor
        | false ->
            let msg = sprintf "failed to add channel for %A" nodeId
            log.LogCritical(msg)
            failwith msg
    
    let onChainObservable =
        eventAggregator.GetObservable<OnChainEvent>()
        
    let peerEventObservable =
        eventAggregator.GetObservable<PeerEventWithContext>()
        
    let channelEventObservable =
        eventAggregator.GetObservable<ChannelEventWithContext>()
        
    let _ =
        peerEventObservable
        |> Observable.flatmapAsync(this.PeerEventListener)
        |> Observable.subscribeWithError
               (id)
               ((sprintf "Channel Manager got error while Observing PeerEvent: %A") >> log.LogCritical)

        
    let _ =
        onChainObservable
        |> Observable.flatmapAsync(this.OnChainEventListener)
        |> Observable.subscribeWithError
            (id)
            ((sprintf "Channel Manager got error while Observing OnChain Event: %A") >> log.LogCritical)
            
    let _ =
        channelEventObservable
        |> Observable.flatmapAsync(this.ChannelEventHandler)
        |> Observable.subscribeWithError
            id
            ((sprintf "Channel Manager got error while observing ChannelEvent %A") >> log.LogCritical)
    /// Values are
    /// 1. actual transaction
    /// 2. other peer's id
    /// 3. absolute height of this tx is included in a block (None if it is not confirmed)
    member val FundingTxs = ConcurrentDictionary<TxId, NodeId * (TxIndexInBlock * BlockHeight) option>()
    
    member val CurrentBlockHeight = BlockHeight.Zero with get, set
    member val RemoteInits: ConcurrentDictionary<NodeId, InitMsg> = ConcurrentDictionary<_,_>()
    
    member val Actors: ConcurrentDictionary<NodeId, ChannelActor> = ConcurrentDictionary<_,_>() with get
    
    member this.OnChainEventListener e =
        let t = unitTask {
            log.LogTrace(sprintf "Channel Manager has observed on-chain event %A" (e.GetType()))
            match e with
            | OnChainEvent.BlockConnected (_, height, txs) ->
                this.CurrentBlockHeight <- height
                log.LogTrace(sprintf "block connected height: %A" height)
                for kv in this.FundingTxs do
                    let mutable found = false
                    for txInBlock in txs do
                        let txId = txInBlock |> snd |> fun tx  -> tx.GetTxId()
                        if (txId = kv.Key) then
                            log.LogInformation(sprintf "we found funding tx (%A) on the blockchain" txId)
                            found <- true
                            let txIndex = txInBlock |> fst |> TxIndexInBlock
                            let (nodeId, _) = kv.Value
                            let depth = 1u |> BlockHeightOffset32
                            let newValue = (nodeId, Some(txIndex, height))
                            this.FundingTxs.TryUpdate(kv.Key, newValue, kv.Value) |> ignore
                            for kv in this.Actors do
                                let actor = kv.Value
                                do! (actor :> IActor<_>).Put(ChannelCommand.ApplyFundingConfirmedOnBC(height, txIndex, depth))
                    if (not found) then
                        match kv.Value with
                        | (_nodeId, None) ->
                            ()
                        |(nodeId, Some(txIndex, heightConfirmed)) ->
                            let depth = (height - heightConfirmed) + BlockHeightOffset32.One
                            log.LogDebug(sprintf "funding tx (%A) confirmed (%d) times" kv.Key depth.Value)
                            for kv in this.Actors do
                                let actor = kv.Value
                                do! (actor :> IActor<_>).Put(ChannelCommand.ApplyFundingConfirmedOnBC(height, txIndex, depth))
                            
            | OnChainEvent.BlockDisconnected blocks ->
                for b in blocks do
                    let _header, _height, txs = b
                    for kv in this.FundingTxs do
                        for txInBlock in txs do
                            let txId = txInBlock |> snd |> fun tx -> tx.GetTxId()
                            // if once confirmed funding tx disappears from the blockchain,
                            // there is not much we can do to reclaim funds.
                            // we just wait for funding tx to appear on the blockchain again.
                            // If it does, we close the channel afterwards.
                            if (kv.Key = txId) then
                                log.LogError(sprintf "funding tx (%A) has disappeared from chain (probably by reorg)" txId)
                                match kv.Value with
                                | (_nodeId, None) -> ()
                                | (nodeId, Some(_txIndex, _heightConfirmed)) ->
                                    let newValue = (nodeId, None)
                                    this.FundingTxs.TryUpdate(kv.Key, newValue, kv.Value) |> ignore
            return ()
        }
        t |> Async.AwaitTask
        
        
    member private this.ChannelEventHandler (e: ChannelEventWithContext) =
        let vt = unitVtask {
            match e.ChannelEvent with
            | ChannelEvent.WeAcceptedAcceptChannel (nextMsg, _) ->
                this.FundingTxs.TryAdd(nextMsg.FundingTxId, (e.NodeId, None)) |> ignore
            | WeResumedDelayedFundingLocked theirFundingSigned ->
                do! (this.Actors.[e.NodeId] :> IActor<_>).Put(ChannelCommand.ApplyFundingLocked theirFundingSigned)
            | _ -> ()
        }
        vt.AsTask() |> Async.AwaitTask
        
    member this.PeerEventListener e =
        let t = unitTask {
            match e.PeerEvent with
            | PeerEvent.ReceivedChannelMsg (msg, _) ->
                log.LogTrace(sprintf "channel manager has observed %A" (msg.GetType()))
                match msg with
                | :? OpenChannelMsg as m ->
                    let channelKeys = keysRepository.GetChannelKeys(true)
                    let initFundee = { InputInitFundee.LocalParams =
                                           let channelPubKeys = channelKeys.ToChannelPubKeys()
                                           let spk = _chainConfig.ShutdownScriptPubKey |> Option.defaultValue (keysRepository.GetShutdownPubKey().WitHash.ScriptPubKey)
                                           _chainConfig.MakeLocalParams(_ourNodeId, channelPubKeys, spk, false, m.FundingSatoshis)
                                       TemporaryChannelId = m.TemporaryChannelId
                                       RemoteInit = this.RemoteInits.[e.NodeId.Value]
                                       ToLocal = m.PushMSat
                                       ChannelKeys = channelKeys }
                    do! this.AcceptCommandAsync({ ChannelCommand =  ChannelCommand.CreateInbound(initFundee);
                                                  NodeId = e.NodeId.Value })
                    return! (this.Actors.[e.NodeId.Value] :> IActor<_>).Put(ChannelCommand.ApplyOpenChannel m)
                | :? AcceptChannelMsg as m ->
                    return! (this.Actors.[e.NodeId.Value] :> IActor<_>).Put( ChannelCommand.ApplyAcceptChannel m)
                | :? FundingCreatedMsg as m ->
                    log.LogTrace(sprintf "we received funding created so adding funding tx %A" m.FundingTxId)
                    this.FundingTxs.TryAdd(m.FundingTxId, (e.NodeId.Value, None)) |> ignore
                    return! (this.Actors.[e.NodeId.Value] :> IActor<_>).Put(ChannelCommand.ApplyFundingCreated m)
                | :? FundingSignedMsg as m ->
                    return! (this.Actors.[e.NodeId.Value] :> IActor<_>).Put(ChannelCommand.ApplyFundingSigned m)
                | :? FundingLockedMsg as m ->
                    return! (this.Actors.[e.NodeId.Value] :> IActor<_>).Put(ChannelCommand.ApplyFundingLocked m)
                | :? ShutdownMsg as m ->
                    return! (this.Actors.[e.NodeId.Value] :> IActor<_>).Put(ChannelCommand.RemoteShutdown m)
                | :? ClosingSignedMsg as m ->
                    return! (this.Actors.[e.NodeId.Value] :> IActor<_>).Put(ChannelCommand.ApplyClosingSigned m)
                | :? UpdateAddHTLCMsg as m ->
                    return! (this.Actors.[e.NodeId.Value] :> IActor<_>).Put(ChannelCommand.ApplyUpdateAddHTLC (m, this.CurrentBlockHeight))
                | :? UpdateFulfillHTLCMsg as m ->
                    return! (this.Actors.[e.NodeId.Value] :> IActor<_>).Put(ChannelCommand.ApplyUpdateFulfillHTLC m)
                | :? UpdateFailHTLCMsg as m ->
                    return! (this.Actors.[e.NodeId.Value] :> IActor<_>).Put(ChannelCommand.ApplyUpdateFailHTLC m)
                | :? UpdateFailMalformedHTLCMsg as m ->
                    return! (this.Actors.[e.NodeId.Value] :> IActor<_>).Put(ChannelCommand.ApplyUpdateFailMalformedHTLC m)
                | :? CommitmentSignedMsg as m ->
                    return! (this.Actors.[e.NodeId.Value] :> IActor<_>).Put(ChannelCommand.ApplyCommitmentSigned m)
                | :? RevokeAndACKMsg as m ->
                    return! (this.Actors.[e.NodeId.Value] :> IActor<_>).Put(ChannelCommand.ApplyRevokeAndACK m)
                | :? UpdateFeeMsg as m ->
                    return! (this.Actors.[e.NodeId.Value] :> IActor<_>).Put(ChannelCommand.ApplyUpdateFee m)
                | m ->
                        return failwithf "Unknown Channel Message (%A). This should never happen" m
            | PeerEvent.ReceivedInit(init, _) ->
                this.RemoteInits.TryAdd(e.NodeId.Value, init) |> ignore
                return ()
            | PeerEvent.FailedToBroadcastTransaction(_tx) ->
                return ()
            | _ -> return ()
        }
        t |> Async.AwaitTask
    
    member this.AcceptCommandAsync cmdWithContext = unitVtask {
            match cmdWithContext with
            | { ChannelCommand = ChannelCommand.CreateInbound(_) as cmd; NodeId = nodeId } ->
                let a = createChannel nodeId
                return! (a :> IActor<_>).Put(cmd)
            | { ChannelCommand = ChannelCommand.CreateOutbound(_) as cmd; NodeId = nodeId } ->
                let a = createChannel nodeId
                return! (a :> IActor<_>).Put(cmd)
            | { ChannelCommand = cmd; NodeId = nodeId } ->
                return! (this.Actors.[nodeId] :> IActor<_>).Put(cmd)
        }
    
    interface IChannelManager with
        member this.AcceptCommandAsync(cmd) = this.AcceptCommandAsync(cmd)
        member val KeysRepository = keysRepository with get
        member this.GetPendingChannels() =
            this.Actors
            |> Seq.choose(fun kv ->
                              let s = kv.Value.State.State
                              s.Phase
                              |> function
                                | ChannelStatePhase.Opening _ -> Some s | _ -> None)
        member this.GetActiveChannels() =
            this.Actors
            |> Seq.choose(fun kv ->
                              kv.Value.State.State
                              |> function ChannelState.Normal _ as normalState -> Some normalState | _ -> None)
                
        member this.GetInactiveChannels() =
            this.Actors
            |> Seq.choose(fun kv ->
                              let s = kv.Value.State.State
                              s.Phase
                              |> function
                                | ChannelStatePhase.Closing _ -> Some s
                                | ChannelStatePhase.Closed _ -> Some s
                                | ChannelStatePhase.Abnormal _ -> Some s
                                | _ -> None)
