namespace DotnetLightning.Infrastructure

open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Chain
open DotNetLightning.LN
open DotNetLightning.Infrastructure

open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options
open Microsoft.Extensions.DependencyInjection

open CustomEventAggregator
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Utils.Primitives
open System
open System.Reactive.Threading.Tasks
open System.Collections.Concurrent
open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharp.Control.Reactive

type IChannelManager =
    abstract AcceptCommandAsync: ChannelCommandWithContext -> ValueTask
    abstract KeysRepository : IKeysRepository with get
    

type ChannelManager(log: ILogger<ChannelActor>,
                    loggerProvider: ILoggerProvider,
                    chainListener: IChainListener,
                    eventAggregator: IEventAggregator,
                    keysRepository: IKeysRepository,
                    channelEventRepo: IChannelEventRepository,
                    feeEstimator: IFeeEstimator,
                    fundingTxProvider: IFundingTxProvider,
                    nodeParams: IOptions<NodeParams>) as this =
    let _nodeParams = nodeParams.Value
    let _internalLog = Logger.fromMicrosoftLogger log
    
    let _ourNodeId = keysRepository.GetNodeSecret().PubKey |> NodeId
    
    let createChannel nodeId =
        let config = _nodeParams.GetChannelConfig()
        let c =
            Channel.CreateCurried
                config
                _internalLog
                chainListener
                keysRepository
                feeEstimator
                (keysRepository.GetNodeSecret())
                (fundingTxProvider.ProvideFundingTx)
                _nodeParams.ChainNetwork
                nodeId
        let logger = loggerProvider.CreateLogger(sprintf "%A" nodeId)
        let channelActor = new ChannelActor(nodeParams, logger, eventAggregator, c, channelEventRepo, keysRepository)
        (channelActor :> IActor).StartAsync() |> ignore
        match this.Actors.TryAdd(nodeId, channelActor) with
        | true ->
            channelActor
        | false ->
            let msg = sprintf "failed to add channel for %A" nodeId
            log.LogCritical(msg)
            failwith msg
    
    let _onChainObservable =
        eventAggregator.GetObservable<OnChainEvent>()
        
    let _peerEventObservable =
        eventAggregator.GetObservable<PeerEventWithContext>()
        
        
    let _ =
        _peerEventObservable
        |> Observable.flatmapAsync(this.PeerEventListener)
        |> Observable.subscribeWithError
               (id)
               ((sprintf "Channel Manager got error while Observing PeerEvent: %A") >> log.LogCritical)

        
    let _ =
        _onChainObservable
        |> Observable.flatmapAsync(this.OnChainEventListener)
        |> Observable.subscribeWithError
            (id)
            ((sprintf "Channel Manager got error while Observing OnChain Event: %A") >> log.LogCritical)
    /// Values are
    /// 1. actual transaction
    /// 2. other peer's id
    /// 3. absolute height of this tx is included in a block (None if it is not confirmed)
    member val FundingTxs = ConcurrentDictionary<TxId, NodeId * (TxIndexInBlock * BlockHeight) option>()
    
    member val CurrentBlockHeight = BlockHeight.Zero with get, set
    member val RemoteInits: ConcurrentDictionary<NodeId, Init> = ConcurrentDictionary<_,_>()
    
    member val Actors: ConcurrentDictionary<NodeId, IChannelActor> = ConcurrentDictionary<_,_>()
    
    member this.OnChainEventListener e =
        let t = unitTask {
            match e with
            | OnChainEvent.BlockConnected (_, height, txs) ->
                this.CurrentBlockHeight <- height
                for kv in this.FundingTxs do
                    let mutable found = false
                    for txInBlock in txs do
                        let txId = txInBlock |> snd |> fun tx  -> tx.GetTxId()
                        if (txId = kv.Key) then
                            log.LogInformation(sprintf "we found funding tx (%A) on the blockchain" txId)
                            found <- true
                            let txIndex = txInBlock |> fst |> TxIndexInBlock
                            let (nodeId, _) = kv.Value
                            let depth = 1us |> BlockHeightOffset
                            let newValue = (nodeId, Some(txIndex, height))
                            this.FundingTxs.TryUpdate(kv.Key, newValue, kv.Value) |> ignore
                            for kv in this.Actors do
                                let actor = kv.Value
                                do! actor.CommunicationChannel.Writer.WriteAsync(ChannelCommand.ApplyFundingConfirmedOnBC(height, txIndex, depth))
                    if (not found) then
                        match kv.Value with
                        | (_nodeId, None) ->
                            ()
                        |(nodeId, Some(txIndex, heightConfirmed)) ->
                            let depth = (height - heightConfirmed) + BlockHeightOffset.One
                            log.LogDebug(sprintf "funding tx (%A) confirmed (%d) times" kv.Key depth.Value)
                            for kv in this.Actors do
                                let actor = kv.Value
                                do! actor.CommunicationChannel.Writer.WriteAsync(ChannelCommand.ApplyFundingConfirmedOnBC(height, txIndex, depth))
                            
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
        
        
    member private this.ChannelEventHandler (e: ChannelEventWithContext) = unitVtask {
        match e.ChannelEvent with
        | ChannelEvent.WeAcceptedAcceptChannel (nextMsg, _) ->
            this.FundingTxs.TryAdd(nextMsg.FundingTxId, (e.NodeId, None)) |> ignore
        | WeResumedDelayedFundingLocked theirFundingSigned ->
            do! this.Actors.[e.NodeId].CommunicationChannel.Writer.WriteAsync(ChannelCommand.ApplyFundingLocked theirFundingSigned)
        | _ -> ()
    }
        
    member this.PeerEventListener e =
        let t = unitTask {
            match e.PeerEvent with
            | PeerEvent.ReceivedChannelMsg (msg) ->
                match msg with
                | :? OpenChannel as m ->
                    let channelKeys = keysRepository.GetChannelKeys(true)
                    let initFundee = { InputInitFundee.LocalParams =
                                           let channelPubKeys = channelKeys.ToChannelPubKeys()
                                           let spk = _nodeParams.ShutdownScriptPubKey |> Option.defaultValue (keysRepository.GetShutdownPubKey().WitHash.ScriptPubKey)
                                           _nodeParams.MakeLocalParams(_ourNodeId, channelPubKeys, spk, false, m.FundingSatoshis)
                                       TemporaryChannelId = m.TemporaryChannelId
                                       RemoteInit = this.RemoteInits.[e.NodeId]
                                       ToLocal = m.PushMSat
                                       ChannelKeys = channelKeys }
                    do! this.Actors.[e.NodeId].CommunicationChannel.Writer.WriteAsync(ChannelCommand.CreateInbound(initFundee))
                    return! this.Actors.[e.NodeId].CommunicationChannel.Writer.WriteAsync(ChannelCommand.ApplyOpenChannel m)
                | :? AcceptChannel as m ->
                    return! this.Actors.[e.NodeId].CommunicationChannel.Writer.WriteAsync( ChannelCommand.ApplyAcceptChannel m)
                | :? FundingCreated as m ->
                    this.FundingTxs.TryAdd(m.FundingTxId, (e.NodeId, None)) |> ignore
                    return! this.Actors.[e.NodeId].CommunicationChannel.Writer.WriteAsync(ChannelCommand.ApplyFundingCreated m)
                | :? FundingSigned as m ->
                    return! this.Actors.[e.NodeId].CommunicationChannel.Writer.WriteAsync(ChannelCommand.ApplyFundingSigned m)
                | :? FundingLocked as m ->
                    return! this.Actors.[e.NodeId].CommunicationChannel.Writer.WriteAsync(ChannelCommand.ApplyFundingLocked m)
                | :? Shutdown as m ->
                    return! this.Actors.[e.NodeId].CommunicationChannel.Writer.WriteAsync(ChannelCommand.RemoteShutdown m)
                | :? ClosingSigned as m ->
                    return! this.Actors.[e.NodeId].CommunicationChannel.Writer.WriteAsync(ChannelCommand.ApplyClosingSigned m)
                | :? UpdateAddHTLC as m ->
                    return! this.Actors.[e.NodeId].CommunicationChannel.Writer.WriteAsync(ChannelCommand.ApplyUpdateAddHTLC (m, this.CurrentBlockHeight))
                | :? UpdateFulfillHTLC as m ->
                    return! this.Actors.[e.NodeId].CommunicationChannel.Writer.WriteAsync(ChannelCommand.ApplyUpdateFulfillHTLC m)
                | :? UpdateFailHTLC as m ->
                    return! this.Actors.[e.NodeId].CommunicationChannel.Writer.WriteAsync(ChannelCommand.ApplyUpdateFailHTLC m)
                | :? UpdateFailMalformedHTLC as m ->
                    return! this.Actors.[e.NodeId].CommunicationChannel.Writer.WriteAsync(ChannelCommand.ApplyUpdateFailMalformedHTLC m)
                | :? CommitmentSigned as m ->
                    return! this.Actors.[e.NodeId].CommunicationChannel.Writer.WriteAsync(ChannelCommand.ApplyCommitmentSigned m)
                | :? RevokeAndACK as m ->
                    return! this.Actors.[e.NodeId].CommunicationChannel.Writer.WriteAsync(ChannelCommand.ApplyRevokeAndACK m)
                | :? UpdateFee as m ->
                    return! this.Actors.[e.NodeId].CommunicationChannel.Writer.WriteAsync(ChannelCommand.ApplyUpdateFee m)
                | m ->
                        return failwithf "Unknown Channel Message (%A). This should never happen" m
            | PeerEvent.ReceivedInit(_init) ->
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
                return! a.CommunicationChannel.Writer.WriteAsync(cmd)
            | { ChannelCommand = ChannelCommand.CreateOutbound(_) as cmd; NodeId = nodeId } ->
                let a = createChannel nodeId
                return! a.CommunicationChannel.Writer.WriteAsync(cmd)
            | { ChannelCommand = cmd; NodeId = nodeId } ->
                return! this.Actors.[nodeId].CommunicationChannel.Writer.WriteAsync(cmd)
        }
    
    interface IChannelManager with
        member this.AcceptCommandAsync(cmd) = this.AcceptCommandAsync(cmd)
        member val KeysRepository = keysRepository with get
