namespace DotNetLightning.Infrastructure

open System.Collections.Concurrent
open System.Net
open System.Threading.Tasks
open System.Threading

open Microsoft.Extensions.Options
open Microsoft.Extensions.Logging
open NBitcoin

open FSharp.Control.Tasks

open CustomEventAggregator
open System
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Chain
open DotNetLightning.LN
open DotNetLightning.Transactions
open FSharp.Control.Reactive

type IChannelManager =
    abstract AcceptCommandAsync: nodeId:NodeId * cmd:ChannelCommand -> Task
    abstract KeysRepository : IKeysRepository with get
type ChannelManager(nodeParams: IOptions<NodeParams>,
                    log: ILogger<ChannelManager>,
                    eventAggregator: IEventAggregator,
                    channelEventRepo: IChannelEventRepository,
                    chainListener: IChainListener,
                    keysRepository: IKeysRepository,
                    feeEstimator: IFeeEstimator,
                    fundingTxProvider: IFundingTxProvider) as this =
    let _nodeParams = nodeParams.Value
    let _internalLog = Logger.fromMicrosoftLogger log
    let _logger = log
    let _chainListener = chainListener
    
    let _eventAggregator = eventAggregator
    
    let _keysRepository = keysRepository
    let _feeEstimator = feeEstimator
    let _fundingTxProvider = fundingTxProvider
    let _createChannel =
        let config = _nodeParams.GetChannelConfig()
        Channel.CreateCurried
            config
            _internalLog
            _chainListener
            _keysRepository
            _feeEstimator
            (_keysRepository.GetNodeSecret())
            (_fundingTxProvider.ProvideFundingTx)
            _nodeParams.ChainNetwork
    let _peerEventObservable =
        _eventAggregator.GetObservable<PeerEvent>()
        
    let _ =
        _peerEventObservable
        |> Observable.flatmapAsync(this.PeerEventListener)
        |> Observable.subscribeWithError
               (id)
               ((sprintf "Channel Manager got error while Observing PeerEvent: %A") >> _logger.LogCritical)

    let _onChainObservable =
        _eventAggregator.GetObservable<OnChainEvent>()
        
    let _ =
        _onChainObservable
        |> Observable.flatmapAsync(this.OnChainEventListener)
        |> Observable.subscribeWithError
            (id)
            ((sprintf "Channel Manager got error while Observing OnChain Event: %A") >> _logger.LogCritical)
    let _ourNodeId = _keysRepository.GetNodeSecret().PubKey |> NodeId
    member val KeysRepository = _keysRepository
    member val KnownChannels: ConcurrentDictionary<NodeId, Channel> = ConcurrentDictionary<_, _>() with get
    member val EventAggregator: IEventAggregator = _eventAggregator with get
    
    member val ChannelEventRepo = channelEventRepo with get
    member val RemoteInits = ConcurrentDictionary<_,_>() with get
    
    member val CurrentBlockHeight = BlockHeight.Zero with get, set
    
    /// Values are
    /// 1. actual transaction
    /// 2. other peer's id
    /// 3. absolute height of this tx is included in a block (None if it is not confirmed)
    member val FundingTxs = ConcurrentDictionary<TxId, NodeId * (TxIndexInBlock * BlockHeight) option>()
    
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
                            _logger.LogInformation(sprintf "we found funding tx (%A) on the blockchain" txId)
                            found <- true
                            let txIndex = txInBlock |> fst |> TxIndexInBlock
                            let (nodeId, _) = kv.Value
                            let depth = 1us |> BlockHeightOffset
                            let newValue = (nodeId, Some(txIndex, height))
                            this.FundingTxs.TryUpdate(kv.Key, newValue, kv.Value) |> ignore
                            do!
                                this.AcceptCommandAsync(nodeId, ChannelCommand.ApplyFundingConfirmedOnBC(height, txIndex, depth))
                    if (not found) then
                        match kv.Value with
                        | (_nodeId, None) ->
                            ()
                        |(nodeId, Some(txIndex, heightConfirmed)) ->
                            let depth = (height - heightConfirmed) + BlockHeightOffset.One
                            _logger.LogDebug(sprintf "funding tx (%A) confirmed (%d) times" kv.Key depth.Value)
                            do! this.AcceptCommandAsync(nodeId, ChannelCommand.ApplyFundingConfirmedOnBC(height, txIndex, depth))
                            
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
                                _logger.LogError(sprintf "funding tx (%A) has disappeared from chain (probably by reorg)" txId)
                                match kv.Value with
                                | (_nodeId, None) -> ()
                                | (nodeId, Some(_txIndex, _heightConfirmed)) ->
                                    let newValue = (nodeId, None)
                                    this.FundingTxs.TryUpdate(kv.Key, newValue, kv.Value) |> ignore
            return ()
        }
        t |> Async.AwaitTask
    member this.PeerEventListener e =
        let t = unitTask {
            match e with
            | PeerEvent.ReceivedChannelMsg (nodeId, msg) ->
                match msg with
                | :? OpenChannel as m ->
                    let remoteInit = this.RemoteInits.TryGet(nodeId)
                    let channelKeys = _keysRepository.GetChannelKeys(true)
                    let initFundee = { InputInitFundee.LocalParams =
                                           let channelPubKeys = channelKeys.ToChannelPubKeys()
                                           let spk = _nodeParams.ShutdownScriptPubKey |> Option.defaultValue (_keysRepository.GetShutdownPubKey().WitHash.ScriptPubKey)
                                           _nodeParams.MakeLocalParams(_ourNodeId, channelPubKeys, spk, false, m.FundingSatoshis)
                                       TemporaryChannelId = m.TemporaryChannelId
                                       RemoteInit = remoteInit
                                       ToLocal = m.PushMSat
                                       ChannelKeys = channelKeys }
                    do! this.AcceptCommandAsync(nodeId, ChannelCommand.CreateInbound(initFundee))
                    return! this.AcceptCommandAsync(nodeId, ChannelCommand.ApplyOpenChannel m)
                | :? AcceptChannel as m ->
                    return! this.AcceptCommandAsync(nodeId, ChannelCommand.ApplyAcceptChannel m)
                | :? FundingCreated as m ->
                    this.FundingTxs.TryAdd(m.FundingTxId, (nodeId, None)) |> ignore
                    return! this.AcceptCommandAsync(nodeId, ChannelCommand.ApplyFundingCreated m)
                | :? FundingSigned as m ->
                    return! this.AcceptCommandAsync(nodeId, ChannelCommand.ApplyFundingSigned m)
                | :? FundingLocked as m ->
                    return! this.AcceptCommandAsync(nodeId, ChannelCommand.ApplyFundingLocked m)
                | :? Shutdown as m ->
                    return! this.AcceptCommandAsync(nodeId, ChannelCommand.RemoteShutdown m)
                | :? ClosingSigned as m ->
                    return! this.AcceptCommandAsync(nodeId, ChannelCommand.ApplyClosingSigned m)
                | :? UpdateAddHTLC as m ->
                    return! this.AcceptCommandAsync(nodeId, ChannelCommand.ApplyUpdateAddHTLC (m, this.CurrentBlockHeight))
                | :? UpdateFulfillHTLC as m ->
                    return! this.AcceptCommandAsync(nodeId, ChannelCommand.ApplyUpdateFulfillHTLC m)
                | :? UpdateFailHTLC as m ->
                    return! this.AcceptCommandAsync(nodeId, ChannelCommand.ApplyUpdateFailHTLC m)
                | :? UpdateFailMalformedHTLC as m ->
                    return! this.AcceptCommandAsync(nodeId, ChannelCommand.ApplyUpdateFailMalformedHTLC m)
                | :? CommitmentSigned as m ->
                    return! this.AcceptCommandAsync(nodeId, ChannelCommand.ApplyCommitmentSigned m)
                | :? RevokeAndACK as m ->
                    return! this.AcceptCommandAsync(nodeId, ChannelCommand.ApplyRevokeAndACK m)
                | :? UpdateFee as m ->
                    return! this.AcceptCommandAsync(nodeId, ChannelCommand.ApplyUpdateFee m)
                | m ->
                        return failwithf "Unknown Channel Message (%A). This should never happen" m
            | PeerEvent.ReceivedInit(nodeId, init) ->
                this.RemoteInits.AddOrReplace(nodeId, init)
                return ()
            | PeerEvent.FailedToBroadcastTransaction(_nodeId, _tx) ->
                return ()
            | _ -> return ()
        }
        t |> Async.AwaitTask
        
    member private this.ChannelEventHandler (e: ChannelEventWithContext) = unitVtask {
        match e.ChannelEvent with
        | ChannelEvent.WeAcceptedAcceptChannel (nextMsg, _) ->
            this.FundingTxs.TryAdd(nextMsg.FundingTxId, (e.NodeId, None)) |> ignore
        | WeResumedDelayedFundingLocked theirFundingSigned ->
            do! this.AcceptCommandAsync(e.NodeId, ChannelCommand.ApplyFundingLocked theirFundingSigned)
        | _ -> ()
    }
        
    member private this.HandleChannelError (nodeId: NodeId) (b: RBad) = unitTask {
            match b with
            | RBad.Exception(ChannelException(ChannelError.Close(msg))) ->
                let closeCMD =
                    let spk = _nodeParams.ShutdownScriptPubKey |> Option.defaultValue (_keysRepository.GetShutdownPubKey().WitHash.ScriptPubKey)
                    ChannelCommand.Close({ CMDClose.ScriptPubKey = Some spk })
                _logger.LogError(sprintf "Closing a channel for a node (%A) due to a following error. \n %s" nodeId msg)
                _logger.LogError(sprintf "%s" msg)
                return! this.AcceptCommandAsync(nodeId, closeCMD)
            | RBad.Exception(ChannelException(ChannelError.Ignore(msg))) ->
                _logger.LogWarning("Observed a following error in a channel. But ignoring")
                _logger.LogWarning(msg)
            | RBad.Object(o) ->
                match o with
                | :? APIError as apiError ->
                    match apiError with
                    | APIMisuseError msg ->
                        _logger.LogWarning(sprintf "Channel returned api misuse error. ignoring. %s" msg)
                    | e ->
                        _logger.LogCritical(sprintf "Unreachable %A" e)
                | e -> sprintf "unreachable %A" e |> _logger.LogCritical
            | o -> sprintf "Observed a following error in a channel %A" o |> _logger.LogError
        }
    member this.AcceptCommandAsync(nodeId: NodeId, cmd: ChannelCommand): Task = unitTask {
        match this.KnownChannels.TryGetValue(nodeId) with
        | true, channel ->
            match Channel.executeCommand channel cmd with
            | Good events ->
                _logger.LogTrace(sprintf "Applying events %A" (events |> List.map (fun e -> e.GetType())))
                let nextChannel = events |> List.fold Channel.applyEvent channel
                let contextEvents =
                    events |> List.map(fun e -> { ChannelEventWithContext.ChannelEvent = e; NodeId = nodeId })
                do! this.ChannelEventRepo.SetEventsAsync(contextEvents)
                match this.KnownChannels.TryUpdate(nodeId, nextChannel, channel) with
                | true ->
                    _logger.LogTrace(sprintf "Updated channel with %A" (nextChannel.State.GetType()))
                | false ->
                    _logger.LogTrace(sprintf "Did not update channel %A" nextChannel.State)
                for e in contextEvents do
                    do! this.ChannelEventHandler e
                contextEvents
                   |> List.iter this.EventAggregator.Publish<ChannelEventWithContext>
            | Bad ex ->
                let ex = ex.Flatten()
                ex |> Array.map (this.HandleChannelError nodeId) |> ignore
                ()
        | false, _ ->
            match cmd with
            | CreateInbound fundeeParameters ->
                let c = _createChannel nodeId
                this.KnownChannels.TryAdd(nodeId, c) |> ignore
                return! this.AcceptCommandAsync(nodeId, CreateInbound fundeeParameters)
            | CreateOutbound funderParameters ->
                let c = _createChannel nodeId
                this.KnownChannels.TryAdd(nodeId, c) |> ignore
                return! this.AcceptCommandAsync(nodeId, CreateOutbound funderParameters)
            | _ ->
                sprintf "Cannot handle command type (%A) for unknown peer %A" (cmd) nodeId
                |> log.LogError
                ()
        }
    interface IChannelManager with
        member this.AcceptCommandAsync(nodeId, cmd:ChannelCommand): Task =
            this.AcceptCommandAsync(nodeId, cmd)
        member val KeysRepository = _keysRepository with get
