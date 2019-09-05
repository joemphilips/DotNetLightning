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
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Chain
open DotNetLightning.LN
open DotNetLightning.Transactions
open FSharp.Control.Reactive

type IChannelManager =
    abstract AcceptCommand: nodeId:NodeId * cmd:ChannelCommand -> unit
    abstract KeysRepository : IKeysRepository with get
type IFundingTxProvider =
    abstract member ConstructFundingTx :  IDestination * Money * FeeRatePerKw -> RResult<FinalizedTx * TxOutIndex>
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
            (_fundingTxProvider.ConstructFundingTx)
            _nodeParams.ChainNetwork
    let _peerEventObservable =
        _eventAggregator.GetObservable<PeerEvent>()
        
    let _ = _peerEventObservable
            |> Observable.subscribe(this.PeerEventListener)

    let _ourNodeId = _keysRepository.GetNodeSecret().PubKey |> NodeId
    member val KeysRepository = _keysRepository
    member val KnownChannels: ConcurrentDictionary<NodeId, Channel> = ConcurrentDictionary<_, _>() with get
    member val EventAggregator: IEventAggregator = _eventAggregator with get
    
    member val ChannelEventRepo = channelEventRepo with get
    member val RemoteInits = ConcurrentDictionary<_,_>() with get
    member this.PeerEventListener e =
        match e with
        | PeerEvent.ReceivedChannelMsg (nodeId, msg) ->
            _logger.LogDebug(sprintf "Received Channel msg %A" msg)
            match msg with
            | :? OpenChannel as m ->
                let remoteInit = this.RemoteInits.TryGet(nodeId)
                let initFundee = { InputInitFundee.LocalParams =
                                       let channelPubKeys = _keysRepository.GetChannelKeys(true).ToChannelPubKeys()
                                       let spk = _nodeParams.ShutdownScriptPubKey |> Option.defaultValue (_keysRepository.GetShutdownPubKey().WitHash.ScriptPubKey)
                                       _nodeParams.MakeLocalParams(_ourNodeId, channelPubKeys, spk, false, m.FundingSatoshis)
                                   TemporaryChannelId = m.TemporaryChannelId
                                   RemoteInit = remoteInit
                                   ToLocal = m.PushMSat
                                   ChannelKeys = _keysRepository.GetChannelKeys(true) }
                this.AcceptCommand(nodeId, ChannelCommand.CreateInbound(initFundee))
                _logger.LogDebug("finished creating inbound so next we are going to apply open_channel")
                this.AcceptCommand(nodeId, ChannelCommand.ApplyOpenChannel m)
            | :? AcceptChannel as m ->
                this.AcceptCommand(nodeId, ChannelCommand.ApplyAcceptChannel m)
            | :? FundingCreated as m ->
                this.AcceptCommand(nodeId, ChannelCommand.ApplyFundingCreated m)
            | :? FundingSigned as m ->
                this.AcceptCommand(nodeId, ChannelCommand.ApplyFundingSigned m)
            | :? FundingLocked as m ->
                this.AcceptCommand(nodeId, ChannelCommand.ApplyFundingLocked m)
            | :? Shutdown as m ->
                this.AcceptCommand(nodeId, ChannelCommand.RemoteShutdown m)
            | :? ClosingSigned as m ->
                this.AcceptCommand(nodeId, ChannelCommand.ApplyClosingSigned m)
            | :? UpdateAddHTLC as m ->
                this.AcceptCommand(nodeId, ChannelCommand.ApplyUpdateAddHTLC m)
            | :? UpdateFulfillHTLC as m ->
                this.AcceptCommand(nodeId, ChannelCommand.ApplyUpdateFulfillHTLC m)
            | :? UpdateFailHTLC as m ->
                this.AcceptCommand(nodeId, ChannelCommand.ApplyUpdateFailHTLC m)
            | :? UpdateFailMalformedHTLC as m ->
                this.AcceptCommand(nodeId, ChannelCommand.ApplyUpdateFailMalformedHTLC m)
            | :? CommitmentSigned as m ->
                this.AcceptCommand(nodeId, ChannelCommand.ApplyCommitmentSigned m)
            | :? RevokeAndACK as m ->
                this.AcceptCommand(nodeId, ChannelCommand.ApplyRevokeAndACK m)
            | :? UpdateFee as m ->
                this.AcceptCommand(nodeId, ChannelCommand.ApplyUpdateFee m)
            | m ->
                    failwithf "Unknown Channel Message (%A). This should never happen" m
        | PeerEvent.ReceivedInit(nodeId, init) ->
            this.RemoteInits.AddOrReplace(nodeId, init)
        | PeerEvent.FailedToBroadcastTransaction(nodeId, tx) ->
            ()
        | e -> ()
        
    member private this.HandleChannelError (nodeId: NodeId) (b: RBad) =
        match b with
        | RBad.Exception(ChannelException(ChannelError.Close(msg))) ->
            let closeCMD =
                let spk = _nodeParams.ShutdownScriptPubKey |> Option.defaultValue (_keysRepository.GetShutdownPubKey().WitHash.ScriptPubKey)
                ChannelCommand.Close({ CMDClose.ScriptPubKey = Some spk })
            _logger.LogError(sprintf "Closing a channel for a node (%A) due to a following error. \n %s" nodeId msg)
            _logger.LogError(sprintf "%s" msg)
            this.AcceptCommand(nodeId, closeCMD)
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
    member this.AcceptCommand(nodeId: NodeId, cmd: ChannelCommand) =
        match this.KnownChannels.TryGetValue(nodeId) with
        | true, channel ->
            match Channel.executeCommand channel cmd with
            | Good events ->
                _logger.LogDebug(sprintf "Applying events %A" events)
                let nextChannel = events |> List.fold Channel.applyEvent channel
                _logger.LogDebug(sprintf "Going to update channel with %A" nextChannel.State)
                let contextEvents = events |> List.map(fun e -> { ChannelEventWithContext.ChannelEvent = e; NodeId = nodeId })
                contextEvents
                   |> List.map(fun e -> printfn "publishing %A from channelman" e; e)
                   |> List.map this.EventAggregator.Publish<ChannelEventWithContext>
                   |> ignore
                _logger.LogDebug("Lets see we can update channel")
                match this.KnownChannels.TryUpdate(nodeId, nextChannel, channel) with
                | true ->
                    _logger.LogDebug(sprintf "Update channel %A" nextChannel.State)
                    ()
                | false ->
                    _logger.LogDebug(sprintf "Dit not update channel %A" nextChannel.State)
            | Bad ex ->
                let ex = ex.Flatten()
                ex |> Array.map (this.HandleChannelError nodeId) |> ignore
                ()
        | false, _ ->
            match cmd with
            | CreateInbound fundeeParameters ->
                let c = _createChannel nodeId
                this.KnownChannels.TryAdd(nodeId, c) |> ignore
                this.AcceptCommand(nodeId, CreateInbound fundeeParameters)
            | CreateOutbound funderParameters ->
                let c = _createChannel nodeId
                this.KnownChannels.TryAdd(nodeId, c) |> ignore
                this.AcceptCommand(nodeId, CreateOutbound funderParameters)
            | _ ->
                sprintf "Cannot handle command type (%A) for unknown peer %A" (cmd) nodeId
                |> log.LogError
                ()
    interface IChannelManager with
        member this.AcceptCommand(nodeId, cmd:ChannelCommand): unit =
            this.AcceptCommand(nodeId, cmd)
        member val KeysRepository = _keysRepository with get
