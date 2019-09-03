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

type IChannelManager =
    abstract AcceptCommandAsync: peerId:PeerId * cmd:ChannelCommand -> Task
    abstract ChannelEventListener: IObserver<ChannelEvent> with get
    abstract ChannelEventObservable: IObservable<ChannelEvent> with get
    abstract ChannelEventRepo : IChannelEventRepository with get
type IFundingTxProvider =
    abstract member ConstructFundingTx :  IDestination * Money * FeeRatePerKw -> RResult<FinalizedTx * TxOutIndex>
type ChannelManagementService(nodeParams: IOptions<NodeParams>,
                              log: ILogger<ChannelManagementService>,
                              eventAggregator: IEventAggregator,
                              channelEventRepo: IChannelEventRepository,
                              chainListener: IChainListener,
                              keysRepository: IKeysRepository,
                              feeEstimator: IFeeEstimator,
                              fundingTxProvider: IFundingTxProvider,
                              keysRepo: IKeysRepository) as this =
    let _nodeParams = nodeParams.Value
    let _internalLog = Logger.fromMicrosoftLogger log
    let _logger = log
    let _chainListener = chainListener
    
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
    let _peerEventObservable = eventAggregator.GetObservable<PeerEvent>()
    do
        _peerEventObservable.Add(this.PeerEventListener)

    member val KnownChannels: ConcurrentDictionary<NodeId, Channel> = ConcurrentDictionary<_, _>() with get
    member val EventAggregator: IEventAggregator = eventAggregator with get
    
    member val ChannelEventRepo = channelEventRepo with get
    member this.PeerEventListener e =
        match e with
        | PeerEvent.ReceivedChannelMsg (nodeId, msg) ->
            match msg with
            | :? OpenChannel as m ->
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
        | PeerEvent.FailedToBroadcastTransaction(nodeId, tx) ->
            ()
        | e -> ()
        
    member private this.HandleChannelError (nodeId: NodeId) (b: RBad) =
        match b with
        | RBad.Exception(ChannelException(ChannelError.Close(msg))) ->
            let closeCMD =
                let spk = keysRepo.GetShutdownPubKey()
                ChannelCommand.Close({ CMDClose.ScriptPubKey = Some spk.WitHash.ScriptPubKey })
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
                let nextChannel = events |> List.fold Channel.applyEvent channel
                events |> List.zip(List.replicate (events.Length) nodeId) |> List.iter this.EventAggregator.Publish<NodeId * ChannelEvent>
                _logger.LogDebug(sprintf "Updated channel with %A" nextChannel)
                match this.KnownChannels.TryUpdate(nodeId, nextChannel, channel) with
                | true ->
                    ()
                | false ->
                    failwith "Failed to update channel, this should never happen"
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
