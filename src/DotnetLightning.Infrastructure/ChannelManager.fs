namespace DotNetLightning.Infrastructure

open System.Collections.Concurrent
open System.Net
open System.Threading.Tasks
open System.Threading

open Microsoft.Extensions.Options
open Microsoft.Extensions.Logging
open NBitcoin

open FSharp.Control.Tasks

open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Chain
open DotNetLightning.LN

type ChannelMsgHandler = IChannelMsg -> Task

module internal ChannelHelper =
    ()
    
type IChannelManager =
    abstract AcceptCommandAsync: peerId:PeerId * cmd:ChannelCommand -> Task
    abstract AcceptMessageAsync: peerId:PeerId * msg:IChannelMsg -> Task

type ChannelManagementService(nodeParams: IOptions<NodeParams>,
                              log: ILogger<ChannelManagementService>,
                              channelEventRepo: IChannelEventRepository,
                              keysRepo: IKeysRepository) =
    let np = nodeParams.Value
    let _internalLog = Logger.fromMicrosoftLogger log

    member val KnownChannels: ConcurrentDictionary<EndPoint, Channel> = ConcurrentDictionary<_, _>()
    member val ChannelEventRepo = channelEventRepo

    member private this.HandleChannelErrorAsync (log: ILogger<ChannelManagementService>) (endPoint: EndPoint) (b: RBad) =
        task {
            match b with
            | RBad.Exception(ChannelException(ChannelError.Close(msg))) ->
                let closeCMD =
                    let spk = keysRepo.GetShutdownPubKey()
                    ChannelCommand.Close({ CMDClose.ScriptPubKey = Some spk.WitHash.ScriptPubKey })
                log.LogError(sprintf "Closing a channel for a peer (%A) due to a following error. \n %s" endPoint msg)
                log.LogError(sprintf "%s" msg)
                do! this.AcceptCommandAsync(endPoint, closeCMD)
            | RBad.Exception(ChannelException(ChannelError.Ignore(msg))) ->
                log.LogWarning("Observed a following error in a channel. But ignoring")
                log.LogWarning(msg)
            | RBad.Object(o) ->
                match o with
                | :? APIError as apiError ->
                    match apiError with
                    | APIMisuseError msg ->
                        log.LogWarning(sprintf "Channel returned api misuse error. ignoring. %s" msg)
                    | e ->
                        log.LogCritical(sprintf "Unreachable %A" e)
                | e -> sprintf "unreachable %A" e |> log.LogCritical
            | o -> sprintf "Observed a following error in a channel %A" o |> log.LogError
        } :> Task

    member this.AcceptCommandAsync(endPoint: EndPoint, cmd: ChannelCommand): Task =
        task {
            match this.KnownChannels.TryGetValue(endPoint) with
            | true, channel ->
                match Channel.executeCommand channel cmd with
                | Good events ->
                    do! (this.ChannelEventRepo.SetEventsAsync(channel.InternalChannelId, events))
                    let nextChannel = events |> List.fold Channel.applyEvent channel
                    log.LogDebug(sprintf "Updated channel with %A" nextChannel)
                    match this.KnownChannels.TryUpdate(endPoint, nextChannel, channel) with
                    | true ->
                        return ()
                    | false ->
                        failwith "Failed to update channel, this should never happen"
                | Bad ex ->
                    let ex = ex.Flatten()
                    do! ex |> Array.map (this.HandleChannelErrorAsync log endPoint) |> Task.WhenAll
                    return ()
            | false, _ ->
                sprintf "unknown peer %A" endPoint
                |> log.LogError
                return ()
        } :> Task

    member this.AcceptMessageAsync(peerId: PeerId, msg: IChannelMsg) =
        failwith ""
