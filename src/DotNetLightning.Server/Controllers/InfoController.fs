namespace DotNetLightning.Server.Controllers
open DotNetLightning.Chain
open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Options
open DotNetLightning.Infrastructure
open DotNetLightning.Infrastructure.ActorManagers
open DotNetLightning.Infrastructure.DTOs
open DotNetLightning.Infrastructure.Services

[<Route("/api/v1/[controller]")>]
[<ApiController>]
type InfoController(chainConfigs: IOptionsSnapshot<ChainConfig>,
                    peerMan: IPeerManager,
                    chanMan: IChannelManager,
                    watchers: ChainWatchers) =
    inherit ControllerBase()
    [<HttpGet>]
    [<Route("{cryptoCode}")>]
    member this.Get(cryptoCode: string): GetInfoResponse =
        let network = ControllerUtils.getNetwork watchers cryptoCode
        let chainConfig = chainConfigs.Get(cryptoCode)
        let watcher = ControllerUtils.getWatcher watchers cryptoCode
        {
            NumPendingChannels = chanMan.GetPendingChannels() |> Seq.length
            NumActiveChannels = chanMan.GetActiveChannels() |> Seq.length
            NumInactiveChannels = chanMan.GetInactiveChannels() |> Seq.length
            BlockHeight = watcher.CurrentTip |> fun (h, _) -> h.Value
            NumPeers = peerMan.GetNumPeers()
            Alias = chainConfig.Alias
            Network = network.ToString()
            SyncedToGraph = failwith "todo"
            NodeId = failwith "todo"
            ChainSynchronizationProgress = failwith "todo"
            Features = failwith "todo"
            Version = failwith "todo"
            Uris = failwith "todo"
            BlockHash = failwith "todo"
            Color = failwith "todo"
            Chains = failwith "todo"
            BestHeaderTimestamp = failwith "todo"
        }

