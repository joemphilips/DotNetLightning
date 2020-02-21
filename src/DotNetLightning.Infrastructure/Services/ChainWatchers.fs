namespace DotNetLightning.Infrastructure.Services

open CustomEventAggregator
open FSharp.Control.Tasks

open DotNetLightning.Infrastructure
open DotNetLightning.Infrastructure.Interfaces
open DotNetLightning.Infrastructure.Services

open System.Threading.Tasks
open Microsoft.Extensions.Hosting

type ChainWatchers(repositoryProvider: IRepositoryProvider,
                   rpcClientProvider: IRPCClientProvider,
                   config: ChainConfig,
                   eventAggregator: IEventAggregator) =
    let watchers =
        DotNetLightningNetworkProvider.getAll()
        |> Seq.map(fun network ->
            repositoryProvider.TryGetRepository(network),
            rpcClientProvider.TryGet(network),
            network)
        |> Seq.choose(function (Some repo, Some rpc, n) -> Some(repo, rpc, n) | _ -> None)
        |> Seq.map(fun (repo, rpc, n) ->
                     (n.CryptoCode), match config.ChainWatcherType with
                                     | BitcoindPolling ->
                                        new BitcoinRPCPollingChainWatcher(rpc, eventAggregator, n, repo) :> IChainWatcher)
        |> dict
        
    member this.GetWatcher(cryptoCode: string): IChainWatcher option =
        match watchers.TryGetValue cryptoCode with
        | true, w -> Some w
        | _ -> None
        
    interface IHostedService with
        member this.StartAsync(ct) =
            watchers |> Seq.map(fun w -> w.Value.StartAsync(ct)) |> Task.WhenAll
            
        member this.StopAsync(ct) =
            watchers |> Seq.map(fun w -> w.Value.StopAsync(ct)) |> Task.WhenAll
