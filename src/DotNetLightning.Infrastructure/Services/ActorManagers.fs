namespace DotNetLightning.Infrastructure.Services

open CustomEventAggregator
open DotNetLightning.Chain
open System
open System
open System.Collections.Generic
open DotNetLightning.Infrastructure
open DotNetLightning.Infrastructure.ActorManagers
open DotNetLightning.Infrastructure.Interfaces
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options

module private ActorManagerFactory =
    let getConf (chainConf: IOptionsSnapshot<ChainConfig>) (cryptoCode: string) =
        try
            chainConf.Get(cryptoCode) |> Some
        with
        | _ -> None
    let getChannelManager (sp: IServiceProvider) (chainConf) (network: DotNetLightningNetwork) =
        getConf (chainConf) (network.CryptoCode)
        |> Option.map(fun conf ->
            let logger = sp.GetRequiredService<ILogger<ChannelManager>>()
            let loggerProvider = sp.GetRequiredService<ILoggerFactory>()
            let ea = ReactiveEventAggregator()
            let keyRepo = sp.GetRequiredService<IKeysRepository>()
            let channelEventStream = sp.GetRequiredService<IChannelEventStream>()
            let feeEst = sp.GetRequiredService<IFeeEstimator>()
            let fundingTxProvider = sp.GetRequiredService<IFundingTxProvider>()
            ChannelManager(logger, loggerProvider, ea, keyRepo, channelEventStream, feeEst, fundingTxProvider, conf)
        )
        
    let getPeerManager (sp: IServiceProvider) (chainConf) (watchers: ChainWatchers) (network: DotNetLightningNetwork) =
        watchers.GetWatcher(network.CryptoCode)
        |> Option.bind(fun watcher ->
            getConf (chainConf) (network.CryptoCode)
            |> Option.map(fun (conf) ->
                let ea = ReactiveEventAggregator()
                let logger = sp.GetRequiredService<ILogger<PeerManager>>()
                let loggerProvider = sp.GetRequiredService<ILoggerFactory>()
                let keyRepo = sp.GetRequiredService<IKeysRepository>()
                let broadCaster = sp.GetRequiredService<IBroadCaster>()
                PeerManager(ea, logger, loggerProvider, keyRepo, conf, watcher, broadCaster, network)
            )
        )
        

type ActorManagers(sp: IServiceProvider,
                   watchers: ChainWatchers,
                   chainConf: IOptionsSnapshot<ChainConfig>) =
    
    let d = Dictionary<_,_>()
    let allNetworks = DotNetLightningNetworkProvider.getAll()
    do
        for network in allNetworks do
            match
                ActorManagerFactory.getChannelManager (sp) (chainConf) (network),
                ActorManagerFactory.getPeerManager(sp) (chainConf) watchers (network) with
            | Some chanMan, Some peerMan ->
                d.Add(network, (chanMan, peerMan))
            | _ -> ()
        ()
        
    member this.TryGetManagers(n: DotNetLightningNetwork) =
        match d.TryGetValue(n) with
        | true, s -> Some s
        | _ -> None
