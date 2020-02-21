namespace DotNetLightning.Infrastructure.Services

open DotNetLightning.Infrastructure
open DotNetLightning.Infrastructure.Interfaces
open NBitcoin.RPC


type RPCClientProvider(chainParams: ChainConfig) =
    do failwith ""
    interface IRPCClientProvider with
        member this.GetAll() =
            failwith ""
        member this.TryGet(network) =
            failwith ""
