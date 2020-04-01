namespace DotNetLightning.Infrastructure.Services

open DotNetLightning.Infrastructure
open DotNetLightning.Infrastructure.Interfaces

open NBitcoin.RPC

type RPCClientProvider(chainParams: ChainConfig) =
    do failwith "Not implemented: RPCClientProvider::ctor"
    interface IRPCClientProvider with
        member this.GetAll() =
            failwith "Not implemented: RPCClientProvider::GetAll"
        member this.TryGet(network) =
            failwith "Not implemented: RPCClientProvider::TryGet"
