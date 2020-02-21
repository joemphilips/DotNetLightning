namespace DotNetLightning.Infrastructure.Interfaces

open DotNetLightning.Infrastructure
open NBitcoin.RPC

type IRPCClientProvider =
    abstract member GetAll: unit -> RPCClient seq
    abstract member TryGet: DotNetLightningNetwork -> RPCClient option

