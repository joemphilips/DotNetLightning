namespace DotNetLightning.Infrastructure

type SupportedChainWatcherType =
    | Dummy
    | Bitcoind of NBitcoin.RPC.RPCClient
    // | NBXplorer