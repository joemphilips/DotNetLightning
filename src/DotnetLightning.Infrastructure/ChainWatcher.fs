namespace DotNetLightning.Infrastructure

open DotNetLightning.Chain
open NBitcoin

type SupportedChainWatcherType =
    | Dummy
    | Bitcoind of NBitcoin.RPC.RPCClient
    // | NBXplorer
    
type ChainWatcher() =
    interface IChainWatcher with
        member this.InstallWatchTx(txHash: uint256, scriptPubKey: Script): bool =
            failwith ""
            
        member this.InstallWatchOutPoint (outPoint: OutPoint, scriptPubKey: Script): bool =
            failwith ""
        member this.WatchAllTxn() =
            failwith ""
        member this.RegisterListener(listener: IChainListener) =
            failwith ""
