namespace DotNetLightning.Infrastructure.Interfaces

open System

open NBitcoin

open DotNetLightning.Utils
open DotNetLightning.Utils.Primitives
open DotNetLightning.Chain
open DotNetLightning.Infrastructure

open Microsoft.Extensions.Hosting

/// type to watch specific bitcoind node.
/// Or whatever information source which we can assume as a `real`
type IChainListener =
    abstract member Id: BlockChainInstanceId
    abstract member ObservableOnChainEvent : IObservable<RawOnChainEvent>

/// an interface responsible for
/// 1. hold more than one IChainListener
/// 2. Aggregate the information from IChainListeners
/// 3. Publish only those are important for us.
type IChainWatcher =
    inherit IHostedService
    abstract member InstallWatchTx: txid: TxId * scriptPubKey: Script -> bool
    abstract member InstallWatchOutPoint: OutPoint * Script -> bool
    abstract member WatchAllTxn: unit -> bool
    abstract member RegisterListener: IChainListener -> bool
    abstract member CurrentTip: BlockHeight * BlockHeader
    abstract member Network: DotNetLightningNetwork
    

