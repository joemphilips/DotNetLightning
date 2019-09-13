namespace DotNetLightning.Chain
open System
open NBitcoin
open DotNetLightning.Utils
open Microsoft.Extensions.Logging


type ChainError =
    | NotSupported
    | NotWatched
    | UnknownTx of Transaction

type BlockChainInstanceId = BlockChainInstanceId of string

/// We want transaction index for channel id and such.
/// So not using NBitcoin.Block directly
type BlockContent = BlockHeader * BlockHeight * (uint32 * Transaction) list
type OnChainEvent =
    | BlockConnected of chainId: BlockChainInstanceId * content: BlockContent
    | BlockDisconnected of chainId: BlockChainInstanceId * BlockHeader

type IChainListener =
    abstract member ObservableOnChainEvent : IObservable<OnChainEvent>


type IChainWatcher =
    abstract member InstallWatchTx: txid: TxId * scriptPubKey: Script -> bool
    abstract member InstallWatchOutPoint: OutPoint * Script -> bool
    abstract member WatchAllTxn: unit -> bool
    abstract member RegisterListener: IChainListener -> bool
    abstract member CurrentTip: BlockHeight * BlockHeader

type IBroadCaster =
    abstract member BroadCastTransaction: (Transaction) -> Async<TxId>

type ConfirmationTarget =
    | Background
    | Normal
    | HighPriority

type IFeeEstimator =
    abstract member GetEstSatPer1000Weight: (ConfirmationTarget) -> FeeRatePerKw

/// Utility for tracking registered txn/outpoints and checking for matches
type ChainWatchedUtil = {
    WatchAll: bool
    WatchedTxn: Map<TxId, Script>
    WatchedOutpoints: Set<OutPoint>
}

module ChainWatchedUtil =
    let Create() =
        {
            WatchAll = false
            WatchedTxn = Map.empty
            WatchedOutpoints = Set.empty
        }

    let registerTx (txid) (scriptPubKey: Script) (util: ChainWatchedUtil) = 
        { util with WatchedTxn = util.WatchedTxn |> Map.add txid scriptPubKey }
    
    let registerOutpoint (outpoint: OutPoint) (util) =
        if util.WatchAll then util
        else
            { util with WatchedOutpoints = util.WatchedOutpoints |> Set.add outpoint }

    let private doesMatchTxOut(tx: Transaction) (util: ChainWatchedUtil) =
        util.WatchedTxn
        |> Map.toArray
        |> Array.map snd
        |> Array.where(fun v -> tx.Outputs.ToArray()
                                |> Array.map(fun o -> o.ScriptPubKey)
                                |> Array.exists ((=)v))
        |> Array.isEmpty |> not

    let private doesMatchTxIn (tx: Transaction) (util: ChainWatchedUtil) =
        util.WatchedOutpoints
        |> Set.toArray
        |> Array.where(fun v -> tx.Inputs.ToArray()
                                |> Array.exists(fun txin -> txin.PrevOut.Equals(v)))
        |> Array.isEmpty |> not

    let doesMatchTx(tx: Transaction) (util: ChainWatchedUtil) = 
        if util.WatchAll then true
        else
            doesMatchTxOut tx util || doesMatchTxIn tx util
    let watchAll util =
        { util with WatchAll = true}
