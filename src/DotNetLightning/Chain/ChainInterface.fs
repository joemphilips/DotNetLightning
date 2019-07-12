namespace DotNetLightning.Chain
open NBitcoin
open DotNetLightning.Utils
open Microsoft.Extensions.Logging


type ChainError =
    | NotSupported
    | NotWatched
    | UnknownTx of Transaction


type IChainListener =
    abstract member BlockConnected: (BlockHeader * BlockHeight * (uint32 * Transaction) list) -> unit
    abstract member BlockDisconnected: BlockHeader -> bool

type IChainWatcher =
    abstract member InstallWatchTx: (uint256 * Script) -> bool
    abstract member InstallWatchOutPoint: (OutPoint * Script) -> bool
    abstract member WatchAllTxn: unit -> bool
    abstract member RegisterListener: IChainListener -> bool

type IBroadCaster =
    abstract member BroadCastTransction: (Transaction) -> Result<unit, string>

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

    let regsiterTx (txid) (scriptPubKey: Script) (util: ChainWatchedUtil) = 
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
/// Utility to capture some parts of ChainWatchInterface implementors
/// Keepking a local copy of this in a ChainWatchInterface implementor is likely useful.
[<CLIMutable>]
type ChainWatchInterfaceUtil = {
    Network: Network
    mutable Watched: ChainWatchedUtil
    mutable Listeners: IChainListener array
    mutable reentered: bool
    Logger: ILogger
}
with
    interface IChainWatcher with
        member this.InstallWatchTx(arg1: uint256 * Script): bool = 
            let (txid, scriptPubKey) = arg1
            this.Watched <- this.Watched |> ChainWatchedUtil.regsiterTx (TxId txid) (scriptPubKey)
            true

        member this.InstallWatchOutPoint(arg1: OutPoint * Script): bool = 
            let (outP, scriptPubKey) = arg1
            this.Watched <- this.Watched |> ChainWatchedUtil.registerOutpoint outP
            true

        member this.RegisterListener(arg1: IChainListener): bool = 
            if this.Listeners |> Array.exists(fun cl -> cl.Equals(arg1)) then false
            else
                this.Listeners <- this.Listeners |> Array.append [|arg1|]
                true

        member this.WatchAllTxn(): bool = 
            this.Watched <- this.Watched |> ChainWatchedUtil.watchAll
            true

    member private this.BlockConnectedChecked(header: BlockHeader, height: BlockHeight, txMatched: (uint32 * Transaction) list): bool =
        let lastSeen = this.reentered
        this.Listeners |> Array.iter(fun l -> l.BlockConnected(header, height, txMatched))
        lastSeen = this.reentered

    member this.BlockConnectedWithFiltering(block: Block, height: BlockHeight) =
        let mutable reentered = true
        while reentered do
            let matched = block.Transactions
                          |> Seq.toList
                          |> List.indexed
                          |> List.where(fun (_, tx: Transaction) -> this.Watched |> ChainWatchedUtil.doesMatchTx tx )
                          |> List.map(fun (i, tx) -> (uint32)i, tx)
            reentered <- this.BlockConnectedChecked (block.Header, height, matched)
        ()


module ChainWatchInterfaceUtil =
    let Create (network, logger) =
        {
            Network = network
            Watched = ChainWatchedUtil.Create()
            Listeners = Array.empty
            Logger = logger
            reentered = false
        }