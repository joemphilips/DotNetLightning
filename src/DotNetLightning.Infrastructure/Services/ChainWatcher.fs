namespace DotNetLightning.Infrastructure.Services

open System
open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Utils.Primitives

/// Utility for tracking registered txn/outpoints and checking for matches
type ChainWatchedUtil = {
    WatchAll: bool
    WatchedTxn: Map<TxId, Script>
    WatchedOutpoints: Set<LNOutPoint>
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
            { util with WatchedOutpoints = util.WatchedOutpoints |> Set.add (LNOutPoint outpoint) }

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
