namespace DotNetLightning.Utils
open NBitcoin

module TransactionUtils =
    let sortTxOut (txOut: List<TxOut>)