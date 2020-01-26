namespace DotNetLightning.Transactions

open DotNetLightning.Utils.Primitives
open NBitcoin

type TransactionError =
    | UnknownHTLC of HTLCId
    | FailedToFinalizeScript of errorMsg: string
    | InvalidSignature of TransactionSignature
    | AmountBelowDustLimit of amount: Money
    /// We tried to close the channel, but there are remaining HTLC we must do something
    /// before closing it
    | HTLCNotClean of remainingHTLCs: HTLCId list
