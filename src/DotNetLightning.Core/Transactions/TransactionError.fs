namespace DotNetLightning.Transactions

open DotNetLightning.Utils.Primitives
open NBitcoin

/// <namespacedoc>
///     This is a module to work with LN-specific transactions.
/// </namespacedoc>
type TransactionError =
    | UnknownHTLC of HTLCId
    | FailedToFinalizeScript of errorMsg: string
    | InvalidSignature of TransactionSignature
    | AmountBelowDustLimit of amount: Money
    /// We tried to close the channel, but there are remaining HTLC we must do something
    /// before closing it
    | HTLCNotClean of remainingHTLCs: list<HTLCId>

    member this.Message =
        match this with
        | UnknownHTLC htlcId -> sprintf "Unknown htlc id (%i)" htlcId.Value
        | FailedToFinalizeScript errorMsg ->
            sprintf "Failed to finalize script: %s" errorMsg
        | InvalidSignature _ -> "Invalid signature"
        | AmountBelowDustLimit amount ->
            sprintf "Amount (%s) is below dust limit" (amount.ToString())
        | HTLCNotClean remainingHTLCs ->
            sprintf
                "Attempted to close channel with %i remaining htlcs"
                remainingHTLCs.Length
