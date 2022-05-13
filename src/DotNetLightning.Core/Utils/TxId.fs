namespace DotNetLightning.Utils

open NBitcoin

/// Simple wrapper type for transaction id.
/// Its only purpose is to annotate the inner hash is a txid and nothing else
[<StructuralComparison; StructuralEquality>]
type TxId =
    | TxId of uint256

    member this.Value = let (TxId v) = this in v
    static member Zero = uint256.Zero |> TxId
