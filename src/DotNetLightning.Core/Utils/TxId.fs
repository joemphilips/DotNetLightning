namespace DotNetLightning.Utils

open NBitcoin

[<StructuralComparison;StructuralEquality>]
type TxId = | TxId of uint256 with
    member x.Value = let (TxId v) = x in v
    static member Zero = uint256.Zero |> TxId

