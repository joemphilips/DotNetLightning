namespace DotNetLightning.Utils

open NBitcoin

[<StructuralComparison;StructuralEquality>]
type TxId = | TxId of uint256 with
    member this.Value = let (TxId v) = this in v
    static member Zero = uint256.Zero |> TxId

