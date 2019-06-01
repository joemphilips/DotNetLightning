namespace DotNetLightning.Utils
open NBitcoin

[<AutoOpen>]
module Primitives =

    /// Absolute block height
    type Blockheight = BlockHeight of uint32

    /// Relative block height used for `OP_CSV` locks,
    /// Since OP_CSV allow only blocknumber of 0 ~ 65535, it is safe
    /// to restrict into the range smaller than BlockHeight
    type BlockHeightOffset = BlockHeightOffset of uint16 with
        member x.Value = let (BlockHeightOffset v) = x in v

    type PaymentPreimage = PaymentPreimage of uint256 with
        member x.Value = let (PaymentPreimage v) = x in v

    type PaymentHash = PaymentHash of uint256 with
        member x.Value = let (PaymentHash v) = x in v
        member x.ToBytes() = x.Value.ToBytes()

    type ChannelId = ChannelId of uint256 with
        member x.Value = let (ChannelId v) = x in v

    type NodeId = NodeId of PubKey with
        member x.Value = let (NodeId v) = x in v

    [<StructuralComparison;StructuralEquality>]
    type TxId = TxId of uint256 with
        member x.Value = let (TxId v) = x in v

    type FeeRatePerKw = FeeRatePerKw of Money with
        member x.Value = let (FeeRatePerKw v) = x in v
    /// Block Hash
    type BlockId = BlockId of uint256 with
        member x.Value = let (BlockId v) = x in v
    type HTLCId = HTLCId of uint64 with
        static member Zero = HTLCId(0UL)
        member x.Value = let (HTLCId v) = x in v

    [<Struct>]
    type ShortChannelId = {
        BlockHeight: uint32
        BlockIndex: uint32
        TxOutIndex: uint16
    }
    type UserId = UserId of uint64
    type Delimiter = 
        Delimiter of byte[]

    type RGB = {
        Red: uint8
        Green: uint8
        Blue: uint8
    }