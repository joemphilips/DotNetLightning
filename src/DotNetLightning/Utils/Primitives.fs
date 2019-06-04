namespace DotNetLightning.Utils
open NBitcoin
open System
open System

[<AutoOpen>]
module Primitives =

    /// Absolute block height
    type Blockheight = BlockHeight of uint32

    /// **Description**
    ///
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

    [<CustomEquality;CustomComparison>]
    type NodeId = NodeId of PubKey with
        member x.Value = let (NodeId v) = x in v
        interface IComparable with
            override this.CompareTo(other) = if isNull other then -1 else this.Value.CompareTo((other :?> NodeId).Value)
        override this.Equals(other) =
            if isNull other then false else this.Value.Equals((other :?> NodeId).Value)
        override this.GetHashCode() =
            this.Value.GetHashCode()

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
    with
        static member FromUInt64(rawData: uint64): ShortChannelId =
            let b = BitConverter.GetBytes(rawData)
            // TODO: do not use Array.concat
            let blockheight: uint32 = BitConverter.ToUInt32(Array.concat[| b.[0..3]; [|0uy|] |], 0)
            let blockIndex = BitConverter.ToUInt32(Array.concat[| b.[3..6]; [| 0uy |] |], 0)
            let txOutIndex = BitConverter.ToUInt16(b.[6..7], 0)
            {
                BlockHeight = blockheight
                BlockIndex = blockIndex
                TxOutIndex = txOutIndex
            }

            
    type UserId = UserId of uint64
    type Delimiter = 
        Delimiter of byte[]

    type RGB = {
        Red: uint8
        Green: uint8
        Blue: uint8
    }