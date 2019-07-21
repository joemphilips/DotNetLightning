namespace DotNetLightning.Utils
open NBitcoin
open System
open System

[<AutoOpen>]
module Primitives =

    type NBitcoin.Utils with
        static member ToUInt16(b: byte[], lendian: bool): uint16 =
            if lendian then
                uint16(b.[0]) + (uint16(b.[1]) <<< 8)
            else
                uint16(b.[1]) + (uint16(b.[0]) <<< 8)

        static member ToBytes(d: uint16, lendian: bool): byte[] =
            let mutable output = Array.zeroCreate 2
            if lendian then
                output.[0] <- byte d
                output.[1] <- byte (d >>> 8)
            else
                output.[0] <- byte (d >>> 8)
                output.[1] <- byte d
            output

    /// Absolute block height
    [<Struct>]
    type BlockHeight = BlockHeight of uint32
        with
            static member Zero = 0u |> BlockHeight
            member x.Value = let (BlockHeight v) = x in v
            static member (+) (a: BlockHeight, b: BlockHeightOffset) =
                a.Value + (uint32 b.Value) |> BlockHeight

            static member (-) (a: BlockHeight, b: BlockHeightOffset) =
                a.Value - (uint32 b.Value) |> BlockHeight

    /// **Description**
    ///
    /// Relative block height used for `OP_CSV` locks,
    /// Since OP_CSV allow only blocknumber of 0 ~ 65535, it is safe
    /// to restrict into the range smaller than BlockHeight
    and [<Struct>] BlockHeightOffset = BlockHeightOffset of uint16 with
        member x.Value = let (BlockHeightOffset v) = x in v
        static member op_Implicit(v: uint16) =
            BlockHeightOffset v
        static member (+) (a: BlockHeightOffset, b: BlockHeightOffset) =
            a.Value + b.Value |> BlockHeightOffset
        static member (-) (a: BlockHeightOffset, b: BlockHeightOffset) =
            a.Value - b.Value |> BlockHeightOffset


    [<Struct>]
    type PaymentPreimage = PaymentPreimage of uint256 with
        member x.Value = let (PaymentPreimage v) = x in v

        member this.ToBytes() =
            this.Value.ToBytes()

        member this.GetHash() =
            this.Value.ToBytes() |> Crypto.Hashes.SHA256 |> uint256 |> PaymentHash

        member this.ToKey() =
            this.Value.ToBytes() |> Key
        member this.ToPubKey() =
            this.ToKey().PubKey

    and PaymentHash = PaymentHash of uint256 with
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

    type FeeRatePerKw = FeeRatePerKw of uint32 with
        member x.Value = let (FeeRatePerKw v) = x in v
        static member FromFee(fee: Money, weight: uint64) =
            (((uint64 fee.Satoshi) * weight) / 1000UL)
            |> uint32
            |> FeeRatePerKw

        member this.ToFee(weight) =
            Money.Satoshis((uint64 this.Value) * weight / 1000UL)
    /// Block Hash
    type BlockId = BlockId of uint256 with
        member x.Value = let (BlockId v) = x in v

    [<Struct>]
    type HTLCId = HTLCId of uint64 with
        static member Zero = HTLCId(0UL)
        member x.Value = let (HTLCId v) = x in v

        static member (+) (a: HTLCId, b: uint64) = (a.Value + b) |> HTLCId

    [<Struct>]
    type TxOutIndex = TxOutIndex of uint16 with
        member x.Value = let (TxOutIndex v) = x in v

    [<Struct>]
    type TxIndexInBlock = TxIndexInBlock of uint32 with
        member x.Value = let (TxIndexInBlock v) = x in v


    [<Measure>]
    type satoshi

    [<Measure>]
    type millisatoshi

    [<Struct>]
    type ShortChannelId = {
        BlockHeight: BlockHeight
        BlockIndex: TxIndexInBlock
        TxOutIndex: TxOutIndex
    }
    with
        static member FromUInt64(rawData: uint64): ShortChannelId =
            let b = NBitcoin.Utils.ToBytes(rawData, false)
            let bh = NBitcoin.Utils.ToUInt32(Array.concat[| b.[0..2]; [|0uy|] |], false)
            let bi = NBitcoin.Utils.ToUInt32(Array.concat[| b.[3..5]; [|0uy |] |], false)
            let txOutIndex = NBitcoin.Utils.ToUInt16(b.[6..7], false)
            
            {
                BlockHeight = bh |> BlockHeight
                BlockIndex = bi |> TxIndexInBlock
                TxOutIndex = txOutIndex |> TxOutIndex
            }
        member this.ToBytes(): byte[] =
            Array.concat[|
                        NBitcoin.Utils.ToBytes(this.BlockHeight.Value, false).[0..2]
                        NBitcoin.Utils.ToBytes(this.BlockIndex.Value, false).[0..2]
                        NBitcoin.Utils.ToBytes(this.TxOutIndex.Value, false)
                    |]
        override this.ToString() =
            sprintf "%dx%dx%d" this.BlockHeight.Value this.BlockIndex.Value this.TxOutIndex.Value

    type UserId = UserId of uint64
    type Delimiter = 
        Delimiter of byte[]

    type RGB = {
        Red: uint8
        Green: uint8
        Blue: uint8
    }