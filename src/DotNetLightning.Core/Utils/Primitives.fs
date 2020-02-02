namespace DotNetLightning.Utils

open NBitcoin
open NBitcoin.Crypto

open System
open System.Net
open System.Linq

[<AutoOpen>]
module Primitives =

    type NBitcoin.Utils with
        static member ToUInt16(b: byte [], lendian: bool): uint16 =
            if lendian then
                uint16 (b.[0]) + (uint16 (b.[1]) <<< 8)
            else
                uint16 (b.[1]) + (uint16 (b.[0]) <<< 8)

        static member ToBytes(d: uint16, lendian: bool): byte [] =
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
    type BlockHeight = | BlockHeight of uint32 with
        static member Zero = 0u |> BlockHeight
        member x.Value = let (BlockHeight v) = x in v
        member x.AsOffset() =
            x.Value |> Checked.uint16 |> BlockHeightOffset
        static member (+) (a: BlockHeight, b: BlockHeightOffset) =
                a.Value + (uint32 b.Value ) |> BlockHeight

        static member (-) (a: BlockHeight, b: BlockHeightOffset) =
            a.Value - (uint32 b.Value) |> BlockHeight
            
        static member (-) (a: BlockHeight, b: BlockHeight) =
            a.Value - (b.Value) |> uint16 |> BlockHeightOffset

    /// **Description**
    ///
    /// Relative block height used for `OP_CSV` locks,
    /// Since OP_CSV allow only block number of 0 ~ 65535, it is safe
    /// to restrict into the range smaller than BlockHeight
    and  [<Struct>] BlockHeightOffset = | BlockHeightOffset of uint16 with
        member x.Value = let (BlockHeightOffset v) = x in v
        static member op_Implicit (v: uint16) =
            BlockHeightOffset v
        static member One = BlockHeightOffset(1us)
        static member (+) (a: BlockHeightOffset, b: BlockHeightOffset) =
            a.Value + b.Value |> BlockHeightOffset
        static member (-) (a: BlockHeightOffset, b: BlockHeightOffset) =
            a.Value - b.Value |> BlockHeightOffset

    /// Wrapper around NBitcoin's ECDSASignature type for convenience. It has following difference
    /// 1. It is equatable
    /// 2. Some Convenience methods for serialization
    /// 3. Custom `ToString`
    [<CustomEquality;NoComparison;StructuredFormatDisplay("{AsString}")>]
    type LNECDSASignature = | LNECDSASignature of ECDSASignature with
        member x.Value = let (LNECDSASignature v) = x in v
        override this.GetHashCode() = hash this.Value
        override this.Equals(obj: obj) =
            match obj with
            | :? LNECDSASignature as o -> (this :> IEquatable<LNECDSASignature>).Equals(o)
            | _ -> false
        interface IEquatable<LNECDSASignature> with
            member this.Equals(o: LNECDSASignature) =
                Utils.ArrayEqual(o.ToBytesCompact(), this.ToBytesCompact())
        /// Originally this method is in NBitcoin.Utils.
        /// But we ported here since it was internal method.
        member private this.BigIntegerToBytes(b: BouncyCastle.Math.BigInteger, numBytes: int) =
            if isNull b then null else
            let a = Array.zeroCreate numBytes
            let a2 = b.ToByteArray()
            let sourceIndex = if (a2.Length = numBytes + 1) then 1 else 0;
            let num = System.Math.Min(a2.Length, numBytes)
            array.Copy(a2, sourceIndex, a, numBytes - num, num);
            a

        override this.ToString() =
            sprintf "LNECDSASignature (%A)" (this.ToBytesCompact())
        member this.AsString = this.ToString()

        /// ** Description **
        ///
        /// Bitcoin Layer 1 forces (by consensus) DER encoding for the signatures.
        /// This is not optimal, but remaining as a rule since changing consensus is not easy.
        /// However in layer2, there are no such rules. So we use more optimal serialization by
        /// This function.
        /// Note it does not include the recovery id. so its always 64 bytes
        ///
        /// **Output**
        ///
        /// (serialized R value + S value) in byte array.
        member this.ToBytesCompact() =
            let r = Array.append (this.BigIntegerToBytes(b = this.Value.R, numBytes = 32)) (this.BigIntegerToBytes(this.Value.S, 32))
            if (isNull <| box r.[0]) then r.[0] <- 255uy
            r
            
        member this.ToDER() =
            this.Value.ToDER()
            
        /// Read 64 bytes as r(32 bytes) and s(32 bytes) value
        /// If `withRecId` is `true`, skip first 1 byte
        static member FromBytesCompact(bytes: byte [], ?withRecId: bool) =
            let withRecId = defaultArg withRecId false
            if withRecId && bytes.Length <> 65 then
                invalidArg "bytes" "ECDSASignature specified to have recovery id, but it was not 65 bytes length"
            else if (not withRecId) && bytes.Length <> 64 then
                invalidArg "bytes" "ECDSASignature was not specified to have recovery id, but it was not 64 bytes length."
            else
                let data = if withRecId then bytes.[1..] else bytes
                let r = NBitcoin.BouncyCastle.Math.BigInteger(1, data.[0..31])
                let s = NBitcoin.BouncyCastle.Math.BigInteger(1, data.[32..63])
                ECDSASignature(r, s) |> LNECDSASignature

        static member op_Implicit (ec: ECDSASignature) =
            ec |> LNECDSASignature

    type PaymentHash = | PaymentHash of uint256 with
        member x.Value = let (PaymentHash v) = x in v
        member x.ToBytes(?lEndian) =
            let e = defaultArg lEndian true
            x.Value.ToBytes(e)

        member x.GetRIPEMD160() =
            let b = x.Value.ToBytes()
            Crypto.Hashes.RIPEMD160(b, b.Length)

    type PaymentPreimage =
        private PaymentPreimage of seq<byte>
            with
                // as per BOLT-2:
                static member LENGTH = 32

                static member Create(data: seq<byte>) =
                    if data.Count() <> PaymentPreimage.LENGTH then
                        raise <| ArgumentException(sprintf "Payment preimage length should be %i" PaymentPreimage.LENGTH)
                    PaymentPreimage data

                member this.Value =
                    let (PaymentPreimage v) = this in v

                member this.ToBytes() =
                    this.Value

                member this.ToByteArray() =
                    this.Value |> Array.ofSeq

                member this.Hash =
                    this.ToByteArray() |> Crypto.Hashes.SHA256 |> uint256 |> PaymentHash

                member this.ToPrivKey() =
                    this.ToByteArray() |> Key

                member this.ToPubKey() =
                    this.ToPrivKey().PubKey

    let (|PaymentPreimage|) x =
        match x with
        | PaymentPreimage x -> x
        
    type ChannelId = | ChannelId of uint256 with
        member x.Value = let (ChannelId v) = x in v

        static member Zero = uint256.Zero |> ChannelId

    type ConnectionId = ConnectionId of Guid
    type PeerId = PeerId of EndPoint

    [<CustomEquality;CustomComparison>]
    type NodeId = | NodeId of PubKey with
        member x.Value = let (NodeId v) = x in v
        interface IComparable with
            override this.CompareTo(other) = if isNull other then -1 else this.Value.CompareTo((other :?> NodeId).Value)
        override this.Equals(other) =
            match other with
            | :? NodeId as n -> this.Value.Equals(n.Value)
            | _              -> false
        override this.GetHashCode() =
            this.Value.GetHashCode()

    [<StructuralComparison;StructuralEquality>]
    type TxId = | TxId of uint256 with
        member x.Value = let (TxId v) = x in v
        
    /// Small wrapper for NBitcoin's OutPoint type
    /// So that it supports comparison and equality constraints
    [<CustomComparison;CustomEquality>]
    type LNOutPoint = LNOutPoint of OutPoint with
        member x.Value = let (LNOutPoint v) = x in v
        
        member this.CompareTo(other: LNOutPoint) =
            if this.Value.Hash > other.Value.Hash then
                1
            else if this.Value.Hash < other.Value.Hash then
                -1
            else
                if this.Value.N > other.Value.N then
                    1
                else if this.Value.N < other.Value.N then
                    -1
                else
                    0
            
        member this.Equals(other: LNOutPoint) =
            (this.Value.Hash = other.Value.Hash) &&
            (this.Value.N = other.Value.N)
            
        override this.Equals(other: obj) =
            if isNull other then false else
            if not <| other :? LNOutPoint then false else
            this.Equals((other :?> LNOutPoint))
            
        override this.GetHashCode() = hash (this.Value.ToBytes())
            
        interface IComparable with
            member this.CompareTo(other) =
                if isNull other then 1 else
                if not <| other :? LNOutPoint then 1 else
                this.CompareTo(other :?> LNOutPoint)
            
        interface IEquatable<LNOutPoint> with
            member this.Equals(other) = this.Equals(other)

    type FeeRatePerKw = | FeeRatePerKw of uint32 with
        member x.Value = let (FeeRatePerKw v) = x in v
        static member FromFee(fee: Money, weight: uint64) =
            (((uint64 fee.Satoshi) * weight) / 1000UL)
            |> uint32
            |> FeeRatePerKw

        member this.ToFee(weight) =
            Money.Satoshis((uint64 this.Value) * weight / 1000UL)
            
        member this.AsNBitcoinFeeRate() =
            this.Value |> uint64 |> Money.Satoshis |> FeeRate

        static member Max(a: FeeRatePerKw, b: FeeRatePerKw) =
            if (a.Value >= b.Value) then a else b
        static member (+) (a: FeeRatePerKw, b: uint32) =
            (a.Value + b) |> FeeRatePerKw
        static member (*) (a: FeeRatePerKw, b: uint32) =
            (a.Value * b) |> FeeRatePerKw
    /// Block Hash
    type BlockId = | BlockId of uint256 with
        member x.Value = let (BlockId v) = x in v

    [<Struct>]
    type HTLCId = | HTLCId of uint64 with
        static member Zero = HTLCId(0UL)
        member x.Value = let (HTLCId v) = x in v

        static member (+) (a: HTLCId, b: uint64) = (a.Value + b) |> HTLCId

    [<Struct>]
    type TxOutIndex = | TxOutIndex of uint16 with
        member x.Value = let (TxOutIndex v) = x in v

    [<Struct>]
    type TxIndexInBlock = | TxIndexInBlock of uint32 with
        member x.Value = let (TxIndexInBlock v) = x in v


    [<Struct>]
    type ShortChannelId = {
        BlockHeight: BlockHeight
        BlockIndex: TxIndexInBlock
        TxOutIndex: TxOutIndex
    }
        with

        static member FromUInt64(rawData: uint64): ShortChannelId =
            let b = NBitcoin.Utils.ToBytes(rawData, false)
            let bh = NBitcoin.Utils.ToUInt32 (Array.concat [| b.[0..2]; [| 0uy |] |], false)
            let bi = NBitcoin.Utils.ToUInt32 (Array.concat [| b.[3..5]; [| 0uy |] |], false)
            let txOutIndex = NBitcoin.Utils.ToUInt16(b.[6..7], false)

            {
                BlockHeight = bh |> BlockHeight
                BlockIndex = bi |> TxIndexInBlock
                TxOutIndex = txOutIndex |> TxOutIndex
            }
        member this.ToBytes(): byte [] =
            Array.concat [|
                        NBitcoin.Utils.ToBytes(this.BlockHeight.Value, false).[0..2]
                        NBitcoin.Utils.ToBytes(this.BlockIndex.Value, false).[0..2]
                        NBitcoin.Utils.ToBytes(this.TxOutIndex.Value, false)
                    |]
        override this.ToString() =
            sprintf "%dx%dx%d" this.BlockHeight.Value this.BlockIndex.Value this.TxOutIndex.Value

    type UserId = UserId of uint64
    type Delimiter =
        Delimiter of byte []

    type RGB = {
        Red: uint8
        Green: uint8
        Blue: uint8
    }
