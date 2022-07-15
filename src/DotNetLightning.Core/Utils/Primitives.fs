namespace DotNetLightning.Utils

open NBitcoin
open NBitcoin.Crypto

open System
open System.Net
open System.Linq

open System.Diagnostics
open DotNetLightning.Core.Utils.Extensions

open ResultUtils
open ResultUtils.Portability

/// <namespacedoc>
///   <summary>
///     "DotNetLightning.Utils" contains
///     1. a lightning-related primitive types.
///     2. very basic helper methods.
///   </summary>
/// </namespacedoc>
[<AutoOpen>]
module Primitives =

    type NBitcoin.Utils with

        static member ToUInt16(b: array<byte>, lendian: bool) : uint16 =
            if lendian then
                uint16(b.[0]) + (uint16(b.[1]) <<< 8)
            else
                uint16(b.[1]) + (uint16(b.[0]) <<< 8)

        static member ToBytes(d: uint16, lendian: bool) : array<byte> =
            let mutable output = Array.zeroCreate 2

            if lendian then
                output.[0] <- byte d
                output.[1] <- byte(d >>> 8)
            else
                output.[0] <- byte(d >>> 8)
                output.[1] <- byte d

            output

    /// **Description**
    ///
    /// 16bit relative block height used for `OP_CSV` locks,
    /// Since OP_CSV allow only block number of 0 ~ 65535, it is safe
    /// to restrict into the range smaller than BlockHeight
#if !NoDUsAsStructs
    [<Struct>]
#endif
    type BlockHeightOffset16 =
        | BlockHeightOffset16 of uint16

        member this.Value = let (BlockHeightOffset16 v) = this in v

        static member op_Implicit(v: uint16) =
            BlockHeightOffset16 v

        static member One = BlockHeightOffset16(1us)
        static member Zero = BlockHeightOffset16(0us)
        static member MaxValue = UInt16.MaxValue |> BlockHeightOffset16

        static member (+)(a: BlockHeightOffset16, b: BlockHeightOffset16) =
            a.Value + b.Value |> BlockHeightOffset16

        static member (-)(a: BlockHeightOffset16, b: BlockHeightOffset16) =
            a.Value - b.Value |> BlockHeightOffset16

    /// **Description**
    ///
    /// 32bit relative block height. For `OP_CSV` locks, BlockHeightOffset16
    /// should be used instead.
#if !NoDUsAsStructs
    [<Struct>]
#endif
    type BlockHeightOffset32 =
        | BlockHeightOffset32 of uint32

        member this.Value = let (BlockHeightOffset32 v) = this in v

        static member toBlockHeightOffset16(bho32: BlockHeightOffset32) =
            BlockHeightOffset16(uint16 bho32.Value)

        static member ofBlockHeightOffset16(bho16: BlockHeightOffset16) =
            BlockHeightOffset32(uint32 bho16.Value)

        static member op_Implicit(v: uint32) =
            BlockHeightOffset32 v

        static member One = BlockHeightOffset32(1u)
        static member Zero = BlockHeightOffset32(0u)
        static member MaxValue = UInt32.MaxValue |> BlockHeightOffset32

        static member (+)(a: BlockHeightOffset32, b: BlockHeightOffset32) =
            a.Value + b.Value |> BlockHeightOffset32

        static member (-)(a: BlockHeightOffset32, b: BlockHeightOffset32) =
            a.Value - b.Value |> BlockHeightOffset32

    /// Absolute block height
#if !NoDUsAsStructs
    [<Struct>]
#endif
    type BlockHeight =
        | BlockHeight of uint32

        static member Zero = 0u |> BlockHeight
        static member One = 1u |> BlockHeight
        member this.Value = let (BlockHeight v) = this in v

        member this.AsOffset() =
            this.Value |> Checked.uint16 |> BlockHeightOffset16

        static member (+)(a: BlockHeight, b: BlockHeightOffset16) =
            a.Value + (uint32 b.Value) |> BlockHeight

        static member (+)(a: BlockHeight, b: BlockHeightOffset32) =
            a.Value + b.Value |> BlockHeight

        static member (-)(a: BlockHeight, b: BlockHeightOffset16) =
            a.Value - (uint32 b.Value) |> BlockHeight

        static member (-)(a: BlockHeight, b: BlockHeightOffset32) =
            a.Value - b.Value |> BlockHeight

        static member (-)(a: BlockHeight, b: BlockHeight) =
            a.Value - (b.Value) |> BlockHeightOffset32

    /// Wrapper around NBitcoin's ECDSASignature type for convenience. It has following difference
    /// 1. It is equatable
    /// 2. Some Convenience methods for serialization
    /// 3. Custom `ToString`
    [<CustomEquality; CustomComparison; StructuredFormatDisplay("{AsString}")>]
    type LNECDSASignature =
        | LNECDSASignature of ECDSASignature
        | Empty

        member this.Value =
            match this with
            | LNECDSASignature s -> s
            | Empty -> failwith "Unreachable!"

        override this.GetHashCode() =
            hash this.Value

        override this.Equals(obj: obj) =
            match obj with
            | :? LNECDSASignature as o ->
                (this :> IEquatable<LNECDSASignature>).Equals(o)
            | _ -> false

        interface IEquatable<LNECDSASignature> with
            member this.Equals(o: LNECDSASignature) =
                Utils.ArrayEqual(o.ToBytesCompact(), this.ToBytesCompact())


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
            this.Value.ToCompact()

        /// Logic does not really matter here. This is just for making life easier by enabling automatic implementation
        /// of `StructuralComparison` for wrapper types.
        member this.CompareTo(e: LNECDSASignature) =
            let a = this.ToBytesCompact() |> fun x -> Utils.ToUInt64(x, true)
            let b = e.ToBytesCompact() |> fun x -> Utils.ToUInt64(x, true)
            a.CompareTo(b)

        interface IComparable with
            member this.CompareTo(o: obj) =
                match o with
                | :? LNECDSASignature as e -> this.CompareTo(e)
                | _ -> -1

        member this.ToDER() =
            this.Value.ToDER()

        /// Read 64 bytes as r(32 bytes) and s(32 bytes) value
        /// If `withRecId` is `true`, skip first 1 byte
        static member FromBytesCompact(bytes: array<byte>, ?withRecId: bool) =
            let withRecId = defaultArg withRecId false

            if withRecId && bytes.Length <> 65 then
                invalidArg
                    "bytes"
                    "ECDSASignature specified to have recovery id, but it was not 65 bytes length"
            else if (not withRecId) && bytes.Length <> 64 then
                invalidArg
                    "bytes"
                    "ECDSASignature was not specified to have recovery id, but it was not 64 bytes length."
            else
                let data =
                    if withRecId then
                        bytes.[1..]
                    else
                        bytes

                match ECDSASignature.TryParseFromCompact data with
                | true, x -> LNECDSASignature x
                | _ ->
                    failwithf "failed to parse compact ecdsa signature %A" data

        static member op_Implicit(ec: ECDSASignature) =
            ec |> LNECDSASignature

    /// Simple wrapper type for NBitcoin.uint256.
    type PaymentHash =
        | PaymentHash of uint256

        member this.Value = let (PaymentHash v) = this in v

        member this.ToBytes(?lEndian) =
            let e = defaultArg lEndian false
            this.Value.ToBytes e

        member this.GetRIPEMD160() =
            let b = this.Value.ToBytes() |> Array.rev
            Crypto.Hashes.RIPEMD160(b, b.Length)

    /// The preimage for HTLC which the LN payment recipient must reveal when
    /// receiving the payment. Thus it also works as proof of payment (receipt)
    type PaymentPreimage =
        private
        | PaymentPreimage of seq<byte>
        // as per BOLT-2:
        static member LENGTH = 32

        static member Create(data: seq<byte>) =
            if data.Count() <> PaymentPreimage.LENGTH then
                raise
                <| ArgumentException(
                    sprintf
                        "Payment preimage length should be %i"
                        PaymentPreimage.LENGTH
                )

            PaymentPreimage data

        member this.Value = let (PaymentPreimage v) = this in v

        member this.ToHex() =
            let h = NBitcoin.DataEncoders.HexEncoder()
            let ba: array<byte> = this.ToByteArray()
            ba |> h.EncodeData

        member this.ToBytes() =
            this.Value

        member this.ToByteArray() =
            this.Value |> Array.ofSeq

        member this.Hash =
            this.ToByteArray()
            |> Crypto.Hashes.SHA256
            |> fun x -> uint256(x, false) |> PaymentHash

        member this.ToPrivKey() =
            this.ToByteArray() |> fun ba -> new Key(ba)

        member this.ToPubKey() =
            this.ToPrivKey().PubKey

    let (|PaymentPreimage|) x =
        match x with
        | PaymentPreimage x -> x

    type ConnectionId = ConnectionId of Guid

    [<CustomEquality; CustomComparison>]
    type PeerId =
        | PeerId of EndPoint

        member this.Value = let (PeerId ep) = this in ep

        override this.GetHashCode() =
            this.Value.GetHashCode()

        member this.Equals(o: PeerId) =
            this
                .Value
                .ToEndpointString()
                .Equals(o.Value.ToEndpointString())

        override this.Equals(o: obj) =
            match o with
            | :? PeerId as p -> this.Equals(p)
            | _ -> false

        interface IEquatable<PeerId> with
            member this.Equals o =
                this.Equals(o)

        member this.CompareTo(o: PeerId) =
            this
                .Value
                .ToEndpointString()
                .CompareTo(o.Value.ToEndpointString())

        interface IComparable with
            member this.CompareTo(o: obj) =
                match o with
                | :? PeerId as p -> this.CompareTo(p)
                | _ -> -1

    /// wrapper for `NBitcoin.PubKey` which supports IComparable.
    /// Used for only those which is not `NodeId`
    /// <seealso cref="NodeId">
    [<CustomEquality; CustomComparison>]
    type ComparablePubKey =
        | ComparablePubKey of PubKey

        member this.Value = let (ComparablePubKey v) = this in v

        interface IComparable with
            override this.CompareTo other =
                match other with
                | :? ComparablePubKey as n -> this.Value.CompareTo(n.Value)
                | _ -> -1

        override this.GetHashCode() =
            this.Value.GetHashCode()

        override this.Equals other =
            match other with
            | :? ComparablePubKey as n -> this.Value.Equals(n.Value)
            | _ -> false

        static member op_Implicit(pk: PubKey) =
            pk |> ComparablePubKey

    /// PubKey for node master private key.
    /// Which is unique for each lightning network nodes.
    /// Used extensively in the protocol. e.g. to
    /// * distinguish each nodes.
    /// * ECDH key-exchange to encrypt all p2p messages.
    [<CustomEquality; CustomComparison>]
    type NodeId =
        | NodeId of PubKey

        member this.Value = let (NodeId v) = this in v

        interface IComparable with
            override this.CompareTo other =
                match other with
                | :? NodeId as n -> this.Value.CompareTo(n.Value)
                | _ -> -1

        override this.Equals other =
            match other with
            | :? NodeId as n -> this.Value.Equals(n.Value)
            | _ -> false

        override this.GetHashCode() =
            this.Value.GetHashCode()

    /// Small wrapper for NBitcoin's OutPoint type
    /// So that it supports comparison and equality constraints
    [<CustomComparison; CustomEquality>]
    type LNOutPoint =
        | LNOutPoint of OutPoint

        member this.Value = let (LNOutPoint v) = this in v

        member this.CompareTo(other: LNOutPoint) =
            if this.Value.Hash > other.Value.Hash then
                1
            else if this.Value.Hash < other.Value.Hash then
                -1
            else if this.Value.N > other.Value.N then
                1
            else if this.Value.N < other.Value.N then
                -1
            else
                0

        member this.Equals(other: LNOutPoint) =
            (this.Value.Hash = other.Value.Hash)
            && (this.Value.N = other.Value.N)

        override this.Equals(other: obj) =
            if isNull other then
                false
            else if not <| other :? LNOutPoint then
                false
            else
                this.Equals((other :?> LNOutPoint))

        override this.GetHashCode() =
            hash(this.Value.ToBytes())

        interface IComparable with
            member this.CompareTo other =
                if isNull other then
                    1
                else if not <| other :? LNOutPoint then
                    1
                else
                    this.CompareTo(other :?> LNOutPoint)

        interface IEquatable<LNOutPoint> with
            member this.Equals other =
                this.Equals(other)

    /// feerate per kilo weight
    type FeeRatePerKw =
        | FeeRatePerKw of uint32

        member this.Value = let (FeeRatePerKw v) = this in v

        static member FromFee(fee: Money, weight: uint64) =
            (((uint64 fee.Satoshi) * 1000UL) / weight) |> uint32 |> FeeRatePerKw

        static member FromFeeAndVSize(fee: Money, vsize: uint64) =
            FeeRatePerKw.FromFee(fee, vsize * 4UL)

        member this.CalculateFeeFromWeight weight =
            Money.Satoshis(uint64 this.Value * weight / 1000UL)

        member this.CalculateFeeFromVirtualSize vSize =
            this.CalculateFeeFromWeight(vSize * 4UL)

        member this.CalculateFeeFromVirtualSize(tx: Transaction) =
            for i in tx.Inputs do
                if isNull i.WitScript || i.WitScript = WitScript.Empty then
                    invalidArg "tx" "Should never hold non-segwit input."

            this.CalculateFeeFromVirtualSize(uint64(tx.GetVirtualSize()))

        member this.AsNBitcoinFeeRate() =
            this.Value |> uint64 |> (*) 4UL |> Money.Satoshis |> FeeRate

        /// <summary>
        ///     Suppose remote = 3.0 and local = 1.0. This formula will calculate the mismatch "ratio" to be:
        ///         abs (2.0 * (remote - local) / (remote + local)) == abs (2.0 * (3.0 - 1.0) / (3.0 + 1.0)) == abs (2.0 * 2.0 / 4.0) == 1.0
        ///     If remote instead equals 0.33 we get the same mismatch ratio:
        ///         abs (2.0 * (remote - local) / (remote + local)) == abs (2.0 * (0.33 - 1.0) / (0.33 + 1.0)) == abs (2.0 * 0.66 / 1.33) == 1.0
        ///     This example demonstrates how the formula is both symmetrical and invariant in
        ///     the scale of the parameters. ie. swapping remote and local, or scaling them
        ///     both by a constant factor, will produce the same mismatch ratio.
        ///     See this wikipedia entry for more info on ways of calculating relative differences:
        ///         https://en.wikipedia.org/wiki/Relative_change_and_difference
        /// </summary>
        member this.MismatchRatio(other: FeeRatePerKw) =
            let local = double this.Value
            let remote = double other.Value
            abs(2.0 * (remote - local) / (remote + local))

        static member Max(a: FeeRatePerKw, b: FeeRatePerKw) =
            if (a.Value >= b.Value) then
                a
            else
                b

        static member (+)(a: FeeRatePerKw, b: uint32) =
            (a.Value + b) |> FeeRatePerKw

        static member (*)(a: FeeRatePerKw, b: uint32) =
            (a.Value * b) |> FeeRatePerKw

    /// a.k.a. Block Header Hash
    type BlockId =
        | BlockId of uint256

        member this.Value = let (BlockId v) = this in v

    /// serial id for each HTLC in the channel.
#if !NoDUsAsStructs
    [<Struct>]
#endif
    type HTLCId =
        | HTLCId of uint64

        static member Zero = HTLCId(0UL)
        member this.Value = let (HTLCId v) = this in v

        static member (+)(a: HTLCId, b: uint64) =
            (a.Value + b) |> HTLCId

    /// a.k.a. vout, index number for the txo in the specific tx.
#if !NoDUsAsStructs
    [<Struct>]
#endif
    type TxOutIndex =
        | TxOutIndex of uint16

        member this.Value = let (TxOutIndex v) = this in v

    /// index number for the tx in the specific block.
#if !NoDUsAsStructs
    [<Struct>]
#endif
    type TxIndexInBlock =
        | TxIndexInBlock of uint32

        member this.Value = let (TxIndexInBlock v) = this in v

    /// Unique index for the tx output in the blockchain (as long as there is no
    /// competing fork).
    /// In LN, this is used for funding txo. which corresponds 1-by-1 to the
    /// channel. Thus works as a unique identifier for the specific channel.
    /// This is only used when the channel is confirmed enough (thus "opened").
    /// Before it gets open, ChannelId is used instead, which is a xor of the
    /// tx hash and the output index.
    /// <seealso cref="DotNetLightning.Utils.ChannelId" />
#if !NoDUsAsStructs
    [<Struct; StructuredFormatDisplay("{AsString}")>]
#else
    [<StructuredFormatDisplay("{AsString}")>]
#endif
    type ShortChannelId =
        {
            BlockHeight: BlockHeight
            BlockIndex: TxIndexInBlock
            TxOutIndex: TxOutIndex
        }

        static member From8Bytes(b: array<byte>) : ShortChannelId =
            let bh =
                NBitcoin.Utils.ToUInt32(
                    Array.concat [| [| 0uy |]; b.[0..2] |],
                    false
                )

            let bi =
                NBitcoin.Utils.ToUInt32(
                    Array.concat [| [| 0uy |]; b.[3..5] |],
                    false
                )

            let txOutIndex = NBitcoin.Utils.ToUInt16(b.[6..7], false)

            {
                BlockHeight = bh |> BlockHeight
                BlockIndex = bi |> TxIndexInBlock
                TxOutIndex = txOutIndex |> TxOutIndex
            }

        static member FromUInt64(rawData: uint64) =
            NBitcoin.Utils.ToBytes(rawData, false) |> ShortChannelId.From8Bytes

        member this.ToBytes() : array<byte> =
            Array.concat
                [|
                    NBitcoin.Utils.ToBytes(this.BlockHeight.Value, false).[1..3]
                    NBitcoin.Utils.ToBytes(this.BlockIndex.Value, false).[1..3]
                    NBitcoin.Utils.ToBytes(this.TxOutIndex.Value, false)
                |]

        member this.ToUInt64() =
            this.ToBytes() |> fun b -> NBitcoin.Utils.ToUInt64(b, false)

        override this.ToString() =
            sprintf
                "%ix%ix%i"
                this.BlockHeight.Value
                this.BlockIndex.Value
                this.TxOutIndex.Value

        member this.AsString = this.ToString()

        static member TryParse(s: string) =
            let items = s.Split('x')
            let err = Error(sprintf "Failed to parse %s" s)

            if (items.Length <> 3) then
                err
            else
                match
                    (items.[0] |> UInt32.TryParse),
                    (items.[1] |> UInt32.TryParse),
                    (items.[2] |> UInt16.TryParse)
                    with
                | (true, h), (true, blockI), (true, outputI) ->
                    {
                        BlockHeight = h |> BlockHeight
                        BlockIndex = blockI |> TxIndexInBlock
                        TxOutIndex = outputI |> TxOutIndex
                    }
                    |> Ok
                | _ -> err

        static member ParseUnsafe(s: string) =
            ShortChannelId.TryParse s
            |> Result.defaultWith(fun _ ->
                raise <| FormatException(sprintf "Failed to parse %s" s)
            )

    [<Obsolete>]
    type UserId = UserId of uint64

    [<Obsolete>]
    type Delimiter = Delimiter of array<byte>

    /// Each lightning node can specify its own RGB color in node_announcement.
    /// Used for e.g. lightning node explorer.
    /// <seealso cref="DotNetLightning.Serialization.Msgs.UnsignedNodeAnnouncementMsg" />
    [<Struct>]
    type RGB =
        {
            Red: uint8
            Green: uint8
            Blue: uint8
        }

    /// Some p2p messages (e.g. [querying the gossip messages for syncing the routing info](https://github.com/lightning/bolts/blob/master/07-routing-gossip.md#query-messages))
    /// Supports compressed message format, This enum is used to annotate
    /// which type of the message format is used.
    /// <seealso cref="DotNetLightning.Serialization.Encoder" />
    /// <seealso cref="DotNetLightning.Serialization.Decoder" />
#if !NoDUsAsStructs
    [<Struct>]
#endif
    type EncodingType =
        | SortedPlain = 0uy
        | ZLib = 1uy

    /// destination script pubkey when closing channel in a cooperative way.
    /// typesafe wrapper for the raw scriptpubkey.
    type ShutdownScriptPubKey =
        private
            {
                ShutdownScript: Script
            }

        static member FromPubKeyP2wpkh(pubKey: PubKey) =
            let script = pubKey.WitHash.ScriptPubKey

            {
                ShutdownScript = script
            }

        static member FromPubKeyP2pkh(pubKey: PubKey) =
            let script = pubKey.Hash.ScriptPubKey

            {
                ShutdownScript = script
            }

        static member FromScriptP2sh(script: Script) =
            let scriptPubKey = script.Hash.ScriptPubKey

            {
                ShutdownScript = scriptPubKey
            }

        static member FromScriptP2wsh(script: Script) =
            let scriptPubKey = script.WitHash.ScriptPubKey

            {
                ShutdownScript = scriptPubKey
            }

        static member TryFromScript
            (scriptPubKey: Script)
            : Result<ShutdownScriptPubKey, string> =
            let isValidFinalScriptPubKey =
                (PayToPubkeyHashTemplate.Instance.CheckScriptPubKey scriptPubKey)
                || (PayToScriptHashTemplate.Instance.CheckScriptPubKey
                        scriptPubKey)
                || (PayToWitPubKeyHashTemplate.Instance.CheckScriptPubKey
                        scriptPubKey)
                || (PayToWitScriptHashTemplate.Instance.CheckScriptPubKey
                        scriptPubKey)

            if isValidFinalScriptPubKey then
                Ok
                    {
                        ShutdownScript = scriptPubKey
                    }
            else
                sprintf
                    "Invalid final script pubkey(%A). it must be one of p2pkh, p2sh, p2wpkh, p2wsh"
                    scriptPubKey
                |> Error

        member this.ScriptPubKey() : Script =
            this.ShutdownScript

        member this.ToBytes() : array<byte> =
            this.ShutdownScript.ToBytes()

    /// see [bolt 07](https://github.com/lightning/bolts/blob/master/07-routing-gossip.md#the-channel_update-message)
    /// <seealso cref="DotNetLightning.Serialization.Msgs.UnsignedChannelUpdateMsg" />
    type ChannelFlags =
        {
            // Set to announce the channel publicly and notify all nodes that they
            // can route via this channel. This should only be set to true for
            // nodes which expect to be online reliably. As the node which funds a
            // channel picks this value this will only apply for new outbound
            // channels unless
            // `ChannelHandshakeLimits.ForceAnnouncedChannelPreferences` is set.
            AnnounceChannel: bool
        }

        static member private AnnounceChannelMask: uint8 = 1uy

        static member FromUInt8(flags: uint8) : ChannelFlags =
            {
                AnnounceChannel =
                    (flags &&& ChannelFlags.AnnounceChannelMask) = ChannelFlags.AnnounceChannelMask
            }

        member this.IntoUInt8() : uint8 =
            if this.AnnounceChannel then
                ChannelFlags.AnnounceChannelMask
            else
                0uy
