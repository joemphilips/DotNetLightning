namespace DotNetLightning.Serialization

open System
open System.IO
open DotNetLightning.Utils
open NBitcoin

open ResultUtils
open ResultUtils.Portability

type internal O = OptionalArgumentAttribute
type internal D = System.Runtime.InteropServices.DefaultParameterValueAttribute

/// Simple Wrapper stream for serializing Lightning Network p2p messages.
/// Why not use BitWriter/Reader? Because it does not support big endian and
/// We might want to extend it in the future.
[<AbstractClass>]
type LightningStream(inner: Stream) =
    inherit Stream()
    let _inner = inner
    member this.Inner = _inner


    override this.CanSeek = this.Inner.CanSeek

    override this.Flush() =
        this.Inner.Flush()

    override this.Length = this.Inner.Length

    override this.Position
        with get () = this.Inner.Position
        and set v = this.Inner.Position <- v

    override this.Seek(offset: int64, origin: SeekOrigin) =
        this.Inner.Seek(offset, origin)

    override this.SetLength(_value: int64) =
        raise
        <| NotSupportedException("SetLength is not supported for LigningStream")

    override this.Close() =
        this.Inner.Close()

    override this.Write(buffer: array<byte>, offset: int, count: int) =
        this.Inner.Write(buffer, offset, count)

    member this.Write(buffer: array<byte>) =
        this.Write(buffer, 0, buffer.Length)

    member this.Write(b: byte) =
        this.WriteByte(b)

    override this.Read(buf: array<byte>, offset: int, count: int) =
        this.Inner.Read(buf, offset, count)

/// <summary>
///     stream for writing lightning p2p msg
/// </summary>
/// <seealso cref="LightningStream"/>
type LightningWriterStream(inner: Stream) =
    inherit LightningStream(inner)

    do
        if not(inner.CanWrite) then
            invalidArg "inner" "inner stream must be writable"

    override this.CanWrite = true
    override this.CanRead = this.Inner.CanRead

    override this.Write(buffer: array<byte>, offset: int, count: int) =
        this.Inner.Write(buffer, offset, count)

    member this.Write(buf: array<byte>) =
        this.Write(buf, 0, buf.Length)

    member this.Write(b: byte) =
        this.WriteByte(b)

    // #############################
    // Custom binary writer methods
    // #############################

    member this.Write(data: uint16, lendian: bool) =
        let mutable buf = Array.zeroCreate 2

        if lendian then
            buf.[0] <- (byte) data
            buf.[1] <- (byte(data >>> 8))
        else
            buf.[0] <- (byte(data >>> 8))
            buf.[1] <- byte data

        this.Write(buf, 0, 2)

    member this.Write(data: uint32, lendian: bool) =
        let mutable buf = Array.zeroCreate 4

        if lendian then
            buf.[0] <- (byte) data
            buf.[1] <- (byte(data >>> 8))
            buf.[2] <- (byte(data >>> 16))
            buf.[3] <- (byte(data >>> 24))
        else
            buf.[0] <- (byte(data >>> 24))
            buf.[1] <- (byte(data >>> 16))
            buf.[2] <- (byte(data >>> 8))
            buf.[3] <- byte data

        this.Write(buf, 0, 4)

    member this.Write(data: uint64, lendian: bool) =
        let mutable buf = Array.zeroCreate 8

        if lendian then
            buf.[0] <- (byte) data
            buf.[1] <- (byte(data >>> 8))
            buf.[2] <- (byte(data >>> 16))
            buf.[3] <- (byte(data >>> 24))
            buf.[4] <- (byte(data >>> 32))
            buf.[5] <- (byte(data >>> 40))
            buf.[6] <- (byte(data >>> 48))
            buf.[7] <- (byte(data >>> 56))
        else
            buf.[0] <- (byte(data >>> 56))
            buf.[1] <- (byte(data >>> 48))
            buf.[2] <- (byte(data >>> 40))
            buf.[3] <- (byte(data >>> 32))
            buf.[4] <- (byte(data >>> 24))
            buf.[5] <- (byte(data >>> 16))
            buf.[6] <- (byte(data >>> 8))
            buf.[7] <- byte data

        this.Write(buf, 0, 8)

    member this.Write(data: int64, lendian: bool) =
        this.Write(uint64 data, lendian)

    member this.Write(data: int32, lendian: bool) =
        this.Write(uint32 data, lendian)

    member this.Write(data: option<array<byte>>) =
        match data with
        | Some d -> this.Write(d, 0, d.Length)
        | None -> ()

    member this.Write(data: ShortChannelId) =
        let d = data.ToBytes()
        this.Write(d, 0, d.Length)

    member this.Write(data: uint256, lendian: bool) =
        let d = data.ToBytes(lendian)
        this.Write(d, 0, d.Length)

    member this.Write(data: PubKey) =
        let d = data.ToBytes()
        this.Write(d, 0, d.Length)

    member this.Write(data: LNECDSASignature) =
        let d = data.ToBytesCompact()
        this.Write(d, 0, d.Length)

    member this.Write(data: RGB) =
        this.Inner.WriteByte(data.Red)
        this.Inner.WriteByte(data.Green)
        this.Inner.WriteByte(data.Blue)

    member this.Write(commitmentNumber: CommitmentNumber) =
        this.Write(
            (UInt48.MaxValue - commitmentNumber.Index())
                .UInt64,
            false
        )

    member this.WriteWithLen(data: array<byte>) =
        let length = data.Length
        this.Write((uint16) length, false)
        this.Write(data, 0, length)

    member this.WriteWithLen(data: option<array<byte>>) =
        match data with
        | Some d -> this.WriteWithLen(d)
        | None -> ()

    member this.WriteBigSize(x: uint64) =
        if x < 0xfdUL then
            this.Write(uint8 x)
        else if x < 0x10000UL then
            this.Write(0xfduy)
            this.Write(uint16 x, false)
        else if x < 0x100000000UL then
            this.Write(0xfeuy)
            this.Write(uint32 x, false)
        else
            this.Write(0xffuy)
            this.Write(x, false)

    member this.WriteTLV(tlv: GenericTLV) =
        this.WriteBigSize(tlv.Type)
        this.WriteBigSize(uint64 tlv.Value.LongLength)
        this.Write(tlv.Value)

    member this.WriteTLVStream(tlvs: #seq<GenericTLV>) =
        tlvs |> Seq.iter(fun tlv -> this.WriteTLV(tlv))

/// <summary>
///     stream for reading lightning p2p msg
/// </summary>
/// <seealso cref="LightningStream"/>
type LightningReaderStream(inner: Stream) =
    inherit LightningStream(inner)

    do
        if (not inner.CanRead) then
            invalidArg "inner" "inner stream must be readable"

    let MaxArraySize = 1024 * 1024
    let mutable buffer = Array.zeroCreate MaxArraySize

    override this.CanRead = true
    override this.CanWrite = this.Inner.CanWrite

    member this.ReadAll() : array<byte> =
        this.ReadBytes(int32 this.Length - int32 this.Position)

    member this.TryReadAll() : option<array<byte>> =
        if (this.Length > this.Position) then
            this.ReadAll() |> Some
        else
            None

    member private this.FillBuffer(numBytes: int) =
        if isNull buffer then
            failwith "buffer was null"

        if (numBytes < 0 || (numBytes > buffer.Length)) then
            raise
            <| ArgumentOutOfRangeException(sprintf "numBytes was %i" numBytes)

        let mutable n = 0

        if (numBytes = 1) then
            n <- this.Inner.ReadByte()

            if (n = -1) then
                raise
                <| EndOfStreamException
                    "Inner Stream for LightningReaderStream has been consumed"

            buffer.[0] <- byte n
        else
            let mutable bytesRead = 0

            while (bytesRead < numBytes) do
                n <- this.Inner.Read(buffer, bytesRead, numBytes - bytesRead)

                if (n = 0) then
                    raise
                    <| EndOfStreamException
                        "Inner Stream for LightningReaderStream has been consumed"

                bytesRead <- bytesRead + n

    // ##### Methods for reading
    member this.ReadBoolean() =
        this.FillBuffer(1)
        buffer.[0] <> 0uy

    member this.ReadByte() : byte =
        let b = this.Inner.ReadByte()

        if (b = -1) then
            raise
            <| EndOfStreamException
                "Inner Stream for LightningReaderStream has been consumed"
        else
            (byte b)

    member this.ReadUInt8() =
        uint8(this.ReadByte())

    member this.ReadSByte() =
        this.FillBuffer(1)
        (sbyte buffer.[0])

    member this.ReadInt16(lendian: bool) =
        this.FillBuffer(2)

        if lendian then
            ((int16 buffer.[0]) ||| (int16 buffer.[1] <<< 8))
        else
            ((int16 buffer.[0]) <<< 8 ||| (int16 buffer.[1]))

    member this.ReadUInt16(lendian: bool) =
        this.FillBuffer(2)

        if lendian then
            ((uint16 buffer.[0]) ||| (uint16 buffer.[1] <<< 8))
        else
            ((uint16 buffer.[0]) <<< 8 ||| (uint16 buffer.[1]))

    member this.ReadInt32(lendian: bool) =
        this.FillBuffer(4)

        if lendian then
            ((int32 buffer.[0])
             ||| (int32 buffer.[1] <<< 8)
             ||| (int32 buffer.[2] <<< 16)
             ||| (int32 buffer.[3] <<< 24))
        else
            ((int32 buffer.[0] <<< 24)
             ||| (int32 buffer.[1] <<< 16)
             ||| (int32 buffer.[2] <<< 8)
             ||| (int32 buffer.[3]))

    member this.ReadUInt32(lendian: bool) =
        this.FillBuffer(4)

        if lendian then
            ((uint32 buffer.[0])
             ||| (uint32 buffer.[1] <<< 8)
             ||| (uint32 buffer.[2] <<< 16)
             ||| (uint32 buffer.[3] <<< 24))
        else
            ((uint32 buffer.[0] <<< 24)
             ||| (uint32 buffer.[1] <<< 16)
             ||| (uint32 buffer.[2] <<< 8)
             ||| (uint32 buffer.[3]))

    member this.ReadInt64(lendian: bool) =
        this.FillBuffer(8)

        if lendian then
            ((int64 buffer.[0])
             ||| (int64 buffer.[1] <<< 8)
             ||| (int64 buffer.[2] <<< 16)
             ||| (int64 buffer.[3] <<< 24)
             ||| (int64 buffer.[4] <<< 32)
             ||| (int64 buffer.[5] <<< 40)
             ||| (int64 buffer.[6] <<< 48)
             ||| (int64 buffer.[7] <<< 56))
        else
            ((int64 buffer.[0]) <<< 56
             ||| (int64 buffer.[1] <<< 48)
             ||| (int64 buffer.[2] <<< 40)
             ||| (int64 buffer.[3] <<< 32)
             ||| (int64 buffer.[4] <<< 24)
             ||| (int64 buffer.[5] <<< 16)
             ||| (int64 buffer.[6] <<< 8)
             ||| (int64 buffer.[7]))

    member this.ReadUInt64(lendian: bool) =
        this.FillBuffer(8)

        if lendian then
            ((uint64 buffer.[0])
             ||| (uint64 buffer.[1] <<< 8)
             ||| (uint64 buffer.[2] <<< 16)
             ||| (uint64 buffer.[3] <<< 24)
             ||| (uint64 buffer.[4] <<< 32)
             ||| (uint64 buffer.[5] <<< 40)
             ||| (uint64 buffer.[6] <<< 48)
             ||| (uint64 buffer.[7] <<< 56))
        else
            ((uint64 buffer.[0]) <<< 56
             ||| (uint64 buffer.[1] <<< 48)
             ||| (uint64 buffer.[2] <<< 40)
             ||| (uint64 buffer.[3] <<< 32)
             ||| (uint64 buffer.[4] <<< 24)
             ||| (uint64 buffer.[5] <<< 16)
             ||| (uint64 buffer.[6] <<< 8)
             ||| (uint64 buffer.[7]))

    member this.ReadUInt256([<O; D(true)>] lendian: bool) : uint256 =
        let b = this.ReadBytes(32)
        uint256(b, lendian)

    member this.ReadWithLen() : array<byte> =
        let len = this.ReadUInt16(false)
        this.ReadBytes(int32 len)

    member this.ReadChannelFlags() : ChannelFlags =
        let flags = this.ReadUInt8()
        ChannelFlags.FromUInt8 flags

    member this.ReadKey() : Key =
        let bytes: array<byte> = this.ReadBytes Key.BytesLength
        new Key(bytes)

    member this.ReadPubKey() : PubKey =
        let bytes = this.ReadBytes PubKey.BytesLength

        match PubKey.TryCreatePubKey bytes with
        | true, pubKey -> pubKey
        | false, _ -> raise <| FormatException("Invalid Pubkey encoding")

    member this.ReadPerCommitmentSecret() : PerCommitmentSecret =
        PerCommitmentSecret <| this.ReadKey()

    member this.ReadPerCommitmentPoint() : PerCommitmentPoint =
        PerCommitmentPoint <| this.ReadPubKey()

    member this.ReadFundingPubKey() : FundingPubKey =
        FundingPubKey <| this.ReadPubKey()

    member this.ReadRevocationBasepoint() : RevocationBasepoint =
        RevocationBasepoint <| this.ReadPubKey()

    member this.ReadPaymentBasepoint() : PaymentBasepoint =
        PaymentBasepoint <| this.ReadPubKey()

    member this.ReadDelayedPaymentBasepoint() : DelayedPaymentBasepoint =
        DelayedPaymentBasepoint <| this.ReadPubKey()

    member this.ReadHtlcBasepoint() : HtlcBasepoint =
        HtlcBasepoint <| this.ReadPubKey()

    member this.ReadCommitmentNumber() : CommitmentNumber =
        let n = this.ReadUInt64 false
        CommitmentNumber <| (UInt48.MaxValue - (UInt48.FromUInt64 n))

    member this.ReadECDSACompact() : LNECDSASignature =
        let data = this.ReadBytes(64)
        LNECDSASignature.FromBytesCompact(data)

    member this.ReadScript() : Script =
        let d = this.ReadWithLen()
        Script.FromBytesUnsafe(d)

    member this.ReadRGB() : RGB =
        let r = this.ReadUInt8()
        let g = this.ReadUInt8()
        let b = this.ReadUInt8()

        {
            Red = r
            Green = g
            Blue = b
        }

    member this.ReadBigSize() : uint64 =
        let x = this.ReadUInt8()

        if x < 0xfduy then
            uint64 x
        else if x = 0xfduy then
            let v = this.ReadUInt16(false) |> uint64

            if (v < 0xfdUL || 0x10000UL <= v) then
                raise <| FormatException("decoded varint is not canonical")

            v
        else if x = 0xfeuy then
            let v = this.ReadUInt32(false) |> uint64

            if (v < 0x10000UL || 0x100000000UL <= v) then
                raise <| FormatException("decoded varint is not canonical")

            v
        else
            let v = this.ReadUInt64(false)

            if (v < 0x100000000UL) then
                raise <| FormatException("decoded varint is not canonical")

            v

    member this.ReadAllAsBigSize() : array<uint64> =
        let mutable rest = int32 this.Length - int32 this.Position
        let result = ResizeArray<uint64>()

        while rest > 0 do
            result.Add(this.ReadBigSize())
            rest <- int32 this.Length - int32 this.Position

        result.ToArray()

    member this.ReadTLV() : GenericTLV =
        let ty = this.ReadBigSize()
        let length = this.ReadBigSize()
        let value = this.ReadBytes(int32 length)

        {
            GenericTLV.Type = ty
            Value = value
        }

    member this.ReadTLVStream() : array<GenericTLV> =
        let mutable rest = int32 this.Length - int32 this.Position
        let result = ResizeArray<GenericTLV>()

        while rest > 0 do
            result.Add(this.ReadTLV())
            rest <- int32 this.Length - int32 this.Position

        result |> Seq.toArray

    member this.ReadShutdownScriptPubKey() : ShutdownScriptPubKey =
        let script = this.ReadScript()

        match ShutdownScriptPubKey.TryFromScript script with
        | Ok shutdownScript -> shutdownScript
        | Error errorMsg -> raise <| FormatException(errorMsg)
