namespace DotNetLightning.Serialize

open System
open System.IO
open DotNetLightning.Utils
open NBitcoin

type Scope(openAction: Action, closeAction: Action) =
    let _close = closeAction
    do openAction.Invoke()
    member private this.Close = _close
    interface IDisposable with
        member this.Dispose() = this.Close.Invoke()

type O = OptionalArgumentAttribute
type D = System.Runtime.InteropServices.DefaultParameterValueAttribute
/// Simple Wrapper stream for serializing Lightning Network p2p messages.
/// Why not use BitWriter/Reader? Because it does not support big endian and
/// We might want to extend it in the future.
[<AbstractClass>]
type LightningStream(inner: Stream) = 
    inherit Stream()
    let _inner = inner
    member this.Inner = _inner


    override this.CanSeek = this.Inner.CanSeek
    override this.Flush() = this.Inner.Flush()
    override this.Length = this.Inner.Length
    override this.Position
        with get() = this.Inner.Position
        and set (v) = this.Inner.Position <- v 
    override this.Seek(offset: int64, origin: SeekOrigin) =
        this.Inner.Seek(offset, origin)
    override this.SetLength(value: int64) =
        raise (NotSupportedException("SetLength is not supported for LigningStream"))
    override this.Close() =
        this.Inner.Close()

    override this.Write(buffer: byte[], offset: int, count: int) =
        this.Inner.Write(buffer, offset, count)
    
    member this.Write(buffer: byte[]) =
        this.Write(buffer, 0 , buffer.Length)

    member this.Write(b: byte) =
        this.WriteByte(b)

    override this.Read(buf: byte[], offset: int, count: int) = 
        this.Inner.Read(buf, offset, count)
type LightningWriterStream(inner: Stream) =
    inherit LightningStream(inner)

    do if not (inner.CanWrite) then invalidArg "inner" "inner stream must be writable"
    override this.CanWrite = true
    override this.CanRead = this.Inner.CanRead

    override this.Write(buffer: byte[], offset: int, count: int) =
        this.Inner.Write(buffer, offset, count)
    
    member this.Write(buffer: ReadOnlySpan<byte>) =
        this.Inner.Write(buffer.ToArray(), 0, buffer.Length)

    member this.Write(buf: byte[]) =
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
            buf.[1] <- (byte (data >>> 8))
        else
            buf.[0] <- (byte (data >>> 8))
            buf.[1] <- byte data
        this.Write(buf, 0, 2)

    member this.Write(data: uint32, lendian: bool) =
        let mutable buf = Array.zeroCreate 4
        if lendian then
            buf.[0] <- (byte) data
            buf.[1] <- (byte (data >>> 8))
            buf.[2] <- (byte (data >>> 16))
            buf.[3] <- (byte (data >>> 24))
        else
            buf.[0] <- (byte (data >>> 24))
            buf.[1] <- (byte (data >>> 16))
            buf.[2] <- (byte (data >>> 8))
            buf.[3] <- byte data
        this.Write(buf, 0, 4)

    member this.Write(data: uint64, lendian: bool) =
        let mutable buf = Array.zeroCreate 8
        if lendian then
            buf.[0] <- (byte) data
            buf.[1] <- (byte (data >>> 8))
            buf.[2] <- (byte (data >>> 16))
            buf.[3] <- (byte (data >>> 24))
            buf.[4] <- (byte (data >>> 32))
            buf.[5] <- (byte (data >>> 40))
            buf.[6] <- (byte (data >>> 48))
            buf.[7] <- (byte (data >>> 56))
        else
            buf.[0] <- (byte (data >>> 56))
            buf.[1] <- (byte (data >>> 48))
            buf.[2] <- (byte (data >>> 40))
            buf.[3] <- (byte (data >>> 32))
            buf.[4] <- (byte (data >>> 24))
            buf.[5] <- (byte (data >>> 16))
            buf.[6] <- (byte (data >>> 8))
            buf.[7] <- byte data
        this.Write(buf, 0, 8)

    member this.Write(data: int64, lendian: bool) =
        this.Write(uint64 data, lendian)

    member this.Write(data: int32, lendian: bool) =
        this.Write(uint32 data, lendian)

    member this.Write (data: byte[] option) =
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

    member this.WriteWithLen(data: byte[]) =
        let length = data.Length
        this.Write((uint16)length, false)
        this.Write(data, 0, length)

    member this.WriteWithLen(data: byte[] option) =
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
type LightningReaderStream(inner: Stream) =
    inherit LightningStream(inner)
    do if (not inner.CanRead) then invalidArg "inner" "inner stream must be readable"
    let MaxArraySize = 1024*1024
    let mutable m_buffer = Array.zeroCreate MaxArraySize

    override this.CanRead = true
    override this.CanWrite = this.Inner.CanWrite

    member this.ReadAll(): byte[] =
        this.ReadBytes(int32 this.Length - int32 this.Position)
        // if (this.Length)  t

    member this.TryReadAll(): byte[] option =
        if (this.Length > this.Position) then
            this.ReadAll() |> Some
        else
            None

    member private this.FillBuffer(numBytes: int) =
        if (isNull m_buffer) then
            failwith "m_buffer was null"
        if (numBytes < 0 || (numBytes > m_buffer.Length)) then
            raise (ArgumentOutOfRangeException(sprintf "numBytes was %d" numBytes))

        let mutable n = 0
        if (numBytes = 1) then
            n <- this.Inner.ReadByte()
            if (n = -1) then
                raise (EndOfStreamException "Inner Stream for LightningReaderStream has been consumed")
            m_buffer.[0] <- byte n
        else
            let mutable bytesRead = 0
            while (bytesRead < numBytes) do
                n <- this.Inner.Read(m_buffer, bytesRead, numBytes - bytesRead)
                if (n = 0) then
                    raise (EndOfStreamException "Inner Stream for LightningReaderStream has been consumed")
                bytesRead <- bytesRead + n

    // ##### Methods for reading
    member this.ReadBoolean() =
        this.FillBuffer(1)
        m_buffer.[0] <> 0uy

    member this.ReadByte(): byte =
        let b = this.Inner.ReadByte()
        if (b = -1) then
            raise (EndOfStreamException "Inner Stream for LightningReaderStream has been consumed")
        else
            (byte b)
            
    member this.ReadBytes(len: int) =
        let res = Array.zeroCreate(len)
        this.FillBuffer(len)
        m_buffer.CopyTo(res, 0)
        res

    member this.ReadUInt8() =
        uint8 (this.ReadByte())
    
    member this.ReadSByte() =
        this.FillBuffer(1)
        (sbyte m_buffer.[0])

    member this.ReadInt16(lendian: bool) =
        this.FillBuffer(2)
        if lendian then
            ((int16 m_buffer.[0]) ||| (int16 m_buffer.[1] <<< 8))
        else
            ((int16 m_buffer.[0]) <<< 8 ||| (int16 m_buffer.[1]))

    member this.ReadUInt16(lendian: bool) =
        this.FillBuffer(2)
        if lendian then
            ((uint16 m_buffer.[0]) ||| (uint16 m_buffer.[1] <<< 8))
        else
            ((uint16 m_buffer.[0]) <<< 8 ||| (uint16 m_buffer.[1]))

    member this.ReadInt32(lendian: bool) =
        this.FillBuffer(4)
        if lendian then
            ((int32 m_buffer.[0]) |||
                (int32 m_buffer.[1] <<< 8) |||
                (int32 m_buffer.[2] <<< 16) |||
                (int32 m_buffer.[3] <<< 24))
        else
            ((int32 m_buffer.[0] <<< 24) |||
                (int32 m_buffer.[1] <<< 16) |||
                (int32 m_buffer.[2] <<< 8) |||
                (int32 m_buffer.[3]))

    member this.ReadUInt32(lendian: bool) =
        this.FillBuffer(4)
        if lendian then
            ((uint32 m_buffer.[0]) |||
             (uint32 m_buffer.[1] <<< 8) |||
             (uint32 m_buffer.[2] <<< 16) |||
             (uint32 m_buffer.[3] <<< 24))
        else
            ((uint32 m_buffer.[0] <<< 24) |||
             (uint32 m_buffer.[1] <<< 16) |||
             (uint32 m_buffer.[2] <<< 8) |||
             (uint32 m_buffer.[3]))
    member this.ReadInt64(lendian: bool) =
        this.FillBuffer(8)
        if lendian then
            ((int64 m_buffer.[0]) |||
                (int64 m_buffer.[1] <<< 8) |||
                (int64 m_buffer.[2] <<< 16) |||
                (int64 m_buffer.[3] <<< 24) |||
                (int64 m_buffer.[4] <<< 32) |||
                (int64 m_buffer.[5] <<< 40) |||
                (int64 m_buffer.[6] <<< 48) |||
                (int64 m_buffer.[7] <<< 56))
        else
            ((int64 m_buffer.[0]) <<< 56 |||
                (int64 m_buffer.[1] <<< 48) |||
                (int64 m_buffer.[2] <<< 40) |||
                (int64 m_buffer.[3] <<< 32) |||
                (int64 m_buffer.[4] <<< 24) |||
                (int64 m_buffer.[5] <<< 16) |||
                (int64 m_buffer.[6] <<< 8) |||
                (int64 m_buffer.[7]))

    member this.ReadUInt64(lendian: bool) =
        this.FillBuffer(8)
        if lendian then
            ((uint64 m_buffer.[0]) |||
                (uint64 m_buffer.[1] <<< 8) |||
                (uint64 m_buffer.[2] <<< 16) |||
                (uint64 m_buffer.[3] <<< 24) |||
                (uint64 m_buffer.[4] <<< 32) |||
                (uint64 m_buffer.[5] <<< 40) |||
                (uint64 m_buffer.[6] <<< 48) |||
                (uint64 m_buffer.[7] <<< 56))
        else
            ((uint64 m_buffer.[0]) <<< 56 |||
                (uint64 m_buffer.[1] <<< 48) |||
                (uint64 m_buffer.[2] <<< 40) |||
                (uint64 m_buffer.[3] <<< 32) |||
                (uint64 m_buffer.[4] <<< 24) |||
                (uint64 m_buffer.[5] <<< 16) |||
                (uint64 m_buffer.[6] <<< 8) |||
                (uint64 m_buffer.[7]))

    member this.ReadUInt256([<O;D(true)>]lendian: bool) =
        let b = this.ReadBytes(32)
        uint256(b, lendian)

    member this.ReadWithLen() =
        let len = this.ReadUInt16(false)
        this.ReadBytes(int32 len)

    member this.ReadPubKey() =
        let b = this.ReadBytes(33)
        if PubKey.Check(b, true) then
            PubKey(b, true)
        else
            raise (FormatException("Invalid Pubkey encoding"))

    member this.ReadECDSACompact() =
        let data = this.ReadBytes(64)
        LNECDSASignature.FromBytesCompact(data)

    member this.ReadScript() =
        let d = this.ReadWithLen()
        Script.FromBytesUnsafe(d)

    member this.ReadRGB() =
        let r = this.ReadUInt8()
        let g = this.ReadUInt8()
        let b = this.ReadUInt8()
        {
            Red = r
            Green = g
            Blue = b
        }
        
    member this.ReadBigSize() =
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
                
        
