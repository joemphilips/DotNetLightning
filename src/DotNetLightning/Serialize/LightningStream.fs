namespace DotNetLightning.Serialize

open System
open System.IO
open System.IO.Pipelines
open System.Threading
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Serialize.Msgs
open NBitcoin
open NBitcoin.Crypto

type Scope(openAction: Action, closeAction: Action) =
    let _close = closeAction
    do openAction.Invoke()
    member private this.Close = _close
    interface IDisposable with
        member this.Dispose() = this.Close.Invoke()

/// Simple Wrapper stream for serializing Lightning Network p2p messages.
/// Why not use BitWriter/Reader? Because it does not support big endian and
/// We might want to extend it in the future.
type LightningStream(inner: Stream) =
    inherit Stream()
    let _inner = inner
    member this.Inner = _inner

    member val MaxArraySize = 1024*1024

    override this.CanRead = this.Inner.CanRead
    override this.CanSeek = this.Inner.CanSeek
    override this.CanWrite = this.Inner.CanWrite
    override this.Flush() = this.Inner.Flush()
    override this.Length = this.Inner.Length
    override this.Position
        with get() = this.Inner.Position
        and set (v) = this.Inner.Position <- v 
    override this.Seek(offset: int64, origin: SeekOrigin) =
        this.Inner.Seek(offset, origin)
    override this.SetLength(value: int64) =
        raise (NotSupportedException("SetLength is not supported for LigningStream"))

    override this.Read(buf: byte[], offset: int, count: int) = 
        this.Inner.Read(buf, offset, count)
    override this.Write(buffer: byte[], offset: int, count: int) =
        this.Inner.Write(buffer, offset, count)
    
    member this.Write(buffer: byte[]) =
        this.Write(buffer, 0 , buffer.Length)

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
    member this.Write(data: ECDSASignature) =
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

    member this.Write(data: NetAddress) =
        match data with
        | IPv4 d ->
            this.Write(d.Address.GetAddressBytes())
            this.Write((uint16)d.Port, false)
        | IPv6 d ->
            this.Write(d.Address.GetAddressBytes())
            this.Write((uint16)d.Port, false)
        | OnionV2 d ->
            this.Write(d.Addr)
            this.Write((uint16)d.Port, false)
        | OnionV3 d ->
            this.Write(d.ed25519PubKey)
            this.Write(d.CheckSum, false)
            this.Write(d.Version)
            this.Write((uint16)d.Port, false)

    member this.Write(data: DateTime) =
        failwith "not impl"

    member this.Write(data: OnionPacket) =
        this.WriteByte(data.Version)
        this.Write(data.PublicKey.ToBytes())
        this.Write(data.HopData)
        this.Write(data.HMAC.ToBytes())
    override this.Close() =
        this.Inner.Close()