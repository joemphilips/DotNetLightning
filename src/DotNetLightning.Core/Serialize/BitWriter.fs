namespace DotNetLightning.Serialize

open DotNetLightning.Core.Utils.Extensions
open System
open System.Collections
open System.IO

type BitWriter(s: Stream) =
    inherit BinaryWriter(s)
    let mutable curByte: bool[] = Array.create(8) false
    let mutable currentBitIndex = 0
    let mutable ba: BitArray = null
    
    let ConvertToByte(bools: bool[]) =
        let mutable b = 0uy
        let mutable bitIndex = 0
        for i in 0..7 do
            if bools.[i] then
                b <- b ||| (1uy <<< bitIndex)
            bitIndex <- bitIndex + 1
        b
    
    override this.Flush() =
        base.Write(ConvertToByte(curByte))
        base.Flush()
    override this.Write(value: bool) =
        curByte.[currentBitIndex] <- value
        currentBitIndex <- currentBitIndex + 1
        
        if (currentBitIndex = 8) then
            base.Write(ConvertToByte(curByte))
            currentBitIndex <- 0
            curByte <- Array.create(0) false
            
    override this.Write(v: byte) =
        ba <- BitArray(Array.singleton(v))
        for i in 0..7 do
            this.Write(ba.[i])
        ba <- null

    override this.Write(buf: byte[]) =
        for i in 0..buf.Length do
            this.Write(buf.[i])
            
    override this.Write(v: uint32) =
        ba <- BitArray(BitConverter.GetBytes(v))
        for i in 0..32 do
            this.Write(ba.[i])
        ba <- null
        
    override this.Write(v: uint64) =
        ba <- BitArray(BitConverter.GetBytes(v))
        for i in 0..64 do
            this.Write(ba.[i])
        ba <- null
        
    override this.Write(v: uint16) =
        ba <- BitArray(BitConverter.GetBytes(v))
        for i in 0..16 do
            this.Write(ba.[i])
        ba <- null
        
type BitReader(s: Stream) =
    inherit BinaryReader(s)
    
    let mutable currentByte = Array.create 8 false
    let mutable currentBitIndex = 0
    let mutable ba: BitArray = BitArray([|base.ReadByte()|])
    do
        ba.CopyTo(currentByte, 0)
        ba <- null
        
    override this.ReadBoolean() =
        if currentBitIndex = 8 then
            ba <- BitArray([|base.ReadByte()|])
            ba.CopyTo(currentByte, 0)
            ba <- null
            currentBitIndex <- 0
        
        let b = currentByte.[currentBitIndex]
        currentBitIndex <- currentBitIndex + 1
        b
        
    override this.ReadByte() =
        let bar = Array.create 8 false
        for i in 0..7 do
            bar.[i] <-  this.ReadBoolean()
        
        let mutable b = 0uy
        let mutable bitIndex = 0
        for i in 0..7 do
            if (bar.[i]) then
                b <- b ||| 1uy <<< bitIndex
            bitIndex <- bitIndex + 1
        b
        
    /// Used for bolt11 timestamp
    member this.ReadBit35BEAsDateTime() =
        let mutable ba = Array.create 35 false
        for i in 0..34 do
            ba.[i] <- this.ReadBoolean()
        ba <- Array.concat [ ba |> Array.rev; Array.zeroCreate(64 - 35) ]
        BitConverter.ToInt64(BitArray(ba).ToByteArray(), 0) |> DateTimeOffset.FromUnixTimeSeconds
        
    /// Read `count` number of bits, and convert it to uint8
    /// count must be 0 < count < 8
    member this.ReadBitsBEAsUInt8(count: int) =
        let mutable ba = Array.create count false
        for i in 0..(count - 1) do
            ba.[i] <- this.ReadBoolean()
        ba <- Array.concat [ ba |> Array.rev; Array.zeroCreate(8 - count) ]
        BitArray(ba).ToByteArray().[0]
        
    member this.ReadBitsBEAsUInt16(count: int) =
        let mutable ba = Array.create count false
        for i in 0..(count - 1) do
            ba.[i] <- this.ReadBoolean()
        ba <- Array.concat[ ba |> Array.rev; Array.zeroCreate(16 - count) ]
        BitConverter.ToUInt16(BitArray(ba).ToByteArray(), 0)
        
    override this.ReadBytes(bitLength: int) =
        base.ReadBytes(bitLength * 8)
        
    override this.ReadUInt16() =
        let bytes = this.ReadBytes(2)
        BitConverter.ToUInt16(bytes, 2)
    override this.ReadUInt32() =
        let bytes = this.ReadBytes(4)
        BitConverter.ToUInt32(bytes, 4)
        
    override this.ReadUInt64() =
        let bytes = this.ReadBytes(8)
        BitConverter.ToUInt64(bytes, 8)
