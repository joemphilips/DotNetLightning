namespace DotNetLightning.Serialize

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
        for i in 0..8 do
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
        for i in 0..8 do
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
