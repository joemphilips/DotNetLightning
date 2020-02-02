namespace DotNetLightning.Serialize

open System
open System.Collections
open System.Text

type BitWriter() =
    let swapEndianBytes(bytes: byte[]) =
        let output = Array.zeroCreate bytes.Length
        for i in 0..output.Length - 1 do
            let mutable newByte = 0uy
            for ib in 0..7 do
                newByte <- (((bytes.[i] >>> ib) &&& 1uy) <<< (7 - ib)) |> byte
            output.[i] <- newByte
        output
        
    let values = ResizeArray()
        
    member val Position = 0 with get, set
    
    member this.Write(value: bool) =
        values.Insert(this.Position, value)
        this.Position <- this.Position + 1
        
    member this.Write(bytes: byte[], bitCount: int) =
        let bytes = swapEndianBytes bytes
        let ba = BitArray(bytes)
        values.InsertRange(this.Position, ba |> Seq.cast |> Seq.take(bitCount))
        this.Position <- bitCount
        
    member this.Write(bytes: byte[]) =
        this.Write(bytes, bytes.Length * 8)
        
    member this.ToBitArray() = BitArray(values.ToArray())
        
    member this.ToByteArray(bits: BitArray) =
        let mutable arrayLength = bits.Length / 8
        if (bits.Length % 8 <> 0) then
            arrayLength <- arrayLength + 1
            
        let array = Array.zeroCreate arrayLength
        for i in 0..bits.Length - 1 do
            let b = i / 8
            let offset = i % 8
            array.[b] <- array.[b] ||| if bits.Get(i) then 1uy <<< offset else 0uy
        array
        
    member this.ToBytes() =
        this.ToBitArray()
        |> this.ToByteArray
        |> swapEndianBytes

type BitReader(ba: BitArray, bitCount: int) =
    
    member val Count = bitCount with get
    member val Position = 0 with get, set
    new (ba) = BitReader(ba, ba.Count)
    
    member this.Read() =
        let v = ba.Get(this.Position)
        this.Position <- this.Position + 1
        v

    member this.ReadULongBE(bitCount: int) =
        let mutable value = 0UL
        for i in 0..(bitCount - 1) do
            let v = if this.Read() then 1UL else 0UL
            value <- value + (v <<< (bitCount - i - 1))
        value
        
    member this.ReadBytes(byteSize: int) =
        let bytes: byte[] = Array.zeroCreate byteSize
        let mutable maxRead = this.Count - this.Position
        let mutable byteIndex = 0
        while (byteIndex < byteSize && maxRead <> 0) do
            let mutable value = 0uy
            for i in 0..7 do
                if (maxRead <> 0) then
                    let v = if this.Read() then 1UL else 0UL
                    value <- value + (byte (v <<< (8 - i - 1)))
                    maxRead <- maxRead - 1
            
            bytes.[byteIndex] <- value
            byteIndex <- byteIndex + 1
        bytes
        
    /// If bitCount is not multiple of 8, this will pad the result with zero
    member this.ReadBits(bitCount: int): byte[] =
        match Math.DivRem(bitCount, 8) with
        | div, remainder ->
            let bytes = this.ReadBytes(div)
            if remainder = 0 then bytes else
                
            let result = Array.zeroCreate (bytes.Length + 1)
            Array.blit bytes 0 result 0 div
                
            let mutable maxRead = this.Count - this.Position
            let mutable value = 0uy
            for i in 0..remainder - 1 do
                if (maxRead <> 0) then
                    let v = if this.Read() then 1uy else 0uy
                    value <- value + (byte (v <<< (8 - i - 1)))
                    maxRead <- maxRead - 1
            result.[result.Length - 1] <- value
            result
        
    member this.Consume(count: int) =
        this.Position <- this.Position + count
        
    member this.SkipTo(position: int) =
        let skip = position - this.Position
        if skip < 0 then
            sprintf "Could not skip BitReader from %d to %d" this.Position position |> Error
        else
            this.Consume(skip)
            Ok()
        
    member this.CanConsume(bitCount: int) =
        this.Position + bitCount <= this.Count
        
    override this.ToString() =
        let sb = StringBuilder(ba.Length)
        for i in 0..(this.Count - 1) do
            if (i <> 0 && i % 8 = 0) then
                sb.Append(' ') |> ignore
            sb.Append(if ba.Get(i) then "1" else "0") |> ignore
        sb.ToString()
