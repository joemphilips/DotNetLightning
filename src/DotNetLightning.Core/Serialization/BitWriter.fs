namespace DotNetLightning.Serialization

open System.Linq
open System.Collections
open System.Collections
open System.Text
open DotNetLightning.Core.Utils.Extensions

// Based on: https://github.com/MetacoSA/NBitcoin/blob/d822f191441b2da5abdd3ab4765cf82296dbea18/NBitcoin/BitWriter.cs
type BitWriter() =
    let values = ResizeArray<bool>()
    
    member this.Count = values.Count
    
    member val Position = 0 with get, set
    
    member this.Write(v: bool) =
        values.Insert(this.Position, v)
        this.Position <- this.Position + 1

    member this.Write(bytes: byte[]) =
        this.Write(bytes, bytes.Length * 8)
        
    static member private SwapEndianBytes(bytes: byte[]) =
        let output = Array.zeroCreate(bytes.Length)
        for i in 0..(output.Length - 1) do
            let mutable newByte = 0uy
            for ib in 0..7 do
                newByte <- newByte + byte ((bytes.[i] >>> ib) &&& 1uy) <<< (7 - ib)
            output.[i] <- newByte
        output
        
    member this.Write(bytes: byte[], bitCount: int) =
        let bytes = BitWriter.SwapEndianBytes(bytes)
        let ba = BitArray(bytes) |> Seq.cast<bool>
        values.InsertRange(this.Position, ba.Take(bitCount))
        this.Position <- this.Position + bitCount

    member this.ToBytes(): byte[] =
        this.ToBitArray().ToByteArray()
        |> BitWriter.SwapEndianBytes
        
    member this.ToBitArray(): BitArray =
        values.ToArray() |> BitArray
        
    member this.ToIntegers() =
        this.ToBitArray() |> NBitcoin.Wordlist.ToIntegers
        
    member this.Write(ba: BitArray) =
        this.Write(ba, ba.Length)
        
    member this.Write(ba: BitArray, bitCount) =
        for i in 0..(bitCount - 1) do
            this.Write(ba.Get(i))

    override this.ToString() =
        let sb = StringBuilder(values.Count)
        for i in 0 ..(this.Count - 1) do
            if (i <> 0 && i % 8 = 0) then
                sb.Append(' ') |> ignore
            sb.Append(if values.[i] then "1" else "0") |> ignore
        sb.ToString()
