namespace DotNetLightning.Serialization

open System.Linq
open System.Collections
open System.Collections
open System.Text
open DotNetLightning.Core.Utils.Extensions

open ResultUtils
open ResultUtils.Portability

/// Based on: https://github.com/MetacoSA/NBitcoin/blob/d822f191441b2da5abdd3ab4765cf82296dbea18/NBitcoin/BitWriter.cs
type internal BitWriter() =
    let values = ResizeArray<bool>()

    member this.Count = values.Count

    member val Position = 0 with get, set

    member this.Write(v: bool) =
        values.Insert(this.Position, v)
        this.Position <- this.Position + 1

    member this.Write(bytes: array<byte>) =
        this.Write(bytes, bytes.Length * 8)

    static member private SwapEndianBytes(bytes: array<byte>) =
        let output = Array.zeroCreate(bytes.Length)

        for i in 0 .. (output.Length - 1) do
            let mutable newByte = 0uy

            for ib in 0..7 do
                newByte <-
                    newByte + byte(((bytes.[i] >>> ib) &&& 1uy) <<< (7 - ib))

            output.[i] <- newByte

        output

    member this.Write(bytes: array<byte>, bitCount: int) =
        let bytes = BitWriter.SwapEndianBytes(bytes)
        let array = BitArray(bytes)
        values.InsertRange(this.Position, array.OfType<bool>().Take(bitCount))
        this.Position <- this.Position + bitCount

    member this.Write(value: uint32, bitCount: int) =
        let mutable v = value
        let mutable count = bitCount
        v <- v <<< (32 - bitCount)

        while (count > 0) do
            this.Write((v >>> (32 - 1) = 1u))
            v <- v <<< 1
            count <- count - 1

    static member private ToByteArray(bits: BitArray) =
        let mutable arrayLength = bits.Length / 8

        if (bits.Length % 8 <> 0) then
            arrayLength <- arrayLength + 1

        let array = Array.zeroCreate arrayLength

        for i in 0 .. bits.Length - 1 do
            let b = i / 8
            let offset = i % 8

            array.[b] <-
                array.[b]
                ||| if (bits.Get i) then
                        byte(1 <<< offset)
                    else
                        0uy

        array

    member this.ToBytes() : array<byte> =
        this.ToBitArray() |> BitWriter.ToByteArray |> BitWriter.SwapEndianBytes

    member this.ToBitArray() : BitArray =
        values.ToArray() |> BitArray

    member this.ToIntegers() =
        this.ToBitArray() |> NBitcoin.Wordlist.ToIntegers

    member this.Write(ba: BitArray) =
        this.Write(ba, ba.Length)

    member this.Write(ba: BitArray, bitCount) =
        for i in 0 .. (bitCount - 1) do
            this.Write(ba.Get(i))

    override this.ToString() =
        let sb = StringBuilder(values.Count)

        for i in 0 .. (this.Count - 1) do
            if (i <> 0 && i % 8 = 0) then
                sb.Append(' ') |> ignore

            sb.Append(
                if values.[i] then
                    "1"
                else
                    "0"
            )
            |> ignore

        sb.ToString()
