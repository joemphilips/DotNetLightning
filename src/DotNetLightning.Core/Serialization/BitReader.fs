namespace DotNetLightning.Serialization

open System
open System.Collections
open System.Text
open System.Text

open ResultUtils
open ResultUtils.Portability

/// simple wrapper reading from BitArray.
type internal BitReader(ba: BitArray, bitCount: int) =

    member val Count = bitCount
    member val Position = 0 with get, set
    new(ba) = BitReader(ba, ba.Count)

    member this.Read() =
        let v = ba.Get(this.Position)
        this.Position <- this.Position + 1
        v

    member this.ReadULongBE(bitCount: int) =
        let mutable value = 0UL

        for i in 0 .. (bitCount - 1) do
            let v =
                if this.Read() then
                    1UL
                else
                    0UL

            value <- value + (v <<< (bitCount - i - 1))

        value

    member this.ReadBytes(byteSize: int) =
        let bytes: array<byte> = Array.zeroCreate byteSize
        let mutable maxRead = this.Count - this.Position
        let mutable byteIndex = 0

        while (byteIndex < byteSize && maxRead <> 0) do
            let mutable value = 0uy

            for i in 0..7 do
                if (maxRead <> 0) then
                    let v =
                        if this.Read() then
                            1UL
                        else
                            0UL

                    value <- value + (byte(v <<< (8 - i - 1)))
                    maxRead <- maxRead - 1

            bytes.[byteIndex] <- value
            byteIndex <- byteIndex + 1

        bytes

    member this.ReadBits(bitCount: int) =
        let result = Array.zeroCreate(bitCount)

        for i in 0 .. bitCount - 1 do
            result.[i] <- this.Read()

        BitArray(result)

    member this.Consume(count: int) =
        this.Position <- this.Position + count

    member this.SkipTo(position: int) =
        let skip = position - this.Position

        if skip < 0 then
            sprintf
                "Could not skip BitReader from %i to %i"
                this.Position
                position
            |> Error
        else
            this.Consume(skip)
            Ok()

    member this.CanConsume(bitCount: int) =
        this.Position + bitCount <= this.Count

    override this.ToString() =
        let sb = StringBuilder(ba.Length)

        for i in 0 .. (this.Count - 1) do
            if (i <> 0 && i % 8 = 0) then
                sb.Append(' ') |> ignore

            sb.Append(
                if ba.Get(i) then
                    "1"
                else
                    "0"
            )
            |> ignore

        sb.ToString()
