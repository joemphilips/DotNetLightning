module internal DotNetLightning.Core.Utils.Extensions

open NBitcoin
open System.Linq
open System
open System.Collections
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Text

open ResultUtils
open ResultUtils.Portability

module Dict =
    let tryGetValue key (dict: IDictionary<_, _>) =
        match dict.TryGetValue key with
        | true, v -> Some v
        | false, _ -> None

type System.UInt64 with

    member this.GetBytesBigEndian() =
        let d = BitConverter.GetBytes(this)

        if BitConverter.IsLittleEndian then
            (d |> Array.rev)
        else
            d

    static member FromBytesBigEndian(bytes8: array<byte>) : uint64 =
        let bytes =
            if BitConverter.IsLittleEndian then
                Array.rev bytes8
            else
                bytes8

        BitConverter.ToUInt64(bytes, 0)

    member this.ToVarInt() =
        if this < 0xfdUL then
            [| uint8 this |]
        elif this < 0x10000UL then
            let buf = Array.zeroCreate(3)
            buf.[0] <- (0xfduy)
            buf.[1] <- (byte(this >>> 8))
            buf.[2] <- byte this
            buf
        elif this < 0x100000000UL then
            let buf = Array.zeroCreate(5)
            buf.[0] <- (0xfeuy)
            buf.[1] <- (byte(this >>> 24))
            buf.[2] <- (byte(this >>> 16))
            buf.[3] <- (byte(this >>> 8))
            buf.[4] <- (byte this)
            buf
        else
            let buf = Array.zeroCreate(9)
            buf.[0] <- (0xffuy)
            buf.[1] <- (byte(this >>> 56))
            buf.[2] <- (byte(this >>> 48))
            buf.[3] <- (byte(this >>> 40))
            buf.[4] <- (byte(this >>> 32))
            buf.[5] <- (byte(this >>> 24))
            buf.[6] <- (byte(this >>> 16))
            buf.[7] <- (byte(this >>> 8))
            buf.[8] <- (byte this)
            buf

type System.UInt32 with

    member this.GetBytesBigEndian() =
        let d = BitConverter.GetBytes(this)

        if BitConverter.IsLittleEndian then
            (d |> Array.rev)
        else
            d

    static member FromBytesBigEndian(bytes4: array<byte>) : uint32 =
        let bytes =
            if BitConverter.IsLittleEndian then
                Array.rev bytes4
            else
                bytes4

        BitConverter.ToUInt32(bytes, 0)

type System.UInt16 with

    member this.GetBytesBigEndian() =
        let d = BitConverter.GetBytes(this)

        if BitConverter.IsLittleEndian then
            (d |> Array.rev)
        else
            d

    static member FromBytesBigEndian(value: array<byte>) =
        ((uint16 value.[0]) <<< 8 ||| (uint16 value.[1]))

type System.Int64 with

    member this.ToVarInt() =
        (uint64 this).ToVarInt()

type System.Byte with

    member a.FlipBit() =
        ((a &&& 0x1uy) <<< 7)
        ||| ((a &&& 0x2uy) <<< 5)
        ||| ((a &&& 0x4uy) <<< 3)
        ||| ((a &&& 0x8uy) <<< 1)
        ||| ((a &&& 0x10uy) >>> 1)
        ||| ((a &&& 0x20uy) >>> 3)
        ||| ((a &&& 0x40uy) >>> 5)
        ||| ((a &&& 0x80uy) >>> 7)

[<Extension; AbstractClass; Sealed>]
type BitArrayExtensions() =
    [<Extension>]
    static member ToHex(this: #seq<byte>) =
        let db = StringBuilder()
        db.Append("0x") |> ignore
        this |> Seq.iter(fun b -> sprintf "%X" b |> db.Append |> ignore)
        db.ToString()

    [<Extension>]
    static member TryPopVarInt(this: array<byte>) =
        let e b =
            sprintf "Decoded VarInt is not canonical %A" b |> Error

        let x = this.[0]

        if x < 0xfduy then
            ((uint64 x), this.[1..]) |> Ok
        else if x = 0xfduy then
            if (this.Length < 2) then
                e this
            else
                let v = this.[1..2] |> UInt16.FromBytesBigEndian |> uint64

                if (v < 0xfdUL || 0x10000UL <= v) then
                    e this
                else
                    (v, this.[3..]) |> Ok
        else if x = 0xfeuy then
            if (this.Length < 4) then
                e this
            else
                let v =
                    this.[1..4]
                    |> fun x -> NBitcoin.Utils.ToUInt32(x, false) |> uint64

                if (v < 0x10000UL || 0x100000000UL <= v) then
                    e this
                else
                    (v, this.[5..]) |> Ok
        else if (this.Length < 8) then
            e this
        else
            let v = this.[1..8] |> fun x -> NBitcoin.Utils.ToUInt64(x, false)

            if (v < 0x100000000UL) then
                e this
            else
                (v, this.[9..]) |> Ok

type System.Collections.BitArray with

    member this.ToByteArray() =
        if this.Length = 0 then
            [||]
        else

            let leadingZeros =
                match (Seq.tryFindIndex (fun b -> b) (Seq.cast this)) with
                | Some i -> i
                | None -> this.Length

            let trueLength = this.Length - leadingZeros
            let desiredLength = ((trueLength + 7) / 8) * 8
            let difference = desiredLength - this.Length

            let bitArray =
                if difference < 0 then
                    // Drop zeroes from the front of the array until we have a multiple of 8 bits
                    let shortenedBitArray = BitArray(desiredLength)

                    for i in 0 .. (desiredLength - 1) do
                        shortenedBitArray.[i] <- this.[i - difference]

                    shortenedBitArray
                else if difference > 0 then
                    // Push zeroes to the front of the array until we have a multiple of 8 bits
                    let lengthenedBitArray = BitArray(desiredLength)

                    for i in 0 .. (this.Length - 1) do
                        lengthenedBitArray.[i + difference] <- this.[i]

                    lengthenedBitArray
                else
                    this

            // Copy the bit array to a byte array, then flip the bytes.
            let byteArray: array<byte> = Array.zeroCreate(desiredLength / 8)
            bitArray.CopyTo(byteArray, 0)
            byteArray |> Array.map(fun b -> b.FlipBit())

    static member From5BitEncoding(b: array<byte>) =
        let bitArray = System.Collections.BitArray(b.Length * 5)

        for di in 0 .. (b.Length - 1) do
            bitArray.Set(di * 5 + 0, ((b.[di] >>> 4) &&& 0x01uy) = 1uy)
            bitArray.Set(di * 5 + 1, ((b.[di] >>> 3) &&& 0x01uy) = 1uy)
            bitArray.Set(di * 5 + 2, ((b.[di] >>> 2) &&& 0x01uy) = 1uy)
            bitArray.Set(di * 5 + 3, ((b.[di] >>> 1) &&& 0x01uy) = 1uy)
            bitArray.Set(di * 5 + 4, ((b.[di] >>> 0) &&& 0x01uy) = 1uy)

        bitArray

    static member Concat(ba: #seq<BitArray>) =
        let a =
            ba |> Seq.map(fun b -> b.Cast<bool>()) |> Seq.concat |> Seq.toArray

        BitArray(a)

    static member FromUInt32(d: uint32) =
        let b = d.GetBytesBigEndian()
        BitArray.From5BitEncoding(b)

    member this.Reverse() =
        let boolArray: array<bool> = Array.ofSeq(Seq.cast this)
        Array.Reverse boolArray
        BitArray(boolArray)

    member this.PrintBits() =
        let sb = StringBuilder()

        for b in this do
            (if b then
                 "1"
             else
                 "0")
            |> sb.Append
            |> ignore

        sb.ToString()

    static member FromInt64(value: int64) =
        let mutable v = value
        let array = Array.zeroCreate 64

        for i in 0 .. (64 - 1) do
            array.[i] <- (v &&& 1L) = 1L
            v <- v >>> 1

        BitArray(array |> Array.rev)

    static member TryParse(str: string) =
        let mutable str = str.Trim().Clone() :?> string

        if str.StartsWith("0b", StringComparison.OrdinalIgnoreCase) then
            str <- str.Substring("0b".Length)

        let array = Array.zeroCreate(str.Length)
        let mutable hasFunnyChar = -1

        for i in 0 .. str.Length - 1 do
            if hasFunnyChar <> -1 then
                ()
            else if str.[i] = '0' then
                array.[i] <- false
            else if str.[i] = '1' then
                array.[i] <- true
            else
                hasFunnyChar <- i

        if hasFunnyChar <> -1 then
            sprintf
                "Failed to parse BitArray! it must have only '0' or '1' but we found %A"
                str.[hasFunnyChar]
            |> Error
        else
            BitArray(array) |> Ok

    /// This flips bits for each byte before passing to the BitArray constructor.
    /// This is necessary for representing bolt 9 feature bits as BitArray
    static member FromBytes(ba: array<byte>) =
        ba |> Array.map(fun b -> b.FlipBit()) |> BitArray

    member this.ToHex() =
        this.ToByteArray().ToHex()

[<Extension; AbstractClass; Sealed>]
type DictionaryExtensions() =

    [<Extension>]
    static member TryGetValueOption(this: IDictionary<_, _>, key) =
        Dict.tryGetValue key this

module Seq =
    let skipSafe num =
        Seq.zip(Seq.initInfinite id)
        >> Seq.skipWhile(fun (i, _) -> i < num)
        >> Seq.map snd
