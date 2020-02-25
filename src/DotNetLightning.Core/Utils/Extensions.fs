module DotNetLightning.Core.Utils.Extensions

open System
open System.Collections
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Text

module Dict =
    let tryGetValue key (dict: IDictionary<_,_>)=
       match dict.TryGetValue key with
       | true, v -> Some v
       | false, _ -> None
type System.UInt64 with
    member this.GetBytesBigEndian() =
        let d = BitConverter.GetBytes(this)
        if BitConverter.IsLittleEndian then (d |> Array.rev) else d
type System.UInt32 with
    member this.GetBytesBigEndian() =
        let d = BitConverter.GetBytes(this)
        if BitConverter.IsLittleEndian then (d |> Array.rev) else d
type System.UInt16 with
    member this.GetBytesBigEndian() =
        let d = BitConverter.GetBytes(this)
        if BitConverter.IsLittleEndian then (d |> Array.rev) else d
type System.Byte
    with
    member a.FlipBit() =
        ((a &&& 0x1uy)  <<< 7) ||| ((a &&& 0x2uy)  <<< 5) |||
        ((a &&& 0x4uy)  <<< 3) ||| ((a &&& 0x8uy)  <<< 1) |||
        ((a &&& 0x10uy) >>> 1) ||| ((a &&& 0x20uy) >>> 3) |||
        ((a &&& 0x40uy) >>> 5) ||| ((a &&& 0x80uy) >>> 7);

type System.Collections.BitArray with
    member this.ToByteArray() =
        if this.Length = 0 then [||] else
        let ret: byte[] = Array.zeroCreate (((this.Length - 1) / 8) + 1)
        let boolArray: bool[] = this.Reverse()
        let t = BitArray(boolArray)
        t.CopyTo(ret, 0)
        ret |> Array.rev
        
    static member From5BitEncoding(b: byte[]) =
        let bitArray = System.Collections.BitArray(b.Length * 5)
        for di in 0..(b.Length - 1) do
            bitArray.Set(di * 5 + 0, ((b.[di] >>> 4) &&& 0x01uy) = 1uy)
            bitArray.Set(di * 5 + 1, ((b.[di] >>> 3) &&& 0x01uy) = 1uy)
            bitArray.Set(di * 5 + 2, ((b.[di] >>> 2) &&& 0x01uy) = 1uy)
            bitArray.Set(di * 5 + 3, ((b.[di] >>> 1) &&& 0x01uy) = 1uy)
            bitArray.Set(di * 5 + 4, ((b.[di] >>> 0) &&& 0x01uy) = 1uy)
        bitArray
    static member FromUInt32(d: uint32) =
        let b = d.GetBytesBigEndian()
        BitArray.From5BitEncoding(b)
        
    member this.Reverse() =
        let length = this.Length
        let mutable result = Array.zeroCreate this.Length
        let mid = length / 2
        for i in 0..mid - 1 do
            let bit = this.[i]
            result.[i] <- this.[length - i - 1]
            result.[length - i - 1] <- bit
        result
    member this.PrintBits() =
        let sb = StringBuilder()
        for b in this do
            (if b then "1" else "0") |> sb.Append |> ignore
        sb.ToString()
        
    static member FromInt64(value: int64) =
        let mutable v = value
        let array = Array.zeroCreate 64
        for i in 0..(64 - 1) do
            array.[i] <- (v &&& 1L) = 1L
            v <- v >>> 1
        BitArray(array |> Array.rev)
        
    /// This flips bits for each byte before passing to the BitArray constructor.
    /// This is necessary for representing bolt 9 feature bits as BitArray
    static member FromBytes(ba: byte[]) =
        ba |> Array.map(fun b -> b.FlipBit()) |> BitArray
        
    static member Parse(str: string) =
        let mutable str = str.Trim().Clone() :?> string
        if str.StartsWith("0b", StringComparison.OrdinalIgnoreCase) then
            str <- str.Substring("0b".Length)
        let array = Array.zeroCreate(str.Length)
        for i in 0..str.Length - 1 do
            array.[i] <-
                if str.[i] = '0' then false else
                if str.[i] = '1' then true else
                raise <| FormatException(sprintf "Failed to parse BitArray! it must have only '0' or '1' but we found %A" str.[i])
        array |> BitArray
        
    
[<Extension;AbstractClass;Sealed>]
type DictionaryExtensions() =

    [<Extension>]
    static member TryGetValueOption(this: IDictionary<_, _>, key) =
        Dict.tryGetValue key this
