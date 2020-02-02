module DotNetLightning.Core.Utils.Extensions

open System
open System.Collections
open System.Collections.Generic
open System.Runtime.CompilerServices

module Dict =
    let tryGetValue key (dict: IDictionary<_,_>)=
       match dict.TryGetValue key with
       | true, v -> Some v
       | false, _ -> None
type System.UInt64 with
    member this.GetBytesBigEndian() =
        let d = BitConverter.GetBytes(this)
        if BitConverter.IsLittleEndian then (d |> Array.rev) else d

type System.Collections.BitArray with
    member this.ToByteArray() =
        let ret: byte[] = Array.zeroCreate (((this.Length - 1) / 8) + 1)
        this.CopyTo(ret, 0)
        ret
        
    static member From5BitEncoding(b: byte[]) =
        let bitArray = System.Collections.BitArray(b.Length * 5)
        for di in 0..(b.Length - 1) do
            bitArray.Set(di * 5 + 0, ((b.[di] >>> 4) &&& 0x01uy) = 1uy)
            bitArray.Set(di * 5 + 1, ((b.[di] >>> 3) &&& 0x01uy) = 1uy)
            bitArray.Set(di * 5 + 2, ((b.[di] >>> 2) &&& 0x01uy) = 1uy)
            bitArray.Set(di * 5 + 3, ((b.[di] >>> 1) &&& 0x01uy) = 1uy)
            bitArray.Set(di * 5 + 4, ((b.[di] >>> 0) &&& 0x01uy) = 1uy)
        bitArray
    
[<Extension>]
type DictionaryExtensions() =

    [<Extension>]
    static member TryGetValueOption(this: IDictionary<_, _>, key) =
        Dict.tryGetValue key this
