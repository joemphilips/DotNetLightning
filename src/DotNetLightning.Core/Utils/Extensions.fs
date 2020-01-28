module DotNetLightning.Core.Utils.Extensions

open System
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
        let ret: byte[] = Array.zeroCreate (this.Length - 1 / (8 + 1))
        this.CopyTo(ret, 0)
        ret
    
[<Extension>]
type DictionaryExtensions() =

    [<Extension>]
    static member TryGetValueOption(this: IDictionary<_, _>, key) =
        Dict.tryGetValue key this
