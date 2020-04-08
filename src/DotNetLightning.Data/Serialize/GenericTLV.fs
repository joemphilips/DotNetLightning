namespace DotNetLightning.Serialize

open System
open ResultUtils
open DotNetLightning.Core.Utils.Extensions

type GenericTLV = {
    Type: uint64
    Value: byte[]
}
    with
    static member TryCreateFromBytes(b: byte[]) =
        result {
            let! ty, b = b.TryPopVarInt()
            let! l, b = b.TryPopVarInt()
            if (l > (uint64 Int32.MaxValue)) then return! Error(sprintf "length for tlv is too long %A" l) else
            let l = l |> int32
            if b.Length < l then return! Error (sprintf "malformed Generic TLV! bytes (%A) are shorter than specified length (%A)" b l) else
            let value = b.[0..(l - 1)]
            return { Type = ty; Value = value }, b.[l..]
        }
        
    /// consumes all bytes.
    static member TryCreateManyFromBytes(bytes: byte[]) =
        result {
          let result = ResizeArray()
          let mutable b = bytes
          let mutable cont = true
          while cont do
              let! tlv, b2 = GenericTLV.TryCreateFromBytes(b)
              result.Add(tlv)
              b <- b2
              cont <- b.Length > 1
          return result.ToArray()
        }
        
    member this.ToBytes() =
        let ty = this.Type.ToVarInt()
        Array.concat[ty; this.Value.LongLength.ToVarInt(); this.Value]
        