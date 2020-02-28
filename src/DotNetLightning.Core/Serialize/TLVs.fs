namespace DotNetLightning.Serialize


open DotNetLightning.Utils.Primitives
open System
open NBitcoin

type QueryShortChannelIdsTLV =
    | QueryFlags of encodingType: EncodingType * encodedQueryFlags: byte[]
    | Unknown of GenericTLV
    with
    static member FromGenericTLV(tlv: GenericTLV) =
        match tlv.Type with
        | 1UL ->
            failwith ""
        | _ -> QueryShortChannelIdsTLV.Unknown (tlv)
        
    member this.ToGenericTLV() =
        match this with
        | QueryFlags (t, flags) ->
            let encodedFlags: byte[] =
                match t with
                | EncodingType.SortedPlain ->
                    flags
                | EncodingType.ZLib ->
                    failwith ""
                | _ -> failwith "unreachable!"
            let v = Array.concat(seq { [|(uint8)t|]; encodedFlags })
            { Type = 1UL; Value = v }
        | Unknown tlv -> tlv
        
        
type InitTLV =
    /// genesis chain hash that the node is interested in
    | Networks of uint256 array
    | Unknown of GenericTLV
    with
    static member FromGenericTLV(tlv: GenericTLV) =
        match tlv.Type with
        | 1UL ->
            let n, rem = Math.DivRem(tlv.Value.Length, 32)
            if rem <> 0 then raise <| FormatException(sprintf "Bogus length for TLV in init message (%d), remainder was (%d)" tlv.Value.Length rem) else
            let result = Array.zeroCreate n
            for i in 0..n - 1 do
                result.[i] <- tlv.Value.[(i * 32)..((i * 32) + 31)] |> uint256
            result |> Networks
        | _ -> Unknown (tlv)
        
    member this.ToGenericTLV() =
        match this with
        | Networks networks ->
            let v = networks |> Array.map(fun x -> x.ToBytes()) |> Array.concat
            { GenericTLV.Type = 1UL; Value = v }
        | Unknown tlv -> tlv
        

