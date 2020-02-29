namespace DotNetLightning.Serialize
(*
    Encoder-Decoder for TLV serialization
*)

open DotNetLightning.Utils.Primitives
open ResultUtils
open System
open System.IO
open System.IO.Compression

type EncodingType =
    | SortedPlain = 0uy
    | ZLib = 1uy
    
module Decoder =
    let private tryDecode (encodingType: EncodingType) (bytes : byte[]) =
        if bytes.Length = 0 then bytes |> Ok else
        let data = bytes
        match encodingType with
        | EncodingType.SortedPlain ->
            data |> Ok
        | EncodingType.ZLib ->
            use ms = new MemoryStream(data)
            ms.Position <- 0L
            use ds = new DeflateStream(ms, CompressionMode.Decompress, true)
            use outputMs = new MemoryStream()
            ds.CopyTo(outputMs)
            outputMs.ToArray() |> Ok
        | x ->
            Error(sprintf "Unknown encoding type %A" x)
        
    let private unwrap b =
        b |> function Ok x -> x | Error msg -> raise <| FormatException(msg)
        
    let private bytesToShortIds (data: byte[]) =
        result {
            if data.Length = 0 then return [||] else
            let count, remainder = Math.DivRem(data.Length, 8)
            if (remainder <> 0) then
                return!
                    sprintf "Bogus encoded_ item! length of short_channel_ids must be multiple of 8. it was %d" data.Length
                    |> Error
            else
                return
                    data
                    |> Array.splitInto count
                    |> Array.map(ShortChannelId.From8Bytes)
        }
    let tryDecodeShortChannelIds encodingType d =
        result {
            let! bytes = tryDecode encodingType d
            return! bytes |> bytesToShortIds
        }
        
    let decodeShortChannelIds e d =
        tryDecodeShortChannelIds e d |> unwrap
        
    let decodeShortChanneelIdsFromBytes (d: byte[]) =
        let encodingType = LanguagePrimitives.EnumOfValue<byte,EncodingType>(d.[0])
        decodeShortChannelIds encodingType (d.[1..])
        
    let private bytesToQueryFlags (data: byte[]) =
        use ms = new MemoryStream(data)
        use ls = new LightningReaderStream(ms)
        let flags =
            ls.ReadAllAsBigSize()
            |> Array.map (QueryFlags.TryCreate)
            |> Seq.sequenceResultM
            |> Result.map Seq.toArray
        flags

    let tryDecodeQueryFlags encodingType d =
        result {
            let! bytes = tryDecode encodingType d
            return! bytes |> bytesToQueryFlags
        }
        
    let decodeQueryFlags encodingType d =
        tryDecodeQueryFlags encodingType d |> unwrap
        
    let decodeQueryFlagsFromBytes (d: byte[]) =
        let encodingType = LanguagePrimitives.EnumOfValue<byte,EncodingType>(d.[0])
        decodeQueryFlags encodingType (d.[1..])
        
module Encoder =
    let private encode(ty: EncodingType) (value:byte[]): byte[] =
        if value.Length = 0 then [||] else
        match ty with
        | EncodingType.SortedPlain ->
            value
        | EncodingType.ZLib ->
            use outputMs = new MemoryStream()
            use ds = new DeflateStream(outputMs, CompressionMode.Compress)
            ds.Write(value, 0, value.Length)
            ds.Flush()
            ds.Close()
            outputMs.ToArray()
        | x ->
            failwithf "Unreachable! Unknown encoding type %A" x
            
    let encodeShortChannelIds (ty) (shortIds: ShortChannelId[]) =
        let b = shortIds |> Array.map(fun i -> i.ToBytes()) |> Array.concat
        encode (ty) (b)
        
    let encodeQueryFlags (ty) (flags: QueryFlags[]) =
        let b = flags |> Array.map(fun i -> i.ToBytes()) |> Array.concat
        encode (ty) (b)
