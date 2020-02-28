namespace DotNetLightning.Serialize

open DotNetLightning.Utils.Primitives
open DotNetLightning.Utils.Primitives
open ResultUtils
open System
open System.IO
open System.IO.Compression

type EncodingType =
    | SortedPlain = 0uy
    | ZLib = 1uy
    
module Encoder =
    let private tryDecode (bytes : byte[]) =
        let encodingType = LanguagePrimitives.EnumOfValue<byte,EncodingType>(bytes.[0])
        let data = bytes.[1..]
        match encodingType with
        | EncodingType.SortedPlain ->
            data |> Ok
        | EncodingType.ZLib ->
            use ms = new MemoryStream(data)
            use ds = new DeflateStream(ms, CompressionMode.Decompress)
            use outputMs = new MemoryStream()
            ds.CopyTo(outputMs)
            outputMs.ToArray() |> Ok
        | x ->
            Error(sprintf "Unknown encoding type %A" x)
        
    let private unwrap b =
        b |> function Ok x -> x | Error msg -> raise <| FormatException(msg)
        
    let private bytesToShortIds (data: byte[]) =
        result {
            let count, remainder = Math.DivRem(data.Length, 8)
            if (remainder <> 0) then
                return!
                    "Bogus encoded_ item! length of short_channel_ids must be multiple of 8"
                    |> Error
            else
                return
                    data
                    |> Array.splitInto count
                    |> Array.map(ShortChannelId.From8Bytes)
        }
    let tryDecodeShortChannelIds d =
        result {
            let! bytes = tryDecode d
            return! bytes |> bytesToShortIds
        }
        
    let decodeShortChannelIds =
        tryDecodeShortChannelIds >> unwrap
        
    let bytesToQueryFlags (data: byte[]) =
        use ms = new MemoryStream(data)
        use ls = new LightningReaderStream(ms)
        let flags =
            ls.ReadAllAsBigSize()
            |> Array.map (QueryFlags.TryCreate)
            |> Seq.sequenceResultM
            |> Result.map Seq.toArray
        flags

    let tryDecodeQueryFlags d =
        result {
            let! bytes = tryDecode d
            return! bytes |> bytesToQueryFlags
        }
        
    let encode<'T>(object: 'T): byte[] =
        failwith ""

