namespace DotNetLightning.Serialization
(*
    Encoder-Decoder for TLV serialization
*)

open DotNetLightning.Utils.Primitives
open System
open System.IO
open System.IO.Compression

open ResultUtils
open ResultUtils.Portability

module private Decoder =
    let private tryDecode (encodingType: EncodingType) (bytes: array<byte>) =
        if bytes.Length = 0 then
            bytes |> Ok
        else
            let data = bytes

            match encodingType with
            | EncodingType.SortedPlain -> data |> Ok
            | EncodingType.ZLib ->
                use ms = new MemoryStream(data)
                ms.Position <- 0L
                use ds = new DeflateStream(ms, CompressionMode.Decompress, true)
                use outputMs = new MemoryStream()
                ds.CopyTo(outputMs)
                outputMs.ToArray() |> Ok
            | x -> Error(sprintf "Unknown encoding type %A" x)

    let private unwrap b =
        b
        |> function
            | Ok x -> x
            | Error msg -> raise <| FormatException(msg)

    let private bytesToShortIds(data: array<byte>) =
        result {
            if data.Length = 0 then
                return [||]
            else
                let count, remainder = Math.DivRem(data.Length, 8)

                if (remainder <> 0) then
                    return!
                        sprintf
                            "Bogus encoded_ item! length of short_channel_ids must be multiple of 8. it was %i"
                            data.Length
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

    let decodeShortChannelIdsFromBytes(d: array<byte>) =
        let encodingType =
            LanguagePrimitives.EnumOfValue<byte, EncodingType>(d.[0])

        decodeShortChannelIds encodingType (d.[1..])

    let private bytesToQueryFlags(data: array<byte>) =
        use ms = new MemoryStream(data)
        use ls = new LightningReaderStream(ms)

        let flags =
            ls.ReadAllAsBigSize()
            |> Array.toList
            |> List.map(QueryFlags.TryCreate)
            |> List.sequenceResultM
            |> Result.map List.toArray

        flags

    let tryDecodeQueryFlags encodingType d =
        result {
            let! bytes = tryDecode encodingType d
            return! bytes |> bytesToQueryFlags
        }

    let decodeQueryFlags encodingType d =
        tryDecodeQueryFlags encodingType d |> unwrap

    let decodeQueryFlagsFromBytes(d: array<byte>) =
        let encodingType =
            LanguagePrimitives.EnumOfValue<byte, EncodingType>(d.[0])

        decodeQueryFlags encodingType (d.[1..])

    let private bytesToUint32Pair(data: array<byte>) =
        result {
            if data.Length = 0 then
                return ([||])
            else
                let div, rem = Math.DivRem(data.Length, 8)

                if (rem <> 0) then
                    return! Error(sprintf "bogus timestamps! %A" data)
                else
                    use ms = new MemoryStream(data)
                    use ls = new LightningReaderStream(ms)
                    let res = Array.zeroCreate div

                    for i in 0 .. (div - 1) do
                        let a = ls.ReadUInt32(false)
                        let b = ls.ReadUInt32(false)
                        res.[i] <- (a, b)

                    return res
        }

    let private bytesToTimestampPair x =
        bytesToUint32Pair x
        |> Result.map(
            Array.map(fun (a, b) ->
                {
                    TwoTimestamps.NodeId1 = a
                    NodeId2 = b
                }
            )
        )

    let tryBytesToChecksumPair x =
        bytesToUint32Pair x
        |> Result.map(
            Array.map(fun (a, b) ->
                {
                    TwoChecksums.NodeId1 = a
                    NodeId2 = b
                }
            )
        )

    let bytesToChecksumPair = tryBytesToChecksumPair >> unwrap

    let tryDecodeTimestampPairs e d =
        result {
            let! bytes = tryDecode e d
            return! bytes |> bytesToTimestampPair
        }

    let decodeTimestampPairs encodingType (d: array<byte>) =
        tryDecodeTimestampPairs encodingType d |> unwrap

module private Encoder =
    let private encode (ty: EncodingType) (value: array<byte>) : array<byte> =
        if value.Length = 0 then
            [||]
        else
            match ty with
            | EncodingType.SortedPlain -> value
            | EncodingType.ZLib ->
                use outputMs = new MemoryStream()
                use ds = new DeflateStream(outputMs, CompressionMode.Compress)
                ds.Write(value, 0, value.Length)
                ds.Flush()
                ds.Close()
                outputMs.ToArray()
            | x -> failwithf "Unreachable! Unknown encoding type %A" x

    let encodeShortChannelIds ty (shortIds: array<ShortChannelId>) =
        let b = shortIds |> Array.map(fun i -> i.ToBytes()) |> Array.concat
        encode (ty) (b)

    let encodeQueryFlags ty (flags: array<QueryFlags>) =
        let b = flags |> Array.map(fun i -> i.ToBytes()) |> Array.concat
        encode (ty) (b)

    let encodeTimestampsPairs ty (timestampPairs: array<TwoTimestamps>) =
        let b =
            timestampPairs |> Array.map(fun i -> i.ToBytes()) |> Array.concat

        encode ty b
