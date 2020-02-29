namespace DotNetLightning.Serialize.Msgs

open System
open System
open DotNetLightning.Serialize
open NBitcoin
open DotNetLightning.Utils

[<CLIMutable>]
type QueryShortChannelIds = {
    mutable ChainHash: uint256
    mutable ShortIdsEncodingType: EncodingType
    mutable ShortIds: ShortChannelId []
    mutable TLVs: QueryShortChannelIdsTLV []
}
    with
    interface ILightningSerializable<QueryShortChannelIds> with
        member this.Deserialize(ls: LightningReaderStream) =
            this.ChainHash <- ls.ReadUInt256(false)
            let shortIdsWithFlag = ls.ReadWithLen()
            this.ShortIdsEncodingType <- LanguagePrimitives.EnumOfValue<byte, EncodingType>(shortIdsWithFlag.[0])
            let shortIds =
                Decoder.decodeShortChannelIds this.ShortIdsEncodingType (shortIdsWithFlag.[1..])
            let tlvs =
                ls.ReadTLVStream()
                |> Array.map(QueryShortChannelIdsTLV.FromGenericTLV)
            let queryFlags =
                tlvs
                |> Seq.choose(function QueryShortChannelIdsTLV.QueryFlags (_, y) -> Some (y) | _ -> None)
                |> Seq.tryExactlyOne
            match queryFlags with
            | None -> raise <| FormatException("no query flags")
            | Some flags ->
                if (shortIds.Length <> (flags |> Seq.length)) then
                    raise <| FormatException(sprintf "query_short_channel_ids have different length for short_ids(%A) and query_flags! (%A)" shortIds flags)
                this.ShortIds <- shortIds
                this.TLVs <- tlvs
        member this.Serialize(ls) =
            ls.Write(this.ChainHash, false)
            let encodedIds = this.ShortIds |> Encoder.encodeShortChannelIds (this.ShortIdsEncodingType)
            [[|(byte)this.ShortIdsEncodingType|]; encodedIds]
            |> Array.concat
            |> ls.WriteWithLen
            this.TLVs |> Array.map(fun tlv -> tlv.ToGenericTLV()) |> ls.WriteTLVStream

