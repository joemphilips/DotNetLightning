namespace DotNetLightning.Serialize.MsgsV2

open System
open DotNetLightning.Serialize
open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs

[<CLIMutable>]
type QueryShortChannelIds = {
    mutable ChainHash: uint256
    mutable ShortIdsEncodingType: EncodingType
    mutable ShortIds: ShortChannelId []
    mutable TLVs: QueryShortChannelIdsTLV []
}
    with
    interface ILightningSerializable<QueryShortChannelIds> with
        member this.Deserialize(ls) =
            this.ChainHash <- ls.ReadUInt256(false)
            let shortIdsWithFlag = ls.ReadWithLen()
            this.ShortIdsEncodingType <- LanguagePrimitives.EnumOfValue<byte, EncodingType>(shortIdsWithFlag.[0])
            let shortIds =
                Decoder.decodeShortChannelIds this.ShortIdsEncodingType (shortIdsWithFlag.[1..])
            let tlvs =
                ls.ReadTLVStream()
                |> Array.map(QueryShortChannelIdsTLV.FromGenericTLV)
            let numQueryFlags =
                tlvs
                |> Seq.filter(function QueryShortChannelIdsTLV.QueryFlags _ -> true | _ -> false)
                |> Seq.length
            if (shortIds.Length <> numQueryFlags) then
                raise <| FormatException("query_short_channel_ids have different length for short_ids and query_flags!")
            this.ShortIds <- shortIds
            this.TLVs <- tlvs
        member this.Serialize(ls) =
            ls.Write(this.ChainHash, false)
            ls.Write((byte)this.ShortIdsEncodingType)
            this.ShortIds |> Encoder.encodeShortChannelIds (this.ShortIdsEncodingType) |> ls.Write
            this.TLVs |> Array.map(fun tlv -> tlv.ToGenericTLV()) |> ls.WriteTLVStream

