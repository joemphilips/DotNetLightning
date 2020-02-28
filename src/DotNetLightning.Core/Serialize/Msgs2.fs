namespace DotNetLightning.Serialize.MsgsV2

open System
open DotNetLightning.Serialize
open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs

[<CLIMutable>]
type QueryShortChannelIds = {
    mutable ChainHash: uint256
    mutable ShortIds: ShortChannelId []
    mutable TLVs: QueryShortChannelIdsTLV []
}
    with
    interface ILightningSerializable<QueryShortChannelIds> with
        member this.Deserialize(ls) =
            this.ChainHash <- ls.ReadUInt256(false)
            let shortIds =
                ls.ReadWithLen()
                |> Encoder.decodeShortChannelIds
            let tlvs =
                ls.ReadTLVStream()
                |> Array.map(QueryShortChannelIdsTLV.FromGenericTLV)
            let numQueryFlags =
                tlvs
                |> Seq.filter(function QueryFlags _ -> true | _ -> false)
                |> Seq.length
            if (shortIds.Length <> numQueryFlags) then
                raise <| FormatException("query_short_channel_ids have different length for short_ids and query_flags!")
            this.ShortIds <- shortIds
            this.TLVs <- tlvs
        member this.Serialize(ls) =
            ls.Write(this.ChainHash, false)
            this.ShortIds |> Array.iter(Encoder.encode<ShortChannelId> >> ls.Write)
            this.TLVs |> Array.map(fun tlv -> tlv.ToGenericTLV()) |> ls.WriteTLVStream

