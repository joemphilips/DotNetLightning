namespace DotNetLightning.Serialize.MsgsV2

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs

module Helpers =
    let decode(bytes : byte[]) =
        failwith ""
        
    let encode<'T>(object: 'T): byte[] =
        failwith ""

[<CLIMutable>]
type QueryShortChannelIds = {
    mutable ChainHash: uint256
    mutable ShortIds: ShortChannelId array
    mutable TLVs: QueryShortChannelIdsTLV []
}
    with
    interface ILightningSerializable<QueryShortChannelIds> with
        member this.Deserialize(ls) =
            this.ChainHash <- ls.ReadUInt256(false)
            this.ShortIds <-
                let bytes =
                    ls.ReadWithLen()
                bytes |> Helpers.decode
            this.TLVs <- ls.ReadTLVStream() |> Array.map(QueryShortChannelIdsTLV.FromGenericTLV)
        member this.Serialize(ls) =
            ls.Write(this.ChainHash, false)
            this.ShortIds |> Array.iter(Helpers.encode<ShortChannelId> >> ls.Write)
            this.TLVs |> Array.map(fun tlv -> tlv.ToGenericTLV()) |> ls.WriteTLVStream

