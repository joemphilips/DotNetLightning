namespace DotNetLightning.Serialization

open System.IO

open ResultUtils
open ResultUtils.Portability

/// See `query_short_channel_ids` msg description in
/// [bolt07](https://github.com/lightning/bolts/blob/master/07-routing-gossip.md)
/// For the detail
type QueryFlags =
    private
    | QueryFlags of uint8

    static member Create data =
        QueryFlags(data)

    static member TryCreate(data: uint64) =
        if data > 0xfcUL then
            Error(
                sprintf
                    "Too large query flag! It must be represented as 1 byte, but it was %A"
                    data
            )
        else
            QueryFlags(uint8 data) |> Ok

    member private x.Value = let (QueryFlags v) = x in v

    member this.RequiresChannelAnnouncement =
        (this.Value &&& 0b00000001uy) = 1uy

    member this.RequiresChannelUpdateForNode1 =
        (this.Value &&& 0b00000010uy) = 1uy

    member this.RequiresChannelUpdateForNode2 =
        (this.Value &&& 0b00000100uy) = 1uy

    member this.RequiresNodeAnnouncementForNode1 =
        (this.Value &&& 0b00001000uy) = 1uy

    member this.RequiresNodeAnnouncementForNode2 =
        (this.Value &&& 0b00010000uy) = 1uy

    member this.ToBytes() =
        [| (byte) this.Value |]

/// See `query_channel_range` msg description in
/// [bolt07](https://github.com/lightning/bolts/blob/master/07-routing-gossip.md)
/// For the detail
type QueryOption =
    private
    | QueryOption of uint8

    static member Create data =
        QueryOption(data)

    static member TryCreate(data: uint64) =
        if data > 0xfcUL then
            Error(
                sprintf
                    "Too large query flag! It must be represented as 1 byte, but it was %A"
                    data
            )
        else
            QueryFlags(uint8 data) |> Ok

    member private x.Value = let (QueryOption v) = x in v
    member this.SenderWantsTimestamps = (this.Value &&& 0b00000001uy) = 1uy
    member this.SenderWantsChecksums = (this.Value &&& 0b00000010uy) = 1uy

    member this.ToBytes() =
        [| (byte) this.Value |]

/// This is a type described in [bolt07](https://github.com/lightning/bolts/blob/master/07-routing-gossip.md) as
/// `channel_update_timestamps`
type TwoTimestamps =
    {
        NodeId1: uint32
        NodeId2: uint32
    }

    member this.ToBytes() =
        use ms = new MemoryStream()
        use ls = new LightningWriterStream(ms)
        ls.Write(this.NodeId1, false)
        ls.Write(this.NodeId2, false)
        ms.ToArray()

/// This is a type described in [bolt07](https://github.com/lightning/bolts/blob/master/07-routing-gossip.md) as
/// `channel_update_checksums`
type TwoChecksums =
    {
        NodeId1: uint32
        NodeId2: uint32
    }

    member this.ToBytes() =
        use ms = new MemoryStream()
        use ls = new LightningWriterStream(ms)
        ls.Write(this.NodeId1, false)
        ls.Write(this.NodeId2, false)
        ms.ToArray()
