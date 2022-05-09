namespace DotNetLightning.Serialization

open System.IO

open ResultUtils
open ResultUtils.Portability

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

    member private this.Value = let (QueryFlags v) = this in v

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

    member private this.Value = let (QueryOption v) = this in v
    member this.SenderWantsTimestamps = (this.Value &&& 0b00000001uy) = 1uy
    member this.SenderWantsChecksums = (this.Value &&& 0b00000010uy) = 1uy

    member this.ToBytes() =
        [| (byte) this.Value |]

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
