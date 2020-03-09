module SerializationPropertyTests

open ResultUtils
open System.IO

open Expecto
open DotNetLightning.Utils
open DotNetLightning.Serialize
open DotNetLightning.Serialize.Msgs
open Generators

let config =
    { FsCheckConfig.defaultConfig with
            arbitrary = [ typeof<P2PMsgGenerators>; typeof<PrimitiveGenerators> ]
            maxTest = 300
        }

[<Tests>]
let testList1 =
    testList "PrimitivesSerializationPropertyTests" [
        testPropertyWithConfig config "ecdsa signature" <| fun (signature: LNECDSASignature) ->
            let actual = LNECDSASignature.FromBytesCompact(signature.ToBytesCompact(), false)
            Expect.equal actual signature (sprintf "failed with actual: %A \n expected: %A" (actual.ToBytesCompact()) (signature.ToBytesCompact()))
    ]

[<Tests>]
let testList2 =
    testList "SerializationPropertyTest" [
        testPropertyWithConfig config "init" <| fun (msg: Init) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "ping" <| fun (msg: Ping) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "pong" <| fun (msg: Pong) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "open_channel" <| fun (msg: OpenChannel) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "accept_channel" <| fun (msg: AcceptChannel) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "funding_created" <| fun (msg: FundingCreated) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "funding_signed" <| fun (msg: FundingSigned) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "funding_locked" <| fun (msg: FundingLocked) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "shutdown" <| fun (msg: Shutdown) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "closing_signed" <| fun (msg: ClosingSigned) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "onion_packet" <| fun (msg: OnionPacket) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "update_add_htlc" <| fun (msg: UpdateAddHTLC) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "update_fulfill_htlc" <| fun (msg: UpdateFulfillHTLC) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "update_fail_htlc" <| fun (msg: UpdateFailHTLC) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "update_fail_malformed_htlc" <| fun (msg: UpdateFailMalformedHTLC) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "commitment_signed" <| fun (msg: CommitmentSigned) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "revoke_and_ack" <| fun (msg: RevokeAndACK) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "update_fee" <| fun (msg: UpdateFee) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "channel_reestablish" <| fun (msg: ChannelReestablish) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "announcement_signatures" <| fun (msg: AnnouncementSignatures) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "node_announcement(unsigned contents)" <| fun (msg: UnsignedNodeAnnouncement) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "node_announcement" <| fun (msg: NodeAnnouncement) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "channel_announcement" <| fun (msg: ChannelAnnouncement) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "channel_update" <| fun (msg: ChannelUpdate) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "query_short_channel_ids" <| fun (msg: QueryShortChannelIds) ->
            Expect.equal (msg.Clone()) (msg) ""
        
        testPropertyWithConfig config "reply_short_channel_ids" <| fun (msg: ReplyShortChannelIdsEnd) ->
            Expect.equal (msg.Clone()) (msg) ""
            
        testPropertyWithConfig config "query_channel_range" <| fun (msg: QueryChannelRange) ->
            Expect.equal (msg.Clone()) (msg) ""
            
        testPropertyWithConfig config "reply_channel_range" <| fun (msg: ReplyChannelRange) ->
            Expect.equal (msg.Clone()) (msg) ""
            
        testPropertyWithConfig config "gossip_timestamp_filter" <| fun (msg: GossipTimestampFilter) ->
            Expect.equal (msg.Clone()) (msg) ""
            
        testPropertyWithConfig config "lightning p2p msg" <| fun (msg: ILightningMsg) ->
            use ms = new MemoryStream()
            use lws = new LightningWriterStream(ms)
            ILightningSerializable.serializeWithFlags (lws) (msg)
            let b = ms.ToArray()
            use ms2 = new MemoryStream(b)
            use lrs = new LightningReaderStream(ms2)
            let actual = ILightningSerializable.deserializeWithFlag (lrs)
            Expect.equal (actual) (msg) ""
            
        testPropertyWithConfig config "lightning p2p msg 2" <| fun (msg: ILightningMsg) ->
            let actualR = LightningMsg.fromBytes(msg.ToBytes())
            match actualR with
            | Ok x -> Expect.equal (msg) x
            | Error ex ->
                failwithf "failed to decode %A" ex
                
        testPropertyWithConfig config "onion payloads" <| fun (payload: OnionPayload) ->
            let b = payload.ToBytes()
            let f = OnionPayload.FromBytes(b) |> Result.deref
            Expect.equal f payload ""
            
    ]