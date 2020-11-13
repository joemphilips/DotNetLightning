module SerializationPropertyTests

open System.IO

open Expecto
open DotNetLightning.Utils
open DotNetLightning.Serialization
open DotNetLightning.Serialization.Msgs
open Generators

open ResultUtils
open ResultUtils.Portability

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
        testPropertyWithConfig config "init" <| fun (msg: InitMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "ping" <| fun (msg: PingMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "pong" <| fun (msg: PongMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "open_channel" <| fun (msg: OpenChannelMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "accept_channel" <| fun (msg: AcceptChannelMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "funding_created" <| fun (msg: FundingCreatedMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "funding_signed" <| fun (msg: FundingSignedMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "funding_locked" <| fun (msg: FundingLockedMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "shutdown" <| fun (msg: ShutdownMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "closing_signed" <| fun (msg: ClosingSignedMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "onion_packet" <| fun (msg: OnionPacket) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "update_add_htlc" <| fun (msg: UpdateAddHTLCMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "update_fulfill_htlc" <| fun (msg: UpdateFulfillHTLCMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "update_fail_htlc" <| fun (msg: UpdateFailHTLCMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "update_fail_malformed_htlc" <| fun (msg: UpdateFailMalformedHTLCMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "commitment_signed" <| fun (msg: CommitmentSignedMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "revoke_and_ack" <| fun (msg: RevokeAndACKMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "update_fee" <| fun (msg: UpdateFeeMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "channel_reestablish" <| fun (msg: ChannelReestablishMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "announcement_signatures" <| fun (msg: AnnouncementSignaturesMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "node_announcement(unsigned contents)" <| fun (msg: UnsignedNodeAnnouncementMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "node_announcement" <| fun (msg: NodeAnnouncementMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "channel_announcement" <| fun (msg: ChannelAnnouncementMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "channel_update" <| fun (msg: ChannelUpdateMsg) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "query_short_channel_ids" <| fun (msg: QueryShortChannelIdsMsg) ->
            Expect.equal (msg.Clone()) (msg) ""
        
        testPropertyWithConfig config "reply_short_channel_ids" <| fun (msg: ReplyShortChannelIdsEndMsg) ->
            Expect.equal (msg.Clone()) (msg) ""
            
        testPropertyWithConfig config "query_channel_range" <| fun (msg: QueryChannelRangeMsg) ->
            Expect.equal (msg.Clone()) (msg) ""
            
        testPropertyWithConfig config "reply_channel_range" <| fun (msg: ReplyChannelRangeMsg) ->
            Expect.equal (msg.Clone()) (msg) ""
            
        testPropertyWithConfig config "gossip_timestamp_filter" <| fun (msg: GossipTimestampFilterMsg) ->
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
