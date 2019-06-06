namespace DotNetLightning.Serialize
open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Utils.NBitcoinExtensions
open System.IO
open System
open NBitcoin
open NBitcoin.Crypto

[<AutoOpen>]
module NetworkSerializer =
    let private serializeWhichChannel (w: LightningStream) (maybeId: WhichChannel) =
        match maybeId with
        | All -> 
            w.Write(Array.zeroCreate<byte> 32)
        | ChannelId (Primitives.ChannelId id) -> 
            w.Write(id.ToBytes())


    let Deserialize (input: BinaryReader): LightningMsg =
        failwith "not impl"

    let fromBytes(b: byte[]) =
        use ms = new MemoryStream(b)
        use reader = new BinaryReader(ms, System.Text.Encoding.ASCII)
        Deserialize reader

    let Serialize (w: LightningStream) (s: LightningMsg) =
        match s with
        | (Init msg) ->
            let l: byte[] = msg.LocalFeatures.Value
            let g: byte[] = msg.GlobalFeatures.Value
            w.WriteWithLen(l)
            w.WriteWithLen(g)
        | (Error msg) ->
            serializeWhichChannel w (msg.ChannelId)
            w.WriteWithLen(msg.Data)
        | (Ping msg) ->
            w.Write(msg.PongLen, false)
            w.Write(msg.BytesLen, false)
            w.Write(Array.zeroCreate<byte> ((int)msg.BytesLen))
        | (Pong msg) ->
            w.Write(msg.BytesLen, false)
            w.Write(Array.zeroCreate<byte> ((int)msg.BytesLen))
        | (OpenChannel msg) ->
            w.Write(msg.Chainhash.ToBytes())
            w.Write(msg.TemporaryChannelId.Value.ToBytes())
            w.Write(msg.FundingSatoshis.Satoshi, false)
            w.Write(msg.PushMSat.MilliSatoshi, false)
            w.Write(msg.DustLimitSatoshis.Satoshi, false)
            w.Write(msg.MaxHTLCValueInFlightMsat.MilliSatoshi, false)
            w.Write(msg.ChannelReserveSatoshis.Satoshi, false)
            w.Write(msg.HTLCMinimumMsat.MilliSatoshi, false)
            w.Write(msg.FeeRatePerKw.Value.Satoshi, false)
            w.Write(msg.ToSelfDelay.Value, false)
            w.Write(msg.FundingPubKey.ToBytes())
            w.Write(msg.RevocationBasepoint.ToBytes())
            w.Write(msg.PaymentBasepoint.ToBytes())
            w.Write(msg.DelayedPaymentBasepoint.ToBytes())
            w.Write(msg.HTLCBasepoint.ToBytes())
            w.Write(msg.FirstPerCommitmentPoint.ToBytes())
            w.Write(msg.ChannelFlags)
            w.WriteWithLen(msg.ShutdownScriptPubKey |> Option.map(fun x -> x.ToBytes()))
        | (AcceptChannel msg) ->
            w.Write(msg.TemporaryChannelId.Value.ToBytes())
            w.Write(msg.DustLimitSatoshis.Satoshi, false)
            w.Write(msg.MaxHTLCValueInFlightMsat.MilliSatoshi, false)
            w.Write(msg.ChannelReserveSatoshis.Satoshi, false)
            w.Write(msg.HTLCMinimumMSat.MilliSatoshi, false)
            w.Write(msg.MinimumDepth, false)
            w.Write(msg.ToSelfDelay.Value, false)
            w.Write(msg.MaxAcceptedHTLCs, false)
            w.Write(msg.FundingPubKey.ToBytes())
            w.Write(msg.RevocationBasepoint.ToBytes())
            w.Write(msg.PaymentBasepoint.ToBytes())
            w.Write(msg.DelayedPaymentBasepoint.ToBytes())
            w.Write(msg.HTLCBasepoint.ToBytes())
            w.Write(msg.FirstPerCommitmentPoint.ToBytes())
            w.WriteWithLen(msg.ShutdownScriptPubKey |> Option.map(fun x -> x.ToBytes()))
        | (FundingCreated msg) ->
            w.Write(msg.TemporaryChannelId.Value.ToBytes())
            w.Write(msg.FundingTxId.Value.ToBytes())
            w.Write(msg.FundingOutputIndex, false)
            w.Write(msg.Signature)
        | (FundingSigned msg) ->
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.Signature)
        | (FundingLocked msg) ->
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.NextPerCommitmentPoint.ToBytes())
        | (Shutdown msg) ->
            w.Write(msg.ChannelId.Value.ToBytes())
            w.WriteWithLen(msg.ScriptPubKey.ToBytes())
        | (ClosingSigned msg) ->
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.FeeSatoshis.Satoshi, false)
            w.Write(msg.Signature)
        | (ChannelReestablish msg) ->
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(Utils.ToBytes(msg.NextLocalCommitmentNumber, false))
            w.Write(Utils.ToBytes(msg.NextRemoteCommitmentNumber, false))
            w.Write(msg.DataLossProtect |> Option.map(fun x -> x.YourLastPerCommitmentSecret.Value.ToBytes()))
            w.Write(msg.DataLossProtect |> Option.map(fun x -> x.MyCurrentPerCommitmentPoint.ToBytes()))
        | (UpdateAddHTLC msg) ->
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.HTLCId.Value, false)
            w.Write(msg.AmountMSat.MilliSatoshi, false)
            w.Write(msg.PaymentHash.Value.ToBytes())
            w.Write(msg.CLTVExpiry, false)
            w.Write(msg.OnionRoutingPacket)
        | (UpdateFulfillHTLC msg) ->
            w.Write((uint16)TypeFlag.UpdateFulfillHTLC, false)
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.HTLCId.Value, false)
            w.Write(msg.PaymentPreimage.Value.ToBytes())
        | (UpdateFailHTLC msg) ->
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.HTLCId.Value, false)
            w.WriteWithLen(msg.Reason.Data)
        | (UpdateFailMalformedHTLC msg) ->
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.HTLCId.Value, false)
            w.Write(msg.FailureCode.Value, false)
        | (CommitmentSigned msg) ->
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.Signature)
            w.Write((uint16)msg.HTLCSignatures.Length, false)
            msg.HTLCSignatures |> List.iter (w.Write)
        | (RevokeAndACK msg) ->
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.PerCommitmentSecret.Value.ToBytes())
            w.Write(msg.NextPerCommitmentPoint.ToBytes())
        | (UpdateFee msg) ->
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.FeeratePerKW.Value.Satoshi, false)
        | (AnnouncementSignatures msg) ->
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.ShortChannelId)
            w.Write(msg.NodeSignature)
            w.Write(msg.BitcoinSignature)
        | (ChannelAnnouncement msg) ->
            w.Write(msg.NodeSignature1)
            w.Write(msg.NodeSignature2)
            w.Write(msg.BitcoinSignature1)
            w.Write(msg.BitcoinSignature2)
            w.WriteWithLen(msg.Contents.Features.Value)
            w.Write(msg.Contents.ChainHash, false)
            w.Write(msg.Contents.ShortChannelId)
            w.Write(msg.Contents.NodeId1.Value)
            w.Write(msg.Contents.NodeId2.Value)
            w.Write(msg.Contents.BitcoinKey1)
            w.Write(msg.Contents.BitcoinKey2)
            w.Write(msg.Contents.ExcessData)
        | (NodeAnnouncement msg) ->
            w.Write(msg.Signature)
            w.WriteWithLen(msg.Contents.Features.Value)
            w.Write(msg.Contents.Timestamp, false)
            w.Write(msg.Contents.NodeId.Value)
            w.Write(msg.Contents.RGB)
            w.Write(msg.Contents.Alias, true)
            let mutable addrLen = (msg.Contents.Addresses |> List.sumBy(fun addr -> addr.Length + 1us)) // 1 byte for type field
            let excessAddrLen = (uint16 msg.Contents.ExcessAddressData.Length)
            addrLen <- excessAddrLen + addrLen
            w.Write(addrLen, false)
            msg.Contents.Addresses
                |> List.iter(fun addr -> w.Write(addr.GetId()); w.Write(addr))
            w.Write(msg.Contents.ExcessAddressData)
            w.Write(msg.Contents.ExcessData)
        | (ChannelUpdate msg) ->
            w.Write(msg.Signature)
            w.Write(msg.Contents.ChainHash, false)
            w.Write(msg.Contents.ShortChannelId)
            w.Write(msg.Contents.Timestamp, false)
            w.Write(msg.Contents.Flags, false)
            w.Write(msg.Contents.CLTVExpiryDelta.Value, false)
            w.Write(msg.Contents.HTLCMinimumMSat.MilliSatoshi, false)
            w.Write((uint32) msg.Contents.FeeBaseMSat.MilliSatoshi, false)
            w.Write((uint32) msg.Contents.FeeProportionalMillionths, false)
            w.Write(msg.Contents.ExcessData)

    let toBytes (s: LightningMsg): byte[] =
        use ms = new MemoryStream()
        use ls = new LightningStream(ms)
        Serialize ls s
        ms.ToArray()

    type LightningMsg with
        member this.ToBytes() =
            toBytes(this)
        static member FromBytes(b: byte[]) =
            fromBytes(b)

    // type LightningParser =
           