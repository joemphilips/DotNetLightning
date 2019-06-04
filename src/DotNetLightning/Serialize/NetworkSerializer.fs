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
    let private serializeWhichChannel (w: BinaryWriter) (maybeId: WhichChannel) =
        match maybeId with
        | All -> 
            w.Write(Array.zeroCreate<byte> 32)
        | ChannelId (Primitives.ChannelId id) -> 
            w.Write(id.ToBytes())

    let private serializeWithLen (w: BinaryWriter) (data: byte[]) =
        w.Write((uint16)data.Length)
        w.Write(data)

    let private maybeSerializeWithLen
        (w: BinaryWriter) (data: byte[] option) =
        match data with
        | Some d -> serializeWithLen w (d)
        | None -> ()

    type BinaryWriter with
        member this.Write (data: byte[] option) =
            match data with | Some d -> this.Write(d) | None -> ()
        member this.Write(data: ShortChannelId) =
            this.Write(BitConverter.GetBytes(data.BlockHeight).AsSpan().Slice(1, 3).ToArray())
            this.Write(BitConverter.GetBytes(data.BlockIndex).AsSpan().Slice(1, 3).ToArray())
            this.Write(data.TxOutIndex)
        member this.Write(data: uint256, lendian: bool) =
            this.Write(data.ToBytes(lendian))
        member this.Write(data: PubKey) =
            this.Write(data.ToBytes())
        member this.Write(data: ECDSASignature) =
            this.Write(data.ToBytesCompact())
        member this.Write(data: RGB) =
            this.Write(data.Red)
            this.Write(data.Green)
            this.Write(data.Blue)
        member this.Write(data: DateTime) =
            failwith "not impl"


    let private serializeOnionPacket (w: BinaryWriter) (data: OnionPacket) =
        w.Write(data.Version)
        w.Write(data.PublicKey.ToBytes())
        w.Write(data.HopData)
        w.Write(data.HMAC.ToBytes())

    let Deserialize (input: BinaryReader): LightningMsg =
        failwith "not impl"

    let fromBytes(b: byte[]) =
        use ms = new MemoryStream(b)
        use reader = new BinaryReader(ms, System.Text.Encoding.ASCII)
        Deserialize reader

    let Serialize (output: #Stream) (s: LightningMsg) =
        use w = new BinaryWriter(output)
        match s with
        | (Init msg) ->
            w.Write((uint16)TypeFlag.Init)
            let l = msg.LocalFeatures.Value
            let g = msg.GlobalFeatures.Value
            serializeWithLen w l
            serializeWithLen w g
        | (Error msg) ->
            w.Write((uint16)TypeFlag.Error)
            serializeWhichChannel w (msg.ChannelId)
            serializeWithLen w msg.Data
        | (Ping msg) ->
            w.Write((uint16)TypeFlag.Ping)
            w.Write(msg.PongLen)
            w.Write(msg.BytesLen)
            w.Write(Array.zeroCreate<byte> ((int)msg.BytesLen))
        | (Pong msg) ->
            w.Write((uint16)TypeFlag.Pong)
            w.Write(msg.BytesLen)
            w.Write(Array.zeroCreate<byte> ((int)msg.BytesLen))
        | (OpenChannel msg) ->
            w.Write((uint16)TypeFlag.OpenChannel)
            w.Write(msg.Chainhash.ToBytes())
            w.Write(msg.TemporaryChannelId.Value.ToBytes())
            w.Write(msg.FundingSatoshis.Satoshi)
            w.Write(msg.PushMSat.MilliSatoshi)
            w.Write(msg.DustLimitSatoshis.Satoshi)
            w.Write(msg.MaxHTLCValueInFlightMsat.MilliSatoshi)
            w.Write(msg.ChannelReserveSatoshis.Satoshi)
            w.Write(msg.HTLCMinimumMsat.MilliSatoshi)
            w.Write(msg.FeeRatePerKw.Value.Satoshi)
            w.Write(msg.ToSelfDelay.Value)
            w.Write(msg.FundingPubKey.ToBytes())
            w.Write(msg.RevocationBasepoint.ToBytes())
            w.Write(msg.PaymentBasepoint.ToBytes())
            w.Write(msg.DelayedPaymentBasepoint.ToBytes())
            w.Write(msg.HTLCBasepoint.ToBytes())
            w.Write(msg.FirstPerCommitmentPoint.ToBytes())
            w.Write(msg.ChannelFlags)
            maybeSerializeWithLen w (msg.ShutdownScriptPubKey |> Option.map(fun x -> x.ToBytes()))
        | (AcceptChannel msg) ->
            w.Write((uint16)TypeFlag.AcceptChannel)
            w.Write(msg.TemporaryChannelId.Value.ToBytes())
            w.Write(msg.DustLimitSatoshis.Satoshi)
            w.Write(msg.MaxHTLCValueInFlightMsat.MilliSatoshi)
            w.Write(msg.ChannelReserveSatoshis.Satoshi)
            w.Write(msg.HTLCMinimumMSat.MilliSatoshi)
            w.Write(msg.MinimumDepth)
            w.Write(msg.ToSelfDelay.Value)
            w.Write(msg.MaxAcceptedHTLCs)
            w.Write(msg.FundingPubKey.ToBytes())
            w.Write(msg.RevocationBasepoint.ToBytes())
            w.Write(msg.PaymentBasepoint.ToBytes())
            w.Write(msg.DelayedPaymentBasepoint.ToBytes())
            w.Write(msg.HTLCBasepoint.ToBytes())
            w.Write(msg.FirstPerCommitmentPoint.ToBytes())
            maybeSerializeWithLen w (msg.ShutdownScriptPubKey |> Option.map(fun x -> x.ToBytes()))
        | (FundingCreated msg) ->
            w.Write((uint16)TypeFlag.FundingCreated)
            w.Write(msg.TemporaryChannelId.Value.ToBytes())
            w.Write(msg.FundingTxId.Value.ToBytes())
            w.Write(msg.FundingOutputIndex)
            w.Write(msg.Signature)
        | (FundingSigned msg) ->
            w.Write((uint16)TypeFlag.FundingSigned)
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.Signature)
        | (FundingLocked msg) ->
            w.Write((uint16)TypeFlag.FundingLocked)
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.NextPerCommitmentPoint.ToBytes())
        | (Shutdown msg) ->
            w.Write((uint16)TypeFlag.Shutdown)
            w.Write(msg.ChannelId.Value.ToBytes())
            serializeWithLen w (msg.ScriptPubKey.ToBytes())
        | (ClosingSigned msg) ->
            w.Write((uint16)TypeFlag.ClosingSigned)
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.FeeSatoshis.Satoshi)
            w.Write(msg.Signature)
        | (ChannelReestablish msg) ->
            // w.Write((uint16)TypeFlag.ChannelReestablish)
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(Utils.ToBytes(msg.NextLocalCommitmentNumber, false))
            w.Write(Utils.ToBytes(msg.NextRemoteCommitmentNumber, false))
            w.Write(msg.DataLossProtect |> Option.map(fun x -> x.YourLastPerCommitmentSecret.Value.ToBytes()))
            w.Write(msg.DataLossProtect |> Option.map(fun x -> x.MyCurrentPerCommitmentPoint.ToBytes()))
        | (UpdateAddHTLC msg) ->
            w.Write((uint16)TypeFlag.UpdateAddHTLC)
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.HTLCId.Value)
            w.Write(msg.AmountMSat.MilliSatoshi)
            w.Write(msg.PaymentHash.Value.ToBytes())
            w.Write(msg.CLTVExpiry)
            serializeOnionPacket w msg.OnionRoutingPacket
        | (UpdateFulfillHTLC msg) ->
            w.Write((uint16)TypeFlag.UpdateFulfillHTLC)
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.HTLCId.Value)
            w.Write(msg.PaymentPreimage.Value.ToBytes())
        | (UpdateFailHTLC msg) ->
            w.Write((uint16)TypeFlag.UpdateFailHTLC)
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.HTLCId.Value)
            serializeWithLen w (msg.Reason.Data)
        | (UpdateFailMalformedHTLC msg) ->
            w.Write((uint16)TypeFlag.UpdateFailMalformedHTLC)
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.HTLCId.Value)
            w.Write(msg.FailureCode.Value)
        | (CommitmentSigned msg) ->
            w.Write((uint16)TypeFlag.CommitmentSigned)
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.Signature)
            w.Write((uint16)msg.HTLCSignatures.Length)
            msg.HTLCSignatures |> List.iter (w.Write)
        | (RevokeAndACK msg) ->
            w.Write((uint16)TypeFlag.RevokeAndACK)
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.PerCommitmentSecret.Value.ToBytes())
            w.Write(msg.MyCurrentPerCommitmentPoint.ToBytes())
        | (UpdateFee msg) ->
            w.Write((uint16)TypeFlag.UpdateFee)
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.FeeratePerKW.Satoshi)
        | (AnnouncementSignatures msg) ->
            w.Write((uint16)TypeFlag.AnnouncementSignatures)
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.ShortChannelId)
        | (ChannelAnnouncement msg) ->
            w.Write((uint16)TypeFlag.ChannelAnnouncement)
            w.Write(msg.NodeSignature1)
            w.Write(msg.NodeSignature2)
            w.Write(msg.BitcoinSignature1)
            w.Write(msg.BitcoinSignature2)
            serializeWithLen w (msg.Contents.Features.Value)
            w.Write(msg.Contents.ChainHash, true)
            w.Write(msg.Contents.ShortChannelId)
            w.Write(msg.Contents.NodeId1.Value)
            w.Write(msg.Contents.NodeId2.Value)
            w.Write(msg.Contents.BitcoinKey1)
            w.Write(msg.Contents.BitcoinKey2)
        | (NodeAnnouncement msg) ->
            w.Write((uint16)TypeFlag.NodeAnnouncement)
            w.Write(msg.Signature)
            serializeWithLen w (msg.Contents.Features.Value)
            w.Write(msg.Contents.Timestamp)
            w.Write(msg.Contents.NodeId.Value)
            w.Write(msg.Contents.RGB)
            w.Write(msg.Contents.Alias, true)
            let addrLen = msg.Contents.Addresses |> List.sumBy(fun addr -> addr.Length)
            w.Write(addrLen)
            msg.Contents.Addresses
                |> List.iter(fun addr -> w.Write(addr.GetId()); addr.WriteTo(w))
        | (ChannelUpdate msg) ->
            w.Write(msg.Signature)
            w.Write(msg.Contents.ChainHash, true)
            w.Write(msg.Contents.ShortChannelId)
            w.Write(msg.Contents.Timestamp)
            w.Write(msg.Contents.Flags)
            w.Write(msg.Contents.CLTVExpiryDelta)
            w.Write(msg.Contents.HTLCMinimumMSat.MilliSatoshi)
            w.Write(msg.Contents.FeeBaseMSat.MilliSatoshi)
            w.Write(msg.Contents.FeeProportionalMillionths)
            w.Write(msg.Contents.HTLCMinimumMSat.MilliSatoshi)

    let toBytes (s: LightningMsg): byte[] =
        use ms = new MemoryStream()
        Serialize ms s
        ms.ToArray()

    type LightningMsg with
        member this.ToBytes() =
            toBytes(this)
        static member FromBytes(b: byte[]) =
            fromBytes(b)

    // type LightningParser =
           