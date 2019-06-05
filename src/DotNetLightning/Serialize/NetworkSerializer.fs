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

    type BinaryWriter with
        member this.Write (data: byte[] option) =
            match data with | Some d -> this.Write(d) | None -> ()
        member this.Write(data: ShortChannelId) =
            this.Write(data.ToBytes())
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
        member this.Write(data: uint32, lendian: bool) =
            let d = BitConverter.GetBytes(data)
            Array.Reverse(d)
            this.Write(d)
        member this.WriteWithLen(data: byte[]) =
            let length = BitConverter.GetBytes((uint16)data.Length)
            Array.Reverse(length)
            this.Write(length)
            this.Write(data)
        member this.Write(data: DateTime) =
            failwith "not impl"

    let private maybeSerializeWithLen
        (w: BinaryWriter) (data: byte[] option) =
        match data with
        | Some d -> w.WriteWithLen(d)
        | None -> ()


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
            let l = msg.LocalFeatures.Value
            let g = msg.GlobalFeatures.Value
            w.WriteWithLen(l)
            w.WriteWithLen(g)
        | (Error msg) ->
            serializeWhichChannel w (msg.ChannelId)
            w.WriteWithLen(msg.Data)
        | (Ping msg) ->
            w.Write(msg.PongLen)
            w.Write(msg.BytesLen)
            w.Write(Array.zeroCreate<byte> ((int)msg.BytesLen))
        | (Pong msg) ->
            w.Write(msg.BytesLen)
            w.Write(Array.zeroCreate<byte> ((int)msg.BytesLen))
        | (OpenChannel msg) ->
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
            w.Write(msg.TemporaryChannelId.Value.ToBytes())
            w.Write(msg.FundingTxId.Value.ToBytes())
            w.Write(msg.FundingOutputIndex)
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
            w.Write(msg.FeeSatoshis.Satoshi)
            w.Write(msg.Signature)
        | (ChannelReestablish msg) ->
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(Utils.ToBytes(msg.NextLocalCommitmentNumber, false))
            w.Write(Utils.ToBytes(msg.NextRemoteCommitmentNumber, false))
            w.Write(msg.DataLossProtect |> Option.map(fun x -> x.YourLastPerCommitmentSecret.Value.ToBytes()))
            w.Write(msg.DataLossProtect |> Option.map(fun x -> x.MyCurrentPerCommitmentPoint.ToBytes()))
        | (UpdateAddHTLC msg) ->
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
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.HTLCId.Value)
            w.WriteWithLen(msg.Reason.Data)
        | (UpdateFailMalformedHTLC msg) ->
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.HTLCId.Value)
            w.Write(msg.FailureCode.Value)
        | (CommitmentSigned msg) ->
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.Signature)
            w.Write((uint16)msg.HTLCSignatures.Length)
            msg.HTLCSignatures |> List.iter (w.Write)
        | (RevokeAndACK msg) ->
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.PerCommitmentSecret.Value.ToBytes())
            w.Write(msg.MyCurrentPerCommitmentPoint.ToBytes())
        | (UpdateFee msg) ->
            w.Write(msg.ChannelId.Value.ToBytes())
            w.Write(msg.FeeratePerKW.Satoshi)
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
            let addrLen = msg.Contents.Addresses |> List.sumBy(fun addr -> addr.Length)
            w.Write(addrLen)
            msg.Contents.Addresses
                |> List.iter(fun addr -> w.Write(addr.GetId()); addr.WriteTo(w))
        | (ChannelUpdate msg) ->
            w.Write(msg.Signature)
            w.Write(msg.Contents.ChainHash, false)
            w.Write(msg.Contents.ShortChannelId)
            w.Write(msg.Contents.Timestamp, false)
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
           