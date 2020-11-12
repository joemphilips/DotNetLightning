namespace rec DotNetLightning.Serialization.Msgs

open System
open System.IO
open System.Runtime.CompilerServices
open System.Linq

open NBitcoin

open DotNetLightning.Utils
open DotNetLightning.Utils.Primitives
open DotNetLightning.Utils.OnionError
open DotNetLightning.Core.Utils.Extensions

open DotNetLightning.Serialization

// #region serialization
type P2PDecodeError =
    | UnknownVersion
    | FeatureError of FeatureError
    | InvalidValue
    | ExtraAddressesPerType
    | BadLengthDescriptor
    | UnexpectedEndOfStream of EndOfStreamException
    | IO of IOException
    
    member this.Message =
        match this with
        | UnknownVersion ->
            "Unknown version"
        | FeatureError featureError ->
            sprintf "Feature error: %s" featureError.Message
        | InvalidValue ->
            "Invalid value"
        | ExtraAddressesPerType ->
            "Extra address per type"
        | BadLengthDescriptor ->
            "Bad length descriptor"
        | UnexpectedEndOfStream endOfStreamException ->
            sprintf "Unexpected end of stream: %s" (endOfStreamException.Message)
        | IO ioException ->
            sprintf "IO error: %s" ioException.Message

type UnknownVersionException(msg) =
    inherit FormatException(msg)

module internal TypeFlag =
    [<Literal>]
    let Init = 16us
    [<Literal>]
    let Error = 17us
    [<Literal>]
    let Ping = 18us
    [<Literal>]
    let Pong = 19us
    [<Literal>]
    let OpenChannel = 32us
    [<Literal>]
    let AcceptChannel = 33us
    [<Literal>]
    let FundingCreated = 34us
    [<Literal>]
    let FundingSigned = 35us
    [<Literal>]
    let FundingLocked = 36us
    [<Literal>]
    let Shutdown = 38us
    [<Literal>]
    let ClosingSigned = 39us
    [<Literal>]
    let UpdateAddHTLC = 128us
    [<Literal>]
    let UpdateFulfillHTLC = 130us
    [<Literal>]
    let UpdateFailHTLC = 131us
    [<Literal>]
    let UpdateFailMalformedHTLC = 135us
    [<Literal>]
    let ChannelReestablish = 136us
    [<Literal>]
    let CommitmentSigned = 132us
    [<Literal>]
    let RevokeAndACK = 133us
    [<Literal>]
    let UpdateFee = 134us
    [<Literal>]
    let AnnouncementSignatures = 259us
    [<Literal>]
    let ChannelAnnouncement = 256us
    [<Literal>]
    let NodeAnnouncement = 257us
    [<Literal>]
    let ChannelUpdate = 258us
    [<Literal>]
    let QueryShortChannelIds = 261us
    [<Literal>]
    let ReplyShortChannelIdsEnd = 262us
    [<Literal>]
    let QueryChannelRange = 263us
    [<Literal>]
    let ReplyChannelRange = 264us
    [<Literal>]
    let GossipTimestampFilter = 265us
    [<Literal>]
    let MonoHopUnidirectionalPayment = 42198us

type ILightningMsg = interface end
type ISetupMsg = inherit ILightningMsg
type IChannelMsg = inherit ILightningMsg
type IHTLCMsg = inherit IChannelMsg
type IUpdateMsg = inherit IChannelMsg
type IRoutingMsg = inherit ILightningMsg
type IQueryMsg = inherit IRoutingMsg

// #endregion 

/// Should throw `FormatException` when it fails
type ILightningSerializable<'T when 'T: (new: unit -> 'T) and 'T :> ILightningSerializable<'T>> =
    abstract Deserialize: LightningReaderStream -> unit
    abstract Serialize: LightningWriterStream -> unit

module ILightningSerializable =
    let internal fromBytes<'T when 'T :(new :unit -> 'T) and 'T :> ILightningSerializable<'T>>(data: byte[]) = 
        use ms = new MemoryStream(data)
        use ls = new LightningReaderStream(ms)
        let instance = new 'T()
        instance.Deserialize(ls)
        instance

    let internal init<'T when 'T :(new :unit -> 'T) and 'T :> ILightningSerializable<'T>>() =
        new 'T()

    let internal deserialize<'T when 'T :(new :unit -> 'T) and 'T :> ILightningSerializable<'T>>(ls: LightningReaderStream) =
        let instance = new 'T()
        instance.Deserialize(ls)
        instance

    let internal deserializeWithFlag(ls: LightningReaderStream): ILightningMsg =
        let t = ls.ReadUInt16(false)
        match t with
        | TypeFlag.Init ->
            deserialize<InitMsg>(ls) :> ILightningMsg
        | TypeFlag.Error ->
            deserialize<ErrorMsg>(ls) :> ILightningMsg
        | TypeFlag.Ping ->
            deserialize<PingMsg>(ls) :> ILightningMsg
        | TypeFlag.Pong ->
            deserialize<PongMsg>(ls) :> ILightningMsg
        | TypeFlag.OpenChannel ->
            deserialize<OpenChannelMsg>(ls) :> ILightningMsg
        | TypeFlag.AcceptChannel ->
            deserialize<AcceptChannelMsg>(ls) :> ILightningMsg
        | TypeFlag.FundingCreated ->
            deserialize<FundingCreatedMsg>(ls) :> ILightningMsg
        | TypeFlag.FundingSigned ->
            deserialize<FundingSignedMsg>(ls) :> ILightningMsg
        | TypeFlag.FundingLocked ->
            deserialize<FundingLockedMsg>(ls) :> ILightningMsg
        | TypeFlag.Shutdown ->
            deserialize<ShutdownMsg>(ls) :> ILightningMsg
        | TypeFlag.ClosingSigned ->
            deserialize<ClosingSignedMsg>(ls) :> ILightningMsg
        | TypeFlag.UpdateAddHTLC ->
            deserialize<UpdateAddHTLCMsg>(ls) :> ILightningMsg
        | TypeFlag.UpdateFulfillHTLC ->
            deserialize<UpdateFulfillHTLCMsg>(ls) :> ILightningMsg
        | TypeFlag.UpdateFailHTLC ->
            deserialize<UpdateFailHTLCMsg>(ls) :> ILightningMsg
        | TypeFlag.UpdateFailMalformedHTLC ->
            deserialize<UpdateFailMalformedHTLCMsg>(ls) :> ILightningMsg
        | TypeFlag.ChannelReestablish ->
            deserialize<ChannelReestablishMsg>(ls) :> ILightningMsg
        | TypeFlag.CommitmentSigned ->
            deserialize<CommitmentSignedMsg>(ls) :> ILightningMsg
        | TypeFlag.RevokeAndACK ->
            deserialize<RevokeAndACKMsg>(ls) :> ILightningMsg
        | TypeFlag.UpdateFee ->
            deserialize<UpdateFeeMsg>(ls) :> ILightningMsg
        | TypeFlag.AnnouncementSignatures ->
            deserialize<AnnouncementSignaturesMsg>(ls) :> ILightningMsg
        | TypeFlag.ChannelAnnouncement ->
            deserialize<ChannelAnnouncementMsg>(ls) :> ILightningMsg
        | TypeFlag.NodeAnnouncement ->
            deserialize<NodeAnnouncementMsg>(ls) :> ILightningMsg
        | TypeFlag.ChannelUpdate ->
            deserialize<ChannelUpdateMsg>(ls) :> ILightningMsg
        | TypeFlag.QueryShortChannelIds ->
            deserialize<QueryShortChannelIdsMsg>(ls) :> ILightningMsg
        | TypeFlag.ReplyShortChannelIdsEnd ->
            deserialize<ReplyShortChannelIdsEndMsg>(ls) :> ILightningMsg
        | TypeFlag.QueryChannelRange ->
            deserialize<QueryChannelRangeMsg>(ls) :> ILightningMsg
        | TypeFlag.ReplyChannelRange ->
            deserialize<ReplyChannelRangeMsg>(ls) :> ILightningMsg
        | TypeFlag.GossipTimestampFilter ->
            deserialize<GossipTimestampFilterMsg>(ls) :> ILightningMsg
        | TypeFlag.MonoHopUnidirectionalPayment ->
            deserialize<MonoHopUnidirectionalPaymentMsg>(ls) :> ILightningMsg
        | x ->
            raise <| FormatException(sprintf "Unknown message type %d" x)
    let serializeWithFlags (ls: LightningWriterStream) (data: ILightningMsg) =
        match data with
        | :? InitMsg as d ->
            ls.Write(TypeFlag.Init, false)
            (d :> ILightningSerializable<InitMsg>).Serialize(ls)
        | :? ErrorMsg as d ->
            ls.Write(TypeFlag.Error, false)
            (d :> ILightningSerializable<ErrorMsg>).Serialize(ls)
        | :? PingMsg as d ->
            ls.Write(TypeFlag.Ping, false)
            (d :> ILightningSerializable<PingMsg>).Serialize(ls)
        | :? PongMsg as d ->
            ls.Write(TypeFlag.Pong, false)
            (d :> ILightningSerializable<PongMsg>).Serialize(ls)
        | :? OpenChannelMsg as d ->
            ls.Write(TypeFlag.OpenChannel, false)
            (d :> ILightningSerializable<OpenChannelMsg>).Serialize(ls)
        | :? AcceptChannelMsg as d ->
            ls.Write(TypeFlag.AcceptChannel, false)
            (d :> ILightningSerializable<AcceptChannelMsg>).Serialize(ls)
        | :? FundingCreatedMsg as d ->
            ls.Write(TypeFlag.FundingCreated, false)
            (d :> ILightningSerializable<FundingCreatedMsg>).Serialize(ls)
        | :? FundingSignedMsg as d ->
            ls.Write(TypeFlag.FundingSigned, false)
            (d :> ILightningSerializable<FundingSignedMsg>).Serialize(ls)
        | :? FundingLockedMsg as d ->
            ls.Write(TypeFlag.FundingLocked, false)
            (d :> ILightningSerializable<FundingLockedMsg>).Serialize(ls)
        | :? ShutdownMsg as d ->
            ls.Write(TypeFlag.Shutdown, false)
            (d :> ILightningSerializable<ShutdownMsg>).Serialize(ls)
        | :? ClosingSignedMsg as d ->
            ls.Write(TypeFlag.ClosingSigned, false)
            (d :> ILightningSerializable<ClosingSignedMsg>).Serialize(ls)
        | :? UpdateAddHTLCMsg as d ->
            ls.Write(TypeFlag.UpdateAddHTLC, false)
            (d :> ILightningSerializable<UpdateAddHTLCMsg>).Serialize(ls)
        | :? UpdateFulfillHTLCMsg as d ->
            ls.Write(TypeFlag.UpdateFulfillHTLC, false)
            (d :> ILightningSerializable<UpdateFulfillHTLCMsg>).Serialize(ls)
        | :? UpdateFailHTLCMsg as d ->
            ls.Write(TypeFlag.UpdateFailHTLC, false)
            (d :> ILightningSerializable<UpdateFailHTLCMsg>).Serialize(ls)
        | :? UpdateFailMalformedHTLCMsg as d ->
            ls.Write(TypeFlag.UpdateFailMalformedHTLC, false)
            (d :> ILightningSerializable<UpdateFailMalformedHTLCMsg>).Serialize(ls)
        | :? ChannelReestablishMsg as d ->
            ls.Write(TypeFlag.ChannelReestablish, false)
            (d :> ILightningSerializable<ChannelReestablishMsg>).Serialize(ls)
        | :? CommitmentSignedMsg as d ->
            ls.Write(TypeFlag.CommitmentSigned, false)
            (d :> ILightningSerializable<CommitmentSignedMsg>).Serialize(ls)
        | :? RevokeAndACKMsg as d ->
            ls.Write(TypeFlag.RevokeAndACK, false)
            (d :> ILightningSerializable<RevokeAndACKMsg>).Serialize(ls)
        | :? UpdateFeeMsg as d ->
            ls.Write(TypeFlag.UpdateFee, false)
            (d :> ILightningSerializable<UpdateFeeMsg>).Serialize(ls)
        | :? AnnouncementSignaturesMsg as d ->
            ls.Write(TypeFlag.AnnouncementSignatures, false)
            (d :> ILightningSerializable<AnnouncementSignaturesMsg>).Serialize(ls)
        | :? ChannelAnnouncementMsg as d ->
            ls.Write(TypeFlag.ChannelAnnouncement, false)
            (d :> ILightningSerializable<ChannelAnnouncementMsg>).Serialize(ls)
        | :? NodeAnnouncementMsg as d ->
            ls.Write(TypeFlag.NodeAnnouncement, false)
            (d :> ILightningSerializable<NodeAnnouncementMsg>).Serialize(ls)
        | :? ChannelUpdateMsg as d ->
            ls.Write(TypeFlag.ChannelUpdate, false)
            (d :> ILightningSerializable<ChannelUpdateMsg>).Serialize(ls)
        | :? QueryShortChannelIdsMsg as d ->
            ls.Write(TypeFlag.QueryShortChannelIds, false)
            (d :> ILightningSerializable<QueryShortChannelIdsMsg>).Serialize(ls)
        | :? ReplyShortChannelIdsEndMsg as d ->
            ls.Write(TypeFlag.ReplyShortChannelIdsEnd, false)
            (d :> ILightningSerializable<ReplyShortChannelIdsEndMsg>).Serialize(ls)
        | :? QueryChannelRangeMsg as d ->
            ls.Write(TypeFlag.QueryChannelRange, false)
            (d :> ILightningSerializable<QueryChannelRangeMsg>).Serialize(ls)
        | :? ReplyChannelRangeMsg as d ->
            ls.Write(TypeFlag.ReplyChannelRange, false)
            (d :> ILightningSerializable<ReplyChannelRangeMsg>).Serialize(ls)
        | :? GossipTimestampFilterMsg as d ->
            ls.Write(TypeFlag.GossipTimestampFilter, false)
            (d :> ILightningSerializable<GossipTimestampFilterMsg>).Serialize(ls)
        | :? MonoHopUnidirectionalPaymentMsg as d ->
            ls.Write(TypeFlag.MonoHopUnidirectionalPayment, false)
            (d :> ILightningSerializable<MonoHopUnidirectionalPaymentMsg>).Serialize(ls)
        | x -> failwithf "%A is not known lightning message. This should never happen" x

module LightningMsg =
    let fromBytes<'T when 'T :> ILightningMsg>(b: byte[]): Result<_, P2PDecodeError> =
        try
            use ms = new MemoryStream(b)
            use ls = new LightningReaderStream(ms)
            ILightningSerializable.deserializeWithFlag ls
            |> Ok
        with
        | :? EndOfStreamException as ex -> UnexpectedEndOfStream ex |> Error
        | :? System.IO.IOException as ex -> P2PDecodeError.IO ex |> Error

[<Extension>]
type ILightningMsgExtension() =
    [<Extension>]
    static member ToBytes(this: ILightningMsg) =
        use ms = new MemoryStream()
        use ls = new LightningWriterStream(ms)
        ILightningSerializable.serializeWithFlags ls this
        ms.ToArray()

[<Extension>]
type ILightningSerializableExtension() =
    [<Extension>]
    static member ToBytes(this: ILightningSerializable<'T>) =
        use ms = new MemoryStream()
        use ls = new LightningWriterStream(ms)
        this.Serialize(ls)
        ms.ToArray()

    [<Extension>]
    static member SerializeWithLen(this: ILightningSerializable<'T>, w: LightningWriterStream) =
        let d = this.ToBytes()
        w.WriteWithLen(d)

    [<Extension>]
    static member ToBytesWithLen(this: ILightningSerializable<'T>) =
        let d = this.ToBytes()
        use ms = new MemoryStream()
        use ls = new LightningWriterStream(ms)
        ls.WriteWithLen(d)
        ms.ToArray()

    [<Extension>]
    static member Clone(this: ILightningSerializable<'T>) =
        ILightningSerializable.fromBytes<'T>(this.ToBytes())


// ---------- network message primitives
type OptionalField<'T> = 'T option

[<CLIMutable;StructuralComparison;StructuralEquality>]
type OnionPacket =
    {
        mutable Version: uint8
        /// This might be 33 bytes of 0uy in case of last packet
        /// So we are not using `PubKey` to represent pubkey
        mutable PublicKey: byte[]
        mutable HopData: byte[]
        mutable HMAC: uint256
    }
    with

        static member LastPacket =
            let o = ILightningSerializable.init<OnionPacket>()
            o.Version <- 0uy
            o.PublicKey <- Array.zeroCreate 33
            o.HopData <- Array.zeroCreate 1300
            o.HMAC <- uint256.Zero
            o

        member this.IsLastPacket =
            this.HMAC = uint256.Zero

        interface ILightningSerializable<OnionPacket> with
            member this.Deserialize(ls: LightningReaderStream) =
                this.Version <-
                    let v = ls.ReadUInt8()
                    if (v <> 0uy) then
                        raise <| UnknownVersionException("Unknown version byte for OnionPacket")
                    else
                        v
                this.PublicKey <- ls.ReadBytes(33)
                this.HopData <- ls.ReadBytes(1300) 
                this.HMAC <- ls.ReadUInt256(true)

            member this.Serialize(ls) =
                ls.Write(this.Version)
                ls.Write(this.PublicKey)
                ls.Write(this.HopData)
                ls.Write(this.HMAC, true)

type OnionErrorPacket = {
    Data: byte[]
}

[<CLIMutable>]
type InitMsg =
    {
        mutable Features: FeatureBits
        mutable TLVStream: InitTLV array
    }
    with
        interface ISetupMsg
        interface ILightningSerializable<InitMsg> with
            member this.Deserialize(ls: LightningReaderStream) =
                // For backwards compatibility reason, we must consider legacy
                // `global features` section. (see bolt 1)
                let globalFeatures = ls.ReadWithLen()
                let localFeatures = ls.ReadWithLen()
                let oredFeatures =
                    let len = Math.Max(globalFeatures.Length, localFeatures.Length)
                    let oredFeatures = Array.zeroCreate len
                    for index in 0 .. (len - 1) do
                        let globalFeaturesByte =
                            if index < globalFeatures.Length then
                                globalFeatures.[globalFeatures.Length - 1 - index]
                            else
                                0uy
                        let localFeaturesByte =
                            if index < localFeatures.Length then
                                localFeatures.[localFeatures.Length - 1 - index]
                            else
                                0uy
                        oredFeatures.[len - 1 - index] <- globalFeaturesByte ||| localFeaturesByte
                    oredFeatures
                this.Features <- oredFeatures |> FeatureBits.CreateUnsafe
                this.TLVStream <- ls.ReadTLVStream() |> Array.map(InitTLV.FromGenericTLV)

            member this.Serialize(ls) =
                // For legacy compatiblity we treat the VariableLengthOnion feature specially.
                // Older versions of the lightning protocol spec had a
                // distinction between global and local features, of which
                // VariableLengthOnion was the only global feature. Although
                // this distinction has since been removed, older clients still
                // expect VariableLengthOnion to appear only in the
                // global_features field of an init message and other features
                // to appear only in the local_features field. This is still
                // compatible with newer clients since those clients simply OR
                // the two feature fields.
                let localFeatures =
                    let localFeatures = this.Features
                    localFeatures.SetFeature
                        Feature.VariableLengthOnion
                        FeaturesSupport.Mandatory
                        false
                    localFeatures.SetFeature
                        Feature.VariableLengthOnion
                        FeaturesSupport.Optional
                        false
                    localFeatures.ToByteArray()
                let globalFeatures =
                    let globalFeatures = FeatureBits.Zero
                    let mandatory =
                        this.Features.HasFeature(
                            Feature.VariableLengthOnion,
                            FeaturesSupport.Mandatory
                        )
                    let optional =
                        this.Features.HasFeature(
                            Feature.VariableLengthOnion,
                            FeaturesSupport.Optional
                        )
                    if mandatory then
                        globalFeatures.SetFeature
                            Feature.VariableLengthOnion
                            FeaturesSupport.Mandatory
                            true
                    if optional then
                        globalFeatures.SetFeature
                            Feature.VariableLengthOnion
                            FeaturesSupport.Optional
                            true
                    globalFeatures.ToByteArray()
                ls.WriteWithLen(globalFeatures)
                ls.WriteWithLen(localFeatures)
                ls.WriteTLVStream(this.TLVStream |> Array.map(fun tlv -> tlv.ToGenericTLV()))

[<CLIMutable>]
type PingMsg = {
    mutable PongLen: uint16
    mutable BytesLen: uint16
}
with
    interface ISetupMsg
    interface ILightningSerializable<PingMsg> with
        member this.Deserialize(ls) =
            this.PongLen <- ls.ReadUInt16(false)
            this.BytesLen <- ls.ReadUInt16(false)
            ls.ReadBytes(int32 this.BytesLen) |> ignore
        member this.Serialize(ls) =
            ls.Write(this.PongLen, false)
            ls.Write(this.BytesLen, false)
            ls.Write(Array.zeroCreate<byte> ((int)this.BytesLen))

[<CLIMutable>]
type PongMsg = {
    mutable BytesLen: uint16
}
with
    interface ISetupMsg
    interface ILightningSerializable<PongMsg> with
        member this.Deserialize(ls) =
            this.BytesLen <- ls.ReadUInt16(false)
            ls.ReadBytes(int32 this.BytesLen) |> ignore
        member this.Serialize(ls) =
            ls.Write(this.BytesLen, false)
            ls.Write(Array.zeroCreate<byte> ((int)this.BytesLen))

[<CLIMutable>]
type OpenChannelMsg = {
    mutable Chainhash: uint256
    mutable TemporaryChannelId: ChannelId
    mutable FundingSatoshis: Money
    mutable PushMSat: LNMoney
    mutable DustLimitSatoshis: Money
    mutable MaxHTLCValueInFlightMsat: LNMoney
    mutable ChannelReserveSatoshis: Money
    mutable HTLCMinimumMsat: LNMoney
    mutable FeeRatePerKw: FeeRatePerKw
    mutable ToSelfDelay: BlockHeightOffset16
    mutable MaxAcceptedHTLCs: uint16
    mutable FundingPubKey: FundingPubKey
    mutable RevocationBasepoint: RevocationBasepoint
    mutable PaymentBasepoint: PaymentBasepoint
    mutable DelayedPaymentBasepoint: DelayedPaymentBasepoint
    mutable HTLCBasepoint: HtlcBasepoint
    mutable FirstPerCommitmentPoint: PerCommitmentPoint
    mutable ChannelFlags: uint8
    mutable ShutdownScriptPubKey: OptionalField<Script>
}
with
    interface IChannelMsg
    interface ILightningSerializable<OpenChannelMsg> with
        member this.Deserialize(ls) =
            this.Chainhash <- ls.ReadUInt256(true)
            this.TemporaryChannelId <- ChannelId(ls.ReadUInt256(true))
            this.FundingSatoshis <- Money.Satoshis(ls.ReadUInt64(false))
            this.PushMSat <- LNMoney.MilliSatoshis(ls.ReadUInt64(false))
            this.DustLimitSatoshis <- Money.Satoshis(ls.ReadUInt64(false))
            this.MaxHTLCValueInFlightMsat <- LNMoney.MilliSatoshis(ls.ReadInt64(false))
            this.ChannelReserveSatoshis <- Money.Satoshis(ls.ReadInt64(false))
            this.HTLCMinimumMsat <- LNMoney.MilliSatoshis(ls.ReadInt64(false))
            this.FeeRatePerKw <- FeeRatePerKw(ls.ReadUInt32(false))
            this.ToSelfDelay <- BlockHeightOffset16(ls.ReadUInt16(false))
            this.MaxAcceptedHTLCs <- ls.ReadUInt16(false)
            this.FundingPubKey <- ls.ReadFundingPubKey()
            this.RevocationBasepoint <- ls.ReadRevocationBasepoint()
            this.PaymentBasepoint <- ls.ReadPaymentBasepoint()
            this.DelayedPaymentBasepoint <- ls.ReadDelayedPaymentBasepoint()
            this.HTLCBasepoint <- ls.ReadHtlcBasepoint()
            this.FirstPerCommitmentPoint <- ls.ReadPerCommitmentPoint()
            this.ChannelFlags <- ls.ReadUInt8()
            this.ShutdownScriptPubKey <-
                if (ls.Position = ls.Length) then None else
                ls.ReadWithLen() |> Script |> Some
        member this.Serialize(ls) =
            ls.Write(this.Chainhash, true)
            ls.Write(this.TemporaryChannelId.Value, true)
            ls.Write(this.FundingSatoshis.Satoshi, false)
            ls.Write(this.PushMSat.MilliSatoshi, false)
            ls.Write(this.DustLimitSatoshis.Satoshi, false)
            ls.Write(this.MaxHTLCValueInFlightMsat.MilliSatoshi, false)
            ls.Write(this.ChannelReserveSatoshis.Satoshi, false)
            ls.Write(this.HTLCMinimumMsat.MilliSatoshi, false)
            ls.Write(this.FeeRatePerKw.Value, false)
            ls.Write(this.ToSelfDelay.Value, false)
            ls.Write(this.MaxAcceptedHTLCs, false)
            ls.Write(this.FundingPubKey.ToBytes())
            ls.Write(this.RevocationBasepoint.ToBytes())
            ls.Write(this.PaymentBasepoint.ToBytes())
            ls.Write(this.DelayedPaymentBasepoint.ToBytes())
            ls.Write(this.HTLCBasepoint.ToBytes())
            ls.Write(this.FirstPerCommitmentPoint.ToBytes())
            ls.Write(this.ChannelFlags)
            ls.WriteWithLen(this.ShutdownScriptPubKey |> Option.map(fun x -> x.ToBytes()))

[<CLIMutable>]
type AcceptChannelMsg = {
    mutable TemporaryChannelId: ChannelId
    mutable DustLimitSatoshis: Money
    mutable MaxHTLCValueInFlightMsat: LNMoney
    mutable ChannelReserveSatoshis: Money
    mutable HTLCMinimumMSat: LNMoney
    mutable MinimumDepth: BlockHeightOffset32
    mutable ToSelfDelay: BlockHeightOffset16
    mutable MaxAcceptedHTLCs: uint16
    mutable FundingPubKey: FundingPubKey
    mutable RevocationBasepoint: RevocationBasepoint
    mutable PaymentBasepoint: PaymentBasepoint
    mutable DelayedPaymentBasepoint: DelayedPaymentBasepoint
    mutable HTLCBasepoint: HtlcBasepoint
    mutable FirstPerCommitmentPoint: PerCommitmentPoint
    mutable ShutdownScriptPubKey: OptionalField<Script>
}
with
    interface IChannelMsg
    interface ILightningSerializable<AcceptChannelMsg> with
        member this.Deserialize(ls) =
            this.TemporaryChannelId <- ChannelId(ls.ReadUInt256(true))
            this.DustLimitSatoshis <- ls.ReadUInt64(false) |> Money.Satoshis
            this.MaxHTLCValueInFlightMsat <- ls.ReadUInt64(false) |> LNMoney.MilliSatoshis
            this.ChannelReserveSatoshis <- ls.ReadUInt64(false) |> Money.Satoshis
            this.HTLCMinimumMSat <- ls.ReadUInt64(false) |> LNMoney.MilliSatoshis
            this.MinimumDepth <- ls.ReadUInt32(false) |> BlockHeightOffset32
            this.ToSelfDelay <- ls.ReadUInt16(false) |> BlockHeightOffset16
            this.MaxAcceptedHTLCs <- ls.ReadUInt16(false)
            this.FundingPubKey <- ls.ReadFundingPubKey()
            this.RevocationBasepoint <- ls.ReadRevocationBasepoint()
            this.PaymentBasepoint <- ls.ReadPaymentBasepoint()
            this.DelayedPaymentBasepoint <- ls.ReadDelayedPaymentBasepoint()
            this.HTLCBasepoint <- ls.ReadHtlcBasepoint()
            this.FirstPerCommitmentPoint <- ls.ReadPerCommitmentPoint()
            this.ShutdownScriptPubKey <-
                if (ls.Position = ls.Length) then None else
                ls.ReadWithLen() |> Script |> Some
        member this.Serialize(ls) =
            ls.Write(this.TemporaryChannelId.Value.ToBytes())
            ls.Write(this.DustLimitSatoshis.Satoshi, false)
            ls.Write(this.MaxHTLCValueInFlightMsat.MilliSatoshi, false)
            ls.Write(this.ChannelReserveSatoshis.Satoshi, false)
            ls.Write(this.HTLCMinimumMSat.MilliSatoshi, false)
            ls.Write(this.MinimumDepth.Value, false)
            ls.Write(this.ToSelfDelay.Value, false)
            ls.Write(this.MaxAcceptedHTLCs, false)
            ls.Write(this.FundingPubKey.ToBytes())
            ls.Write(this.RevocationBasepoint.ToBytes())
            ls.Write(this.PaymentBasepoint.ToBytes())
            ls.Write(this.DelayedPaymentBasepoint.ToBytes())
            ls.Write(this.HTLCBasepoint.ToBytes())
            ls.Write(this.FirstPerCommitmentPoint.ToBytes())
            ls.WriteWithLen(this.ShutdownScriptPubKey |> Option.map(fun x -> x.ToBytes()))

[<CLIMutable>]
type FundingCreatedMsg = {
    mutable TemporaryChannelId: ChannelId
    mutable FundingTxId: TxId
    mutable FundingOutputIndex: TxOutIndex
    mutable Signature: LNECDSASignature
}
with
    interface IChannelMsg
    interface ILightningSerializable<FundingCreatedMsg> with
        member this.Deserialize(ls) =
            this.TemporaryChannelId <- ls.ReadUInt256(true) |> ChannelId
            this.FundingTxId <- ls.ReadUInt256(true) |> TxId
            this.FundingOutputIndex <- ls.ReadUInt16(false) |> TxOutIndex
            this.Signature <- ls.ReadECDSACompact()
        member this.Serialize(ls) =
            ls.Write(this.TemporaryChannelId.Value.ToBytes())
            ls.Write(this.FundingTxId.Value.ToBytes())
            ls.Write(this.FundingOutputIndex.Value, false)
            ls.Write(this.Signature)

[<CLIMutable>]
type FundingSignedMsg = {
    mutable ChannelId: ChannelId
    mutable Signature: LNECDSASignature
}
with
    interface IChannelMsg
    interface ILightningSerializable<FundingSignedMsg> with
        member this.Deserialize(ls) =
            this.ChannelId <- ChannelId(ls.ReadUInt256(true))
            this.Signature <- ls.ReadECDSACompact()
        member this.Serialize(ls) =
            ls.Write(this.ChannelId.Value.ToBytes())
            ls.Write(this.Signature)

[<CLIMutable>]
type FundingLockedMsg = {
    mutable ChannelId: ChannelId
    mutable NextPerCommitmentPoint: PerCommitmentPoint
}
with
    interface IChannelMsg
    interface ILightningSerializable<FundingLockedMsg> with
        member this.Deserialize(ls) =
            this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
            this.NextPerCommitmentPoint <- ls.ReadPerCommitmentPoint()
        member this.Serialize(ls) =
            ls.Write(this.ChannelId.Value.ToBytes())
            ls.Write(this.NextPerCommitmentPoint.ToBytes())

[<CLIMutable>]
type ShutdownMsg = {
    mutable ChannelId: ChannelId
    mutable ScriptPubKey: Script
}
with
    interface IChannelMsg
    interface ILightningSerializable<ShutdownMsg> with
        member this.Deserialize(ls) =
            this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
            this.ScriptPubKey <- ls.ReadScript()
        member this.Serialize(ls) =
            ls.Write(this.ChannelId.Value.ToBytes())
            ls.WriteWithLen(this.ScriptPubKey.ToBytes())

[<CLIMutable>]
type ClosingSignedMsg = {
    mutable ChannelId: ChannelId
    mutable FeeSatoshis: Money
    mutable Signature: LNECDSASignature
}
with
    interface IChannelMsg
    interface ILightningSerializable<ClosingSignedMsg> with
        member this.Deserialize(ls) =
            this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
            this.FeeSatoshis <- ls.ReadUInt64(false) |> Money.Satoshis
            this.Signature <- ls.ReadECDSACompact()
        member this.Serialize(ls) =
            ls.Write(this.ChannelId.Value.ToBytes())
            ls.Write(this.FeeSatoshis.Satoshi, false)
            ls.Write(this.Signature)

[<CLIMutable;StructuralComparison;StructuralEquality>]
type UpdateAddHTLCMsg = {
    mutable ChannelId: ChannelId
    mutable HTLCId: HTLCId
    mutable Amount: LNMoney
    mutable PaymentHash: PaymentHash
    mutable CLTVExpiry: BlockHeight
    mutable OnionRoutingPacket: OnionPacket
}
with
    interface IHTLCMsg
    interface IUpdateMsg
    interface ILightningSerializable<UpdateAddHTLCMsg> with
        member this.Deserialize(ls) =
            this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
            this.HTLCId <- ls.ReadUInt64(false) |> HTLCId
            this.Amount <- ls.ReadUInt64(false) |> LNMoney.MilliSatoshis
            this.PaymentHash <- ls.ReadUInt256(false) |> PaymentHash
            this.CLTVExpiry <- ls.ReadUInt32(false) |> BlockHeight
            this.OnionRoutingPacket <- ILightningSerializable.deserialize<OnionPacket>(ls)
        member this.Serialize(ls) =
            ls.Write(this.ChannelId.Value.ToBytes())
            ls.Write(this.HTLCId.Value, false)
            ls.Write(this.Amount.MilliSatoshi, false)
            ls.Write(this.PaymentHash.ToBytes())
            ls.Write(this.CLTVExpiry.Value, false)
            (this.OnionRoutingPacket :> ILightningSerializable<OnionPacket>).Serialize(ls)

[<CLIMutable>]
type UpdateFulfillHTLCMsg = {
    mutable ChannelId: ChannelId
    mutable HTLCId: HTLCId
    mutable PaymentPreimage: PaymentPreimage
}
with
    interface IHTLCMsg
    interface IUpdateMsg
    interface ILightningSerializable<UpdateFulfillHTLCMsg> with
        member this.Deserialize(ls) =
            this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
            this.HTLCId <- ls.ReadUInt64(false) |> HTLCId
            this.PaymentPreimage <- ls.ReadBytes PaymentPreimage.LENGTH |> PaymentPreimage.Create
        member this.Serialize(ls) =
            ls.Write(this.ChannelId.Value.ToBytes())
            ls.Write(this.HTLCId.Value, false)
            ls.Write(this.PaymentPreimage.ToByteArray())

[<CLIMutable>]
type UpdateFailHTLCMsg = {
    mutable ChannelId: ChannelId
    mutable HTLCId: HTLCId
    mutable Reason: OnionErrorPacket
}
with
    interface IHTLCMsg
    interface IUpdateMsg
    interface ILightningSerializable<UpdateFailHTLCMsg> with
        member this.Deserialize(ls) =
            this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
            this.HTLCId <- ls.ReadUInt64(false) |> HTLCId
            this.Reason <- { Data = ls.ReadWithLen() }
        member this.Serialize(ls) =
            ls.Write(this.ChannelId.Value.ToBytes())
            ls.Write(this.HTLCId.Value, false)
            ls.WriteWithLen(this.Reason.Data)

[<CLIMutable>]
type UpdateFailMalformedHTLCMsg = {
    mutable ChannelId: ChannelId
    mutable HTLCId: HTLCId
    mutable Sha256OfOnion: uint256
    mutable FailureCode: FailureCode
}
with
    interface IHTLCMsg
    interface IUpdateMsg
    interface ILightningSerializable<UpdateFailMalformedHTLCMsg> with
        member this.Deserialize(ls) =
            this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
            this.HTLCId <- ls.ReadUInt64(false) |> HTLCId
            this.Sha256OfOnion <- ls.ReadUInt256(true)
            this.FailureCode <- ls.ReadUInt16(false) |> OnionError.FailureCode
        member this.Serialize(ls) =
            ls.Write(this.ChannelId.Value.ToBytes())
            ls.Write(this.HTLCId.Value, false)
            ls.Write(this.Sha256OfOnion, true)
            ls.Write(this.FailureCode.Value, false)

[<CLIMutable>]
type CommitmentSignedMsg = {
    mutable ChannelId: ChannelId
    mutable Signature: LNECDSASignature
    mutable HTLCSignatures: LNECDSASignature list
}
with
    interface IHTLCMsg
    interface ILightningSerializable<CommitmentSignedMsg> with
        member this.Deserialize(ls) =
            this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
            this.Signature <- ls.ReadECDSACompact()
            this.HTLCSignatures <- 
                let len = ls.ReadUInt16(false)
                [ 1us..len ] |> List.map(fun _ -> ls.ReadECDSACompact())
        member this.Serialize(ls) =
            ls.Write(this.ChannelId.Value.ToBytes())
            ls.Write(this.Signature)
            ls.Write((uint16)this.HTLCSignatures.Length, false)
            this.HTLCSignatures |> List.iter (ls.Write)

[<CLIMutable>]
type RevokeAndACKMsg = {
    mutable ChannelId: ChannelId
    mutable PerCommitmentSecret: PerCommitmentSecret
    mutable NextPerCommitmentPoint: PerCommitmentPoint
}
with
    interface IHTLCMsg
    interface ILightningSerializable<RevokeAndACKMsg> with
        member this.Deserialize(ls) =
            this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
            this.PerCommitmentSecret <- ls.ReadPerCommitmentSecret()
            this.NextPerCommitmentPoint <- ls.ReadPerCommitmentPoint()
        member this.Serialize(ls) =
            ls.Write(this.ChannelId.Value.ToBytes())
            ls.Write(this.PerCommitmentSecret.ToBytes())
            ls.Write(this.NextPerCommitmentPoint.ToBytes())

[<CLIMutable>]
type UpdateFeeMsg = {
    mutable ChannelId: ChannelId
    mutable FeeRatePerKw: FeeRatePerKw
}
with
    interface IChannelMsg
    interface IUpdateMsg
    interface ILightningSerializable<UpdateFeeMsg> with
        member this.Deserialize(ls) =
            this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
            this.FeeRatePerKw <- ls.ReadUInt32(false) |> FeeRatePerKw
        member this.Serialize(ls) =
            ls.Write(this.ChannelId.Value.ToBytes())
            ls.Write(this.FeeRatePerKw.Value, false)

[<CLIMutable>]
type DataLossProtect = {
    mutable YourLastPerCommitmentSecret: Option<PerCommitmentSecret>
    mutable MyCurrentPerCommitmentPoint: PerCommitmentPoint
}
    with
        interface ILightningSerializable<DataLossProtect> with
            member this.Deserialize(ls: LightningReaderStream) =
                this.YourLastPerCommitmentSecret <-
                    let bytes = ls.ReadBytes PerCommitmentSecret.BytesLength
                    if bytes.All(fun b -> b = 0uy) then
                        None
                    else
                        Some <| PerCommitmentSecret.FromBytes bytes
                this.MyCurrentPerCommitmentPoint <- ls.ReadPerCommitmentPoint()
            member this.Serialize(ls: LightningWriterStream): unit = 
                match this.YourLastPerCommitmentSecret with
                | Some perCommitmentSecret -> ls.Write(perCommitmentSecret.ToBytes())
                | None -> ls.Write(Array.zeroCreate PerCommitmentSecret.BytesLength)
                ls.Write(this.MyCurrentPerCommitmentPoint.ToBytes())


[<CLIMutable>]
type ChannelReestablishMsg = {
    mutable ChannelId: ChannelId
    mutable NextCommitmentNumber: CommitmentNumber
    mutable NextRevocationNumber: CommitmentNumber
    mutable DataLossProtect: OptionalField<DataLossProtect>
}
with
    interface IChannelMsg
    interface ILightningSerializable<ChannelReestablishMsg> with
        member this.Deserialize(ls) =
            this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
            this.NextCommitmentNumber <- ls.ReadCommitmentNumber()
            this.NextRevocationNumber <- ls.ReadCommitmentNumber()
            this.DataLossProtect <- ls.TryReadAll() |> Option.map ILightningSerializable.fromBytes<DataLossProtect>
        member this.Serialize(ls) =
            ls.Write(this.ChannelId.Value.ToBytes())
            ls.Write this.NextCommitmentNumber
            ls.Write this.NextRevocationNumber
            match this.DataLossProtect with
            | None -> ()
            | Some dataLossProtect -> (dataLossProtect :> ILightningSerializable<DataLossProtect>).Serialize ls

[<CLIMutable>]
type AnnouncementSignaturesMsg = {
    mutable ChannelId: ChannelId
    mutable ShortChannelId: ShortChannelId
    mutable NodeSignature: LNECDSASignature
    mutable BitcoinSignature: LNECDSASignature
}
with
    interface IRoutingMsg
    interface ILightningSerializable<AnnouncementSignaturesMsg> with
        member this.Deserialize(ls) =
            this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
            this.ShortChannelId <- ls.ReadUInt64(false) |> ShortChannelId.FromUInt64
            this.NodeSignature <- ls.ReadECDSACompact()
            this.BitcoinSignature <- ls.ReadECDSACompact()
        member this.Serialize(ls) =
            ls.Write(this.ChannelId.Value.ToBytes())
            ls.Write(this.ShortChannelId)
            ls.Write(this.NodeSignature)
            ls.Write(this.BitcoinSignature)

type NetAddress =
    | IPv4 of IPv4Or6Data
    | IPv6 of IPv4Or6Data
    | OnionV2 of OnionV2EndPoint
    | OnionV3 of OnionV3EndPoint

    member this.GetId() =
        match this with
        | IPv4 _ -> 1uy
        | IPv6 _ -> 2uy
        | OnionV2 _ -> 3uy
        | OnionV3 _ -> 4uy

    member this.Length with get() =
                            match this with
                            | IPv4 _ -> 6us
                            | IPv6 _ -> 18us
                            | OnionV2 _ -> 12us
                            | OnionV3 _ -> 37us

    member this.WriteTo(ls: LightningWriterStream) =
        ls.Write(this.GetId())
        match this with
        | IPv4 d ->
            ls.Write(d.Addr)
            ls.Write(d.Port, false)
        | IPv6 d ->
            ls.Write(d.Addr)
            ls.Write(d.Port, false)
        | OnionV2 d ->
            ls.Write(d.Addr)
            ls.Write(d.Port, false)
        | OnionV3 d ->
            ls.Write(d.ed25519PubKey)
            ls.Write(d.CheckSum, false)
            ls.Write(d.Version)
            ls.Write(d.Port, false)

    static member ReadFrom(ls: LightningReaderStream): NetAdddrSerilizationResult =
        let id = ls.ReadUInt8()
        match id with
        | 1uy ->
            let addr = ls.ReadBytes(4)
            let port = ls.ReadUInt16(false)
            IPv4 { Addr = addr; Port = port }
            |> Ok
        | 2uy ->
            let addr = ls.ReadBytes(16)
            let port = ls.ReadUInt16((false))
            IPv6 { Addr = addr; Port = port }
            |> Ok
        | 3uy ->
            let addr = ls.ReadBytes(10)
            let port = ls.ReadUInt16(false)
            OnionV2 { Addr = addr; Port = port }
            |> Ok
        | 4uy ->
            let ed25519PK = ls.ReadBytes(32)
            let checkSum = ls.ReadUInt16(false)
            let v = ls.ReadUInt8()
            let port = ls.ReadUInt16(false)
            OnionV3 {
                OnionV3EndPoint.ed25519PubKey = ed25519PK
                CheckSum = checkSum
                Version = v
                Port = port
            }
            |> Ok
        | unknown ->
            Result.Error (unknown)
and IPv4Or6Data = {
    /// 4 byte in case of IPv4. 16 byes in case of IPv6
    Addr: byte[]
    Port: uint16 
}

and OnionV2EndPoint = {
    /// 10 bytes
    Addr: byte[]
    Port: uint16
}
and OnionV3EndPoint = {
    ed25519PubKey: byte[]
    CheckSum: uint16
    Version: uint8
    Port: uint16
}
and NetAdddrSerilizationResult = Result<NetAddress, UnknownNetAddr>
and UnknownNetAddr = byte


/// Only exposed as broadcast of node_announcement should be filtered by node_id
/// The unsigned part of node_anouncement
[<CLIMutable>]
type UnsignedNodeAnnouncementMsg = {
    mutable Features: FeatureBits
    mutable Timestamp: uint32
    mutable NodeId: NodeId
    mutable RGB: RGB
    mutable Alias: uint256
    mutable Addresses: NetAddress []
    mutable ExcessAddressData: byte[]
    mutable ExcessData: byte[]
}
with
    interface ILightningSerializable<UnsignedNodeAnnouncementMsg> with
        member this.Deserialize(ls) =
            this.Features <- ls.ReadWithLen() |> FeatureBits.CreateUnsafe
            this.Timestamp <- ls.ReadUInt32(false)
            this.NodeId <- ls.ReadPubKey() |> NodeId
            this.RGB <- ls.ReadRGB()
            this.Alias <- ls.ReadUInt256(true)
            let addrLen = ls.ReadUInt16(false)
            let mutable addresses: NetAddress list = []
            let mutable addr_readPos = 0us
            let mutable foundUnknown = false
            let mutable excessAddressDataByte = 0uy
            this.Addresses <-
                while addr_readPos < addrLen && (not foundUnknown) do
                    let addr = NetAddress.ReadFrom ls
                    ignore <| match addr with
                              | Ok (IPv4 _) ->
                                  if addresses.Length > 0 then
                                      raise <| FormatException(sprintf "Extra Address per type %A" addresses)
                              | Ok (IPv6 _) ->
                                  if addresses.Length > 1 || (addresses.Length = 1 && addresses.[0].GetId() <> 1uy) then
                                      raise <| FormatException(sprintf "Extra Address per type %A" addresses)
                              | Ok(OnionV2 _) ->
                                  if addresses.Length > 2 || (addresses.Length > 0 && addresses.[0].GetId() > 2uy) then
                                      raise <| FormatException(sprintf "Extra Address per type %A" addresses)
                              | Ok(OnionV3 _) ->
                                  if addresses.Length > 3 || (addresses.Length > 0 && addresses.[0].GetId() > 3uy) then
                                      raise <| FormatException(sprintf "Extra Address per type %A" addresses)
                              | Result.Error v ->
                                  excessAddressDataByte <- v
                                  foundUnknown <- true
                                  addr_readPos <- addr_readPos + 1us
                    if (not foundUnknown) then
                        match addr with
                        | Ok addr ->
                            addr_readPos <- addr_readPos + (1us + addr.Length)
                            addresses <- addr :: addresses
                        | Result.Error _ -> failwith "Unreachable"
                addresses |> List.rev |> Array.ofList
            this.ExcessAddressData <-
                if addr_readPos < addrLen then
                    if foundUnknown then
                        Array.append [|excessAddressDataByte|] (ls.ReadBytes(int (addrLen - addr_readPos)))
                    else
                        (ls.ReadBytes(int (addrLen - addr_readPos)))
                else
                    if foundUnknown then
                        [|excessAddressDataByte|]
                    else
                        [||]

            this.ExcessData <- match ls.TryReadAll() with Some b -> b | None -> [||]
        member this.Serialize(ls) =
            ls.WriteWithLen(this.Features.ToByteArray())
            ls.Write(this.Timestamp, false)
            ls.Write(this.NodeId.Value)
            ls.Write(this.RGB)
            ls.Write(this.Alias, true)
            let mutable addrLen:uint16 = (this.Addresses |> Array.sumBy(fun addr -> addr.Length + 1us)) // 1 byte for type field
            let excessAddrLen = (uint16 this.ExcessAddressData.Length)
            addrLen <- excessAddrLen + addrLen
            ls.Write(addrLen, false)
            this.Addresses
                |> Array.iter(fun addr -> addr.WriteTo(ls))
            ls.Write(this.ExcessAddressData)
            ls.Write(this.ExcessData)

[<CLIMutable>]
type NodeAnnouncementMsg = {
    mutable Signature: LNECDSASignature
    mutable Contents: UnsignedNodeAnnouncementMsg
}
with
    interface IRoutingMsg
    interface ILightningSerializable<NodeAnnouncementMsg> with
        member this.Deserialize(ls) =
            this.Signature <- ls.ReadECDSACompact()
            this.Contents <- ILightningSerializable.deserialize<UnsignedNodeAnnouncementMsg>(ls)
        member this.Serialize(ls) =
            ls.Write(this.Signature)
            (this.Contents :> ILightningSerializable<UnsignedNodeAnnouncementMsg>).Serialize(ls)


[<StructuralComparison;StructuralEquality;CLIMutable>]
type UnsignedChannelAnnouncementMsg = {
    mutable Features: FeatureBits
    mutable ChainHash: uint256
    mutable ShortChannelId: ShortChannelId
    mutable NodeId1: NodeId
    mutable NodeId2: NodeId
    mutable BitcoinKey1: ComparablePubKey
    mutable BitcoinKey2: ComparablePubKey
    mutable ExcessData: byte[]
}
with
    interface ILightningSerializable<UnsignedChannelAnnouncementMsg> with
        member this.Deserialize(ls) =
            this.Features <-
                ls.ReadWithLen() |> FeatureBits.CreateUnsafe
            this.ChainHash <- ls.ReadUInt256(true)
            this.ShortChannelId <- ls.ReadUInt64(false) |> ShortChannelId.FromUInt64
            this.NodeId1 <- ls.ReadPubKey() |> NodeId
            this.NodeId2 <- ls.ReadPubKey() |> NodeId
            this.BitcoinKey1 <- ls.ReadPubKey() |> ComparablePubKey
            this.BitcoinKey2 <- ls.ReadPubKey() |> ComparablePubKey
            this.ExcessData <- match ls.TryReadAll() with Some b -> b | None -> [||]
        member this.Serialize(ls) =
            ls.WriteWithLen(this.Features.ToByteArray())
            ls.Write(this.ChainHash, true)
            ls.Write(this.ShortChannelId)
            ls.Write(this.NodeId1.Value)
            ls.Write(this.NodeId2.Value)
            ls.Write(this.BitcoinKey1.Value)
            ls.Write(this.BitcoinKey2.Value)
            ls.Write(this.ExcessData)

[<CLIMutable>]
type ChannelAnnouncementMsg = {
    mutable NodeSignature1: LNECDSASignature
    mutable NodeSignature2: LNECDSASignature
    mutable BitcoinSignature1: LNECDSASignature
    mutable BitcoinSignature2: LNECDSASignature
    mutable Contents: UnsignedChannelAnnouncementMsg
}
with
    interface IRoutingMsg
    interface ILightningSerializable<ChannelAnnouncementMsg> with
        member this.Deserialize(ls) =
            this.NodeSignature1 <- ls.ReadECDSACompact()
            this.NodeSignature2 <- ls.ReadECDSACompact()
            this.BitcoinSignature1 <- ls.ReadECDSACompact()
            this.BitcoinSignature2 <- ls.ReadECDSACompact()
            this.Contents <- ILightningSerializable.deserialize<UnsignedChannelAnnouncementMsg>(ls)
        member this.Serialize(ls) =
            ls.Write(this.NodeSignature1)
            ls.Write(this.NodeSignature2)
            ls.Write(this.BitcoinSignature1)
            ls.Write(this.BitcoinSignature2)
            (this.Contents :> ILightningSerializable<UnsignedChannelAnnouncementMsg>).Serialize(ls)

[<CLIMutable>]
type UnsignedChannelUpdateMsg = {
    mutable ChainHash: uint256
    mutable ShortChannelId: ShortChannelId
    mutable Timestamp: uint32
    mutable MessageFlags: uint8
    mutable ChannelFlags: uint8
    mutable CLTVExpiryDelta: BlockHeightOffset16
    mutable HTLCMinimumMSat: LNMoney
    mutable FeeBaseMSat: LNMoney
    mutable FeeProportionalMillionths: uint32
    mutable HTLCMaximumMSat: OptionalField<LNMoney>
}
    with
    interface IRoutingMsg
    interface  ILightningSerializable<UnsignedChannelUpdateMsg> with
        member this.Deserialize(ls: LightningReaderStream): unit = 
            this.ChainHash <- ls.ReadUInt256(true)
            this.ShortChannelId <- ls.ReadUInt64(false) |> ShortChannelId.FromUInt64
            this.Timestamp <- ls.ReadUInt32(false)
            this.MessageFlags <- ls.ReadByte()
            this.ChannelFlags <- ls.ReadByte()
            this.CLTVExpiryDelta <- ls.ReadUInt16(false) |> BlockHeightOffset16
            this.HTLCMinimumMSat <- ls.ReadUInt64(false) |> LNMoney.MilliSatoshis
            this.FeeBaseMSat <- ls.ReadUInt32(false) |> uint64 |> LNMoney.MilliSatoshis
            this.FeeProportionalMillionths <- ls.ReadUInt32(false)
            this.HTLCMaximumMSat <-
                if ((this.MessageFlags &&& 0b00000001uy) = 1uy) then
                    ls.ReadUInt64(false) |> LNMoney.MilliSatoshis |> Some
                else
                    None
        member this.Serialize(ls: LightningWriterStream): unit = 
            ls.Write(this.ChainHash, true)
            ls.Write(this.ShortChannelId)
            ls.Write(this.Timestamp, false)
            ls.Write(this.MessageFlags)
            ls.Write(this.ChannelFlags)
            ls.Write(this.CLTVExpiryDelta.Value, false)
            ls.Write(this.HTLCMinimumMSat.MilliSatoshi, false)
            ls.Write(uint32 this.FeeBaseMSat.MilliSatoshi, false)
            ls.Write(uint32 this.FeeProportionalMillionths, false)
            match this.HTLCMaximumMSat with
            | Some s -> ls.Write(s.MilliSatoshi, false)
            | None -> ()


[<CLIMutable>]
type ChannelUpdateMsg = {
    mutable Signature: LNECDSASignature
    mutable Contents: UnsignedChannelUpdateMsg
}
with
    member this.IsNode1 =
        (this.Contents.ChannelFlags &&& 1uy) = 0uy
    interface IRoutingMsg
    interface ILightningSerializable<ChannelUpdateMsg> with
        member this.Deserialize(ls) =
            this.Signature <- ls.ReadECDSACompact()
            this.Contents <- ILightningSerializable.deserialize<UnsignedChannelUpdateMsg>(ls)
        member this.Serialize(ls) =
            ls.Write(this.Signature)
            (this.Contents :> ILightningSerializable<UnsignedChannelUpdateMsg>).Serialize(ls)

type FailureMsgData =
    | InvalidRealm
    | TemporaryNodeFailure
    | PermanentNodeFailure
    | RequiredNodeFeatureMissing
    | InvalidOnionVersion of onionHash: uint256
    | InvalidOnionHmac of onionHash: uint256
    | InvalidOnionKey of onionHash: uint256
    | TemporaryChannelFailure of update: ChannelUpdateMsg
    | PermanentChannelFailure
    | RequiredChannelFeatureMissing
    | UnknownNextPeer
    | AmountBelowMinimum of amount: LNMoney * update: ChannelUpdateMsg
    | FeeInsufficient of amount: LNMoney * update: ChannelUpdateMsg
    | ChannelDisabled of Flags: uint16 * update: ChannelUpdateMsg
    | IncorrectCLTVExpiry of expiry: BlockHeight  * update: ChannelUpdateMsg
    | UnknownPaymentHash
    | IncorrectPaymentAmount
    | ExpiryTooSoon of update: ChannelUpdateMsg
    | FinalExpiryTooSoon
    | FinalIncorrectCLTVExpiry of expiry: BlockHeight
    | FinalIncorrectCLTVAmount of amountMSat: LNMoney
    | ExpiryTooFar
    | Unknown of byte[]

[<CLIMutable>]
type FailureMsg = {
    mutable Data: FailureMsgData
    mutable Code: FailureCode
}
    with
        interface ILightningSerializable<FailureMsg> with
            member this.Deserialize(r: LightningReaderStream): unit =
                let t = r.ReadUInt16(false)
                this.Code <- t |> FailureCode
                match t with
                | (INVALID_REALM) ->
                    this.Data <- InvalidRealm
                | (TEMPORARY_NODE_FAILURE) -> this.Data <- TemporaryNodeFailure
                | (PERMANENT_NODE_FAILURE) -> this.Data <- PermanentNodeFailure
                | (REQUIRED_NODE_FEATURE_MISSING) -> this.Data <- RequiredNodeFeatureMissing
                | (INVALID_ONION_VERSION) ->
                    let v = r.ReadUInt256(true)
                    this.Data <- InvalidOnionVersion(v)
                | (INVALID_ONION_HMAC) ->
                    this.Data <- r.ReadUInt256(true) |> InvalidOnionHmac
                | (INVALID_ONION_KEY) ->
                    this.Data <- r.ReadUInt256(true) |> InvalidOnionKey
                | (TEMPORARY_CHANNEL_FAILURE) ->
                    let d = ILightningSerializable.deserialize<ChannelUpdateMsg>(r)
                    this.Data <- d |> TemporaryChannelFailure
                | (PERMANENT_CHANNEL_FAILURE) ->
                    this.Data <- PermanentChannelFailure
                | (REQUIRED_CHANNEL_FEATURE_MISSING) ->
                    this.Data <- RequiredChannelFeatureMissing
                | (UNKNOWN_NEXT_PEER) ->
                    this.Data <- UnknownNextPeer
                | (AMOUNT_BELOW_MINIMUM) ->
                    let amountMSat = r.ReadUInt64(false) |> LNMoney.MilliSatoshis
                    let d = ILightningSerializable.init<ChannelUpdateMsg>()
                    (d :> ILightningSerializable<ChannelUpdateMsg>).Deserialize(r)
                    this.Data <- (amountMSat, d) |> AmountBelowMinimum
                | (FEE_INSUFFICIENT) ->
                    let amountMSat = r.ReadUInt64(false) |> LNMoney.MilliSatoshis
                    let d = ILightningSerializable.init<ChannelUpdateMsg>()
                    (d :> ILightningSerializable<ChannelUpdateMsg>).Deserialize(r)
                    this.Data <- (amountMSat, d) |> FeeInsufficient
                | (CHANNEL_DISABLED) ->
                    let flags = r.ReadUInt16(false)
                    let d = ILightningSerializable.deserialize<ChannelUpdateMsg>(r)
                    this.Data <- (flags, d ) |> ChannelDisabled
                | (INOCCORRECT_CLTV_EXPIRY) ->
                    let expiry = r.ReadUInt32(false) |> BlockHeight
                    let d = ILightningSerializable.deserialize<ChannelUpdateMsg>(r)
                    this.Data <- (expiry, d) |> IncorrectCLTVExpiry
                | (UNKNOWN_PAYMENT_HASH) ->
                    this.Data <- UnknownPaymentHash
                | (INCORRECT_PAYMENT_AMOUNT) ->
                    this.Data <- IncorrectPaymentAmount
                | (EXPIRY_TOO_SOON) ->
                    let d = ILightningSerializable.deserialize<ChannelUpdateMsg>(r)
                    this.Data <- d |> ExpiryTooSoon
                | FINAL_EXPIRY_TOO_SOON ->
                    this.Data <- FinalExpiryTooSoon
                | (FINAL_INCORRECT_CLTV_EXPIRY) ->
                    let expiry = r.ReadUInt32(false) |> BlockHeight
                    this.Data <- expiry |> FinalIncorrectCLTVExpiry
                | (FINAL_INCORRECT_HTLC_AMOUNT) ->
                    let expiry = r.ReadUInt64(false) |> LNMoney.MilliSatoshis
                    this.Data <- expiry |> FinalIncorrectCLTVAmount
                | (EXPIRY_TOO_FAR) ->
                    this.Data <- ExpiryTooFar
                | _ ->
                    this.Data <- r.ReadAll() |> Unknown
            member this.Serialize(w: LightningWriterStream): unit =
                w.Write(this.Code.Value, false)
                match this.Data with
                | InvalidOnionVersion onionHash ->
                    w.Write(onionHash, false)
                | InvalidOnionHmac onionHash ->
                    w.Write(onionHash, false)
                | InvalidOnionKey onionHash ->
                    w.Write(onionHash, false)
                | TemporaryChannelFailure update ->
                    (update :> ILightningSerializable<ChannelUpdateMsg>).SerializeWithLen(w)
                | AmountBelowMinimum (amount, update) ->
                    w.Write(uint64 amount.Value, false)
                    (update :> ILightningSerializable<ChannelUpdateMsg>).SerializeWithLen(w)
                | FeeInsufficient (amount, update) ->
                    w.Write(uint64 amount.Value, false)
                    (update :> ILightningSerializable<ChannelUpdateMsg>).SerializeWithLen(w)
                | ChannelDisabled (flags, update) ->
                    w.Write(flags, false)
                    (update :> ILightningSerializable<ChannelUpdateMsg>).SerializeWithLen(w)
                | IncorrectCLTVExpiry (expiry, update) ->
                    w.Write(expiry.Value, false)
                    (update :> ILightningSerializable<ChannelUpdateMsg>).SerializeWithLen(w)
                | ExpiryTooSoon (update) ->
                    (update :> ILightningSerializable<ChannelUpdateMsg>).SerializeWithLen(w)
                | FinalIncorrectCLTVExpiry (expiry) ->
                    w.Write(expiry.Value, false)
                | FinalIncorrectCLTVAmount (amountMSat) ->
                    w.Write(amountMSat.Value, false)
                | FailureMsgData.Unknown b ->
                    w.Write(b)
                | _ ->
                    ()



[<CLIMutable>]
type ErrorMsg =
    {
        mutable ChannelId: WhichChannel
        mutable Data: byte[]
    }
    with
        interface ISetupMsg
        interface ILightningSerializable<ErrorMsg> with
            member this.Deserialize(ls) =
                match ls.ReadUInt256(true) with
                | id when id = uint256.Zero ->
                    this.ChannelId <- All
                | id ->
                    this.ChannelId <- SpecificChannel(ChannelId id)
                this.Data <- ls.ReadWithLen()
            member this.Serialize(ls) =
                match this.ChannelId with
                | SpecificChannel (ChannelId id) -> ls.Write(id.ToBytes())
                | All -> ls.Write(Array.zeroCreate 32)
                ls.WriteWithLen(this.Data)

        member this.GetFailureMsgData() =
            let minPrintableAsciiChar = 32uy
            let isPrintableAsciiChar (asciiChar: byte) =
                asciiChar >= minPrintableAsciiChar
            let isPrintableAsciiString =
                not <| Seq.exists
                    (fun asciiChar -> not (isPrintableAsciiChar asciiChar))
                    this.Data
            if isPrintableAsciiString then
                System.Text.ASCIIEncoding.ASCII.GetString this.Data
            else
                Seq.fold
                    (fun msg (asciiChar: byte) -> sprintf "%s %02x" msg asciiChar)
                    "<error contains non-printable binary data>:"
                    this.Data

and WhichChannel =
    | SpecificChannel of ChannelId
    | All

type ErrorAction = 
    | DisconnectPeer of ErrorMsg option
    | IgnoreError
    | SendErrorMessage of ErrorMsg

type HandleError = {
    Error: string
    Action: ErrorAction option
}


/// Struct used to return valeus from revoke_and_ack messages, cotaining a bunch of commitment
/// transaction updates if they were pending.
type CommitmentUpdate = {
    UpdateAddHTLCs: UpdateAddHTLCMsg list
    UpdateFulfillHTLCs: UpdateFulfillHTLCMsg list
    UpdateFailHTLCs: UpdateFailHTLCMsg list
    UpdateFailMalformedHTLCs: UpdateFailMalformedHTLCMsg list
    UpdateFee: UpdateFeeMsg option
    CommitmentSigned: CommitmentSignedMsg
}

/// The information we received from a peer along the route of a payment we originated. This is
/// returned by ChannelMessageHandler.HandleUpdateFailHTLC to be passed into
/// RoutingMessageHandler.HandleHTLCFailChannelUpdate to update our network map.
type HTLCFailChannelUpdate = {
    ChannelUpdateMessage: ChannelUpdateMsg
    ChannelClosed: ChannelClosed
    NodeFailure: NodeFailure
}

and ChannelClosed = {
    ShortChannelId: ShortChannelId
    /// when this true, this channel should be permanently removed from the
    /// consideration. Otherwiser, this channel can be restored as new ChannelUpdateMsg is received
    IsPermanent: bool
}
and NodeFailure = {
    NodeId: NodeId
    IsPermanent: bool
}


[<CLIMutable>]
type QueryShortChannelIdsMsg = {
    mutable ChainHash: uint256
    mutable ShortIdsEncodingType: EncodingType
    mutable ShortIds: ShortChannelId []
    mutable TLVs: QueryShortChannelIdsTLV []
}
    with
    interface IQueryMsg
    interface ILightningSerializable<QueryShortChannelIdsMsg> with
        member this.Deserialize(ls: LightningReaderStream) =
            this.ChainHash <- ls.ReadUInt256(true)
            let shortIdsWithFlag = ls.ReadWithLen()
            this.ShortIdsEncodingType <- LanguagePrimitives.EnumOfValue<byte, EncodingType>(shortIdsWithFlag.[0])
            let shortIds =
                Decoder.decodeShortChannelIds this.ShortIdsEncodingType (shortIdsWithFlag.[1..])
            let tlvs =
                ls.ReadTLVStream()
                |> Array.map(QueryShortChannelIdsTLV.FromGenericTLV)
            let queryFlags =
                tlvs
                |> Seq.choose(function QueryShortChannelIdsTLV.QueryFlags (_, y) -> Some (y) | _ -> None)
                |> Seq.tryExactlyOne
            match queryFlags with
            | None ->
                this.ShortIds <- shortIds
                this.TLVs <- tlvs
            | Some flags ->
                if (shortIds.Length <> (flags |> Seq.length)) then
                    raise <| FormatException(sprintf "query_short_channel_ids have different length for short_ids(%A) and query_flags! (%A)" shortIds flags)
                this.ShortIds <- shortIds
                this.TLVs <- tlvs
        member this.Serialize(ls) =
            ls.Write(this.ChainHash, true)
            let encodedIds = this.ShortIds |> Encoder.encodeShortChannelIds (this.ShortIdsEncodingType)
            [[|(byte)this.ShortIdsEncodingType|]; encodedIds]
            |> Array.concat
            |> ls.WriteWithLen
            this.TLVs |> Array.map(fun tlv -> tlv.ToGenericTLV()) |> ls.WriteTLVStream

[<CLIMutable>]
type ReplyShortChannelIdsEndMsg = {
    mutable ChainHash: uint256
    mutable Complete: bool
}
    with
    interface IQueryMsg
    interface ILightningSerializable<ReplyShortChannelIdsEndMsg> with
        member this.Deserialize(ls) =
            this.ChainHash <- ls.ReadUInt256(true)
            this.Complete <-
                let b = ls.ReadByte()
                if (b = 0uy) then false else
                if (b = 1uy) then true else
                raise <| FormatException(sprintf "reply_short_channel_ids has unknown byte in `complete` field %A" b)
        member this.Serialize(ls) =
            ls.Write(this.ChainHash, true)
            ls.Write(if (this.Complete) then 1uy else 0uy)
[<CLIMutable>]
type QueryChannelRangeMsg = {
    mutable ChainHash: uint256
    mutable FirstBlockNum: BlockHeight
    mutable NumberOfBlocks: uint32
    mutable TLVs: QueryChannelRangeTLV []
}
    with
    interface IQueryMsg
    interface ILightningSerializable<QueryChannelRangeMsg> with
        member this.Deserialize(ls) =
            this.ChainHash <- ls.ReadUInt256(true)
            this.FirstBlockNum <- ls.ReadUInt32(false) |> BlockHeight
            this.NumberOfBlocks <- ls.ReadUInt32(false)
            this.TLVs <-
                let r = ls.ReadTLVStream()
                r
                |> Array.map(QueryChannelRangeTLV.FromGenericTLV)
        member this.Serialize(ls) =
            ls.Write(this.ChainHash, true)
            ls.Write(this.FirstBlockNum.Value, false)
            ls.Write(this.NumberOfBlocks, false)
            this.TLVs |> Array.map(fun tlv -> tlv.ToGenericTLV()) |> ls.WriteTLVStream

[<CLIMutable>]
type ReplyChannelRangeMsg = {
    mutable ChainHash: uint256
    mutable FirstBlockNum: BlockHeight
    mutable NumOfBlocks: uint32
    mutable Complete: bool
    mutable ShortIdsEncodingType: EncodingType
    mutable ShortIds: ShortChannelId[]
    mutable TLVs: ReplyChannelRangeTLV[]
}
    with
    interface IQueryMsg
    interface ILightningSerializable<ReplyChannelRangeMsg> with
        member this.Deserialize(ls) =
            this.ChainHash <- ls.ReadUInt256(true)
            this.FirstBlockNum <- ls.ReadUInt32(false) |> BlockHeight
            this.NumOfBlocks <- ls.ReadUInt32(false)
            this.Complete <-
                let b =ls.ReadByte()
                if b = 0uy then false else
                if b = 1uy then true else
                raise <| FormatException(sprintf "reply_channel_range has unknown byte in `complete` field %A" b)
            let shortIdsWithFlag = ls.ReadWithLen()
            this.ShortIdsEncodingType <- LanguagePrimitives.EnumOfValue<byte, EncodingType>(shortIdsWithFlag.[0])
            this.ShortIds <-
                Decoder.decodeShortChannelIds this.ShortIdsEncodingType (shortIdsWithFlag.[1..])
            this.TLVs <-
                ls.ReadTLVStream() |> Array.map(ReplyChannelRangeTLV.FromGenericTLV)
        member this.Serialize(ls) =
            ls.Write(this.ChainHash, true)
            ls.Write(this.FirstBlockNum.Value, false)
            ls.Write(this.NumOfBlocks, false)
            ls.Write(if this.Complete then 1uy else 0uy)
            let encodedIds = this.ShortIds |> Encoder.encodeShortChannelIds (this.ShortIdsEncodingType)
            [[|(byte)this.ShortIdsEncodingType|]; encodedIds]
            |> Array.concat
            |> ls.WriteWithLen
            this.TLVs |> Array.map(fun x -> x.ToGenericTLV()) |> ls.WriteTLVStream
            
[<CLIMutable>]
type GossipTimestampFilterMsg = {
    mutable ChainHash: uint256
    mutable FirstTimestamp: uint32
    mutable TimestampRange: uint32
}
    with
    interface IQueryMsg
    interface ILightningSerializable<GossipTimestampFilterMsg> with
        member this.Deserialize(ls) =
            this.ChainHash <- ls.ReadUInt256(true)
            this.FirstTimestamp <- ls.ReadUInt32(false)
            this.TimestampRange <- ls.ReadUInt32(false)
        member this.Serialize(ls) =
            ls.Write(this.ChainHash, true)
            ls.Write(this.FirstTimestamp, false)
            ls.Write(this.TimestampRange, false)
            
[<CLIMutable>]
type MonoHopUnidirectionalPaymentMsg = {
    mutable ChannelId: ChannelId
    mutable Amount: LNMoney
}
with
    interface IHTLCMsg
    interface IUpdateMsg
    interface ILightningSerializable<MonoHopUnidirectionalPaymentMsg> with
        member this.Deserialize(ls) =
            this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
            this.Amount <- ls.ReadUInt64(false) |> LNMoney.MilliSatoshis
        member this.Serialize(ls) =
            ls.Write(this.ChannelId.Value.ToBytes())
            ls.Write(this.Amount.MilliSatoshi, false)

