namespace DotNetLightning.Serialize
open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Utils.RResult
open System
open System.Runtime.CompilerServices
open System.IO
open System.Net
open NBitcoin.Crypto
open DotNetLightning.Utils.Error

// #region serialization
module Msgs =
    [<Struct>]
    type DecodeError =
        | UnknownVersion
        | UnknownRequiredFeature
        | InvalidValue
        | ShortRead
        | ExtraAddressesPerType
        | BadLengthDescriptor
        | IO of IOException

    [<Struct>]
    type LocalFeatures =
        Flags of byte[]
            member x.Value = let (Flags v) = x in v
            member x.SupportsDataLossProect(): bool =
                (x.Value.[0] &&& 3uy) <> 0uy

            member x.RequiresDataLossProect(): bool =
                (x.Value.[0] &&& 1uy) <> 0uy

            member x.InitialRoutingSync(): bool =
                (x.Value.[0] &&& (1uy <<< 3)) <> 0uy

    module LocalFeatures =
        let setInitialRoutingSync(x: LocalFeatures) =
            if
                x.Value.Length = 0
            then
                Flags [|1uy <<< 3|]
            else
                x.Value.[0] <- (x.Value.[0] ||| (1uy <<< 3))
                x


    type TypeFlag =
        | Init = 16us
        | Error = 17us
        | Ping = 18us
        | Pong = 19us
        | OpenChannel = 32us
        | AcceptChannel = 33us
        | FundingCreated = 34us
        | FundingSigned = 35us
        | FundingLocked = 36us
        | Shutdown = 38us
        | ClosingSigned = 39us
        | UpdateAddHTLC = 128us
        | UpdateFulfillHTLC = 130us
        | UpdateFailHTLC = 131us
        | UpdateFailMalformedHTLC = 135us
        | ChannelReestablish = 136us
        | CommitmentSigned = 132us
        | RevokeAndACK = 133us
        | UpdateFee = 134us
        | AnnouncementSignatures = 259us
        | ChannelAnnouncement = 256us
        | NodeAnnouncement = 257us
        | ChannelUpdate = 258us

    type ILightningMsg = interface end
    type ISetupMsg = inherit ILightningMsg
    type IChannelMsg = inherit ILightningMsg
    type IHTLCMsg = inherit ILightningMsg
    type IRoutingMsg = inherit ILightningMsg
    type IUpdateMsg = inherit ILightningMsg

    // #endregion 
    type ILightningSerializable<'T when 'T: (new: unit -> 'T) and 'T :> ILightningSerializable<'T>> =
        abstract Deserialize: LightningReaderStream -> unit
        abstract Serialize: LightningWriterStream -> unit

    [<Extension>]
    type ILightningSerializableExtension() =
        [<Extension>]
        static member ToBytes(this: ILightningSerializable<'T>) =
            use ms = new MemoryStream()
            use ls = new LightningWriterStream(ms)
            this.Serialize(ls)
            ms.ToArray()

        [<Extension>]
        static member FromBytes(this: ILightningSerializable<'T>, data: byte[]) =
            use ms = new MemoryStream(data)
            use ls = new LightningReaderStream(ms)
            let instance = new 'T()
            instance.Deserialize(ls)
            instance

        [<Extension>]
        static member Clone(this: ILightningSerializable<'T>) =
            this.FromBytes(this.ToBytes())


    [<Struct>]
    type GlobalFeatures =
        Flags of uint8[]
            member x.Value = let (Flags v) = x in v

    // ---------- network message primitives
    type OptionalField<'T> = 'T option

    [<CLIMutable;StructuralComparison;StructuralEquality>]
    type OnionPacket =
        {
            mutable Version: uint8
            mutable PublicKey: PubKey
            mutable HopData: byte[]
            mutable HMAC: uint256
        }
        with
            interface ILightningSerializable<OnionPacket> with
                member this.Deserialize(ls: LightningReaderStream) =
                    this.Version <- ls.ReadUInt8()
                    this.PublicKey <- ls.ReadPubKey()
                    this.HopData <- ls.ReadBytes(1366) 
                    this.HMAC <- ls.ReadUInt256(false)

                member this.Serialize(ls) =
                    ls.Write(this.Version)
                    ls.Write(this.PublicKey.ToBytes())
                    ls.Write(this.HopData)
                    ls.Write(this.HMAC, false)

    type OnionErrorPacket = {
        Data: byte[]
    }


    [<CLIMutable>]
    type Init =
        {
            mutable GlobalFeatures: GlobalFeatures
            mutable LocalFeatures: LocalFeatures
        }
        with
            interface ISetupMsg
            interface ILightningSerializable<Init> with
                member this.Deserialize(ls: LightningReaderStream) =
                    this.GlobalFeatures <- GlobalFeatures.Flags(ls.ReadWithLen())
                    this.LocalFeatures <- LocalFeatures.Flags(ls.ReadWithLen())
                member this.Serialize(ls) =
                    let g: byte[] = this.GlobalFeatures.Value
                    let l: byte[] = this.LocalFeatures.Value
                    ls.WriteWithLen(g)
                    ls.WriteWithLen(l)

    [<CLIMutable>]
    type ErrorMessage = 
        {
            mutable ChannelId: WhichChannel
            mutable Data: byte[]
        }
        with
            interface ISetupMsg
            interface ILightningSerializable<ErrorMessage> with
                member this.Deserialize(ls) =
                    match ls.ReadUInt256(false) with
                    | id when id = uint256.Zero ->
                        this.ChannelId <- All
                    | id ->
                        this.ChannelId <- SpecificChannel(Primitives.ChannelId(id))
                    this.Data <- ls.ReadWithLen()
                member this.Serialize(ls) =
                    match this.ChannelId with
                    | SpecificChannel (Primitives.ChannelId id) -> ls.Write(id.ToBytes())
                    | All -> ls.Write(Array.zeroCreate 32)
                    ls.WriteWithLen(this.Data)

    and WhichChannel =
        | SpecificChannel of ChannelId
        | All

    [<CLIMutable>]
    type Ping = {
        mutable PongLen: uint16
        mutable BytesLen: uint16
    }
    with
        interface ISetupMsg
        interface ILightningSerializable<Ping> with
            member this.Deserialize(ls) =
                this.PongLen <- ls.ReadUInt16(false)
                this.BytesLen <- ls.ReadUInt16(false)
                ls.ReadBytes(int32 this.BytesLen) |> ignore
            member this.Serialize(ls) =
                ls.Write(this.PongLen, false)
                ls.Write(this.BytesLen, false)
                ls.Write(Array.zeroCreate<byte> ((int)this.BytesLen))

    [<CLIMutable>]
    type Pong = {
        mutable BytesLen: uint16
    }
    with
        interface ISetupMsg
        interface ILightningSerializable<Pong> with
            member this.Deserialize(ls) =
                this.BytesLen <- ls.ReadUInt16(false)
                ls.ReadBytes(int32 this.BytesLen) |> ignore
            member this.Serialize(ls) =
                ls.Write(this.BytesLen, false)
                ls.Write(Array.zeroCreate<byte> ((int)this.BytesLen))

    [<CLIMutable>]
    type OpenChannel = {
        mutable Chainhash: uint256
        mutable TemporaryChannelId: ChannelId
        mutable FundingSatoshis: Money
        mutable PushMSat: LNMoney
        mutable DustLimitSatoshis: Money
        mutable MaxHTLCValueInFlightMsat: LNMoney
        mutable ChannelReserveSatoshis: Money
        mutable HTLCMinimumMsat: LNMoney
        mutable FeeRatePerKw: FeeRatePerKw
        mutable ToSelfDelay: BlockHeightOffset
        mutable MaxAcceptedHTLCs: uint16
        mutable FundingPubKey: PubKey
        mutable RevocationBasepoint: PubKey
        mutable PaymentBasepoint: PubKey
        mutable DelayedPaymentBasepoint: PubKey
        mutable HTLCBasepoint: PubKey
        mutable FirstPerCommitmentPoint: PubKey
        mutable ChannelFlags: uint8
        mutable ShutdownScriptPubKey: OptionalField<Script>
    }
    with
        interface IChannelMsg
        interface ILightningSerializable<OpenChannel> with
            member this.Deserialize(ls) =
                this.Chainhash <- ls.ReadUInt256(false)
                this.TemporaryChannelId <- ChannelId(ls.ReadUInt256(true))
                this.FundingSatoshis <- Money.Satoshis(ls.ReadInt64(false))
                this.PushMSat <- LNMoney.MilliSatoshis(ls.ReadUInt64(false))
                this.DustLimitSatoshis <- Money.Satoshis(ls.ReadInt64(false))
                this.MaxHTLCValueInFlightMsat <- LNMoney.MilliSatoshis(ls.ReadInt64(false))
                this.ChannelReserveSatoshis <- Money.Satoshis(ls.ReadInt64(false))
                this.HTLCMinimumMsat <- LNMoney.MilliSatoshis(ls.ReadInt64(false))
                this.FeeRatePerKw <- FeeRatePerKw(ls.ReadUInt32(false))
                this.ToSelfDelay <- BlockHeightOffset(ls.ReadUInt16(false))
                this.MaxAcceptedHTLCs <- ls.ReadUInt16(false)
                this.FundingPubKey <- ls.ReadPubKey()
                this.RevocationBasepoint <- ls.ReadPubKey()
                this.PaymentBasepoint <- ls.ReadPubKey()
                this.DelayedPaymentBasepoint <- ls.ReadPubKey()
                this.HTLCBasepoint <- ls.ReadPubKey()
                this.FirstPerCommitmentPoint <- ls.ReadPubKey()
                this.ChannelFlags <- ls.ReadUInt8()
                this.ShutdownScriptPubKey |> ignore
                failwith "update OptionalField part"
            member this.Serialize(ls) =
                ls.Write(this.Chainhash, false)
                ls.Write(this.TemporaryChannelId.Value.ToBytes())
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
    type AcceptChannel = {
        mutable TemporaryChannelId: ChannelId
        mutable DustLimitSatoshis: Money
        mutable MaxHTLCValueInFlightMsat: LNMoney
        mutable ChannelReserveSatoshis: Money
        mutable HTLCMinimumMSat: LNMoney
        mutable MinimumDepth: uint32
        mutable ToSelfDelay: BlockHeightOffset
        mutable MaxAcceptedHTLCs: uint16
        mutable FundingPubKey: PubKey
        mutable RevocationBasepoint: PubKey
        mutable PaymentBasepoint: PubKey
        mutable DelayedPaymentBasepoint: PubKey
        mutable HTLCBasepoint: PubKey
        mutable FirstPerCommitmentPoint: PubKey
        mutable ShutdownScriptPubKey: OptionalField<Script>
    }
    with
        interface IChannelMsg
        interface ILightningSerializable<AcceptChannel> with
            member this.Deserialize(ls) =
                this.TemporaryChannelId <- ChannelId(ls.ReadUInt256(false))
                this.DustLimitSatoshis <- ls.ReadUInt64(false) |> Money.Satoshis
                this.MaxHTLCValueInFlightMsat <- ls.ReadUInt64(false) |> LNMoney.MilliSatoshis
                this.ChannelReserveSatoshis <- ls.ReadUInt64(false) |> Money.Satoshis
                this.HTLCMinimumMSat <- ls.ReadUInt64(false) |> LNMoney.MilliSatoshis
                this.MinimumDepth <- ls.ReadUInt32(false)
                this.ToSelfDelay <- ls.ReadUInt16(false) |> BlockHeightOffset
                this.MaxAcceptedHTLCs <- ls.ReadUInt16(false)
                this.FundingPubKey <- ls.ReadPubKey()
                this.RevocationBasepoint <- ls.ReadPubKey()
                this.PaymentBasepoint <- ls.ReadPubKey()
                this.DelayedPaymentBasepoint <- ls.ReadPubKey()
                this.HTLCBasepoint <- ls.ReadPubKey()
                this.FirstPerCommitmentPoint <- ls.ReadPubKey()
                failwith "consider optional field handling"
                this.ShutdownScriptPubKey |> ignore
            member this.Serialize(ls) =
                ls.Write(this.TemporaryChannelId.Value.ToBytes())
                ls.Write(this.DustLimitSatoshis.Satoshi, false)
                ls.Write(this.MaxHTLCValueInFlightMsat.MilliSatoshi, false)
                ls.Write(this.ChannelReserveSatoshis.Satoshi, false)
                ls.Write(this.HTLCMinimumMSat.MilliSatoshi, false)
                ls.Write(this.MinimumDepth, false)
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
    type FundingCreated = {
        mutable TemporaryChannelId: ChannelId
        mutable FundingTxId: TxId
        mutable FundingOutputIndex: uint16
        mutable Signature: ECDSASignature
    }
    with
        interface IChannelMsg
        interface ILightningSerializable<FundingCreated> with
            member this.Deserialize(ls) =
                failwith ""
            member this.Serialize(ls) =
                ls.Write(this.TemporaryChannelId.Value.ToBytes())
                ls.Write(this.FundingTxId.Value.ToBytes())
                ls.Write(this.FundingOutputIndex, false)
                ls.Write(this.Signature)

    [<CLIMutable>]
    type FundingSigned = {
        mutable ChannelId: ChannelId
        mutable Signature: ECDSASignature
    }
    with
        interface IChannelMsg
        interface ILightningSerializable<FundingSigned> with
            member this.Deserialize(ls) =
                this.ChannelId <- ChannelId(ls.ReadUInt256(false))
                this.Signature <- ls.ReadECDSACompact()
            member this.Serialize(ls) =
                ls.Write(this.ChannelId.Value.ToBytes())
                ls.Write(this.Signature)

    [<CLIMutable>]
    type FundingLocked = {
        mutable ChannelId: ChannelId
        mutable NextPerCommitmentPoint: PubKey
    }
    with
        interface IChannelMsg
        interface ILightningSerializable<FundingLocked> with
            member this.Deserialize(ls) =
                this.ChannelId <- ls.ReadUInt256(false) |> ChannelId
                this.NextPerCommitmentPoint <- ls.ReadPubKey()
            member this.Serialize(ls) =
                ls.Write(this.ChannelId.Value.ToBytes())
                ls.Write(this.NextPerCommitmentPoint.ToBytes())

    [<CLIMutable>]
    type Shutdown = {
        mutable ChannelId: ChannelId
        mutable ScriptPubKey: Script
    }
    with
        interface IChannelMsg
        interface ILightningSerializable<Shutdown> with
            member this.Deserialize(ls) =
                this.ChannelId <- ls.ReadUInt256(false) |> ChannelId
                this.ScriptPubKey <- ls.ReadScript()
            member this.Serialize(ls) =
                ls.Write(this.ChannelId.Value.ToBytes())
                ls.WriteWithLen(this.ScriptPubKey.ToBytes())

    [<CLIMutable>]
    type ClosingSigned = {
        mutable ChannelId: ChannelId
        mutable FeeSatoshis: Money
        mutable Signature: ECDSASignature
    }
    with
        interface IChannelMsg
        interface ILightningSerializable<ClosingSigned> with
            member this.Deserialize(ls) =
                this.ChannelId <- ls.ReadUInt256(false) |> ChannelId
                this.FeeSatoshis <- ls.ReadUInt64(false) |> Money.Satoshis
                this.Signature <- ls.ReadECDSACompact()
            member this.Serialize(ls) =
                ls.Write(this.ChannelId.Value.ToBytes())
                ls.Write(this.FeeSatoshis.Satoshi, false)
                ls.Write(this.Signature)

    [<CLIMutable;StructuralComparison;StructuralEquality>]
    type UpdateAddHTLC = {
        mutable ChannelId: ChannelId
        mutable HTLCId: HTLCId
        mutable AmountMSat: LNMoney
        mutable PaymentHash: PaymentHash
        mutable CLTVExpiry: uint32
        mutable OnionRoutingPacket: OnionPacket
    }
    with
        interface IHTLCMsg
        interface IUpdateMsg
        interface ILightningSerializable<UpdateAddHTLC> with
            member this.Deserialize(ls) =
                this.ChannelId <- ls.ReadUInt256(false) |> ChannelId
                this.HTLCId <- ls.ReadUInt64(false) |> HTLCId
                this.AmountMSat <- ls.ReadUInt64(false) |> LNMoney.MilliSatoshis
                this.PaymentHash <- ls.ReadUInt256(false) |> PaymentHash
                this.CLTVExpiry <- ls.ReadUInt32(false)
                (this.OnionRoutingPacket :> ILightningSerializable<OnionPacket>).Deserialize(ls)
            member this.Serialize(ls) =
                ls.Write(this.ChannelId.Value.ToBytes())
                ls.Write(this.HTLCId.Value, false)
                ls.Write(this.AmountMSat.MilliSatoshi, false)
                ls.Write(this.PaymentHash.Value.ToBytes())
                ls.Write(this.CLTVExpiry, false)
                (this.OnionRoutingPacket :> ILightningSerializable<OnionPacket>).Serialize(ls)

    [<CLIMutable>]
    type UpdateFulfillHTLC = {
        mutable ChannelId: ChannelId
        mutable HTLCId: HTLCId
        mutable PaymentPreimage: PaymentPreimage
    }
    with
        interface IHTLCMsg
        interface IUpdateMsg
        interface ILightningSerializable<UpdateFulfillHTLC> with
            member this.Deserialize(ls) =
                this.ChannelId <- ls.ReadUInt256(false) |> ChannelId
                this.HTLCId <- ls.ReadUInt64(false) |> HTLCId
                this.PaymentPreimage <- ls.ReadUInt256(false) |> PaymentPreimage
            member this.Serialize(ls) =
                ls.Write(this.ChannelId.Value.ToBytes())
                ls.Write(this.HTLCId.Value, false)
                ls.Write(this.PaymentPreimage.Value.ToBytes())

    [<CLIMutable>]
    type UpdateFailHTLC = {
        mutable ChannelId: ChannelId
        mutable HTLCId: HTLCId
        mutable Reason: OnionErrorPacket
    }
    with
        interface IHTLCMsg
        interface IUpdateMsg
        interface ILightningSerializable<UpdateFailHTLC> with
            member this.Deserialize(ls) =
                this.ChannelId <- ls.ReadUInt256(false) |> ChannelId
                this.HTLCId <- ls.ReadUInt64(false) |> HTLCId
                this.Reason <- { Data = ls.ReadWithLen() }
            member this.Serialize(ls) =
                ls.Write(this.ChannelId.Value.ToBytes())
                ls.Write(this.HTLCId.Value, false)
                ls.WriteWithLen(this.Reason.Data)

    [<CLIMutable>]
    type UpdateFailMalformedHTLC = {
        mutable ChannelId: ChannelId
        mutable HTLCId: HTLCId
        mutable Sha256OfOnion: uint256
        mutable FailureCode: ErrorCode
    }
    with
        interface IHTLCMsg
        interface IUpdateMsg
        interface ILightningSerializable<UpdateFailMalformedHTLC> with
            member this.Deserialize(ls) =
                this.ChannelId <- ls.ReadUInt256(false) |> ChannelId
                this.HTLCId <- ls.ReadUInt64(false) |> HTLCId
                this.Sha256OfOnion <- ls.ReadUInt256(false)
                this.FailureCode <- ls.ReadUInt16(false) |> ErrorCode
            member this.Serialize(ls) =
                ls.Write(this.ChannelId.Value.ToBytes())
                ls.Write(this.HTLCId.Value, false)
                ls.Write(this.Sha256OfOnion, false)
                ls.Write(this.FailureCode.Value, false)

    [<CLIMutable>]
    type CommitmentSigned = {
        mutable ChannelId: ChannelId
        mutable Signature: ECDSASignature
        mutable HTLCSignatures: ECDSASignature list
    }
    with
        interface IHTLCMsg
        interface ILightningSerializable<CommitmentSigned> with
            member this.Deserialize(ls) =
                this.ChannelId <- ls.ReadUInt256(false) |> ChannelId
                this.Signature <- ls.ReadECDSACompact()
                this.HTLCSignatures <- 
                    let len = ls.ReadUInt16(false)
                    seq { 0us..len-1us } |> Seq.map(fun _ -> ls.ReadECDSACompact()) |> Seq.toList
            member this.Serialize(ls) =
                ls.Write(this.ChannelId.Value.ToBytes())
                ls.Write(this.Signature)
                ls.Write((uint16)this.HTLCSignatures.Length, false)
                this.HTLCSignatures |> List.iter (ls.Write)

    [<CLIMutable>]
    type RevokeAndACK = {
        mutable ChannelId: ChannelId
        mutable PerCommitmentSecret: PaymentPreimage
        mutable NextPerCommitmentPoint: PubKey
    }
    with
        interface IHTLCMsg
        interface ILightningSerializable<RevokeAndACK> with
            member this.Deserialize(ls) =
                this.ChannelId <- ls.ReadUInt256(false) |> ChannelId
                this.PerCommitmentSecret <- ls.ReadUInt256(false) |> PaymentPreimage
                this.NextPerCommitmentPoint <- ls.ReadPubKey()
            member this.Serialize(ls) =
                ls.Write(this.ChannelId.Value.ToBytes())
                ls.Write(this.PerCommitmentSecret.Value.ToBytes())
                ls.Write(this.NextPerCommitmentPoint.ToBytes())

    [<CLIMutable>]
    type UpdateFee = {
        mutable ChannelId: ChannelId
        mutable FeeRatePerKw: FeeRatePerKw
    }
    with
        interface IChannelMsg
        interface IUpdateMsg
        interface ILightningSerializable<UpdateFee> with
            member this.Deserialize(ls) =
                this.ChannelId <- ls.ReadUInt256(false) |> ChannelId
                this.FeeRatePerKw <- ls.ReadUInt32(false) |> FeeRatePerKw
            member this.Serialize(ls) =
                ls.Write(this.ChannelId.Value.ToBytes())
                ls.Write(this.FeeRatePerKw.Value, false)

    [<CLIMutable>]
    type DataLossProtect = {
        mutable YourLastPerCommitmentSecret: PaymentPreimage
        mutable MyCurrentPerCommitmentPoint: PubKey
    }

    [<CLIMutable>]
    type ChannelReestablish = {
        mutable ChannelId: ChannelId
        mutable NextLocalCommitmentNumber: uint64
        mutable NextRemoteCommitmentNumber: uint64
        mutable DataLossProtect: OptionalField<DataLossProtect>
    }
    with
        interface IChannelMsg
        interface ILightningSerializable<ChannelReestablish> with
            member this.Deserialize(ls) =
                this.ChannelId <- ls.ReadUInt256(false) |> ChannelId
                this.NextLocalCommitmentNumber <- ls.ReadUInt64(false)
                this.NextRemoteCommitmentNumber <- ls.ReadUInt64(false)
                this.DataLossProtect |> ignore
                failwith "Update optional field part"
            member this.Serialize(ls) =
                ls.Write(this.ChannelId.Value.ToBytes())
                ls.Write(Utils.ToBytes(this.NextLocalCommitmentNumber, false))
                ls.Write(Utils.ToBytes(this.NextRemoteCommitmentNumber, false))
                ls.Write(this.DataLossProtect |> Option.map(fun x -> x.YourLastPerCommitmentSecret.Value.ToBytes()))
                ls.Write(this.DataLossProtect |> Option.map(fun x -> x.MyCurrentPerCommitmentPoint.ToBytes()))

    [<CLIMutable>]
    type AnnouncementSignatures = {
        mutable ChannelId: ChannelId
        mutable ShortChannelId: ShortChannelId
        mutable NodeSignature: ECDSASignature
        mutable BitcoinSignature: ECDSASignature
    }
    with
        interface IRoutingMsg
        interface ILightningSerializable<AnnouncementSignatures> with
            member this.Deserialize(ls) =
                this.ChannelId <- ls.ReadUInt256(false) |> ChannelId
                this.ShortChannelId <- ls.ReadUInt64(false) |> ShortChannelId.FromUInt64
                this.NodeSignature <- ls.ReadECDSACompact()
                this.BitcoinSignature <- ls.ReadECDSACompact()
            member this.Serialize(ls) =
                ls.Write(this.ChannelId.Value.ToBytes())
                ls.Write(this.ShortChannelId)
                ls.Write(this.NodeSignature)
                ls.Write(this.BitcoinSignature)

    type NetAddress =
        | IPv4 of IPEndPoint
        | IPv6 of IPEndPoint
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
         match this with
         | IPv4 d ->
             ls.Write(d.Address.GetAddressBytes())
             ls.Write((uint16)d.Port, false)
         | IPv6 d ->
             ls.Write(d.Address.GetAddressBytes())
             ls.Write((uint16)d.Port, false)
         | OnionV2 d ->
             ls.Write(d.Addr)
             ls.Write((uint16)d.Port, false)
         | OnionV3 d ->
             ls.Write(d.ed25519PubKey)
             ls.Write(d.CheckSum, false)
             ls.Write(d.Version)
             ls.Write((uint16)d.Port, false)

        static member ReadFrom(ls) =
            failwith ""

    and OnionV2EndPoint = {
        Addr: byte[]
        Port: uint16
    }
    and OnionV3EndPoint = {
        ed25519PubKey: byte[]
        CheckSum: uint16
        Version: uint8
        Port: uint16
    }


    /// Only exposed as broadcast of node_announcement should be filtered by node_id
    /// The unsigned part of node_anouncement
    [<CLIMutable>]
    type UnsignedNodeAnnouncement = {
        mutable Features: GlobalFeatures
        mutable Timestamp: uint32
        mutable NodeId: NodeId
        mutable RGB: RGB
        mutable Alias: uint256
        mutable Addresses: NetAddress list
        mutable ExcessAddressData: byte[]
        mutable ExcessData: byte[]
    }
    with
        interface ILightningSerializable<UnsignedNodeAnnouncement> with
            member this.Deserialize(ls) =
                this.Features <- ls.ReadWithLen() |> GlobalFeatures.Flags
                this.Timestamp <- ls.ReadUInt32(false)
                this.NodeId <- ls.ReadPubKey() |> NodeId
                this.RGB <- ls.ReadRGB()
                this.Alias <- ls.ReadUInt256(false)
                let addrlen = ls.ReadUInt16(false)
                this.Addresses <- [1us..addrlen] |> List.map(fun a -> NetAddress.ReadFrom(ls))
                failwith "Update Excess data part"
            member this.Serialize(ls) =
                ls.WriteWithLen(this.Features.Value)
                ls.Write(this.Timestamp, false)
                ls.Write(this.NodeId.Value)
                ls.Write(this.RGB)
                ls.Write(this.Alias, true)
                let mutable addrLen = (this.Addresses |> List.sumBy(fun addr -> addr.Length + 1us)) // 1 byte for type field
                let excessAddrLen = (uint16 this.ExcessAddressData.Length)
                addrLen <- excessAddrLen + addrLen
                ls.Write(addrLen, false)
                this.Addresses
                    |> List.iter(fun addr -> ls.Write(addr.GetId()); addr.WriteTo(ls))
                ls.Write(this.ExcessAddressData)
                ls.Write(this.ExcessData)

    [<CLIMutable>]
    type NodeAnnouncement = {
        mutable Signature: ECDSASignature
        mutable Contents: UnsignedNodeAnnouncement
    }
    with
        interface IRoutingMsg
        interface ILightningSerializable<NodeAnnouncement> with
            member this.Deserialize(ls) =
                this.Signature <- ls.ReadECDSACompact()
                (this.Contents :> ILightningSerializable<UnsignedNodeAnnouncement>).Deserialize(ls)
            member this.Serialize(ls) =
                ls.Write(this.Signature)
                (this.Contents :> ILightningSerializable<UnsignedNodeAnnouncement>).Serialize(ls)


    [<CLIMutable>]
    type UnsignedChannelAnnouncement = {
        mutable Features: GlobalFeatures
        mutable ChainHash: uint256
        mutable ShortChannelId: ShortChannelId
        mutable NodeId1: NodeId
        mutable NodeId2: NodeId
        mutable BitcoinKey1: PubKey
        mutable BitcoinKey2: PubKey
        mutable ExcessData: byte[]
    }
    with
        interface ILightningSerializable<UnsignedChannelAnnouncement> with
            member this.Deserialize(ls) =
                this.Features <- ls.ReadWithLen() |> GlobalFeatures.Flags
                this.ChainHash <- ls.ReadUInt256(false)
                this.ShortChannelId <- ls.ReadUInt64(false) |> ShortChannelId.FromUInt64
                this.NodeId1 <- ls.ReadPubKey() |> NodeId
                this.NodeId2 <- ls.ReadPubKey() |> NodeId
                this.BitcoinKey1 <- ls.ReadPubKey()
                this.BitcoinKey2 <- ls.ReadPubKey()
                failwith "Handle Excessdata"
            member this.Serialize(ls) =
                ls.WriteWithLen(this.Features.Value)
                ls.Write(this.ChainHash, false)
                ls.Write(this.ShortChannelId)
                ls.Write(this.NodeId1.Value)
                ls.Write(this.NodeId2.Value)
                ls.Write(this.BitcoinKey1)
                ls.Write(this.BitcoinKey2)
                ls.Write(this.ExcessData)

    [<CLIMutable>]
    type ChannelAnnouncement = {
        mutable NodeSignature1: ECDSASignature
        mutable NodeSignature2: ECDSASignature
        mutable BitcoinSignature1: ECDSASignature
        mutable BitcoinSignature2: ECDSASignature
        mutable Contents: UnsignedChannelAnnouncement
    }
    with
        interface IRoutingMsg
        interface ILightningSerializable<ChannelAnnouncement> with
            member this.Deserialize(ls) =
                this.NodeSignature1 <- ls.ReadECDSACompact()
                this.NodeSignature2 <- ls.ReadECDSACompact()
                this.BitcoinSignature1 <- ls.ReadECDSACompact()
                this.BitcoinSignature2 <- ls.ReadECDSACompact()
                (this.Contents :> ILightningSerializable<UnsignedChannelAnnouncement>).Deserialize(ls)
            member this.Serialize(ls) =
                ls.Write(this.NodeSignature1)
                ls.Write(this.NodeSignature2)
                ls.Write(this.BitcoinSignature1)
                ls.Write(this.BitcoinSignature2)
                (this.Contents :> ILightningSerializable<UnsignedChannelAnnouncement>).Serialize(ls)

    [<CLIMutable>]
    type UnsignedChannelUpdate = {
        mutable ChainHash: uint256
        mutable ShortChannelId: ShortChannelId
        mutable Timestamp: uint32
        mutable Flags: uint16
        mutable CLTVExpiryDelta: BlockHeightOffset
        mutable HTLCMinimumMSat: LNMoney
        mutable FeeBaseMSat: LNMoney
        mutable FeeProportionalMillionths: uint32
        mutable ExcessData: byte[]
    }

    [<CLIMutable>]
    type ChannelUpdate = {
        mutable Signature: ECDSASignature
        mutable Contents: UnsignedChannelUpdate
    }
    with
        interface IRoutingMsg
        interface ILightningSerializable<ChannelUpdate> with
            member this.Deserialize(ls) =
                this.Signature <- ls.ReadECDSACompact()
                let chainHash = ls.ReadUInt256(false)
                let shortChannelId = ls.ReadUInt64(false) |> ShortChannelId.FromUInt64
                let ts = ls.ReadUInt32(false)
                let f = ls.ReadUInt16(false)
                let cltvE = ls.ReadUInt16(false) |> BlockHeightOffset
                let htlcMinimum = ls.ReadUInt64(false) |> LNMoney.MilliSatoshis
                let feeBase = ls.ReadUInt64(false) |> LNMoney.MilliSatoshis
                let feeProportionalMillionths = ls.ReadUInt32(false)
                this.Contents <- {
                    ChainHash = chainHash
                    ShortChannelId = shortChannelId
                    Timestamp = ts
                    Flags = f
                    CLTVExpiryDelta = cltvE
                    HTLCMinimumMSat = htlcMinimum
                    FeeBaseMSat = feeBase
                    FeeProportionalMillionths = feeProportionalMillionths
                    ExcessData = [||]
                }
                failwith "Handle Excess data"
            member this.Serialize(ls) =
                ls.Write(this.Signature)
                ls.Write(this.Contents.ChainHash, false)
                ls.Write(this.Contents.ShortChannelId)
                ls.Write(this.Contents.Timestamp, false)
                ls.Write(this.Contents.Flags, false)
                ls.Write(this.Contents.CLTVExpiryDelta.Value, false)
                ls.Write(this.Contents.HTLCMinimumMSat.MilliSatoshi, false)
                ls.Write((uint32) this.Contents.FeeBaseMSat.MilliSatoshi, false)
                ls.Write((uint32) this.Contents.FeeProportionalMillionths, false)
                ls.Write(this.Contents.ExcessData)

    type ErrorAction = 
        | DisconnectPeer of ErrorMessage option
        | IgnoreError
        | SendErrorMessage of ErrorMessage

    type HandleError = {
        Error: string
        Action: ErrorAction option
    }

    /// Struct used to return valeus from revoke_and_ack messages, cotaining a bunch of commitment
    /// transaction updates if they were pending.
    type CommitmentUpdate = {
        UpdateAddHTLCs: UpdateAddHTLC list
        UpdateFulfillHTLCs: UpdateFulfillHTLC list
        UpdateFailHTLCs: UpdateFailHTLC list
        UpdateFailMalformedHTLCs: UpdateFailMalformedHTLC list
        UpdateFee: UpdateFee option
        CommitmentSigned: CommitmentSigned
    }

    /// The information we received from a peer along the route of a payment we originated. This is
    /// returned by ChannelMessageHandler.HandleUpdateFailHTLC to be passed into
    /// RoutingMessageHandler.HandleHTLCFailChannelUpdate to update our network map.
    type HTLCFailChannelUpdate = {
        ChannelUpdateMessage: ChannelUpdate
        ChannelClosed: ChannelClosed
        NodeFailure: NodeFailure
    }

    and ChannelClosed = {
        ShortChannelId: ShortChannelId
        /// when this true, this channel should be permanently removed from the
        /// consideration. Otherwiser, this channel can be restored as new ChannelUpdate is received
        IsPermanent: bool
    }
    and NodeFailure = {
        NodeId: NodeId
        IsPermanent: bool
    }

    [<Struct>]
    type LightningMsgHeader =
        | InitType = 16us
        | ErrorType = 17us
        | PingType = 18us
        | PongType = 19us
        | OpenChannelType = 32us
        | AcceptChannelType = 33us
        | FundingCreatedType = 34us
        | FundingSignedType = 35us
        | FundingLockedType = 36us
        | ShutDownType = 38us
        | ClosingSignedType= 39us
        | UpdateAddHTLCType= 128us
        | UpdateFulFillHTLCtype = 130us
        | UpdateFailHTLCType= 131us
        | UpdateMalformedHTLCType = 135us
        | CommitmentSignedType = 132us
        | RevokeAndACKType = 133us
        | UpdateFeeType = 134us
        | ChannelReestablishType = 136us
        | AnnouncementSignatureType = 259us
        | ChannelAnnouncementType = 256us
        | NodeAnnouncementType = 257us
        | ChannelUpdateType = 258us


    let lnMsgType = typeof<LightningMsgHeader>

    /// TODO: use Active Pattern when possible
    let matchMsgType (f : uint16) =
        if Enum.IsDefined(lnMsgType, f) then
            ValueSome (LanguagePrimitives.EnumOfValue f)
        else
            ValueNone



    // -----------
    type IChannelMessageHandler =
        abstract member HandleOpenChannel: (NodeId * OpenChannel) -> RResult<unit>
        abstract member HandleAcceptChannel: (NodeId * AcceptChannel) -> RResult<unit>
        abstract member HandleFundingCreated: (NodeId * FundingCreated) -> RResult<unit>
        abstract member HandleFundingSigned: (NodeId * FundingSigned) -> RResult<unit>
        abstract member HandleFundingLocked: (NodeId * FundingLocked) -> RResult<unit>
        abstract member HandleShutdown: (NodeId * Shutdown) -> RResult<unit>
        abstract member HandleClosingSigned: (NodeId * ClosingSigned) -> RResult<unit>
        abstract member HandleUpdateAddHTLC: (NodeId * UpdateAddHTLC) -> RResult<unit>
        abstract member HandleUpdateFulfillHTLC: (NodeId * UpdateFulfillHTLC) -> RResult<unit>
        abstract member HandleUpdateFailHTLC: (NodeId * UpdateFailHTLC) -> RResult<unit>
        abstract member HandleUpdateFailMalformedHTLC: (NodeId * UpdateFailMalformedHTLC) -> RResult<unit>
        abstract member HandleCommitmentSigned: (NodeId * CommitmentSigned) -> RResult<unit>
        abstract member HandleRevokeAndACK: (NodeId * RevokeAndACK) -> RResult<unit>
        abstract member HandleUpdateFee: (NodeId * UpdateFee) -> RResult<unit>
        abstract member HandleAnnouncementSignatures: (NodeId * AnnouncementSignatures) -> RResult<unit>
        abstract member PeerDisconnected: (NodeId * bool) -> RResult<unit>
        abstract member PeerConnected: (NodeId) -> RResult<unit>
        abstract member HandleChannelReestablish: their: NodeId * msg: ChannelReestablish -> RResult<unit>
        abstract member HandleError: their: NodeId * msg: ErrorMessage -> RResult<unit>

    type IRoutingMessageHandler =
        abstract member HandleNodeAnnouncement: msg:NodeAnnouncement -> RResult<bool>
        abstract member HandleChannelAnnouncement: msg:ChannelAnnouncement -> RResult<bool>
        abstract member HandleChannelUpdate: msg:ChannelUpdate -> RResult<bool>
        abstract member HandleHTLCFailChannelUpdate: msg:HTLCFailChannelUpdate -> RResult<unit>
        abstract member GetNextChannelAnnouncements: startingPoint: ShortChannelId * batchAmount: uint8 -> (ChannelAnnouncement * ChannelUpdate * ChannelUpdate) list
        abstract member GetNextNodeAnnouncements: startingPoint: PubKey option * batchAMount: uint8 -> NodeAnnouncement list

    type OnionRealm0HopData = {
        ShortChannelId: ShortChannelId
        AmtToForward: LNMoney
        OutgoingCLTVValue: uint32
    }
