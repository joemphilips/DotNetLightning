namespace DotNetLightning.Serialize
open DotNetLightning.Core.Utils
open NBitcoin
open DotNetLightning.Utils
open System
open System.Collections
open System.Collections
open System.Runtime.CompilerServices
open System.IO
open DotNetLightning.Utils.OnionError
open DotNetLightning.Core.Utils.Extensions

// #region serialization
module rec Msgs =

    type P2PDecodeError =
        | UnknownVersion
        | UnknownRequiredFeature
        | InvalidValue
        | ExtraAddressesPerType
        | BadLengthDescriptor
        | UnexpectedEndOfStream of EndOfStreamException
        | IO of IOException

    type UnknownVersionException(msg) =
        inherit FormatException(msg)
    
    type UnknownRequiredFeatureException(msg) =
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


    type ILightningMsg = interface end
    type ISetupMsg = inherit ILightningMsg
    type IChannelMsg = inherit ILightningMsg
    type IHTLCMsg = inherit IChannelMsg
    type IUpdateMsg = inherit IChannelMsg
    type IRoutingMsg = inherit ILightningMsg

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
                deserialize<Init>(ls) :> ILightningMsg
            | TypeFlag.Error ->
                deserialize<ErrorMessage>(ls) :> ILightningMsg
            | TypeFlag.Ping ->
                deserialize<Ping>(ls) :> ILightningMsg
            | TypeFlag.Pong ->
                deserialize<Pong>(ls) :> ILightningMsg
            | TypeFlag.OpenChannel ->
                deserialize<OpenChannel>(ls) :> ILightningMsg
            | TypeFlag.AcceptChannel ->
                deserialize<AcceptChannel>(ls) :> ILightningMsg
            | TypeFlag.FundingCreated ->
                deserialize<FundingCreated>(ls) :> ILightningMsg
            | TypeFlag.FundingSigned ->
                deserialize<FundingSigned>(ls) :> ILightningMsg
            | TypeFlag.FundingLocked ->
                deserialize<FundingLocked>(ls) :> ILightningMsg
            | TypeFlag.Shutdown ->
                deserialize<Shutdown>(ls) :> ILightningMsg
            | TypeFlag.ClosingSigned ->
                deserialize<ClosingSigned>(ls) :> ILightningMsg
            | TypeFlag.UpdateAddHTLC ->
                deserialize<UpdateAddHTLC>(ls) :> ILightningMsg
            | TypeFlag.UpdateFulfillHTLC ->
                deserialize<UpdateFulfillHTLC>(ls) :> ILightningMsg
            | TypeFlag.UpdateFailHTLC ->
                deserialize<UpdateFailHTLC>(ls) :> ILightningMsg
            | TypeFlag.UpdateFailMalformedHTLC ->
                deserialize<UpdateFailMalformedHTLC>(ls) :> ILightningMsg
            | TypeFlag.ChannelReestablish ->
                deserialize<ChannelReestablish>(ls) :> ILightningMsg
            | TypeFlag.CommitmentSigned ->
                deserialize<CommitmentSigned>(ls) :> ILightningMsg
            | TypeFlag.RevokeAndACK ->
                deserialize<RevokeAndACK>(ls) :> ILightningMsg
            | TypeFlag.UpdateFee ->
                deserialize<UpdateFee>(ls) :> ILightningMsg
            | TypeFlag.AnnouncementSignatures ->
                deserialize<AnnouncementSignatures>(ls) :> ILightningMsg
            | TypeFlag.ChannelAnnouncement ->
                deserialize<ChannelAnnouncement>(ls) :> ILightningMsg
            | TypeFlag.NodeAnnouncement ->
                deserialize<NodeAnnouncement>(ls) :> ILightningMsg
            | TypeFlag.ChannelUpdate ->
                deserialize<ChannelUpdate>(ls) :> ILightningMsg
            | x ->
                raise <| FormatException(sprintf "Unknown message type %d" x)
        let serializeWithFlags (ls: LightningWriterStream) (data: ILightningMsg) =
            match data with
            | :? Init as d ->
                ls.Write(TypeFlag.Init, false)
                (d :> ILightningSerializable<Init>).Serialize(ls)
            | :? ErrorMessage as d ->
                ls.Write(TypeFlag.Error, false)
                (d :> ILightningSerializable<ErrorMessage>).Serialize(ls)
            | :? Ping as d ->
                ls.Write(TypeFlag.Ping, false)
                (d :> ILightningSerializable<Ping>).Serialize(ls)
            | :? Pong as d ->
                ls.Write(TypeFlag.Pong, false)
                (d :> ILightningSerializable<Pong>).Serialize(ls)
            | :? OpenChannel as d ->
                ls.Write(TypeFlag.OpenChannel, false)
                (d :> ILightningSerializable<OpenChannel>).Serialize(ls)
            | :? AcceptChannel as d ->
                ls.Write(TypeFlag.AcceptChannel, false)
                (d :> ILightningSerializable<AcceptChannel>).Serialize(ls)
            | :? FundingCreated as d ->
                ls.Write(TypeFlag.FundingCreated, false)
                (d :> ILightningSerializable<FundingCreated>).Serialize(ls)
            | :? FundingSigned as d ->
                ls.Write(TypeFlag.FundingSigned, false)
                (d :> ILightningSerializable<FundingSigned>).Serialize(ls)
            | :? FundingLocked as d ->
                ls.Write(TypeFlag.FundingLocked, false)
                (d :> ILightningSerializable<FundingLocked>).Serialize(ls)
            | :? Shutdown as d ->
                ls.Write(TypeFlag.Shutdown, false)
                (d :> ILightningSerializable<Shutdown>).Serialize(ls)
            | :? ClosingSigned as d ->
                ls.Write(TypeFlag.ClosingSigned, false)
                (d :> ILightningSerializable<ClosingSigned>).Serialize(ls)
            | :? UpdateAddHTLC as d ->
                ls.Write(TypeFlag.UpdateAddHTLC, false)
                (d :> ILightningSerializable<UpdateAddHTLC>).Serialize(ls)
            | :? UpdateFulfillHTLC as d ->
                ls.Write(TypeFlag.UpdateFulfillHTLC, false)
                (d :> ILightningSerializable<UpdateFulfillHTLC>).Serialize(ls)
            | :? UpdateFailHTLC as d ->
                ls.Write(TypeFlag.UpdateFailHTLC, false)
                (d :> ILightningSerializable<UpdateFailHTLC>).Serialize(ls)
            | :? UpdateFailMalformedHTLC as d ->
                ls.Write(TypeFlag.UpdateFailMalformedHTLC, false)
                (d :> ILightningSerializable<UpdateFailMalformedHTLC>).Serialize(ls)
            | :? ChannelReestablish as d ->
                ls.Write(TypeFlag.ChannelReestablish, false)
                (d :> ILightningSerializable<ChannelReestablish>).Serialize(ls)
            | :? CommitmentSigned as d ->
                ls.Write(TypeFlag.CommitmentSigned, false)
                (d :> ILightningSerializable<CommitmentSigned>).Serialize(ls)
            | :? RevokeAndACK as d ->
                ls.Write(TypeFlag.RevokeAndACK, false)
                (d :> ILightningSerializable<RevokeAndACK>).Serialize(ls)
            | :? UpdateFee as d ->
                ls.Write(TypeFlag.UpdateFee, false)
                (d :> ILightningSerializable<UpdateFee>).Serialize(ls)
            | :? AnnouncementSignatures as d ->
                ls.Write(TypeFlag.AnnouncementSignatures, false)
                (d :> ILightningSerializable<AnnouncementSignatures>).Serialize(ls)
            | :? ChannelAnnouncement as d ->
                ls.Write(TypeFlag.ChannelAnnouncement, false)
                (d :> ILightningSerializable<ChannelAnnouncement>).Serialize(ls)
            | :? NodeAnnouncement as d ->
                ls.Write(TypeFlag.NodeAnnouncement, false)
                (d :> ILightningSerializable<NodeAnnouncement>).Serialize(ls)
            | :? ChannelUpdate as d ->
                ls.Write(TypeFlag.ChannelUpdate, false)
                (d :> ILightningSerializable<ChannelUpdate>).Serialize(ls)
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

    type InitTLV =
        /// genesis chain hash that the node is interested in
        | Networks of uint256 array
        | Unknown of GenericTLV
        with
        static member FromGenericTLV(tlv: GenericTLV) =
            match tlv.Type with
            | 1UL ->
                let n, rem = Math.DivRem(tlv.Value.Length, 32)
                if rem <> 0 then raise <| FormatException(sprintf "Bogus length for TLV in init message %d" tlv.Value.Length) else
                let result = Array.zeroCreate n
                for i in 0..n - 1 do
                    result.[i] <- tlv.Value.[(n * 32)..(n * 32 + 31)] |> uint256
                result |> Networks
            | _ -> Unknown (tlv)
            
        member this.ToGenericTLV() =
            match this with
            | Networks networks ->
                let v = networks |> Array.map(fun x -> x.ToBytes()) |> Array.concat
                { GenericTLV.Type = 1UL; Value = v }
            | Unknown tlv -> tlv
            

    [<CLIMutable>]
    type Init =
        {
            mutable Features: BitArray
            mutable TLVStream: InitTLV array
        }
        with
            interface ISetupMsg
            interface ILightningSerializable<Init> with
                member this.Deserialize(ls: LightningReaderStream) =
                    // For backwards compatibility reason, we must consider legacy `global features` section. (see bolt 1)
                    let globalFeatures = ls.ReadWithLen()
                    let localFeatures = ls.ReadWithLen()
                    this.Features <- Array.concat [globalFeatures; localFeatures] |> BitArray.FromBytes
                    this.TLVStream <- ls.ReadTLVStream() |> Array.map(InitTLV.FromGenericTLV)
                member this.Serialize(ls) =
                    // For backwards compatibility reason, we must consider legacy `global features` section. (see bolt 1)
                    let g: byte[] = [||]
                    ls.WriteWithLen(g)
                    ls.WriteWithLen(this.Features.ToByteArray())
                    ls.WriteTLVStream(this.TLVStream |> Array.map(fun tlv -> tlv.ToGenericTLV()))

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
                this.FundingSatoshis <- Money.Satoshis(ls.ReadUInt64(false))
                this.PushMSat <- LNMoney.MilliSatoshis(ls.ReadUInt64(false))
                this.DustLimitSatoshis <- Money.Satoshis(ls.ReadUInt64(false))
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
                this.ShutdownScriptPubKey <-
                    if (ls.Position = ls.Length) then None else
                    ls.ReadWithLen() |> Script |> Some
            member this.Serialize(ls) =
                ls.Write(this.Chainhash, false)
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
    type AcceptChannel = {
        mutable TemporaryChannelId: ChannelId
        mutable DustLimitSatoshis: Money
        mutable MaxHTLCValueInFlightMsat: LNMoney
        mutable ChannelReserveSatoshis: Money
        mutable HTLCMinimumMSat: LNMoney
        mutable MinimumDepth: BlockHeight
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
                this.TemporaryChannelId <- ChannelId(ls.ReadUInt256(true))
                this.DustLimitSatoshis <- ls.ReadUInt64(false) |> Money.Satoshis
                this.MaxHTLCValueInFlightMsat <- ls.ReadUInt64(false) |> LNMoney.MilliSatoshis
                this.ChannelReserveSatoshis <- ls.ReadUInt64(false) |> Money.Satoshis
                this.HTLCMinimumMSat <- ls.ReadUInt64(false) |> LNMoney.MilliSatoshis
                this.MinimumDepth <- ls.ReadUInt32(false) |> BlockHeight
                this.ToSelfDelay <- ls.ReadUInt16(false) |> BlockHeightOffset
                this.MaxAcceptedHTLCs <- ls.ReadUInt16(false)
                this.FundingPubKey <- ls.ReadPubKey()
                this.RevocationBasepoint <- ls.ReadPubKey()
                this.PaymentBasepoint <- ls.ReadPubKey()
                this.DelayedPaymentBasepoint <- ls.ReadPubKey()
                this.HTLCBasepoint <- ls.ReadPubKey()
                this.FirstPerCommitmentPoint <- ls.ReadPubKey()
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
    type FundingCreated = {
        mutable TemporaryChannelId: ChannelId
        mutable FundingTxId: TxId
        mutable FundingOutputIndex: TxOutIndex
        mutable Signature: LNECDSASignature
    }
    with
        interface IChannelMsg
        interface ILightningSerializable<FundingCreated> with
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
    type FundingSigned = {
        mutable ChannelId: ChannelId
        mutable Signature: LNECDSASignature
    }
    with
        interface IChannelMsg
        interface ILightningSerializable<FundingSigned> with
            member this.Deserialize(ls) =
                this.ChannelId <- ChannelId(ls.ReadUInt256(true))
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
                this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
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
                this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
                this.ScriptPubKey <- ls.ReadScript()
            member this.Serialize(ls) =
                ls.Write(this.ChannelId.Value.ToBytes())
                ls.WriteWithLen(this.ScriptPubKey.ToBytes())

    [<CLIMutable>]
    type ClosingSigned = {
        mutable ChannelId: ChannelId
        mutable FeeSatoshis: Money
        mutable Signature: LNECDSASignature
    }
    with
        interface IChannelMsg
        interface ILightningSerializable<ClosingSigned> with
            member this.Deserialize(ls) =
                this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
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
        mutable CLTVExpiry: BlockHeight
        mutable OnionRoutingPacket: OnionPacket
    }
    with
        interface IHTLCMsg
        interface IUpdateMsg
        interface ILightningSerializable<UpdateAddHTLC> with
            member this.Deserialize(ls) =
                this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
                this.HTLCId <- ls.ReadUInt64(false) |> HTLCId
                this.AmountMSat <- ls.ReadUInt64(false) |> LNMoney.MilliSatoshis
                this.PaymentHash <- ls.ReadUInt256(true) |> PaymentHash
                this.CLTVExpiry <- ls.ReadUInt32(false) |> BlockHeight
                this.OnionRoutingPacket <- ILightningSerializable.deserialize<OnionPacket>(ls)
            member this.Serialize(ls) =
                ls.Write(this.ChannelId.Value.ToBytes())
                ls.Write(this.HTLCId.Value, false)
                ls.Write(this.AmountMSat.MilliSatoshi, false)
                ls.Write(this.PaymentHash.Value.ToBytes())
                ls.Write(this.CLTVExpiry.Value, false)
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
                this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
                this.HTLCId <- ls.ReadUInt64(false) |> HTLCId
                this.PaymentPreimage <- ls.ReadBytes PaymentPreimage.LENGTH |> PaymentPreimage.Create
            member this.Serialize(ls) =
                ls.Write(this.ChannelId.Value.ToBytes())
                ls.Write(this.HTLCId.Value, false)
                ls.Write(this.PaymentPreimage.ToByteArray())

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
                this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
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
        mutable FailureCode: FailureCode
    }
    with
        interface IHTLCMsg
        interface IUpdateMsg
        interface ILightningSerializable<UpdateFailMalformedHTLC> with
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
    type CommitmentSigned = {
        mutable ChannelId: ChannelId
        mutable Signature: LNECDSASignature
        mutable HTLCSignatures: LNECDSASignature list
    }
    with
        interface IHTLCMsg
        interface ILightningSerializable<CommitmentSigned> with
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
    type RevokeAndACK = {
        mutable ChannelId: ChannelId
        mutable PerCommitmentSecret: PaymentPreimage
        mutable NextPerCommitmentPoint: PubKey
    }
    with
        interface IHTLCMsg
        interface ILightningSerializable<RevokeAndACK> with
            member this.Deserialize(ls) =
                this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
                this.PerCommitmentSecret <- ls.ReadBytes PaymentPreimage.LENGTH |> PaymentPreimage.Create
                this.NextPerCommitmentPoint <- ls.ReadPubKey()
            member this.Serialize(ls) =
                ls.Write(this.ChannelId.Value.ToBytes())
                ls.Write(this.PerCommitmentSecret.ToByteArray())
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
                this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
                this.FeeRatePerKw <- ls.ReadUInt32(false) |> FeeRatePerKw
            member this.Serialize(ls) =
                ls.Write(this.ChannelId.Value.ToBytes())
                ls.Write(this.FeeRatePerKw.Value, false)

    [<CLIMutable>]
    type DataLossProtect = {
        mutable YourLastPerCommitmentSecret: PaymentPreimage
        mutable MyCurrentPerCommitmentPoint: PubKey
    }
        with
            interface ILightningSerializable<DataLossProtect> with
                member this.Deserialize(ls: LightningReaderStream) =
                    this.YourLastPerCommitmentSecret <- ls.ReadBytes PaymentPreimage.LENGTH |> PaymentPreimage.Create
                    this.MyCurrentPerCommitmentPoint <- ls.ReadPubKey()
                member this.Serialize(ls: LightningWriterStream): unit = 
                    ls.Write(this.YourLastPerCommitmentSecret.ToByteArray())
                    ls.Write(this.MyCurrentPerCommitmentPoint)


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
                this.ChannelId <- ls.ReadUInt256(true) |> ChannelId
                this.NextLocalCommitmentNumber <- ls.ReadUInt64(false)
                this.NextRemoteCommitmentNumber <- ls.ReadUInt64(false)
                this.DataLossProtect <- ls.TryReadAll() |> Option.map ILightningSerializable.fromBytes<DataLossProtect>
            member this.Serialize(ls) =
                ls.Write(this.ChannelId.Value.ToBytes())
                ls.Write(Utils.ToBytes(this.NextLocalCommitmentNumber, false))
                ls.Write(Utils.ToBytes(this.NextRemoteCommitmentNumber, false))
                ls.Write(this.DataLossProtect |> Option.map(fun x -> x.YourLastPerCommitmentSecret.ToByteArray()))
                ls.Write(this.DataLossProtect |> Option.map(fun x -> x.MyCurrentPerCommitmentPoint.ToBytes()))

    [<CLIMutable>]
    type AnnouncementSignatures = {
        mutable ChannelId: ChannelId
        mutable ShortChannelId: ShortChannelId
        mutable NodeSignature: LNECDSASignature
        mutable BitcoinSignature: LNECDSASignature
    }
    with
        interface IRoutingMsg
        interface ILightningSerializable<AnnouncementSignatures> with
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
    type UnsignedNodeAnnouncement = {
        mutable Features: byte[]
        mutable Timestamp: uint32
        mutable NodeId: NodeId
        mutable RGB: RGB
        mutable Alias: uint256
        mutable Addresses: NetAddress []
        mutable ExcessAddressData: byte[]
        mutable ExcessData: byte[]
    }
    with
        interface ILightningSerializable<UnsignedNodeAnnouncement> with
            member this.Deserialize(ls) =
                this.Features <- ls.ReadWithLen()
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
                ls.WriteWithLen(this.Features)
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
    type NodeAnnouncement = {
        mutable Signature: LNECDSASignature
        mutable Contents: UnsignedNodeAnnouncement
    }
    with
        interface IRoutingMsg
        interface ILightningSerializable<NodeAnnouncement> with
            member this.Deserialize(ls) =
                this.Signature <- ls.ReadECDSACompact()
                this.Contents <- ILightningSerializable.deserialize<UnsignedNodeAnnouncement>(ls)
            member this.Serialize(ls) =
                ls.Write(this.Signature)
                (this.Contents :> ILightningSerializable<UnsignedNodeAnnouncement>).Serialize(ls)


    [<CLIMutable>]
    type UnsignedChannelAnnouncement = {
        mutable Features: BitArray
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
                this.Features <-
                    let g = ls.ReadWithLen() |> BitArray.FromBytes
                    if Feature.areSupported(g) then
                        raise <| UnknownRequiredFeatureException(sprintf "Channel Annoucement contains Unknown requied feature %A" (g.PrintBits()))
                    else
                        g
                this.ChainHash <- ls.ReadUInt256(false)
                this.ShortChannelId <- ls.ReadUInt64(false) |> ShortChannelId.FromUInt64
                this.NodeId1 <- ls.ReadPubKey() |> NodeId
                this.NodeId2 <- ls.ReadPubKey() |> NodeId
                this.BitcoinKey1 <- ls.ReadPubKey()
                this.BitcoinKey2 <- ls.ReadPubKey()
                this.ExcessData <- match ls.TryReadAll() with Some b -> b | None -> [||]
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
        mutable NodeSignature1: LNECDSASignature
        mutable NodeSignature2: LNECDSASignature
        mutable BitcoinSignature1: LNECDSASignature
        mutable BitcoinSignature2: LNECDSASignature
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
                this.Contents <- ILightningSerializable.deserialize<UnsignedChannelAnnouncement>(ls)
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
        with
            interface IRoutingMsg
            interface  ILightningSerializable<UnsignedChannelUpdate> with
                member this.Deserialize(ls: LightningReaderStream): unit = 
                    this.ChainHash <- ls.ReadUInt256(false)
                    this.ShortChannelId <- ls.ReadUInt64(false) |> ShortChannelId.FromUInt64
                    this.Timestamp <- ls.ReadUInt32(false)
                    this.Flags <- ls.ReadUInt16(false)
                    this.CLTVExpiryDelta <- ls.ReadUInt16(false) |> BlockHeightOffset
                    this.HTLCMinimumMSat <- ls.ReadUInt64(false) |> LNMoney.MilliSatoshis
                    this.FeeBaseMSat <- ls.ReadUInt32(false) |> uint64 |> LNMoney.MilliSatoshis
                    this.FeeProportionalMillionths <- ls.ReadUInt32(false)
                    this.ExcessData <- match ls.TryReadAll() with | Some v -> v | None -> [||]
                member this.Serialize(ls: LightningWriterStream): unit = 
                    ls.Write(this.ChainHash, false)
                    ls.Write(this.ShortChannelId)
                    ls.Write(this.Timestamp, false)
                    ls.Write(this.Flags, false)
                    ls.Write(this.CLTVExpiryDelta.Value, false)
                    ls.Write(this.HTLCMinimumMSat.MilliSatoshi, false)
                    ls.Write(uint32 this.FeeBaseMSat.MilliSatoshi, false)
                    ls.Write(uint32 this.FeeProportionalMillionths, false)
                    ls.Write(this.ExcessData)


    [<CLIMutable>]
    type ChannelUpdate = {
        mutable Signature: LNECDSASignature
        mutable Contents: UnsignedChannelUpdate
    }
    with
        interface IRoutingMsg
        interface ILightningSerializable<ChannelUpdate> with
            member this.Deserialize(ls) =
                this.Signature <- ls.ReadECDSACompact()
                this.Contents <- ILightningSerializable.deserialize<UnsignedChannelUpdate>(ls)
            member this.Serialize(ls) =
                ls.Write(this.Signature)
                (this.Contents :> ILightningSerializable<UnsignedChannelUpdate>).Serialize(ls)

    type FailureMsgData =
        | InvalidRealm
        | TemporaryNodeFailure
        | PermanentNodeFailure
        | RequiredNodeFeatureMissing
        | InvalidOnionVersion of onionHash: uint256
        | InvalidOnionHmac of onionHash: uint256
        | InvalidOnionKey of onionHash: uint256
        | TemporaryChannelFailure of update: ChannelUpdate
        | PermanentChannelFailure
        | RequiredChannelFeatureMissing
        | UnknownNextPeer
        | AmountBelowMinimum of amount: LNMoney * update: ChannelUpdate
        | FeeInsufficient of amount: LNMoney * update: ChannelUpdate
        | ChannelDisabled of Flags: uint16 * update: ChannelUpdate
        | IncorrectCLTVExpiry of expiry: BlockHeight  * update: ChannelUpdate
        | UnknownPaymentHash
        | IncorrectPaymentAmount
        | ExpiryTooSoon of update: ChannelUpdate
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
                        let d = ILightningSerializable.deserialize<ChannelUpdate>(r)
                        this.Data <- d |> TemporaryChannelFailure
                    | (PERMANENT_CHANNEL_FAILURE) ->
                        this.Data <- PermanentChannelFailure
                    | (REQUIRED_CHANNEL_FEATURE_MISSING) ->
                        this.Data <- RequiredChannelFeatureMissing
                    | (UNKNOWN_NEXT_PEER) ->
                        this.Data <- UnknownNextPeer
                    | (AMOUNT_BELOW_MINIMUM) ->
                        let amountMSat = r.ReadUInt64(false) |> LNMoney.MilliSatoshis
                        let d = ILightningSerializable.init<ChannelUpdate>()
                        (d :> ILightningSerializable<ChannelUpdate>).Deserialize(r)
                        this.Data <- (amountMSat, d) |> AmountBelowMinimum
                    | (FEE_INSUFFICIENT) ->
                        let amountMSat = r.ReadUInt64(false) |> LNMoney.MilliSatoshis
                        let d = ILightningSerializable.init<ChannelUpdate>()
                        (d :> ILightningSerializable<ChannelUpdate>).Deserialize(r)
                        this.Data <- (amountMSat, d) |> FeeInsufficient
                    | (CHANNEL_DISABLED) ->
                        let flags = r.ReadUInt16(false)
                        let d = ILightningSerializable.deserialize<ChannelUpdate>(r)
                        this.Data <- (flags, d ) |> ChannelDisabled
                    | (INOCCORRECT_CLTV_EXPIRY) ->
                        let expiry = r.ReadUInt32(false) |> BlockHeight
                        let d = ILightningSerializable.deserialize<ChannelUpdate>(r)
                        this.Data <- (expiry, d) |> IncorrectCLTVExpiry
                    | (UNKNOWN_PAYMENT_HASH) ->
                        this.Data <- UnknownPaymentHash
                    | (INCORRECT_PAYMENT_AMOUNT) ->
                        this.Data <- IncorrectPaymentAmount
                    | (EXPIRY_TOO_SOON) ->
                        let d = ILightningSerializable.deserialize<ChannelUpdate>(r)
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
                    | d ->
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
                        (update :> ILightningSerializable<ChannelUpdate>).SerializeWithLen(w)
                    | AmountBelowMinimum (amount, update) ->
                        w.Write(uint64 amount.Value, false)
                        (update :> ILightningSerializable<ChannelUpdate>).SerializeWithLen(w)
                    | FeeInsufficient (amount, update) ->
                        w.Write(uint64 amount.Value, false)
                        (update :> ILightningSerializable<ChannelUpdate>).SerializeWithLen(w)
                    | ChannelDisabled (flags, update) ->
                        w.Write(flags, false)
                        (update :> ILightningSerializable<ChannelUpdate>).SerializeWithLen(w)
                    | IncorrectCLTVExpiry (expiry, update) ->
                        w.Write(expiry.Value, false)
                        (update :> ILightningSerializable<ChannelUpdate>).SerializeWithLen(w)
                    | ExpiryTooSoon (update) ->
                        (update :> ILightningSerializable<ChannelUpdate>).SerializeWithLen(w)
                    | FinalIncorrectCLTVExpiry (expiry) ->
                        w.Write(expiry.Value, false)
                    | FinalIncorrectCLTVAmount (amountMSat) ->
                        w.Write(amountMSat.Value, false)
                    | Unknown b ->
                        w.Write(b)
                    | _ ->
                        ()



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
                    match ls.ReadUInt256(true) with
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

            member this.GetFailureMsgData() =
                System.Text.ASCIIEncoding.ASCII.GetString(this.Data)

    and WhichChannel =
        | SpecificChannel of ChannelId
        | All

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


    type OnionRealm0HopData = {
        ShortChannelId: ShortChannelId
        AmtToForward: LNMoney
        OutgoingCLTVValue: uint32
    }
