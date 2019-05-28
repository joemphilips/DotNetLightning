module DotNetLightning.Serialize.Msgs
open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Utils.RResult
open System.IO
open BTCPayServer.Lightning
open System.Net
open NBitcoin.Crypto
open System.Runtime.CompilerServices
open NBitcoin.Crypto
open System

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
    let SetInitialRoutingSync(x: LocalFeatures) =
        if
            x.Value.Length = 0
        then
            Flags [|1uy <<< 3|]
        else
            x.Value.[0] <- (x.Value.[0] ||| (1uy <<< 3))
            x

// #region serialization


type ILightningSerializable<'T> =
    abstract Serialize: stream: Stream -> Stream
    abstract ToBytes: unit -> byte[]
    abstract Deserialize: stream: Stream -> Stream
    abstract FromBytes: byte[] -> 'T

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

// #endregion 

[<Struct>]
type GlobalFeatures =
    Flags of uint8[]
        member x.Value = let (Flags v) = x in v

// ---------- network message primitives
type OptionalField<'T> = 'T option

[<Struct>]
type OnionPacket = {
    Version: uint8
    PublicKey: PubKey
    HopData: byte[]
    HMAC: uint256
}

type OnionErrorPacket = {
    Data: byte[]
}

let ToBinaryArray(f: BinaryWriter -> unit) =
    use ms = new MemoryStream()
    use os = new BinaryWriter(ms)
    f(os)
    ms.ToArray()

type Init =
    {
        GlobalFeatures: GlobalFeatures
        LocalFeatures: LocalFeatures
    } with
   interface ILightningSerializable<Init> with
        member this.Deserialize(stream: Stream): Stream = 
            failwith "Not Implemented"
        member this.FromBytes(arg1: byte []): Init = 
            failwith "Not Implemented"
        member this.Serialize(stream: Stream): Stream = 
            failwith "Not Implemented"
        override this.ToBytes() = ToBinaryArray(fun os ->
                let (Flags f) = this.GlobalFeatures
                os.Write(f)
            )


type ErrorMessage = 
    {
        ChannelId: WhichChannel
        Data: byte[]
    }
and WhichChannel =
    | ChannelId of ChannelId
    | All

type Ping = {
    PongLen: uint16
    BytesLen: uint16
}

type Pong = {
    BytesLen: uint16
}

type OpenChannel = {
    Chainhash: uint256
    TemporaryChannelId: ChannelId
    FundingSatoshis: Money
    PushMSat: LightMoney
    DustLimitSatoshis: Money
    MaxHTLCValueInFlightMsat: LightMoney
    ChannelReserveSatoshis: Money
    HTLCMinimumMsat: LightMoney
    FeeRatePerKw: Money
    ToSelfDelay: uint16
    MaxAcceptedHTLCs: uint16
    FundingPubKey: PubKey
    RevocationBasepoint: PubKey
    PaymentBasepoint: PubKey
    DelayedPaymentBasepoint: PubKey
    HTLCBasepoint: PubKey
    FirstPerCommitmentPoint: PubKey
    ChannelFlags: uint8
    ShutdownScriptPubKey: OptionalField<Script>
}
type AcceptChannel = {
    TemporaryChannelId: ChannelId
    DustLimitSatoshis: Money
    MaxHTLCValueInFlightMsat: uint64
    ChannelReserveSatoshis: uint64
    HTLCMinimumMSat: uint64
    MinimumDepth: uint32
    ToSelfDelay: DateTime
    MaxAcceptedHTLCs: uint16
    FundingPubKey: PubKey
    RevocationBasepoint: PubKey
    PaymentBasepoint: PubKey
    DelayedPaymentBasepoint: PubKey
    HTLCBasepoint: PubKey
    FirstPerCommitmentPoint: PubKey
    ShutdownScriptPubKey: OptionalField<Script>
}

type FundingCreated = {
    TemporaryChannelId: ChannelId
    FundingTxId: TxId
    FundingOutputIndex: uint16
    Signature: ECDSASignature
}

type FundingSigned = {
    ChannelId: ChannelId
    Signature: ECDSASignature
}

type FundingLocked = {
    ChannelId: ChannelId
    NextPerCommitmentPoint: PubKey
    Shutdown: ChannelId
    ScriptPubKey: Script
}

type Shutdown = {
    ChannelId: ChannelId
    ScriptPubKey: Script
}

type ClosingSigned = {
    ChannelId: ChannelId
    FeeSatoshis: Money
    Signature: ECDSASignature
}

type UpdateAddHTLC = {
    ChannelId: ChannelId
    HTLCId: HTLCId
    AmountMSat: LightMoney
    PaymentHash: PaymentHash
    CLTVExpiry: uint32
    OnionRoutingPacket: OnionPacket
}

type UpdateFulfillHTLC = {
    ChannelId: ChannelId
    HTLCId: HTLCId
    PaymentPreimage: PaymentPreimage
}

type UpdateFailHTLC = {
    ChannelId: ChannelId
    HTLCId: HTLCId
    Reason: OnionErrorPacket
}

type UpdateFailMalformedHTLC = {
    ChannelId: ChannelId
    HTLCId: HTLCId
    Sha256OfOnion: uint256
    FailureCode: uint16
}

type CommitmentSigned = {
    ChannelId: ChannelId
    Signature: ECDSASignature
    HTLCSignatures: ECDSASignature list
}

type RevokeAndACK = {
    ChannelId: ChannelId
    PerCommitmentSecret: PaymentPreimage
    MyCurrentPerCommitmentPoint: PubKey
}

type UpdateFee = {
    ChannelId: ChannelId
    FeeratePerKW: Money
}

type DataLossProtect = {
    YourLastPerCommitmentSecret: PaymentPreimage
    MyCurrentPerCommitmentPoint: PubKey
}

type ChannelReestablish = {
    ChannelId: ChannelId
    NextLocalCommitmentNumber: uint64
    NextRemoteRevocationNumber: uint64
    DataLossProtect: OptionalField<DataLossProtect>
}

type AnnouncementSignatures = {
    ChannelId: ChannelId
    ShortChannelId: ShortChannelId
    NodeSignature: ECDSASignature
    BitcoinSignature: ECDSASignature
}

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
    member this.WriteTo(w: BinaryWriter) =
        match this with
        | IPv4 d ->
            w.Write(d.Address.GetAddressBytes())
            w.Write((uint16)d.Port)
        | IPv6 d ->
            w.Write(d.Address.GetAddressBytes())
            w.Write((uint16)d.Port)
        | OnionV2 d ->
            w.Write(d.Addr)
            w.Write((uint16)d.Port)
        | OnionV3 d ->
            w.Write(d.ed25519PubKey)
            w.Write(d.CheckSum)
            w.Write(d.Version)
            w.Write((uint16)d.Port)


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
type UnsignedNodeAnnouncement = {
    Features: GlobalFeatures
    Timestamp: uint32
    NodeId: NodeId
    RGB: RGB
    Alias: uint256
    Addresses: NetAddress list
    ExcessAddressData: byte[]
    ExcessData: byte[]
}

type NodeAnnouncement = {
    Signature: ECDSASignature
    Contents: UnsignedNodeAnnouncement
}


type UnsignedChannelAnnouncement = {
    Features: GlobalFeatures
    ChainHash: uint256
    ShortChannelId: ShortChannelId
    NodeId1: NodeId
    NodeId2: NodeId
    BitcoinKey1: PubKey
    BitcoinKey2: PubKey
}

type ChannelAnnouncement = {
    NodeSignature1: ECDSASignature
    NodeSignature2: ECDSASignature
    BitcoinSignature1: ECDSASignature
    BitcoinSignature2: ECDSASignature
    Contents: UnsignedChannelAnnouncement
}

type UnsignedChannelUpdate = {
    ChainHash: uint256
    ShortChannelId: ShortChannelId
    Timestamp: uint32
    Flags: uint16
    CLTVExpiryDelta: uint16
    HTLCMinimumMSat: LightMoney
    FeeBaseMSat: LightMoney
    FeeProportionalMillionths: uint32
    ExcessData: byte[]
}

type ChannelUpdate = {
    Signature: ECDSASignature
    Contents: UnsignedChannelUpdate
}

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
/// returned by ChannelMessageHandler::HandleUpdateFailHTLC to be passed into
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

/// All possible messages in Lightning P2P Network
type LightningMsg = 
    /// BOLT1: Basic Control
    | Init of Init
    | Error of ErrorMessage
    | Ping of Ping
    | Pong of Pong

    /// BOLT2: Channel Establishment and closing.
    // Channel establish msgs.
    | OpenChannel of OpenChannel
    | AcceptChannel of AcceptChannel
    | FundingCreated of FundingCreated
    | FundingSigned of FundingSigned
    | FundingLocked of FundingLocked
    // Closing
    | Shutdown of Shutdown
    | ClosingSigned of ClosingSigned
    // Message retrasmission
    | ChannelReestablish of ChannelReestablish

    /// BOLT2: Normal operations
    | UpdateAddHTLC of UpdateAddHTLC
    | UpdateFulfillHTLC of UpdateFulfillHTLC
    | UpdateFailHTLC of UpdateFailHTLC
    | UpdateFailMalformedHTLC of UpdateFailMalformedHTLC
    | CommitmentSigned of CommitmentSigned
    | RevokeAndACK of RevokeAndACK
    | UpdateFee of UpdateFee

    /// BOLT7: Routing
    | AnnouncementSignatures of AnnouncementSignatures
    | ChannelAnnouncement of ChannelAnnouncement
    | NodeAnnouncement of NodeAnnouncement
    | ChannelUpdate of ChannelUpdate

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
    AmtToForward: LightMoney
    OutgoingCLTVValue: uint32
}
