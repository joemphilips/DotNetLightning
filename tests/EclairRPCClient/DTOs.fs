namespace EclairRPCClient

open DotNetLightning.Utils
open DotNetLightning.Payment
open NBitcoin
open NBitcoin

type GetInfoResult = {
    NodeId: NodeId
    Alias: string
    ChainHash: uint256
    BlockHeight: int64
    PublicAddresses: string list
}

type PeerInfo = {
    NodeId: NodeId
    State: PeerState
    Address: string option
    Channels: int
}

type NodeInfo = {
    Signature: LNECDSASignature
    Features: string
    Timestamp: int64
    NodeId: NodeId
    RGBColor: RGB
    alias: string
    addresses: string[]
}

type OpenChannelInfo = {
    NodeId: NodeId
    ShortChannelId: ShortChannelId
}

type ChannelDesc = {
    ShortChannelId: ShortChannelId
    A: NodeId
    B: NodeId
}
type AuditResult = {
    Sent: SentPayment
    Relayed: RelayedPayment
    Received: ReceivedPayment
}
and SentPayment = {
    Id: PaymentId
    PaymentHash: PaymentHash
    PaymentPreimage: PaymentPreimage
    Parts: SentPaymentPart[]
}
and SentPaymentPart = {
    Id: PaymentId
    Amount: LNMoney
    FeesPaid: LNMoney
    ToChannelId: ChannelId
    Timestamp: uint32
}

and RelayedPayment = {
    AmountIn: LNMoney
    AmountOut: LNMoney
    PaymentHash: PaymentHash
    FromChannelId: ChannelId
    ToChannelId: ChannelId
    Timestamp: uint32
}

and ReceivedPayment = {
    PaymentHash: PaymentHash
    Parts: ReceivedPaymentPart []
}
and ReceivedPaymentPart = {
    Amount: LNMoney
    FromChannelId: ChannelId
    Timestamp: uint32
}

