namespace DotNetLightning.Payment

open System
open DotNetLightning.Utils
open DotNetLightning.Crypto
open DotNetLightning.Channel

type PaymentId = PaymentId of Guid

type PaymentFailure =
    /// A failure happened locally, preventing the payment from being sent (e.g. no route found)
    | LocalFailure of ChannelError
    /// A remote node failed the payment and we were able to decrypt the onion failure packet
    | RemoteFailure of route: RouteHop list * err: Sphinx.ErrorPacket
    /// A remote node failed the payment but we couldn't decrypt the failure
    /// (e.g. a malicious node tampered with the message)
    | UnreadableRemoteFailure of route: RouteHop list

type PaymentEvent =
    | PaymentSent of PaymentSentEvent
    | PaymentFailed of PaymentFailedEvent
    | PaymentRelayed of PaymentRelayedEvent
    | PaymentReceived of PaymentReceivedEvent
    | PaymentSettlingOnChain of PaymentSettlingOnChainEvent
and PaymentSentEvent = {
    Id: PaymentId
    PaymentHash: PaymentHash
    PaymentPreimage: PaymentPreimage
    Timestamp: DateTimeOffset
    Amount: LNMoney
    Parts: PartialPayment
    FeesPaid: LNMoney
}
and PartialPayment = {
    Id: PaymentId
    Amount: LNMoney
    FeesPaid: LNMoney
    ToChannelId: ChannelId
    Route: RouteHop list option
    Timestamp: DateTimeOffset
}

and PaymentFailedEvent = {
    Id: PaymentId
    PaymentHash: PaymentHash
    Failures: PaymentFailure list
}

and PaymentRelayedEvent = {
    PaymentHash: PaymentHash
    Timestamp: DateTimeOffset
    AmountIn: LNMoney
    AmountOut: LNMoney
    FromChannelId: ChannelId
    ToChannelId: ChannelId
}
and PaymentReceivedEvent = {
    PartialPayment: ReceivedPartialPayment
}
and ReceivedPartialPayment = private {
    Amount: LNMoney
    FromChannelId: ChannelId
    Timestamp: DateTimeOffset
}
    with
    static member Create(amount, fromChannelId) = {
        Amount = amount
        FromChannelId = fromChannelId
        Timestamp = DateTimeOffset.Now
    }
and PaymentSettlingOnChainEvent = private {
    Id: PaymentId
    Amount: LNMoney
    PaymentHash: PaymentHash
    Timestamp: DateTimeOffset
}
    with
    static member Create(id, amount, paymentHash) = {
        Id = id
        Amount = amount
        PaymentHash = paymentHash
        Timestamp = DateTimeOffset.Now
    }
