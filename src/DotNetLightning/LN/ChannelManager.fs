namespace DotNetLightning.LN
open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Utils.Primitives
open DotNetLightning.Serialize.Msgs

type PendingForwardHTLCInfo = {
    OnionPacket: OnionPacket option
    IncomingSharedSecret: uint8
    PaymentHash: uint256
    ShortChannelId: ShortChannelId
    AmountToForward: LNMoney
    OutgoingCLTVValue: uint32
}

type HTLCFailureMsg =
    | Relay of UpdateFailHTLC
    | Malformed of UpdateFailMalformedHTLC

type PendingHTLCStatus =
    | Forward of PendingForwardHTLCInfo

type HTLCPreviousHopData =
    | ShortChannelId of ShortChannelId
    | HTLCId of HTLCId
    | IncomingPacketSharedSecret of PaymentPreimage

type OutboundRoute = {
    Route: Route
    SessionPriv: Key
    FirstHopHTLC: LNMoney
}

type HTLCSource =
    | PreviousHopData of HTLCPreviousHopData
    | OutboundRoute of OutboundRoute
    with
        /// For test
        static member internal Dummy() =
            OutboundRoute {
                Route = Route []
                SessionPriv = Key([| for _ in 0..31 -> 1uy|])
                FirstHopHTLC = LNMoney.Zero
            }

type HTLCFailureData = {
    Data: byte[]
    FailureCode: uint16
}

type HTLCFailReason =
    | ErrorPacket of OnionErrorPacket
    | Reason of HTLCFailureData

type RAACommitmentOrder =
    | CommitmentFirst
    | RevokeAndACKFirst