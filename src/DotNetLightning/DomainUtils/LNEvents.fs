namespace DotNetLightning.DomainUtils

open NBitcoin
open DotNetLightning.Serialize.Msgs

// Defines events in layer 2

type MessageSendEvent =
    | AcceptChannelHasSent of AcceptChannelHasSent
    | OpenChannelHasSent of OpenChannelHasSent
    | FundingCreatedHasSent of FundingCreatedHasSent
    | FundingLockedHasSent of FundingLockedHasSent
    | AnnouncementSignaturesHasSent of AnnouncementSignaturesHasSent
    | UpdateHTLCs of UpdateHTLCsHasSent
and AcceptChannelHasSent = {
    NodeId: PubKey
    Msg: AcceptChannel
}
and OpenChannelHasSent = {
    NodeId: PubKey
    Msg: OpenChannel
}
and FundingCreatedHasSent = {
    NodeId: PubKey
    Msg: FundingCreated
}
and FundingLockedHasSent = {
    NodeId: PubKey
    Msg: FundingLocked
}
and AnnouncementSignaturesHasSent = {
    NodeId: PubKey
    Msg: AnnouncementSignatures
}

and UpdateHTLCsHasSent = {
    NodeId: PubKey
    Updates: CommitmentUpdate
}

type IMessageSendEventsProvider =
    abstract member GetAndClearPendingMsgEvents: unit -> MessageSendEvent list
