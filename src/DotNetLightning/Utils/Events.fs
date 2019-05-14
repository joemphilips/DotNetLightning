namespace DotNetLightning.Utils
open NBitcoin
open BTCPayServer.Lightning
open DotNetLightning.Serialize.Msgs

type MessageSendEvent =
    | SendAcceptChannel of SendAcceptChannel
    | SendOpenChannel of SendOpenChannel
    | SendFundingCreated of SendFundingCreated
    | SendFundingLocked of SendFundingLocked
    | SendAnnouncementSignatures of SendAnnouncementSignatures
    | UpdateHTLCs of UpdateHTLCS
and SendAcceptChannel = {
    NodeId: PubKey
    Msg: AcceptChannel
}
and SendOpenChannel = {
    NodeId: PubKey
    Msg: OpenChannel
}
and SendFundingCreated = {
    NodeId: PubKey
    Msg: FundingCreated
}
and SendFundingLocked = {
    NodeId: PubKey
    Msg: FundingLocked
}
and SendAnnouncementSignatures = {
    NodeId: PubKey
    Msg: AnnouncementSignatures
}

and UpdateHTLCS = {
    NodeId: PubKey
    Updates: CommitmentUpdate
}

type IMessageSendEventsProvider =
    abstract member GetAndClearPendingMsgEvents: unit -> MessageSendEvent list
