namespace DotNetLightning.Infrastructure

open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs

// -----------
type IChannelMessageHandler =
    abstract member HandleMsg: data: IChannelMsg -> RResult<unit>

type ChannelMessageHandler() =

    interface IChannelMessageHandler with
        member this.HandleMsg(data: IChannelMsg): RResult<unit> = 
            match data with
            | :? OpenChannel -> failwith ""

    (*
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
    *)