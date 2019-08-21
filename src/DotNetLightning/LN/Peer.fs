namespace DotNetLightning.LN
open NBitcoin
open System
open DotNetLightning.Utils
open DotNetLightning.Utils.RResult
open DotNetLightning.Serialize.Msgs
open Microsoft.Extensions.Logging
open DotNetLightning.Serialize
open System.Net

type PeerHandleError = {
    /// Used to indicate that we probably can't make any future connecitons to this peer, implying
    /// we should go ahead and force-close any channels we have with it.
    NoConnectionPossible: bool
}

exception PeerHandleException of PeerHandleError

type InitSyncTracker =
    | ChannelsSyncing of ChannelId
    | NodeSyncing of PubKey
    | NoSyncRequested

module private PeerConstants =
    [<Literal>]
    let INITIAL_SYNCS_TO_SEND = 5u

type Peer = {
    PeerId: PeerId
    ChannelEncryptor: PeerChannelEncryptor
    IsOutBound : bool
    TheirNodeId: NodeId option
    TheirGlobalFeatures: GlobalFeatures option
    TheirLocalFeatures: LocalFeatures option

    SyncStatus: InitSyncTracker
    GetOurNodeSecret: unit -> Key
}
    with
        /// Returns true if the channel announcements/updates for the given channel should be
        /// Forwarded to this peer.
        /// If we are sending our routing table to this peer and we have not yet sent channel
        /// announceents/updates for the given channel_id then we will send it when we get to that
        /// point and we shouldn't send it yet to avoid sending
        member this.ShouldForwardChannel(channelId: ChannelId) =
            match this.SyncStatus with
            | NoSyncRequested -> true
            | ChannelsSyncing i -> i < channelId
            | NodeSyncing _ -> true