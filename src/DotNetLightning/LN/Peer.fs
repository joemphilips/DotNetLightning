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
    TheirNodeId: PubKey option
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

type PeerManager =
    {
        Peers: Map<ConnectionId, Peer>
        GetOurNodeSecret: unit -> Key
    }
    with
        member this.NewOutBoundConnection (theirNodeId: NodeId)
                                          (connId: ConnectionId): RResult<PeerManager * byte[]> =
            let act1, peerEncryptor =
                PeerChannelEncryptor.newOutBound(theirNodeId)
                |> PeerChannelEncryptor.getActOne
            let pendingReadBuffer = Array.zeroCreate(50) // Noise act 2 is 50 bytes. 
            let newPeerMap = this.Peers
                             |> Map.add connId {
                                ChannelEncryptor = peerEncryptor
                                IsOutBound = true
                                TheirNodeId = None
                                TheirGlobalFeatures = None
                                TheirLocalFeatures = None
                                SyncStatus = InitSyncTracker.NoSyncRequested
                                PeerId = failwith "Not Implemented"
                                GetOurNodeSecret = failwith "Not Implemented"
                                }
            if true then
                Good({ this with Peers = newPeerMap }, act1)
            else
                failwith "PeerManager driver duplicated descriptors!"

        member this.NewInboundConnection(connId: ConnectionId) (pm: PeerManager) =
            let peerEncryptor = PeerChannelEncryptor.newInBound(pm.GetOurNodeSecret())
            let pendingReadBuffer = Array.zeroCreate 50
            let newPeer = {
                ChannelEncryptor = peerEncryptor
                IsOutBound = false
                TheirNodeId = None
                TheirGlobalFeatures = None
                TheirLocalFeatures = None

                SyncStatus = InitSyncTracker.NoSyncRequested
                PeerId = failwith "Not Implemented"
                GetOurNodeSecret = failwith "Not Implemented"
            }
            {
                this with Peers = this.Peers |> Map.add connId newPeer
            }

        member this.DoAttemptWriteData (connId: ConnectionId) (peer: Peer) (pm: PeerManager) =
            failwith ""