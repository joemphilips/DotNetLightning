namespace DotNetLightning.Infrastructure

open NBitcoin

open DotNetLightning.Utils
open DotNetLightning.Crypto
open DotNetLightning.LN


type PeerError =
    | DuplicateConnection
type IPeerManager =
    abstract member NewOutboundConnection: theirId: NodeId * peerId: PeerId -> Result<byte[], PeerError>
    abstract member NewInboundConnection: peerId: PeerId -> Result<byte[], PeerError>

type PeerManager =
    {
        Peers: Map<PeerId, Peer>
        GetOurNodeSecret: unit -> Key
    }
    with
        member this.NewOutBoundConnection (theirNodeId: NodeId)
                                          (connId: PeerId): RResult<PeerManager * byte[]> =
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

        member this.NewInboundConnection(connId: PeerId) (pm: PeerManager) =
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