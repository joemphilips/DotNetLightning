namespace DotNetLightning.LN
open NBitcoin
open System
open DotNetLightning.Utils
open DotNetLightning.Utils.RResult
open DotNetLightning.Serialize.Msgs
open Microsoft.Extensions.Logging
open DotNetLightning.Serialize
open System.Net

/// Provides references to interface which handle differnt types of messages.
type MessageHandler = {
    /// A message handler which handles messages specific to channnels
    ChanHandler: IChannelMessageHandler
    /// A meesage handler which handles messages updating our knowledge of the network channel
    /// graph.
    RouteHandler: IRoutingMessageHandler
}


type ConnectionId =
        ConnectionId of string

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

type Peer = {
    ChannelEncryptor: PeerChannelEncryptor
    IsOutBound : bool
    TheirNodeId: PubKey option
    TheirGlobalFeatures: GlobalFeatures option
    TheirLocalFeatures: LocalFeatures option
    PendingOutBoundBuffer: byte[] list
    PendingOutBoundBufferFirstMsgOffset: uint32
    AwaitingWriteEvent: bool
    PendingReadBuffer: byte[]
    PendingReadBufferPos: uint32
    PendingReadIsHeader: bool
    SyncStatus: InitSyncTracker
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

type PeerHolder = {
    Peers: Map<ConnectionId, Peer>
    NodeIdToConnectionId: Map<NodeId, ConnectionId>
}
    with
        static member Peers_ =
            (fun ph -> ph.Peers),
            (fun peers ph -> { ph with Peers = peers })

        static member NodeIdToConnectionid_ =
            (fun ph -> ph.NodeIdToConnectionId ),
            (fun newMap ph -> { ph with NodeIdToConnectionId = newMap })


/// A PeerManager manages a set of peers, described by their SocketDescirptor and marshalls socket
/// events into messages which it passes onto its MessageHandlers.
type PeerManager = {
    MessageHandler: MessageHandler
    Peers: PeerHolder
    OurNodeSecret: Key
    InitialSyncsSent: uint32
    Logger: ILogger
}
    with
        static member Peers_ =
            (fun pm -> pm.Peers),
            (fun ps pm -> { pm with PeerManager.Peers = ps})

[<AutoOpen>]
module PeerManager =
    [<Literal>]
    let INITIAL_SYNCS_TO_SEND = 5u

    /// Manages and reacts to connection events.
    /// PeerIds may repeat, but only after disconnect_event( has been called.
    let create (messageHandler: MessageHandler)
               (ourNodeSecret: Key)
               (logger: ILogger): PeerManager =
        {
            MessageHandler = messageHandler
            Peers = {
                Peers = Map.empty
                NodeIdToConnectionId = Map.empty
            }
            OurNodeSecret = ourNodeSecret
            InitialSyncsSent = 0u
            Logger = logger
        }

    /// Get the list of node ids for peers which have completed the initial handshake
    /// 
    /// For outbound connections, this will be the same as the their_node_id parameter passed into
    /// new_outbound_connection, however entries will only appear once the initial handshake has
    /// completed and we are sure the remote peer has the private key for the given node_id.
    let getPeerNodeIds (pm: PeerManager): PubKey list =
       pm.Peers.Peers
       |> Map.toList
       |> List.choose (fun (k, v) -> if (not (v.ChannelEncryptor.IsReadyForEncryption() || v.TheirNodeId.IsNone)) then
                                        None
                                     else Some (v.TheirNodeId.Value)
                      )

    /// Indicates a new outbound connection has been established to a node with the given node_id.
    /// Note that if an Error is returned here you MUST NOT call disconnect_event for the new
    /// Descriptor but must disconnect the conneciton immediately.
    ///
    /// Returns a small number of bytes to send to the remote node (currently always 50)
    ///
    /// throws error when descriptor is duplicative with some other descriptor which has not yet
    /// has a disconnect_event.
    let newOutBoundConnection (theirNodeId: NodeId)
                              (connId: ConnectionId)
                              (pm: PeerManager): RResult<PeerManager * byte[]> =
        let act1, peerEncryptor =
            PeerChannelEncryptor.newOutBound(theirNodeId)
            |> PeerChannelEncryptor.getActOne
        let pendingReadBuffer = Array.zeroCreate(50) // Noise act 2 is 50 bytes. 
        let newPeerMap = pm.Peers.Peers
                         |> Map.add connId {
                            ChannelEncryptor = peerEncryptor
                            IsOutBound = true
                            TheirNodeId = None
                            TheirGlobalFeatures = None
                            TheirLocalFeatures = None
                            PendingOutBoundBuffer = List.empty
                            PendingOutBoundBufferFirstMsgOffset = 0u
                            AwaitingWriteEvent = false
                            PendingReadBuffer = pendingReadBuffer
                            PendingReadBufferPos =  0u
                            PendingReadIsHeader = false
                            SyncStatus = InitSyncTracker.NoSyncRequested
                            }
        if true then
            Good({ pm with Peers = { pm.Peers with Peers = newPeerMap }}, act1)
        else
            failwith "PeerManager driver duplicated descriptors!"

    let newInboundConnection(connId: ConnectionId) (pm: PeerManager) =
        let peerEncryptor = PeerChannelEncryptor.newInBound(pm.OurNodeSecret)
        let pendingReadBuffer = Array.zeroCreate 50
        let newPeer = {
            ChannelEncryptor = peerEncryptor
            IsOutBound = false
            TheirNodeId = None
            TheirGlobalFeatures = None
            TheirLocalFeatures = None

            PendingOutBoundBuffer = List.empty
            PendingOutBoundBufferFirstMsgOffset = 0u
            AwaitingWriteEvent = false
            PendingReadBuffer = pendingReadBuffer
            PendingReadBufferPos = 0u
            PendingReadIsHeader = false
            SyncStatus = InitSyncTracker.NoSyncRequested
        }
        {
            pm with Peers = { pm.Peers with Peers = pm.Peers.Peers |> Map.add connId newPeer }
        }

    let doAttemptWriteData (connId: ConnectionId) (peer: Peer) (pm: PeerManager) =
        failwith ""