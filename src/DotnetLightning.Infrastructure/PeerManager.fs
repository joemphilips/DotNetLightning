namespace DotNetLightning.Infrastructure

open NBitcoin

open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading.Tasks

open DotNetLightning.Utils
open DotNetLightning.Chain
open DotNetLightning.Crypto
open DotNetLightning.LN


type PeerError =
    | DuplicateConnection of PeerId
    | UnexpectedByteLength of expected: int * actual: int

type IPeerManager =
    abstract member HandleData: PeerId * byte[] -> Task

type PeerManager(keyRepo: IKeysRepository) =
    member val KnownPeers = ConcurrentDictionary<PeerId, Peer>() with get, set
    member val OpenedPeers = ConcurrentDictionary<PeerId, Peer>() with get, set

    member this.NewOutBoundConnection (theirNodeId: NodeId, peerId: PeerId): Result<byte[], PeerError> =
        let act1, peerEncryptor =
            PeerChannelEncryptor.newOutBound(theirNodeId)
            |> PeerChannelEncryptor.getActOne
        let newPeer = {
                            ChannelEncryptor = peerEncryptor
                            IsOutBound = true
                            TheirNodeId = None
                            TheirGlobalFeatures = None
                            TheirLocalFeatures = None
                            SyncStatus = InitSyncTracker.NoSyncRequested
                            PeerId = peerId
                            GetOurNodeSecret = keyRepo.GetNodeSecret
                      }
        match this.KnownPeers.TryAdd(peerId, newPeer) with
        | true ->
            peerId
            |> DuplicateConnection
            |> Result.Error
        | false ->
            Ok(act1)

    member this.NewInboundConnection(peerId: PeerId, actOne: byte[]): RResult<byte[]> =
        if (actOne.Length <> 50) then RResult.rbad (RBad.Object(UnexpectedByteLength(50, actOne.Length))) else
        let secret = keyRepo.GetNodeSecret()
        let peerEncryptor = PeerChannelEncryptor.newInBound(secret)
        PeerChannelEncryptor.processActOneWithKey actOne secret peerEncryptor
        >>= fun (actTwo, pce) ->
            let newPeer = {
                ChannelEncryptor = pce
                IsOutBound = false
                TheirNodeId = None
                TheirGlobalFeatures = None
                TheirLocalFeatures = None

                SyncStatus = InitSyncTracker.NoSyncRequested
                PeerId = peerId
                GetOurNodeSecret = keyRepo.GetNodeSecret
            }
            match this.KnownPeers.TryAdd(peerId, newPeer) with
            | true ->
                peerId
                |> DuplicateConnection
                |> box
                |> RBad.Object
                |> RResult.rbad
            | false ->
                actTwo
                |> RResult.Good

    member this.GetNextLengthToRead(peerId: PeerId) =
        match this.KnownPeers.TryGetValue(peerId) with
        | true, peer ->
            match peer.ChannelEncryptor.NoiseState with
            | _ ->
                failwith "TODO"
        | false, _ ->
            failwith "unknown peer"
    member this.DoAttemptWriteData (connId: ConnectionId) (peer: Peer) (pm: PeerManager) =
        failwith ""