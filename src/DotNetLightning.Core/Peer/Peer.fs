namespace DotNetLightning.Peer

open NBitcoin

open DotNetLightning.Serialization
open System.Collections
open DotNetLightning.Utils
open DotNetLightning.Serialization.Msgs
open DotNetLightning.Utils.Aether

open ResultUtils
open ResultUtils.Portability

type PeerHandleError = {
    /// Used to indicate that we probably can't make any future connections to this peer, implying
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
    TheirFeatures: FeatureBits option
    SyncStatus: InitSyncTracker
}
    with
        static member CreateInbound(peerId, ourNodeSecret) = {
            PeerId = peerId
            ChannelEncryptor = PeerChannelEncryptor.newInBound(ourNodeSecret)
            IsOutBound = false
            TheirNodeId = None
            TheirFeatures = None
            SyncStatus = NoSyncRequested
        }
        
        static member CreateOutbound(peerId, theirNodeId, ourNodeSecret: Key) = {
            PeerId = peerId
            ChannelEncryptor = PeerChannelEncryptor.newOutBound(theirNodeId, ourNodeSecret)
            IsOutBound = true
            TheirNodeId = Some theirNodeId
            TheirFeatures = None
            SyncStatus = NoSyncRequested
        }
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
            
        static member ChannelEncryptor_: Lens<_,_> =
            (fun p -> p.ChannelEncryptor),
            (fun pce p -> { p with ChannelEncryptor = pce })
            
module Peer =
    let executeCommand (state: Peer) (cmd: PeerCommand) =
        let noiseStep = state.ChannelEncryptor.GetNoiseStep()
        match noiseStep, cmd with
        | ActOne, ProcessActOne (actOne, ourNodeSecret) ->
            result {
                let! actTwo = state.ChannelEncryptor |> PeerChannelEncryptor.processActOneWithKey actOne ourNodeSecret
                return actTwo |> ActOneProcessed  |> List.singleton
            }
        | ActTwo, ProcessActTwo (actTwo, ourNodeSecret) ->
            result {
                let! actThree = state.ChannelEncryptor |> PeerChannelEncryptor.processActTwo actTwo ourNodeSecret
                return actThree |> (ActTwoProcessed >> List.singleton)
            }
        | ActThree, ProcessActThree(actThree) ->
            result {
                let! theirNodeId = state.ChannelEncryptor |> PeerChannelEncryptor.processActThree actThree
                return theirNodeId |> (ActThreeProcessed >> List.singleton)
            }
        | NoiseComplete, DecodeCipherPacket (lengthPacket, reader) ->
            result {
                let! (len, pce) =
                    PeerChannelEncryptor.decryptLengthHeader lengthPacket state.ChannelEncryptor
                    
                let packet = reader (int len + 16)
                let! (b, newPCE) = pce |> PeerChannelEncryptor.decryptMessage packet
                let! msg = LightningMsg.fromBytes b |> Result.mapError(PeerError.P2PMessageDecodeError)
                return
                    match msg with
                    | :? ErrorMsg as msg ->
                        [ReceivedError (msg, newPCE)]
                    | :? PingMsg as msg ->
                        [ReceivedPing (msg, newPCE)]
                    | :? PongMsg as msg ->
                        [ReceivedPong (msg, newPCE)]
                    | :? InitMsg as msg ->
                        [ReceivedInit (msg, newPCE)]
                    | :? IRoutingMsg as msg ->
                        [ReceivedRoutingMsg (msg, newPCE)]
                    | :? IChannelMsg as msg ->
                        [ReceivedChannelMsg (msg, newPCE)]
                    | _ ->
                        failwithf "unreachable %A" msg
            }
        | NoiseComplete, EncodeMsg (msg) ->
            state.ChannelEncryptor |> PeerChannelEncryptor.encryptMessage (msg.ToBytes())
            |> (MsgEncoded >> List.singleton >> Ok)
        | _, cmd ->
            failwithf "Peer does not know how to handle %A while in noise step %A" cmd noiseStep
        
    let applyEvent (state: Peer) (event: PeerEvent): Peer =
        let noiseStep = state.ChannelEncryptor.GetNoiseStep()
        match noiseStep, event with
        | ActOne, ActOneProcessed (_, pce) ->
            { state with ChannelEncryptor = pce }
        | ActTwo, ActTwoProcessed((_, nodeId), pce) ->
            { state with ChannelEncryptor = pce; TheirNodeId = Some nodeId }
        | ActThree, ActThreeProcessed(nodeId, pce) ->
            { state with ChannelEncryptor = pce; TheirNodeId = Some nodeId }
        | NoiseComplete, ReceivedError (_, pce)
        | NoiseComplete, ReceivedPing (_, pce)
        | NoiseComplete, ReceivedPong (_, pce)
        | NoiseComplete, ReceivedRoutingMsg (_, pce)
        | NoiseComplete, ReceivedChannelMsg (_, pce) ->
            { state with ChannelEncryptor = pce; }
        | NoiseComplete, ReceivedInit (initMsg, pce) ->
            { state with
                ChannelEncryptor = pce;
                TheirFeatures = Some initMsg.Features }
        | NoiseComplete, MsgEncoded(_, pce) ->
            { state with ChannelEncryptor = pce }
        | state, e ->
            failwithf "Unreachable! applied event %A while in state %A" e state
