namespace DotNetLightning.LN

open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open NBitcoin

type PeerEvent =
    | Connected
    | ReceivedError of error: ErrorMessage
    | ReceivedPing of ping: Ping
    | ReceivedPong of ping: Pong
    | ReceivedInit of init: Init
    | ReceivedRoutingMsg of msg: IRoutingMsg
    | ReceivedChannelMsg of msg: IChannelMsg
    | FailedToBroadcastTransaction of tx: Transaction

type PeerCommand =
    | Connect of theirNodeId: NodeId
    | SendPing of ping: Ping
