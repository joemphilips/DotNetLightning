namespace DotNetLightning.LN

open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs

type PeerEvent =
    | Connected of theirNodeId: NodeId
    | ReceivedError of theirNodeId: NodeId * error: ErrorMessage
    | ReceivedRoutingMsg of theirNodeId: NodeId * msg: IRoutingMsg
    | ReceivedChannelMsg of theirNodeId: NodeId * msg: IChannelMsg
    
