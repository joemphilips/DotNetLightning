namespace DotNetLightning.Peer

open DotNetLightning.Utils
open DotNetLightning.Serialization.Msgs
open NBitcoin

type PeerEvent =
    // --- initialization ---
    | ActOneProcessed of actTwo: byte[] * newPCE: PeerChannelEncryptor
    | ActTwoProcessed of (byte[] * NodeId) * newPCE: PeerChannelEncryptor
    | ActThreeProcessed of theirNodeId: NodeId * newPCE: PeerChannelEncryptor
    
    // --- else ---
    // --- receiving ---
    | ReceivedError of error: ErrorMsg * newPCE: PeerChannelEncryptor
    | ReceivedPing of ping: PingMsg * newPCE: PeerChannelEncryptor
    | ReceivedPong of ping: PongMsg * newPCE: PeerChannelEncryptor
    | ReceivedInit of initMsg: InitMsg * newPCE: PeerChannelEncryptor
    | ReceivedRoutingMsg of msg: IRoutingMsg * newPCE: PeerChannelEncryptor
    | ReceivedChannelMsg of msg: IChannelMsg * newPCE: PeerChannelEncryptor
    
    // --- sending ---
    | MsgEncoded of msg: byte[] * newPCE: PeerChannelEncryptor
    | FailedToBroadcastTransaction of tx: Transaction

type PeerCommand =
    | ProcessActOne of actOne: byte[] * ourNodeSecret: Key
    | ProcessActTwo of actTwo: byte[] * ourNodeSecret: Key
    | ProcessActThree of actThree: byte[]
    | DecodeCipherPacket of lengthHeader: byte[] * reader: (int -> byte[])
    | EncodeMsg of msg: ILightningMsg
