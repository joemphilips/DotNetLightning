namespace DotNetLightning.LN

open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open NBitcoin

type PeerEvent =
    // --- initialization ---
    | Connected
    | ActOneProcessed of actTwo: byte[] * newPCE: PeerChannelEncryptor
    | ActTwoProcessed of (byte[] * NodeId) * newPCE: PeerChannelEncryptor
    | ActThreeProcessed of theirNodeId: NodeId * newPCE: PeerChannelEncryptor
    
    // --- else ---
    // --- receiving ---
    | LengthDecrypted of length: uint16 * newPCE: PeerChannelEncryptor
    | ReceivedError of error: ErrorMessage * newPCE: PeerChannelEncryptor
    | ReceivedPing of ping: Ping * newPCE: PeerChannelEncryptor
    | ReceivedPong of ping: Pong * newPCE: PeerChannelEncryptor
    | ReceivedInit of init: Init * newPCE: PeerChannelEncryptor
    | ReceivedRoutingMsg of msg: IRoutingMsg * newPCE: PeerChannelEncryptor
    | ReceivedChannelMsg of msg: IChannelMsg * newPCE: PeerChannelEncryptor
    
    // --- sending ---
    | MsgEncoded of msg: byte[] * newPCE: PeerChannelEncryptor
    | FailedToBroadcastTransaction of tx: Transaction

type PeerCommand =
    | Connect of theirPeerId: NodeId
    | Disconnect of theirNodeId: NodeId
    | ProcessActOne of actOne: byte[] * ourNodeSecret: Key
    | ProcessActTwo of actTwo: byte[] * ourNodeSecret: Key
    | ProcessActThree of actThree: byte[]
    | DecodeLength of byte[]
    | DecodeCipherPacket of byte[]
    | EncodeMsg of msg: ILightningMsg
