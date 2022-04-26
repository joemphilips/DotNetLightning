namespace DotNetLightning.Peer

open DotNetLightning.Utils
open DotNetLightning.Serialization.Msgs
open NBitcoin

type PeerEvent =
    // --- initialization ---
    | ActOneProcessed of actTwo: array<byte> * newPCE: PeerChannelEncryptor
    | ActTwoProcessed of (array<byte> * NodeId) * newPCE: PeerChannelEncryptor
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
    | MsgEncoded of msg: array<byte> * newPCE: PeerChannelEncryptor
    | FailedToBroadcastTransaction of tx: Transaction

type PeerCommand =
    | ProcessActOne of actOne: array<byte> * ourNodeSecret: Key
    | ProcessActTwo of actTwo: array<byte> * ourNodeSecret: Key
    | ProcessActThree of actThree: array<byte>
    | DecodeCipherPacket of
        lengthHeader: array<byte> *
        reader: (int -> array<byte>)
    | EncodeMsg of msg: ILightningMsg
