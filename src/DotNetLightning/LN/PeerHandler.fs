namespace DotNetLightning.LN
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Serialize.PeerChannelEncryptor

type MessageHandler = {
    ChanHandler: IChannelMessageHandler
    RouteHandler: IRoutingMessageHandler
}

type Peer = {
    ChannelEncryptor: PeerChannelEncryptorStream
}