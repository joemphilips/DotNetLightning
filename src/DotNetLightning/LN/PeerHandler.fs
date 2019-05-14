namespace DotNetLightning.LN
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Serialize

type MessageHandler = {
    ChanHandler: IChannelMessageHandler
    RouteHandler: IRoutingMessageHandler
}

type Peer = {
    ChannelEncryptor: PeerChannelEncryptorStream
}