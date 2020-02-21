namespace rec DotNetLightning.Infrastructure.Error

open DotNetLightning.Channel
open DotNetLightning.Peer

exception DotNetLightningException of DotNetLightningError
type DotNetLightningError =
    private
    | PeerError of httpCode: int * PeerError
    | ChannelError of httpCode: int * ChannelError
    | PublicAPIMisuseError of httpCode: int * code: string * msg: string
    with
    static member FromPeerError(e) =
        PeerError(500, e)
    static member FromChannelError(e) =
        PeerError(500, e)
    static member APIMisuse(httpCode, errorCode, msg) =
        PublicAPIMisuseError(httpCode, errorCode, msg)
    member this.RaiseAsException() =
        raise <| DotNetLightningException(this)

