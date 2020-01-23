namespace DotNetLightning.Peer

open DotNetLightning.Crypto
open DotNetLightning.Serialize.Msgs

/// Consumer of the api is probably not interested in the detail of CryptoError
/// and P2PMessageDecodeError, So we should make it simpler by e.g. just having error message
/// as a data for those cases.
/// TODO: fix
type PeerError =
    | CryptoError of CryptoError
    
    | P2PMessageDecodeError of P2PDecodeError
    
    // ---- handshake logic errors ----
    | UnknownHandshakeVersionNumber of uint8
    | UnexpectedHandshake
    
