namespace DotNetLightning.Server.P2PPipeline
open Microsoft.AspNetCore.Connections
open Microsoft.Extensions.Logging


/// Middleware to perform noise handshake
type NoiseHandler(next: ConnectionDelegate, handler: P2PHandler, loggerFactory: ILoggerFactory) =
    do failwith ""
