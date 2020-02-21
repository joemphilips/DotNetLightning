namespace DotNetLightning.Server.P2PConnectionMiddleware

open Microsoft.AspNetCore.Connections
open Microsoft.Extensions.Logging

type P2PConnectionMiddleware(next: ConnectionDelegate, loggerFactory: ILoggerFactory) =
    let logger = loggerFactory.CreateLogger<P2PConnectionMiddleware>()
