
namespace DotNetLightning.Server.P2P

open System.Threading.Tasks
open System.Net
open System.Threading
open Microsoft.AspNetCore.Connections
open Microsoft.Extensions.Logging

type P2PConnectionFactory(loggerFactory : ILoggerFactory) =
    let _loggerFactory = loggerFactory
    interface IConnectionFactory with
        member this.ConnectAsync(endPoint: EndPoint, token: CancellationToken) =
            failwith ""

