namespace DotNetLightning.Server.P2P

open FSharp.Control.Tasks
open System.Net
open Microsoft.AspNetCore.Connections
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.Extensions.Logging

type P2PConnectionListenerFactory(loggerFactory: ILoggerFactory) =
    let _loggerFactory = loggerFactory
    interface IConnectionListenerFactory with
        member this.BindAsync(endPoint: EndPoint, token) = vtask {
            
            let listener = P2PConnectionListener(endPoint)
            do! listener.StartAsync()
            return listener :> IConnectionListener
        }

