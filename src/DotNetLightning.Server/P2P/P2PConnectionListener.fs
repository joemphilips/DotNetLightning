namespace DotNetLightning.Server.P2P

open FSharp.Control.Tasks
open System.Threading.Tasks
open Microsoft.AspNetCore.Connections

type P2PConnectionListener(endPoint) =
    member val EndPoint = endPoint with get
    
    member this.StartAsync() = task {
        return failwith ""
    }
    
    interface IConnectionListener with
        member this.EndPoint
            with get () = this.EndPoint
        member this.AcceptAsync(token):ValueTask<ConnectionContext> =
            failwith ""
            
        member this.DisposeAsync() = failwith ""
        
        member this.UnbindAsync(token): ValueTask =
            failwith ""
