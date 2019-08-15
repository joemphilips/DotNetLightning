namespace DotNetLightning.Server

open System.Net
open System.Threading
open System.Threading.Tasks
open System.Threading.Channels

open FSharp.Control.Tasks

open Microsoft.AspNetCore.Connections


type P2PConnectionListener(endPoint: UriEndPoint) =
    let _listeningSource = new CancellationTokenSource()
    let _listeningToken = _listeningSource.Token
    let _acceptedQueue = Channel.CreateUnbounded<ConnectionContext>()
    let _listeningTask: Task = null // this.StartAsync()

    member val EndPoint = endPoint with get
    member val ListeningTask = _listeningTask with get, set

    member this.StartAsync() =
        this.ListeningTask <- task {
            failwith "TODO: Listen to socket and write context into channel"
            return ()
        }
        this.ListeningTask

    member this.AcceptAsync(token) =
        vtask {
            let! r1 = _acceptedQueue.Reader.WaitToReadAsync(token)
            let mutable r = r1
            let mutable result = null
            while r do
                match (_acceptedQueue.Reader.TryRead()) with
                | true, c ->
                    result <- c
                    r <- false
                | false, _ ->
                    r <- true
                if r then
                    let! r2 = _acceptedQueue.Reader.WaitToReadAsync(token)
                    r <- r2
            return result
        }


    interface IConnectionListener with
        member this.AcceptAsync(cancellationToken: System.Threading.CancellationToken): System.Threading.Tasks.ValueTask<ConnectionContext> = 
            this.AcceptAsync(cancellationToken)
        member this.DisposeAsync(): System.Threading.Tasks.ValueTask = 
            _listeningSource.Dispose()
            Unchecked.defaultof<ValueTask>
        member this.EndPoint: EndPoint = 
            this.EndPoint :> EndPoint
        member this.UnbindAsync(cancellationToken: System.Threading.CancellationToken): System.Threading.Tasks.ValueTask = 
            unitVtask {
                _listeningSource.Cancel()
                do! _listeningTask
            }
