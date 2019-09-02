namespace FSharp.Control

open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks

module internal Core =
    let infinite = Seq.initInfinite id
    
    let noopAsync = fun _ -> unitTask { () }
    
    let canceller () =
        let cts = new CancellationTokenSource()
        let cancel () = unitVtask {
            cts.Cancel()
        }
        let disposable = { new IAsyncDisposable with member __.DisposeAsync() = cancel() }
        disposable, cts.Token
        
    let safeObserver (obv : IAsyncObserver<'a>) : IAsyncObserver<'a> =
        let agent = MailboxProcessor.Start(fun inbox ->
            let rec messageLoop stopped = async {
                let! n = inbox.Receive()
                if stopped then
                    return! messageLoop stopped
                    
                let! stop = async {
                    match n with
                    | OnNext x ->
                        try
                            do! obv.OnNextAsync x |> Async.AwaitTask
                            return false
                        with
                        | ex ->
                            do! obv.OnErrorAsync ex |> Async.AwaitTask
                            return true
                    | OnError ex ->
                        do! obv.OnErrorAsync ex |> Async.AwaitTask
                        return true
                    | OnCompleted ->
                        do! obv.OnCompletedAsync() |> Async.AwaitTask
                        return true
                }
                return! messageLoop stop
            }
            messageLoop false
        )
        {
            new IAsyncObserver<'a> with
                member this.OnNextAsync x = task {
                    OnNext x |> agent.Post
                }
                member this.OnErrorAsync err = task {
                    OnError err |> agent.Post
                }
                member this.OnCompletedAsync() = task {
                    OnCompleted |> agent.Post
                }
        }
        
        
module Subjects =
    open Core
    // A cold stream that only supports a single subscriber
    let singleSubjects<'a>(): IAsyncObserver<'a> * IAsyncObservable<'a> =
        let mutable oobv : IAsyncObserver<'a> option = None
        let cts = new CancellationTokenSource()
        let subscribeAsync(aobv: IAsyncObserver<'a>) : Task<IAsyncDisposable> =
            let sobv = safeObserver aobv
            if Option.isSome oobv then
                failwith "singleStream: Already subscribed"
                
            oobv <- Some sobv
            cts.Cancel()
            task {
                let cancel() = unitVtask {
                    oobv <- None
                }
                return AsyncDisposable.Create cancel
            }
            
        let obv (n: Notification<'a>) = task {
            return failwith ""
        }
        let obs = { new IAsyncObservable<'a> with member __.SubscribeAsync o = subscribeAsync o }
        AsyncObserver obv :> IAsyncObserver<'a>, obs
        
    // A mailbox subject is a subscribable mailbox.
    // Each message is broadcasted to all subscribed observers.
    let mbSubject<'a>(): MailboxProcessor<Notification<'a>> * IAsyncObservable<'a> =
        let obvs = List<IAsyncObserver<'a>>()
        let cts = new CancellationTokenSource()
        
        let mb = MailboxProcessor.Start(fun inbox ->
            let rec messageLoop _ = async {
                let! n = inbox.Receive()
                
                for aobv in obvs do
                    match n with
                    | OnNext x ->
                        try
                            do! aobv.OnNextAsync x |> Async.AwaitTask
                        with ex ->
                            do! aobv.OnErrorAsync ex |> Async.AwaitTask
                            cts.Cancel()
                    | OnError ex ->
                        do! aobv.OnErrorAsync ex |> Async.AwaitTask
                        cts.Cancel()
                    | OnCompleted ->
                        do! aobv.OnCompletedAsync() |> Async.AwaitTask
                        cts.Cancel()
                return! messageLoop()
            }
            messageLoop()
        ,cts.Token)
        let subscribeAsync (aobv: IAsyncObserver<'a>): Task<IAsyncDisposable> = task {
            let sobv = safeObserver aobv
            obvs.Add sobv
            let cancel () = unitVtask {
                obvs.Remove sobv |> ignore
            }
            return AsyncDisposable.Create cancel
        }
        (mb, { new IAsyncObservable<'a> with member __.SubscribeAsync o = subscribeAsync o })
       
    let subject<'a> () : IAsyncObserver<'a> * IAsyncObservable<'a> =
        let mb, obs = mbSubject<'a>()
        
        let obv = { new IAsyncObserver<'a> with
                      member this.OnNextAsync x = task { OnNext x |> mb.Post  }
                      member this.OnErrorAsync err = task { OnError err |> mb.Post }
                      member this.OnCompletedAsync() = task { OnCompleted |> mb.Post }}
        obv, obs
