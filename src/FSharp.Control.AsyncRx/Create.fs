namespace FSharp.Control

open System
open System.Threading
open System.Threading.Tasks

open FSharp.Control.Tasks

[<RequireQualifiedAccess>]
module internal Create =
    open Core
    let create (subsc: IAsyncObserver<'a> -> Task<IAsyncDisposable>) : IAsyncObservable<'a> =
        { new IAsyncObservable<'a> with member __.SubscribeAsync o = subsc o }
        
    /// Create async observable from async worker function
    let ofTaskWorker (worker: IAsyncObserver<'a> -> CancellationToken -> Task) : IAsyncObservable<'a> =
        let subscribeAsync (aobv: IAsyncObserver<_>) : Task<IAsyncDisposable> =
            let disposable, token = canceller()
            let safeObv = safeObserver aobv
            task {
                do! ((worker safeObv token))
                return disposable
            }
            
        { new IAsyncObservable<'a> with member __.SubscribeAsync o = subscribeAsync o }
        
    /// Returns the async observable sequence whose single element is
    /// the result of the given async workflow.
    let ofTask(workflow: Task<'a>) : IAsyncObservable<'a> =
        let subscribeAsync (aobv: IAsyncObserver<_>): Task<IAsyncDisposable> =
            let safeObv = safeObserver aobv
            task {
                let! result = workflow
                return AsyncDisposable.Empty
            }
        { new IAsyncObservable<'a> with member __.SubscribeAsync o = subscribeAsync o }
        
    /// Returns an observable sequence containing the single specified element.
    let single (value: 'a) =
        let subscribeAsync (aobv: IAsyncObserver<_>) : Task<IAsyncDisposable> =
            let safeObv  = safeObserver aobv
            task {
                do! safeObv.OnNextAsync value
                do! safeObv.OnCompletedAsync()
                return AsyncDisposable.Empty
            }
            
        { new IAsyncObservable<'a> with member __.SubscribeAsync o = subscribeAsync o }
        
    /// Returns an observable sequence with no elements
    let empty<'a>(): IAsyncObservable<'a> =
        let subscribeAsync (aobv: IAsyncObserver<_>) : Task<IAsyncDisposable> =
            task {
                do! aobv.OnCompletedAsync()
                return AsyncDisposable.Empty
            }
        { new IAsyncObservable<'a> with member __.SubscribeAsync o = subscribeAsync o }
        
    let never<'a> () : IAsyncObservable<'a> =
        let subscribeAsync (_: IAsyncObserver<_>) : Task<IAsyncDisposable> =
            task {
                return AsyncDisposable.Empty
            }
        { new IAsyncObservable<'a> with member __.SubscribeAsync o = subscribeAsync o }
        
    ///
    let inline fail<'a>(error: exn) : IAsyncObservable<'a> =
        ofTaskWorker(fun obv _ -> unitTask {
            do! obv.OnErrorAsync error
        })
        
        
    /// Returns the async observable sequence whose elements are pulled
    /// from the given enumerable sequence.
    let inline ofSeq(xs: 'a seq): IAsyncObservable<'a> =
        ofTaskWorker(fun obv token -> unitTask {
            for x in xs do
                try
                    do! obv.OnNextAsync x
                with ex ->
                    do! obv.OnErrorAsync ex
            do! obv.OnCompletedAsync()
        })
