namespace FSharp.Control

open FSharp.Control.Tasks
open System.Threading.Tasks

[<RequireQualifiedAccess>]
module internal Aggregation =
    open Core
    /// Asynchronous version of scan. But the user must specify the initial value
    let scanInitAsync (initial: 's) (accumulator : 's -> 'a -> Task<'s>) (source: IAsyncObservable<'a>): IAsyncObservable<'s> =
        let subscribeAsync (aobv: IAsyncObserver<'s>) =
            let safeObserver = safeObserver aobv
            let mutable state = initial
            
            task {
                let obv n =
                    task {
                        match n with
                        | OnNext x ->
                            try
                                let! state' = accumulator state x
                                state <- state'
                                do! safeObserver.OnNextAsync state
                            with
                            | err -> do! safeObserver.OnErrorAsync err
                        | OnError e -> do! safeObserver.OnErrorAsync e
                        | OnCompleted -> do! safeObserver.OnCompletedAsync()
                    }
                return! AsyncObserver obv |> source.SubscribeAsync
            }
        { new IAsyncObservable<'s> with member __.SubscribeAsync o = subscribeAsync o }
        
    /// Asynchronous version of `Observable.scan`
    let scanAsync (accumulator: 'a -> 'a -> Task<'a>) (source: IAsyncObservable<'a>): IAsyncObservable<'a> =
        let subscribeAsync (aobv: IAsyncObserver<'a>) =
            let safeObserver = safeObserver aobv
            let mutable states = None
            
            task {
                let obv n =
                    task {
                        match n with
                        | OnNext x ->
                            match states with
                            | Some state ->
                                try
                                    let! state' = accumulator state x
                                    states <- Some state'
                                    do! safeObserver.OnNextAsync state
                                with
                                | err -> do! safeObserver.OnErrorAsync err
                            | None ->
                                states <- Some x
                        | OnError e -> do! safeObserver.OnErrorAsync e
                        | OnCompleted -> do! safeObserver.OnCompletedAsync()
                    }
                return! AsyncObserver obv |> source.SubscribeAsync
            }
        { new IAsyncObservable<'a> with member __.SubscribeAsync o = subscribeAsync o }