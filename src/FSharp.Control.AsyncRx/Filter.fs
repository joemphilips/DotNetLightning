namespace FSharp.Control
open System
open System.Threading.Tasks
open FSharp.Control.Tasks


[<RequireQualifiedAccess>]
module internal Filter =
    open Core
    
    /// Applies the given async function to each element of the stream and
    /// returns the stream comprised of the results for each element
    /// where the function returns Some with some value.
    let chooseAsync(chooser: 'a -> Task<'b option>) (source: IAsyncObservable<'a>): IAsyncObservable<'b> =
        let subscribeAsync(obvAsync : IAsyncObserver<'b>) = task {
            let _obv =
                {
                    new IAsyncObserver<'a> with
                        member this.OnNextAsync x = task {
                            let! result = chooser x
                            match result with
                            | Some b ->
                                do! obvAsync.OnNextAsync b
                            | None -> ()
                        }
                        member this.OnErrorAsync err = task {
                            do! obvAsync.OnErrorAsync err
                        }
                        member this.OnCompletedAsync () = task {
                            do! obvAsync.OnCompletedAsync()
                        }
                }
            return! source.SubscribeAsync _obv
        }
        { new IAsyncObservable<'b> with member __.SubscribeAsync o = subscribeAsync  o }
        
        
    let choose(chooser: 'a -> 'b option) (source: IAsyncObservable<'a>) : IAsyncObservable<'b> =
        chooseAsync (fun x -> task { return chooser x }) source

    let filterAsync (predicate: 'a -> Task<bool>) (source: IAsyncObservable<'a>) : IAsyncObservable<'a> =
        let predicate' a = task {
            let! result = predicate a
            match result with
            | true -> return Some a
            | _ -> return None
        }
        chooseAsync predicate' source
        
    /// Return an observable sequence only containing the distinct contiguous elements from the source sequence.
    let distinctUntilChanged (source: IAsyncObservable<'a>) : IAsyncObservable<'a> =
        let subscribeAsync(aobv: IAsyncObserver<'a>) =
            let safeObserver = safeObserver aobv
            let agent = MailboxProcessor.Start(fun inbox ->
                let rec messageLoop(latest: Notification<'a>) = async {
                    let! n = inbox.Receive()
                    let! latest' = async {
                        match n with
                        | OnNext x ->
                            if n <> latest  then
                                try
                                    do! safeObserver.OnNextAsync x |> Async.AwaitTask
                                with
                                | ex ->
                                    do! safeObserver.OnErrorAsync ex |> Async.AwaitTask
                        | OnError err ->
                            do! safeObserver.OnErrorAsync err |> Async.AwaitTask
                        | OnCompleted ->
                            do! safeObserver.OnCompletedAsync() |> Async.AwaitTask
                        return n
                    }
                    return! messageLoop latest'
                }
                messageLoop OnCompleted
            )
            task {
                let obv n =
                    task {
                        agent.Post n
                    }
                return! AsyncObserver obv |> source.SubscribeAsync
            }
        { new IAsyncObservable<'a> with member __.SubscribeAsync o = subscribeAsync o }

    /// Bypasses a specified number of elements in an observable sequence
    /// and then returns the remaining elements.
    let skip (count: int) (source: IAsyncObservable<'a>) : IAsyncObservable<'a> =
        let subscribeAsync (obvAsync: IAsyncObserver<'a>) =
            let safeObv = safeObserver obvAsync
            task {
                let mutable remaining = count
                let _obv ( n: Notification<'a>) =
                    task {
                        match n with
                        | OnNext x ->
                            if remaining <= 0 then
                                do! safeObv.OnNextAsync x
                            else
                                remaining <- remaining - 1
                        | OnError ex -> do! safeObv.OnErrorAsync ex
                        | OnCompleted -> do! safeObv.OnCompletedAsync()
                    }
                return! source.SubscribeAsync(AsyncObserver.Create _obv)
            }
        { new IAsyncObservable<'a> with member __.SubscribeAsync o = subscribeAsync o }
        
    let take (count: int) (source: IAsyncObservable<'a>) : IAsyncObservable<'a> =
        let subscribeAsync (obvAsync: IAsyncObserver<'a>) =
            let safeObv = safeObserver obvAsync
            task {
                let mutable remaining = count
                let _obv (n: Notification<'a>) =
                    task {
                        match n with
                        | OnNext x ->
                            if remaining > 0 then
                                do! safeObv.OnNextAsync x
                                remaining <- remaining - 1
                            if remaining = 0 then
                                do! safeObv.OnCompletedAsync()
                                remaining <- remaining - 1
                        | OnError ex -> do! safeObv.OnErrorAsync ex
                        | OnCompleted -> do! safeObv.OnCompletedAsync()
                    }
                return! source.SubscribeAsync (AsyncObserver.Create _obv)
            }
            
        { new IAsyncObservable<'a> with member __.SubscribeAsync o = subscribeAsync o }