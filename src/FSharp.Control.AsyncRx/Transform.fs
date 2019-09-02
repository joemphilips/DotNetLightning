namespace FSharp.Control

open FSharp.Control.Tasks
open System.Threading.Tasks
open Core

[<RequireQualifiedAccess>]
module internal Transformation =
    let mapAsync(mapperAsync: 'a -> Task<'b>) (source: IAsyncObservable<'a>): IAsyncObservable<'b> =
        let subscribeAsync(aobv: IAsyncObserver<'b>) : Task<IAsyncDisposable> =
            task {
                let _obv =
                    {
                        new IAsyncObserver<'a> with
                            member this.OnNextAsync x = task {
                                let! b = mapperAsync x
                                do! aobv.OnNextAsync b
                            }
                            member this.OnErrorAsync ex = task {
                                do! aobv.OnErrorAsync ex
                            }
                            member this.OnCompletedAsync() = task {
                                do! aobv.OnCompletedAsync()
                            }
                    }
                return! source.SubscribeAsync _obv
            }
        { new IAsyncObservable<'b> with member __.SubscribeAsync o = subscribeAsync o }

    let map (mapper: 'a -> 'b) (source: IAsyncObservable<'a>) : IAsyncObservable<'b> =
       mapAsync(fun x -> task {return mapper x}) source
       
    let mapiAsync(mapper: 'a * int -> Async<'b>) (source: IAsyncObservable<'a>) : IAsyncObservable<'b> =
        failwith "TODO"
