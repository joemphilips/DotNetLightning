namespace FSharp.Control

open System
open System.Threading.Tasks

type IAsyncDisposable = System.IAsyncDisposable

type IAsyncObserver<'a> =
    abstract member OnNextAsync : 'a -> Task<unit>
    abstract member OnErrorAsync: exn -> Task<unit>
    abstract member OnCompletedAsync : unit -> Task<unit>
    
type IAsyncObservable<'a> =
    abstract member SubscribeAsync: IAsyncObserver<'a> -> Task<IAsyncDisposable>
    
type Notification<'a> =
    | OnNext of 'a
    | OnError of exn
    | OnCompleted