namespace FSharp.Control

open System.Threading.Tasks

type AsyncObserver<'a> (fn : Notification<'a> -> Task<unit>) =
    
    interface IAsyncObserver<'a> with
        member this.OnNextAsync (x: 'a) = OnNext x |> fn
        member this.OnErrorAsync (x: exn) = OnError x |> fn
        member this.OnCompletedAsync () = OnCompleted |> fn
        
    static member Create(fn) : IAsyncObserver<'a> =
        AsyncObserver<'a> fn :> IAsyncObserver<'a>
