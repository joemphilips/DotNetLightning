namespace FSharp.Control

open FSharp.Control.Tasks

type AsyncDisposable (cancel) =
    interface IAsyncDisposable with
        member this.DisposeAsync() =
            unitVtask {
                do! cancel()
            }
            
    static member Create(cancel) : IAsyncDisposable =
        AsyncDisposable cancel :> IAsyncDisposable
        
    static member Empty: IAsyncDisposable =
        let cancel() = unitVtask {
            return ()
        }
        AsyncDisposable cancel :> IAsyncDisposable
        
    static member Composite (disposables: IAsyncDisposable seq) : IAsyncDisposable =
        let cancel() = unitVtask {
            for d in disposables do
                do! d.DisposeAsync()
        }
        AsyncDisposable cancel :> IAsyncDisposable
