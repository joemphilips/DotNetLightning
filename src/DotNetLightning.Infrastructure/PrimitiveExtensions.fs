[<AutoOpen>]
module DotnetLightning.Infrastructure.PrimitiveExtensions

open System.Threading
open System.Threading.Tasks

type Async with
    static member AwaitTaskWithTimeout(timeout: int) (t: Task<'T>) =
        async {
            use cts = new CancellationTokenSource()
            use timer = Task.Delay(timeout, cts.Token)
            let! completed = Async.AwaitTask <| Task.WhenAny(t, timer)
            if completed = (t :> Task) then
                cts.Cancel()
                let! result = Async.AwaitTask t
                return Some result
            else
                return None
        }