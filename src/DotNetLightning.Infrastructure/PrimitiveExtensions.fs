[<AutoOpen>]
module DotNetLightning.Infrastructure.PrimitiveExtensions

open System.Threading
open System.Threading.Tasks

module Printf =
    let cprintf c fmt =
        Printf.kprintf
            (fun s ->
                let old = System.Console.ForegroundColor
                try
                    System.Console.ForegroundColor <- c
                    System.Console.Write s
                finally
                    System.Console.ForegroundColor <- old
            )
            fmt
            
    let cprintfn c fmt =
        Printf.kprintf
            (fun s ->
                let old = System.Console.ForegroundColor
                try
                    System.Console.ForegroundColor <- c
                    System.Console.WriteLine s
                finally
                    System.Console.ForegroundColor <- old
            )
            fmt
    
    
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