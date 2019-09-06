[<AutoOpen>]
module TestLogger

open System
open Microsoft.Extensions.Logging
open Expecto
open Expecto.Logging
open Expecto.Logging.Message


type ExpectoLogger<'T>() =
    
    /// Dirty workaround to avoid deadlock when running test in parallel
    /// We wanted to use built-in mechanism of Expecto, but it sometimes throws
    /// NullReference Exception. So we are using just simple printfn
    let _lockObj = new obj()
    let _output: string -> unit =
        fun _ -> ()
        // lock _lockObj (fun () -> printfn "%s")
        
    interface ILogger<'T> with
        member this.Log<'TState>(logLevel, eventId, state: 'TState, except, formatter) =
            _output(formatter.Invoke(state, except))
            
        member this.BeginScope(state) = this :> IDisposable
        member this.IsEnabled(level) = true
        
    interface IDisposable with
        member this.Dispose() =
                ()

let create<'T>(): ILogger<'T> =
    new ExpectoLogger<'T>() :> ILogger<'T>

