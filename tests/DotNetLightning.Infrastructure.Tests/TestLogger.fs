[<AutoOpen>]
module TestLogger

open System
open Microsoft.Extensions.Logging
open Expecto
open Expecto.Logging
open Expecto.Logging.Message


type ExpectoLogger<'T>() =
    let _output: string -> unit = fun str -> ()// Console.WriteLine
        // let logger = Log.create "Expecto ILogger"
        // let logCore = eventX >> logger.info
        // (logCore)
        
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

