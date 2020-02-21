[<AutoOpen>]
module TestLogger

open System
open Expecto
open DotNetLightning.Infrastructure.PrimitiveExtensions
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Abstractions

let _lockObj = new obj()
type ExpectoLogger<'T>(color: ConsoleColor) =
    let log = Printf.cprintfn color
    
    /// Dirty workaround to avoid deadlock when running test in parallel
    /// We wanted to use built-in mechanism of Expecto, but it sometimes throws
    /// NullReference Exception. So we are simply using Printf method.
    let _output: string -> unit =
        // fun _ -> ()
        lock _lockObj (fun () ->
                log "%s"
            )
        
    interface ILogger<'T> with
        member this.Log<'TState>(logLevel, eventId, state: 'TState, except, formatter) =
            _output(formatter.Invoke(state, except))
            
        member this.BeginScope(state) = this :> IDisposable
        member this.IsEnabled(level) = true
    interface IDisposable with
        member this.Dispose() =
                ()

let create<'T>(color): ILogger<'T> =
    new ExpectoLogger<'T>(color) :> ILogger<'T>

let getTestLoggerFactory() = LoggerFactory.Create(fun builder ->
    builder.AddConsole() |> ignore
    builder.AddDebug() |> ignore;
    ())
