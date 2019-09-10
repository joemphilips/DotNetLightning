namespace DotNetLightning.Tests.Utils
open DotNetLightning.Chain
open Expecto.Logging
open Microsoft.Extensions.Logging

type Node = {
    ChainMonitor: ChainWatchInterfaceUtil
}

type TestLogger = private {
    Level: LogLevel
    Id: string
    _lockObj: obj
}
    with
        interface ILogger with
            member this.BeginScope(state: 'TState): System.IDisposable = 
                failwith "Not Implemented"
            member this.IsEnabled(logLevel: LogLevel): bool = 
                true
            member this.Log(logLevel: LogLevel, eventId: EventId, state: 'TState, ``exception``: exn, formatter: System.Func<'TState,exn,string>): unit = 
                lock this._lockObj (fun _ -> (printf "[%O]: %s" logLevel (formatter.Invoke(state, ``exception``))))


        static member Zero =
            TestLogger.Create("")
            
        member this.LogSimple(str: string) =
            let _l = this :> ILogger
            _l.LogInformation(str + "\n")

        static member Create(id) =
            {
                Level = LogLevel.Debug
                Id = id
                _lockObj = new obj()
            }

        member this.Enable(level) = { this with Level = level }


[<AutoOpen>]
module FromRustLN =
    let createNetwork (nodeCount: uint32) =
        ()