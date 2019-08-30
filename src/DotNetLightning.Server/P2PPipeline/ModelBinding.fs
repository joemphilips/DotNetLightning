namespace DotNetLightning.Server.P2PPipeline

open System
open System.Threading.Tasks

open DotNetLightning.Server.P2P
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Connections
open Microsoft.Extensions.Logging

type P2PMiddleware(next: ConnectionDelegate, handler: P2PHandler, loggerFactory: ILoggerFactory) =
    do if isNull next then raise <| ArgumentException("Request delegate can not be null")
    // pre-compile the handler pipeline
    let func : P2PFunc = handler (Some >> Task.FromResult)
    
    member this.Invoke(ctx: P2PConnectionContext) =
        task {
            let start = System.Diagnostics.Stopwatch.GetTimestamp()
            let! result = func ctx
            let  logger = loggerFactory.CreateLogger<P2PMiddleware>()
            if (logger.IsEnabled LogLevel.Debug) then
                let freq = double System.Diagnostics.Stopwatch.Frequency
                let stop = System.Diagnostics.Stopwatch.GetTimestamp()
                let elapsedMs = (double (stop - start)) * 1000.0 / freq

                logger.LogDebug(
                    "DotNetLightning returned {SomeNoneResult} items {Items} for {ConnectionId} in {ElapsedMs}",
                    (if result.IsSome then "Some" else "None"),
                    ctx.ConnectionId,
                    ctx.Items,
                    elapsedMs)
            if (result.IsNone) then
                return! (next.Invoke ctx)
        }
        
type P2PErrorHandlingMiddleware(next: ConnectionDelegate, handler: ErrorHandler, loggerFactory: ILoggerFactory) =
    do if isNull next then raise (ArgumentNullException("next"))

    member __.Invoke (ctx : P2PConnectionContext) =
        task {
            try return! next.Invoke ctx
            with ex ->
                let logger = loggerFactory.CreateLogger<P2PErrorHandlingMiddleware>()
                try
                    let func = (Some >> Task.FromResult)
                    let! _ = handler ex logger func ctx
                    return ()
                with ex2 ->
                    logger.LogError(EventId(0), ex,  "An unhandled exception has occurred while executing the p2p message.")
                    logger.LogError(EventId(0), ex2, "An exception was thrown attempting to handle the original exception.")
        }


