namespace DotNetLightning.Infrastructure

open System
open System.Threading.Tasks
open System.Threading.Channels
open FSharp.Control.Tasks

open DotNetLightning.Utils
open Microsoft.Extensions.Logging
    
type IActor<'TCommand> =
    inherit IDisposable
    abstract member StartAsync: unit -> Task
    /// Put specific item on processing queue
    abstract member Put: 'TCommand -> Task
    /// Use this instead of Put if you want caller to be blocked until the state is updated
    /// Usually this is not needed, when you need is caller is referencing actors state directly
    abstract member PutAndWaitProcess: 'TCommand -> Task
    
/// Simple actor model agent utilizing System.Threading.Channels.
/// All inputs to this should be given through CommunicationChannel (To ensure only one change will take place at the time.)
/// And all outputs to other services will go through PublishEvent (typically using EventAggregator)
[<AbstractClass>]
type Actor<'TState, 'TCommand, 'TEvent>(aggregate: Aggregate<'TState, 'TCommand, 'TEvent>, log: ILogger, ?capacity: int) =
    let mutable disposed = false
    let capacity = defaultArg capacity 600
    let communicationChannel =
        let options = BoundedChannelOptions(capacity)
        options.SingleReader <- true
        options.SingleWriter <- false
        System.Threading.Channels.Channel.CreateBounded<'TCommand * TaskCompletionSource<unit> option>(options)
        
        
        
    member val State = aggregate.InitialState with get, set
    abstract member PublishEvent: e: 'TEvent -> Task
    abstract member HandleError: RBad -> Task
    interface IActor<'TCommand> with
        member this.StartAsync() = unitTask {
            let mutable nonFinished = true
            while nonFinished && (not disposed) do
                let! cont = communicationChannel.Reader.WaitToReadAsync()
                nonFinished <- cont
                if nonFinished && (not disposed) then
                    match (communicationChannel.Reader.TryRead()) with
                    | true, (cmd, maybeTcs)->
                        let msg = sprintf "read cmd '%A from communication channel" (cmd)
                        log.LogTrace(msg)
                        match aggregate.ExecuteCommand this.State cmd with
                        | Good events ->
                            let msg = sprintf "Successfully executed command (%A) and got events %A" cmd events
                            log.LogTrace(msg)
                            this.State <- events |> List.fold aggregate.ApplyEvent this.State
                            maybeTcs |> Option.iter(fun tcs -> tcs.SetResult())
                            for e in events do
                                do! this.PublishEvent e
                        | Bad ex ->
                            let ex = ex.Flatten()
                            log.LogTrace(sprintf "failed to execute command and got error %A" ex)
                            ex |> Array.map (this.HandleError) |> ignore
                            maybeTcs |> Option.iter(fun tcs -> tcs.SetException(exn(sprintf "%A" ex)))
                    | false, _ ->
                        ()
            log.LogInformation "disposing actor"
            return ()
        }
        
        member this.Put(cmd: 'TCommand) = unitTask {
                do! communicationChannel.Writer.WriteAsync((cmd, None))
            }
        
        member this.PutAndWaitProcess(cmd: 'TCommand) =
            let tcs = TaskCompletionSource()
            communicationChannel.Writer.WriteAsync((cmd, Some(tcs))) |> ignore
            tcs.Task :> Task
            
        member this.Dispose() =
            disposed <- true
            ()

