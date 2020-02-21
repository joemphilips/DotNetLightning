namespace DotNetLightning.Infrastructure.Services

open System
open System.Collections.Concurrent
open DotNetLightning.Infrastructure
open DotNetLightning.Infrastructure.Interfaces
open System.Threading.Tasks

type InMemoryChannelEventStream() =
    member val List = ConcurrentBag<(DateTimeOffset * ChannelEventWithContext)>() with get, set
    interface IChannelEventStream with
        member this.SetEventsAsync(events: ChannelEventWithContext seq): Task = 
            let now = DateTimeOffset.UtcNow
            events
                |> Seq.map(fun e -> now, e)
                |> Seq.iter(this.List.Add)
            Task.CompletedTask

