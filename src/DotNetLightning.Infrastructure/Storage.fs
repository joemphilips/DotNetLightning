namespace DotNetLightning.Infrastructure

open System
open System.IO
open System.Threading.Tasks
open System.Collections.Concurrent
open DotNetLightning.Serialize

type FlatFileDB(path: string, codec: SupportedCodec) =
    let _path = path

    member this.Write() =
        use fs = new FileStream(_path, FileMode.Open)
        use stream = new LightningWriterStream(fs)
        failwith ""

type SupportedDBType =
    | Null
    /// Only for testing
    | InMemory
    | MongoDB of connectionString: string

type IChannelEventRepository =
    abstract member SetEventsAsync: events: ChannelEventWithContext seq -> Task
    abstract member GetEventsAsync: since: DateTimeOffset -> Task<ChannelEventWithContext list>

type O = OptionalArgumentAttribute
type D = System.Runtime.InteropServices.DefaultParameterValueAttribute

type InMemoryChannelEventRepository() =
    member val List = ConcurrentBag<(DateTimeOffset * ChannelEventWithContext)>() with get, set
    interface IChannelEventRepository with
        member this.GetEventsAsync([<O;D(null)>] since: DateTimeOffset): Task<ChannelEventWithContext list> =
            this.List :> seq<_>
                |> Seq.filter(fun (time, e) -> if not <| isNull (box since) then time > since else true)
                |> Seq.toList
                |> List.sortBy(fst)
                |> List.map snd
                |> Task.FromResult 
        member this.SetEventsAsync(events: ChannelEventWithContext seq): Task = 
            let now = DateTimeOffset.UtcNow
            events
                |> Seq.map(fun e -> now, e)
                |> Seq.iter(this.List.Add)
            Task.CompletedTask

