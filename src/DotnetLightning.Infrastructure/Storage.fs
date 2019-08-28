namespace DotNetLightning.Infrastructure

open System
open System.IO
open System.Threading.Tasks
open System.Collections.Concurrent

open DotNetLightning.Serialize
open DotNetLightning.Crypto
open DotNetLightning.LN
open DotNetLightning.DomainUtils.Types

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
    abstract member SetEventsAsync: id: InternalChannelId * events: ChannelEvent seq -> Task
    abstract member GetEventsAsync: id: InternalChannelId * since: DateTimeOffset -> Task<ChannelEvent list>

type O = OptionalArgumentAttribute
type D = System.Runtime.InteropServices.DefaultParameterValueAttribute

type InMemoryChannelEventRepository() =
    member val Dict = ConcurrentDictionary<InternalChannelId, (DateTimeOffset * ChannelEvent) list>() with get
    interface IChannelEventRepository with
        member this.GetEventsAsync(id: InternalChannelId, [<O;D(null)>] since: DateTimeOffset): Task<ChannelEvent list> = 
            match this.Dict.TryGetValue id with
            | true, e ->
                (e |> List.filter(fun (time, e) -> if not <| isNull (box since) then time > since else true) |> List.map snd)
                |> Task.FromResult 
            | false, _ -> failwith "not found id"
        member this.SetEventsAsync(id: InternalChannelId, events: ChannelEvent seq): Task = 
            let now = DateTimeOffset.UtcNow
            let d = events |> List.ofSeq |> List.map(fun e -> now, e)
            match this.Dict.TryAdd(id, d) with
            | true -> Task.CompletedTask
            | false ->
                failwith "Failed to set event"


// --- same things with simple function type ----

type ChannelEventCommand = InternalChannelId * ChannelEvent seq -> Task
module EventCommands =
    let inMemoryEventCommand: ChannelEventCommand =
        let dict = ConcurrentDictionary<InternalChannelId, (DateTimeOffset * ChannelEvent) list>()
        let innerFn (id: InternalChannelId, events: ChannelEvent seq) =
            let now = DateTimeOffset.UtcNow
            let d = events |> List.ofSeq |> List.map(fun e -> now, e)
            match dict.TryAdd (id, d) with
            | true -> Task.CompletedTask
            | false ->
                failwith "Failed to set event"
        innerFn

    let nullEventCommand: ChannelEventCommand =
        fun (id, events) -> Task.CompletedTask

    let mongoDbEventCommand: ChannelEventCommand =
        failwith "not implemented"