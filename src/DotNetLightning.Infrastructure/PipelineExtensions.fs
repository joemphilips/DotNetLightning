namespace DotNetLightning.Infrastructure

open System
open System.Buffers
open System.Runtime.CompilerServices
open System.IO.Pipelines

open FSharp.Control.Tasks

[<Extension>]
type PipeReaderExtensions() =

    /// TODO: do not copy.
    [<Extension>]
    static member ReadExactAsync(this: PipeReader, length: int, ?advance: bool) =
        let advance = defaultArg advance true
        vtask {
            let! result = this.ReadAsync()
            let buf = result.Buffer
            let res = buf.Slice(0, length).ToArray()
            if advance then
                let pos = buf.GetPosition(int64 length)
                this.AdvanceTo(pos)
            return res
        }

[<Extension>]
type PipeWriterExtensions() =

    [<Extension>]
    static member WriteAsync(this: PipeWriter, data: byte[]) =
        unitVtask {
            return! this.WriteAsync(ReadOnlyMemory(data))
        }