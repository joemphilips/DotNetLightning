namespace CustomBedrock.Framework.Infrastructure

open System
open System.IO
open System.IO.Pipelines
open FSharp.Control.Tasks

type DuplexPipeStreamAdapter<'TStream when 'TStream :> Stream>(duplexPipe: IDuplexPipe,
                                       readerOptions, writerOptions,
                                       createStream: Func<Stream, 'TStream>) as this =
    inherit DuplexPipeStream(duplexPipe.Input, duplexPipe.Output)
    let stream = createStream.Invoke(this)
    let _disposeLock = obj()
    let mutable _disposed = false
    member val Stream = stream with get
    member val Input = PipeReader.Create(stream, readerOptions)
    member val Output = PipeWriter.Create(stream, writerOptions)
    
    override this.DisposeAsync() =
        lock _disposeLock <| fun _ ->
            if (_disposed) then () else
            _disposed <- true
        unitVtask {
            do! this.Input.CompleteAsync().ConfigureAwait(false)
            do! this.Output.CompleteAsync().ConfigureAwait(false)
        }
        
    member this.Dispose() = raise <| NotSupportedException()
    interface IDisposable with
        member this.Dispose() = this.Dispose()

    interface IAsyncDisposable with
        member this.DisposeAsync() = this.DisposeAsync()

    interface IDuplexPipe with
        member this.Input = this.Input
        member this.Output = this.Output

    new(duplexPipe: IDuplexPipe, createStream:Func<Stream, 'TStream>) =
        let rOption = StreamPipeReaderOptions(leaveOpen=true)
        let wOption = StreamPipeWriterOptions(leaveOpen=true)
        new DuplexPipeStreamAdapter<'TStream>(duplexPipe, rOption,wOption, createStream)