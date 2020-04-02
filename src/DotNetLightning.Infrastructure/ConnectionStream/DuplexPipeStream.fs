namespace CustomBedrock.Framework.Infrastructure

open System
open System.IO
open System.IO.Pipelines
open System.Runtime.InteropServices
open System.Threading
open System.Threading.Tasks

open System
open FSharp.Control.Tasks

type D = DefaultParameterValueAttribute

type DuplexPipeStream(input: PipeReader, output: PipeWriter, ?throwOnCanceled: bool) =
    inherit Stream()
    let throwOnCanceled = Option.defaultValue false throwOnCanceled
    let mutable cancelCalled = true

    member this.CancelPendingRead() =
        cancelCalled <- true
        input.CancelPendingRead()
        
    override this.CanRead = true
    override this.CanSeek = false
    override this.CanWrite = true
    
    override this.Length =
        raise <| NotSupportedException()
    override val Position =
        raise <| NotSupportedException() with get, set

    override this.Seek(offset, origin: SeekOrigin) =
        raise <| NotSupportedException()
    override this.SetLength(value) =
        raise <| NotSupportedException()
    member private this.ReadAsyncInternal(destination: Memory<byte>, ct: CancellationToken) =
        vtask {
            let mutable loop = true
            let mutable returnV = 0
            while loop do
                let! result = input.ReadAsync(ct).ConfigureAwait(false)
                let mutable readableBuffer = result.Buffer
                try
                    if (throwOnCanceled && result.IsCanceled && cancelCalled) then
                        cancelCalled <- false
                        raise <| OperationCanceledException()
                    else if (not <| readableBuffer.IsEmpty) then
                        let count = Math.Min((int)readableBuffer.Length, destination.Length) |> int
                        readableBuffer <- readableBuffer.Slice(0, count)
                        returnV <- count
                        loop <- false
                    else if (result.IsCompleted) then
                        returnV <- 0
                        loop <- false
                finally
                    input.AdvanceTo(readableBuffer.End, readableBuffer.End)
            return
                returnV
        }

    override this.Read(buf: byte[], offset: int, count: int) =
        this.ReadAsyncInternal(Memory<_>(buf, offset, count), Unchecked.defaultof<CancellationToken>).Result
        
    override this.ReadAsync(buf: byte[], offset: int, count: int, ct: CancellationToken): Task<int> =
        this.ReadAsyncInternal(Memory<_>(buf, offset, count), ct).AsTask()
    override this.ReadAsync(dest: Memory<_>, ct: CancellationToken) =
        this.ReadAsyncInternal(dest, ct)
        
    override this.WriteAsync(buf: byte[], offset, count, ct): Task =
        unitTask {
            if (not <| isNull buf) then
                let! _ = output.WriteAsync(ReadOnlyMemory<byte>(buf, offset, count))
                ()
            else
                ()
            let! _ = output.FlushAsync(ct).ConfigureAwait(false)
            return ()
        }
        
    override this.Write(buf, offset, count) =
        this.WriteAsync(buf, offset, count).GetAwaiter().GetResult()
        
        
    override this.WriteAsync(source: ReadOnlyMemory<byte>, ct) =
        unitVtask {
            let! _ = output.WriteAsync(source)
            let! _ = output.FlushAsync(ct).ConfigureAwait(false)
            return ()
        }
        
    override this.FlushAsync(ct): Task =
        this.WriteAsync(null, 0, 0, ct)
    override this.Flush() =
        this.FlushAsync(CancellationToken.None).GetAwaiter().GetResult()

    override this.BeginRead(buf, offset, count, asyncCB, state) =
        failwith "Not implemented: DuplexPipeStream::BeginRead"
    override this.EndRead(asyncResult): int =
        failwith "Not implemented: DuplexPipeStream::EndRead"
    override this.BeginWrite(buf, offset, count, asyncCB, state) =
        failwith "Not implemented: DuplexPipeStream::BeginWrite"
        
    override this.EndWrite(asyncResult: IAsyncResult) =
        failwith "Not implemented: DuplexPipeStream::EndWrite"
