namespace DotNetLightning.Server
open System
open System.Buffers
open System.Net.Sockets
open System.IO.Pipelines
open FSharp.Control

/// https://devblogs.microsoft.com/dotnet/system-io-pipelines-high-performance-io-in-net/
module TCPServer =
    let FillPipeAsync (socket: Socket, writer: PipeWriter): Async<unit> =
        let minimumBufferSize = 512
        let rec fillPipe (socket: Socket) (writer: PipeWriter) =
            async {
                let mem = writer.GetMemory minimumBufferSize
                try
                    let! bytesRead = socket.ReceiveAsync(mem, SocketFlags.None).AsTask() |> Async.AwaitTask
                    if bytesRead = 0 then
                        return ()
                    else
                        match! writer.FlushAsync().AsTask() |> Async.AwaitTask with
                        | t when t.IsCompleted -> ()
                        | _ -> return! fillPipe socket writer
                with
                    | ex ->
                        return printfn "fillpipe failed with %O" ex
            }
        async {
            do! fillPipe socket writer
            writer.Complete()
        }

    let rec ReadPipeAsync (delimiter: byte) ( processor ) (reader: PipeReader): Async<unit> =
        let rec readPipe buf =
            async {
                let pos = buf.PositionOf(delimiter)
                if pos.HasValue then
                    processor (buf.Slice(0, pos.Value))
                    return! readPipe buf
                else
                    return ()
            }
        async {
            let! res = reader.ReadAsync().AsTask() |> Async.AwaitTask
            let buf = res.Buffer
            do! readPipe buf
            reader.AdvanceTo(buf.Start, buf.End)

            if not (res.IsCompleted) then
                return! ReadPipeAsync delimiter processor reader
            else
                return ()
        }
    let private defaultHandler: ReadOnlySequence<byte> -> unit = printfn "%A"
    let ProcessLinesAsync (socket: Socket) (handler: ReadOnlySequence<byte> -> unit) =
        let p = Pipe()
        let w = FillPipeAsync(socket, p.Writer)
        let r = ReadPipeAsync ((byte)'\n') handler (p.Reader)
        seq [r; w] |> Async.Parallel 

    let asyncSeqWatch(socket) =
        asyncSeq {
            yield 1
            while true do
                do! Async.Sleep 10000
                yield 3
        }