module PlayGround
open System.IO.Pipelines
open System.Text
open System

/// https://blog.marcgravell.com/2018/07/pipe-dreams-part-1.html
module PipeTest =
    let WriteSomeDataAsync(w: PipeWriter) =
        async {
            let workspace = w.GetMemory(20)
            let b = Encoding.ASCII.GetBytes("Hello, world!".AsSpan(), workspace.Span)
            w.Advance(b)
            return! w.FlushAsync().AsTask() |> Async.AwaitTask
        }

    let Main () =
        async {
            let p = Pipe()
            let! _ = WriteSomeDataAsync(p.Writer)
            return ()
        }


module Observable =