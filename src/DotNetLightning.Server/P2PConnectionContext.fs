namespace DotNetLightning.Server

open System
open System.IO.Pipelines
open System.Net
open System.Net.Sockets

open Microsoft.AspNetCore.Connections.Features
open Microsoft.AspNetCore.Connections

type P2PConnectionContext(stream: NetworkStream) =
    // inherit ConnectionContext()

    let mutable _connectionId = Guid().ToString()

    member val Input = PipeReader.Create(stream) with get
    member val Output = PipeWriter.Create(stream) with get

    //override this.ConnectionId
      //  with get() = _connectionId
       // and set v = _connectionId <- v