namespace DotNetLightning.Server

open System
open System.Buffers
open System.Threading.Tasks
open System.IO.Pipelines
open System.Runtime.CompilerServices

open FSharp.Control.Tasks

open Microsoft.AspNetCore.Connections
open Microsoft.Extensions.Logging

open DotNetLightning.Utils
open DotNetLightning.Infrastructure

[<Extension>]
type SegmentExtensions() = 

    [<Extension>]
    /// TODO: avoid copy
    static member ReadLength<'T when 'T : equality>(this: inref<ReadOnlySequence<'T>>, length: int64) =
        if (this.IsEmpty) then [||] else
        this.Slice(int64 length).ToArray()

type P2PConnectionHandler(peerManager: IPeerManager, logger: ILogger<P2PConnectionHandler>) =
    inherit ConnectionHandler()
    let _logger = logger

    override this.OnConnectedAsync(connectionCtx: ConnectionContext) =
        unitTask {
            let remoteEndPoint = connectionCtx.RemoteEndPoint
            _logger.LogInformation(connectionCtx.ConnectionId + (sprintf " connected with %A" remoteEndPoint))
            while true do
                do! peerManager.ProcessMessageAsync(PeerId remoteEndPoint, connectionCtx.Transport)
            _logger.LogInformation(connectionCtx.ConnectionId + " disconnected")

        }
