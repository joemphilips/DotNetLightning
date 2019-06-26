namespace DotNetLightning.Server
open DotNetLightning.Utils
open DotNetLightning.Utils.RResult

open Microsoft.Extensions.Logging
open System
open System.Buffers
open System.IO.Pipelines
open FSharp.Control
open Microsoft.AspNetCore.Connections
open System.Threading.Tasks
open DotNetLightning.Serialize
open DotNetLightning.Serialize.Msgs
open DotNetLightning.LN
open System
open System.Buffers


type PeerConnectionHandler(logger: ILogger<PeerConnectionHandler>,
                           peerManager: PeerManager) =
    inherit ConnectionHandler()
    member val Logger = logger with get
    member val PeerManager = peerManager with get
    override this.OnConnectedAsync(connection: ConnectionContext) =
        let noiseState = this.PeerManager.Peers
        let handler = this.PeerManager.Peers.Peers |> Map.tryFind(ConnectionId connection.ConnectionId)
        let input = connection.Transport.Input.AsStream()
        use input2 = new LightningReaderStream(input)
        let rec loop () =
            async {
                let msg = input2.ReadUInt16(false)
                match matchMsgType msg with
                | ValueSome (LightningMsgHeader.InitType) -> failwith ""
                | ValueSome (LightningMsgHeader.PingType) -> failwith ""
                | ValueNone ->
                    logger.LogWarning(sprintf "Unknown msg type %d" msg)
                return! loop()
            }
        loop () |> Async.StartAsTask :> Task

        (*
        task {
            let input = connection.Transport.Input
            let rec loop () = vtask {
                let! result = input.ReadAsync()
                let buf = result.Buffer
                if (result.IsCanceled) then
                    return ()
                elif buf.IsEmpty then
                    return ()
                else
                    let nextStep = this.PeerManager.Peers
                    match (_parser.TryParseMessage(&buf, handler)) with
                    | Good msg ->
                        match msg with
                        | Init m -> failwith ""
                        | OpenChannel m ->  failwith "" //this.PeerManager.MessageHandler.ChanHandler.HandleOpenChannel m
                        | _ -> failwith "unknowns msg"
                    | Bad _ -> failwith "failed to parse msg"
                    return ()
            }
            return! loop()
        } :> Task
    *)