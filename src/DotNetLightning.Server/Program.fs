namespace DotNetLightning.Server

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting

open Microsoft.AspNetCore.Connections
open Microsoft.AspNetCore.Server
open Microsoft.AspNetCore.Server.Kestrel

open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

open FSharp.Control.Tasks

open Ply

open DotNetLightning.Infrastructure


type ServerParams() =
    member val P2PIpEndpoint = System.Net.IPEndPoint.Parse("tcp://localhost:127.0.0.1:9735") with get, set

type P2PConnectionHandler(channelManager: ChannelManagementService) =
    inherit ConnectionHandler()

    override this.OnConnectedAsync(cctx) =
        let ip = cctx.RemoteEndPoint
        let inputS = cctx.Transport.Input.AsStream()
        let rec loop () = task {
            return ()
        }
        loop () :> Task

module Program =
    let exitCode = 0

    let CreateHostBuilder args =
        let configureConfig (builder: IConfigurationBuilder) =
            builder.SetBasePath(Directory.GetCurrentDirectory()) |> ignore
            let iniPath = Path.Join(Constants.homePath, Constants.HOME_DIRECTORY_NAME, "dotnetlightning.conf")
            builder.AddIniFile(iniPath, optional=true) |> ignore
            builder.AddEnvironmentVariables("DOTNETLIGHTNING") |> ignore
            builder.AddCommandLine(args=args) |> ignore
            builder

        let config =
            (configureConfig (ConfigurationBuilder()))
                .Build()

        let serverP = ServerParams()
        config.GetSection("server").Bind(serverP)

        Host.CreateDefaultBuilder(args)
            .ConfigureAppConfiguration(configureConfig >> ignore)
            .ConfigureLogging(fun (logging) -> 
                logging.AddConsole() |> ignore
                ()
            )
            .ConfigureWebHostDefaults(fun webBuilder ->
                // for RPC web interface
                webBuilder.UseStartup<Startup>() |> ignore

                // for p2p connections
                webBuilder.UseKestrel(fun (webhostBuilder) ->
                    webhostBuilder.Listen(serverP.P2PIpEndpoint, fun options ->
                        options.UseConnectionLogging() |> ignore
                        options.UseConnectionHandler<P2PConnectionHandler>() |> ignore
                        ()
                    )
                    ()
                ) |> ignore
                ()
            )

    [<EntryPoint>]
    let main args =
        CreateHostBuilder(args)
            .Build().Run()

        exitCode
