namespace DotNetLightning.Server

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

module Program =
    let exitCode = 0

    let private homePath =
        if (Environment.OSVersion.Platform = PlatformID.Unix) || (Environment.OSVersion.Platform = PlatformID.MacOSX) then
            Environment.GetEnvironmentVariable("HOME")
        else
            Environment.ExpandEnvironmentVariables("%HOMEDRIVE%%HOMEPATH%")

    let CreateHostBuilder args =
        Host.CreateDefaultBuilder(args)
            .ConfigureAppConfiguration(fun (builder) ->
                builder.SetBasePath(Directory.GetCurrentDirectory()) |> ignore
                let iniPath = Path.Join(homePath, "dotnetlightning.config")
                builder.AddIniFile(iniPath, optional=true) |> ignore
                builder.AddEnvironmentVariables("DOTNETLIGHTNING") |> ignore
                builder.AddCommandLine(args) |> ignore
                ()
            )
            .ConfigureWebHostDefaults(fun webBuilder ->
                webBuilder.UseStartup<Startup>() |> ignore
            )

    [<EntryPoint>]
    let main args =
        CreateHostBuilder(args).Build().Run()

        exitCode
