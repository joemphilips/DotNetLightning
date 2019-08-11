namespace DotNetLightning.Server

open System
open System.Net
open System.Collections.Generic
open System.Linq
open System.Threading.Tasks
open System.Text.Json
open System.Text.Json.Serialization

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.HttpsPolicy;
open Microsoft.AspNetCore.Mvc
open Microsoft.AspNetCore.Connections
open Microsoft.AspNetCore.Routing

open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

open DotNetLightning.Infrastructure
open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open System.IO



type Startup private () =
    new (configuration: IConfiguration, hostingEnv: IHostEnvironment) as this =
        Startup() then
        this.Configuration <- configuration
        this.Env <- hostingEnv

    // This method gets called by the runtime. Use this method to add services to the container.
    member this.ConfigureServices(services: IServiceCollection) =
        // Add framework services.
        services.AddControllers()
            .AddJsonOptions(fun options ->
                options.JsonSerializerOptions.Converters.Add(JsonFSharpConverter())
                ) |> ignore
        // Set default configurations for node params
        services.Configure<NodeParams>(fun (nodeP: NodeParams) -> 
            nodeP.Alias <- ""
            // default color will be the same with github's `F#`
            // refs: https://github.com/ozh/github-colors
            nodeP.Color <- { RGB.Red = 184uy; Green = 69uy; Blue = 252uy }
            nodeP.PublicAddresses <- []
            nodeP.LocalFeatures <- (LocalFeatures.Flags [||])
            nodeP.DataDirPath <- Path.Join(Constants.homePath, "data")

            ()) |> ignore
        // overwrite default settings if it is specified in config
        let nodeP = this.Configuration.GetSection("nodeparams")
        services.AddOptions<NodeParams>().Bind(nodeP)
            .Validate(fun nodeP -> nodeP.Validate() |> function Good _ -> true | Bad tree -> failwith (tree.Describe())) |> ignore
        services.AddAuthorization() |> ignore
        Services.register(services, this.Env)

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    member this.Configure(app: IApplicationBuilder, env: IWebHostEnvironment) =
        if (env.IsDevelopment()) then
            app.UseDeveloperExceptionPage() |> ignore
        else
            // The default HSTS value is 30 days. You may want to change this for production scenarios, see https://aka.ms/aspnetcore-hsts.
            app.UseHsts() |> ignore

        app.UseHttpsRedirection() |> ignore
        app.UseRouting() |> ignore

        app.UseAuthorization() |> ignore

        app.UseEndpoints(fun (endpoints: IEndpointRouteBuilder) ->
            // rpc api
            endpoints.MapControllers() |> ignore
            ) |> ignore

    member val Configuration : IConfiguration = null with get, set
    member val Env: IHostEnvironment = null with get, set
