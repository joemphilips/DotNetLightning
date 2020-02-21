namespace DotNetLightning.Server

open System
open System.IO
open System.Net
open System.Collections.Generic
open System.Linq
open System.Threading.Tasks
open System.Runtime.CompilerServices
open System.Text.Json
open System.Text.Json.Serialization

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Routing

open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.DependencyInjection.Extensions
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

open ResultUtils

open CustomEventAggregator
open DotNetLightning.Chain
open DotNetLightning.Infrastructure
open DotNetLightning.Infrastructure
open DotNetLightning.Infrastructure.ActorManagers
open DotNetLightning.Infrastructure.Interfaces
open DotNetLightning.Infrastructure.Services
open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open Microsoft.Extensions.Options


[<Extension;AbstractClass;Sealed>]
type IServiceCollectionExtension =
    [<Extension>]
    static member AddDotNetLightning(services: IServiceCollection) =
        services.AddSingleton<IRepositoryProvider, RepositoryProvider>() |> ignore
        services.AddSingleton<IRPCClientProvider, RPCClientProvider>() |> ignore
        services.AddHostedService<ChainWatchers>() |> ignore
        services.AddSingleton<IKeysRepository>() |> ignore
        services.AddSingleton<IBroadCaster>() |> ignore
        services.AddSingleton<IChannelEventStream>() |> ignore
        services.AddSingleton<IRepositoryProvider, RepositoryProvider>() |> ignore
        services.AddSingleton<IFeeEstimator, BitcoindRPCFeeEstimator>() |> ignore
        services.AddSingleton<IFundingTxProvider>() |> ignore
        services.AddSingleton<ActorManagers>() |> ignore
        
    [<Extension>]
    static member ConfigureDotNetLightning(services: IServiceCollection, conf: IConfiguration) =
        let allNetworks = DotNetLightningNetworkProvider.getAll()
        for network in allNetworks do
            let chainSettings = conf.GetSection(network.CryptoCode)
            if isNull chainSettings then () else
            
            // Add each chains settings as named-option pattern
            // FYI, this line is doing the same thing with
            // `services.Configure<ChainConfig>(network.CryptoCode, chainSettings) |> ignore`
            services.AddOptions<ChainConfig>(network.CryptoCode).Bind(chainSettings)
                .Validate(fun nodeP -> nodeP.Validate() |> Result.isOk) |> ignore
        ()

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
        services.AddDotNetLightning()
        services.ConfigureDotNetLightning(this.Configuration)
        // Set default configurations for node params
        services.AddMvc() |> ignore
        services.AddAuthorization() |> ignore

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
