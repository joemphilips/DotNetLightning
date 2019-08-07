namespace DotNetLightning.Server

open System
open System.Collections.Generic
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.HttpsPolicy;
open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting

open DotNetLightning.Infrastructure
open DotNetLightning.Utils

module private Helpers =
    let inline bind<'T when 'T : (new: unit -> 'T)>(c: IConfiguration) =
        let t = new 'T()
        c.Bind(t)
        t


type Startup private () =
    new (configuration: IConfiguration) as this =
        Startup() then
        this.Configuration <- configuration

    // This method gets called by the runtime. Use this method to add services to the container.
    member this.ConfigureServices(services: IServiceCollection) =
        // Add framework services.
        services.AddControllers() |> ignore
        // Set default configurations for node params
        services.Configure<NodeParams>(fun (nodeP: NodeParams) -> 
            nodeP.Alias <- ""
            // default color will be the same with github's `F#`
            // refs: https://github.com/ozh/github-colors
            nodeP.Color <- { RGB.Red = 184uy; Green = 69uy; Blue = 252uy }
            ()) |> ignore
        Helpers.bind<NodeParams>(this.Configuration.GetSection("node-params")) |> ignore
        services.AddAuthorization() |> ignore

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    member this.Configure(app: IApplicationBuilder, env: IWebHostEnvironment) =
        if (env.IsDevelopment()) then
            app.UseDeveloperExceptionPage() |> ignore
        else
            app.UseHttpsRedirection() |> ignore
            // The default HSTS value is 30 days. You may want to change this for production scenarios, see https://aka.ms/aspnetcore-hsts.
            app.UseHsts() |> ignore

        app.UseRouting() |> ignore

        app.UseAuthorization() |> ignore

        app.UseEndpoints(fun endpoints ->
            endpoints.MapControllers() |> ignore
            ) |> ignore

    member val Configuration : IConfiguration = null with get, set
