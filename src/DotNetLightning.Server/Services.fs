namespace DotNetLightning.Server
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Connections

open System.IO
open System

open DotNetLightning.Infrastructure

type ChannelService() =
    let _ = ()

type ServiceBuilder() =
    member __.CreateChannelManagementService() =
        ChannelManager

module Services =
    let register(services: IServiceCollection, env: IHostEnvironment) =
        let regF (factory: IServiceProvider -> 'T) = services.AddSingleton<'T>(factory) |> ignore
        regF <| fun _sp -> ServiceBuilder()
        regF <| fun sp ->
            if (env.IsDevelopment()) then
                failwith ""
            else
                failwith ""
            sp.GetService<ServiceBuilder>().CreateChannelManagementService()
    ()