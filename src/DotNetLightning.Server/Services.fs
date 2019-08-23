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
        ChannelManagementService

module Services =
    let register(services: IServiceCollection, env: IHostEnvironment) =
        let regF (factory: IServiceProvider -> 'T) = services.AddSingleton<'T>(factory) |> ignore
        regF <| fun _sp -> ServiceBuilder()
        regF <| fun sp ->
            if (env.IsDevelopment()) then
                let cmd = EventCommands.inMemoryEventCommand
                failwith ""
            else
                let cmd = EventCommands.mongoDbEventCommand
                failwith ""
            sp.GetService<ServiceBuilder>().CreateChannelManagementService()
    ()