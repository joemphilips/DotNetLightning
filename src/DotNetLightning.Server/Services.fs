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
            let mutable channelEventCommand = null
            if (env.IsDevelopment()) then
                channelEventCommand <- EventCommands.inMemoryEventCommand
            else
                channelEventCommand <- EventCommands.mongoDbEventCommand
            sp.GetService<ServiceBuilder>().CreateChannelManagementService()
    ()