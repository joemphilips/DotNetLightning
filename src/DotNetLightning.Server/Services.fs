namespace DotNetLightning.Server
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Configuration
open Microsoft.AspNetCore.Connections

open System.IO

type ChannelService() =
    inherit ConnectionHandler()

    override this.OnConnectedAsync(conn: ConnectionContext) =
        failwith ""

type ServiceBuilder() =
    member __.CreateChannelManagementService() =
        failwith ""

module Services =
    let getNodeParamsFromConfig() =
        failwith ""

    let register(services: IServiceCollection) =
        failwith ""
    ()