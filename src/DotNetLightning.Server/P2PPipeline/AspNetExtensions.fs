namespace DotNetLightning.Server.P2PPipeline

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection

[<AutoOpen>]
module Extensions =
    type IApplicationBuilder with
        member this.UseDotNetLightning(handler: P2PHandler) =
            this.UseMiddleware<P2PMiddleware>(handler) |> ignore
            
        member this.UseDotNetLightningErrorHandler(handler: ErrorHandler) =
            this.UseMiddleware<P2PErrorHandlingMiddleware>(handler)
            
    type IServiceCollection with
        // TODO: Add all services
        member this.AddDotNetLightning() =
            failwith "TODO: Update"

