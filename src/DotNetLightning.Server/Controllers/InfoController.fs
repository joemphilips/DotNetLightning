namespace DotNetLightning.Server.Controllers
open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Options
open DotNetLightning.Infrastructure

[<Route("api/[controller]")>]
type InfoController(nodeparam: IOptions<NodeParams>) =
    inherit ControllerBase()
    let __nodeparam = nodeparam

    [<HttpGet>]
    member this.Get() =
        this.NodeParam

    member val NodeParam = __nodeparam with get, set