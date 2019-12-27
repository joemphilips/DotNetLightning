namespace EclairRPCClient

type EclairAPIOperation<'a> =
    | AllChannels of unit * (ChannelDesc list -> 'a)
    | AllNodes of unit * (NodeInfo -> 'a)
    | Audit of unit * (AuditResult -> 'a)
    | GetInfo of unit * (GetInfoResult -> 'a)
    
type EclairAPIProgram<'a> =
    | Free of EclairAPIOperation<EclairAPIProgram<'a>>
    | Pure of 'a
    
[<AutoOpen>]
module EclairAPI =
    let private mapI f = function
        | AllChannels (x, next) -> AllChannels(x, next >> f)
        | AllNodes (x, next) -> AllNodes(x, next >> f)
        | Audit(x, next) -> Audit(x, next >> f)
        | GetInfo(x, next) -> GetInfo(x, next >> f)
        
    let rec bind f = function
        | Free x -> x |> mapI (bind f) |> Free
        | Pure x -> f x

    type EclairAPIBuilder() =
        member this.Bind(x, f) = bind f x
        member this.Return x = Pure x
        member this.ReturnFrom x = x
        member this.Zero() = Pure()
    let eclairapi = EclairAPIBuilder()
    
    let allChannels() = ((), Pure) |> AllChannels |> Free
    let allNodes() = ((), Pure) |> AllNodes |> Free
    let audit = ((), Pure) |> Audit |> Free
    let getInfo = ((), Pure) |> GetInfo |> Free
