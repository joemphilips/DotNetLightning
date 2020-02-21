module internal DotNetLightning.Server.Controllers.ControllerUtils

open DotNetLightning.Infrastructure
open DotNetLightning.Infrastructure.Error
open DotNetLightning.Infrastructure.Services
open DotNetLightning.Server

let getWatcher(watchers: ChainWatchers) (cryptoCode: string) =
    checkNull cryptoCode
    let cryptoCode = cryptoCode.ToUpperInvariant()
    match watchers.GetWatcher(cryptoCode) with
    | None ->
        DotNetLightningError.APIMisuse(404, "cryptoCode-not-supported", sprintf "%s is not supported" cryptoCode).RaiseAsException()
    | Some watcher ->
        watcher
let getNetwork (watchers: ChainWatchers) (cryptoCode: string): DotNetLightningNetwork =
    getWatcher watchers cryptoCode |> fun w -> w.Network

