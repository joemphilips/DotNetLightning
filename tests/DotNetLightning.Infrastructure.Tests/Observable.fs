[<AutoOpen>]
module ObservableExtensions

open DotnetLightning.Infrastructure
open FSharp.Control.Reactive
open System.Reactive.Threading.Tasks

module Observable =
    let awaitFirst(f) =
        Observable.choose(f)
        >> Observable.first
        >> fun o -> o.ToTask()
        >> Async.AwaitTaskWithTimeout(500)