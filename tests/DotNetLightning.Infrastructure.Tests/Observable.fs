
[<AutoOpen>]
module ObservableExtensions

open System
open DotnetLightning.Infrastructure
open FSharp.Control.Reactive
open System.Reactive.Threading.Tasks
open CustomEventAggregator
open DotNetLightning.Infrastructure
open FSharp.Reflection

module Observable =
    let awaitFirst(f) =
        Observable.choose(f)
        >> Observable.first
        >> fun o -> o.ToTask()
        >> Async.AwaitTaskWithTimeout(1000)
        
        
type IEventAggregator with
    member this.AwaitChannelEvent() =
        this.GetObservable<ChannelEventWithContext>()
        |> Observable.map(fun e -> e.ChannelEvent)
        |> Observable.awaitFirst(Some)
    member this.AwaitChannelEvent(f) =
        this.GetObservable<ChannelEventWithContext>()
        |> Observable.map(fun e -> e.ChannelEvent)
        |> Observable.awaitFirst(f)
        
    member this.AwaitPeerEvent(f) =
        this.GetObservable<PeerEventWithContext>()
        |> Observable.awaitFirst(f)
