namespace CustomEventAggregator

open System

type IEventAggregator =
    abstract member Publish: 'T -> unit
    abstract member GetObservable: unit -> IObservable<'T>

type ReactiveEventAggregator() =
    let _event = Event<obj>()
    interface IEventAggregator with
        member this.Publish<'T>(event: 'T) =
            _event.Trigger(event)

        member this.GetObservable<'T>() =
            _event.Publish
            |> Observable.choose(fun e -> match e with | :? 'T as et -> Some et | _ -> None)