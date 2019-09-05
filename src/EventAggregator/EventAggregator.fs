namespace CustomEventAggregator

open System
open System.Reactive.Linq
open System.Reactive.Subjects

type IEventAggregator =
    abstract member Publish: 'T -> unit
    abstract member GetObservable: unit -> IObservable<'T>

type ReactiveEventAggregator() =
    let _subject = new Subject<obj>()
    interface IEventAggregator with
        member this.Publish<'T>(event: 'T) =
            try
                _subject.OnNext(event)
            with
            | e ->
                printfn "Failed to trigger event"
                printfn "%A" (e.StackTrace)
                failwithf "%A" e

        member this.GetObservable<'T>() =
            _subject.OfType<'T>().AsObservable()
