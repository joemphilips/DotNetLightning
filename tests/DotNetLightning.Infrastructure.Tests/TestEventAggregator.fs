[<AutoOpen>]
module TestEventAggregator

open System.Reactive.Subjects
open System
open CustomEventAggregator

type TestEventAggregator() =
    let _sbj = new Subject<obj>()
    interface IEventAggregator with
        member this.Publish<'T>(msg: 'T) =
            _sbj.OnNext(msg)
            
        member this.GetObservable<'T>(): IObservable<'T> =
            failwith "Fail: not implemented: TestEventAggregator::GetObservable"
