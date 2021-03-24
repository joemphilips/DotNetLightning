module Tests

open Expecto
open CustomEventAggregator

type SampleEvent = {
  Status: int
}

[<Tests>]
let tests =
  testList "EventAggregator" [
    testCase "subscribe" <| fun _ ->
      let mutable eventWasRaised = false
      let agg: IEventAggregator = ReactiveEventAggregator() :> IEventAggregator

      agg.GetObservable<SampleEvent>() |> Observable.subscribe(fun _se -> eventWasRaised <- true) |> ignore
      agg.Publish<SampleEvent>({ Status = 1 })
      Expect.isTrue eventWasRaised ""

    testCase "Unsubscribe" <| fun _ ->
      let mutable eventWasRaised = false
      let agg = ReactiveEventAggregator() :> IEventAggregator
      let subscription = agg.GetObservable<SampleEvent>() |> Observable.subscribe(fun _se -> eventWasRaised <- true)
      subscription.Dispose() |> ignore
      agg.Publish<SampleEvent>({ Status = 1})
      Expect.isFalse eventWasRaised ""

    testCase "selective subscribe" <| fun _ ->
      let mutable eventWasRaised = false
      let agg = ReactiveEventAggregator() :> IEventAggregator
      agg.GetObservable<SampleEvent>()
        |> Observable.filter(fun se -> se.Status = 1)
        |> Observable.subscribe(fun _se -> eventWasRaised <- true)
        |> ignore

      agg.Publish<SampleEvent>({ Status = 0 })
      Expect.isFalse eventWasRaised ""

      agg.Publish<SampleEvent>({ Status = 1 })
      Expect.isTrue eventWasRaised ""
  ]
