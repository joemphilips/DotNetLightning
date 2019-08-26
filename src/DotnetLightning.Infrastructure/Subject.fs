module Observable

open System
open System.Collections.Generic

type Subject<'T> () =
    let sync = obj()
    let mutable stopped = false
    let observers = List<IObserver<'T>>()
    let iter f = observers |> Seq.iter f

    let onCompleted() =
        if not stopped then
            stopped <- true
            iter (fun observer -> observer.OnCompleted())
        ()

    let onError ex () =
        if not stopped then
            iter(fun observer -> observer.OnError ex)

    let next v () =
        if not stopped then
            iter (fun obs -> obs.OnNext(v))
        ()

    let remove observer () =
        observers.Remove observer |> ignore
    
    member this.Next value = lock sync <| next value
    member this.Error ex = lock sync <| onError ex
    member this.Completed () = lock sync <| onCompleted

    interface IObserver<'T> with
        member this.OnCompleted(): unit = 
            this.Completed()
        member this.OnError(error: exn): unit = 
            this.Error error
        member this.OnNext(value: 'T): unit = 
            this.Next value

    interface IObservable<'T> with
        member this.Subscribe(observer: IObserver<'T>): IDisposable = 
            observers.Add(observer)
            {
                new IDisposable with
                    member this.Dispose() = lock sync <| remove observer
            }