namespace DotNetLightning.Utils
open System
open System.Reactive.Threading.Tasks
open System.Reactive.Linq


module Observable =
    // Observable to Async interoperation utilities
    let ofAsync (a: Async<'a>) : IObservable<'a> =
        a |> Async.StartAsTask |> (fun t -> t.ToObservable())

    let bind (f: 'a -> IObservable<'b>) (o: IObservable<'a>): IObservable<'b> =
        failwith "todo"

    let filterObs (f: 'a -> IObservable<bool>) : IObservable<'a> -> IObservable<'a>  =
        bind <| fun a ->
            f a
            |> Observable.choose(function | true -> Some a | false -> None)

    let filterAsync (f: 'a -> Async<bool>) : IObservable<'a> -> IObservable<'a> =
        filterObs (f >> ofAsync)

    let mapAsync (f:'a -> Async<'b>) : IObservable<'a> -> IObservable<'b> =
        bind (f >> ofAsync)
