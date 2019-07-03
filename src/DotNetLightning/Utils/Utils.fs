namespace DotNetLightning.Utils
open System
open System.Threading.Tasks

[<AutoOpen>]
module Utils =
    let inline (!>) (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b )x )

module Async =
    let result = async.Return

    let map f value = async {
        let! v = value
        return! f v
    }

    let bind f xAsync = async {
        let! x = xAsync
        return! f x
    }

    let withTimeout timeoutMillis op =
        async {
            let! child = Async.StartChild(op, timeoutMillis)
            try
                let! result = child
                return Some result
            with :? TimeoutException ->
                return None
        }
    let apply fAsync xAsync = async {
        let! fChild = Async.StartChild fAsync
        let! xChild = Async.StartChild xAsync
        let! f = fChild
        let! x = xChild
        return f x
    }

    let lift2 f x y =
        apply (apply (result f) x) y

    let lift3 f x y z =
        apply (apply (apply (result f) x) y) z

    let lift4 f x y z a =
        apply (apply (apply (apply (result f) x) y) z) a

    let lift5 f x y z a b =
        apply (apply (apply (apply (apply (result f) x) y) z) a) b

    module Operators =

        let inline (>>=) m f = bind f m

        let inline (=<<) f m = bind f m

        let inline (<*>) f m = apply f m

        let inline (<!>) f m = map f m

        let inline ( *>) m1 m2 = lift2 (fun _ x -> x) m1 m2

        let inline ( <*) m1 m2 = lift2 (fun x _ -> x) m1 m2

    /// extend default async builder to enable awaiting task without Async.AwaitTask
    type AsyncBuilder with
        member x.Bind(t: Task<'T>, f: 'T -> Async<'R>): Async<'R> =
            async.Bind(Async.AwaitTask t, f)

        member x.Bind(t: Task, f: unit -> Async<'R>): Async<'R> =
            async.Bind(Async.AwaitTask t, f)
module Set = 
    let first (predicate: 'a -> bool) (items: Set<'a>) =
        let res = items |> Set.filter predicate
        if (Seq.isEmpty res) then
            None
        else
            Some(res |> Set.toList |> fun x -> x.[0])


module List =
    /// Map a Async producing function over a list to get a new Async using
    /// applicative style. ('a -> Async<'b>) -> 'a list -> Async<'b list>
    let rec traverseAsyncA f list =
        let (<*>) = Async.apply
        let cons head tail = head :: tail
        let initState = Async.result []
        let folder head tail =
            Async.result cons <*> (f head) <*> tail
        List.foldBack folder list initState

    /// Transform a "list<Async>" into a "Async<list>" and collect the results
    /// using apply.
    let sequenceAsyncA x = traverseAsyncA id x

    let rec traverseRResult f list =
        let cons head tail = head :: tail
        let initState = RResult.Good []
        let folder h t =
            RResult.Good cons <*> (f h) <*> t
        List.foldBack folder list initState

    let sequenceRResult x = traverseRResult id x