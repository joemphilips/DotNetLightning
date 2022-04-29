namespace DotNetLightning.Utils

open System

open System.Threading.Tasks
open System.Runtime.CompilerServices

[<AutoOpen>]
module Utils =
    let inline checkNull name o =
        if isNull(box <| o) then
            nullArg(name)
        else
            ()

    /// Shorthand for op_Implicit for any types which implements.
    let inline (!>)(x: ^a) : ^b =
        ((^a or ^b): (static member op_Implicit: ^a -> ^b) x)

    let inline curry2 f a b =
        f(a, b)

    let inline uncurry2 f t =
        let (a, b) = t in f a b

    let inline curry3 f a b c =
        f(a, b, c)

    let inline uncurry3 f t =
        let (a, b, c) = t in f a b c

    let inline curry4 f a b c d =
        f(a, b, c, d)

    let inline uncurry4 f t =
        let (a, b, c, d) = t in f a b c d

    let inline curry5 f a b c d e =
        f(a, b, c, d, e)

    let inline uncurry5 f t =
        let (a, b, c, d, e) = t in f a b c d e

    let inline curry6 f a b c d e g =
        f(a, b, c, d, e, g)

    let inline uncurry6 f t =
        let (a, b, c, d, e, g) = t in f a b c d e g

    let inline curry7 f a b c d e g h =
        f(a, b, c, d, e, g, h)

    let inline uncurry7 f t =
        let (a, b, c, d, e, g, h) = t in f a b c d e g h

    let inline curry8 f a b c d e g h i =
        f(a, b, c, d, e, g, h, i)

    let inline uncurry8 f t =
        let (a, b, c, d, e, g, h, i) = t in f a b c d e g h i

    let inline curry9 f a b c d e g h i j =
        f(a, b, c, d, e, g, h, i, j)

    let inline uncurry9 f t =
        let (a, b, c, d, e, g, h, i, j) = t in f a b c d e g h i j

    let inline curry10 f a b c d e g h i j k =
        f(a, b, c, d, e, g, h, i, j, k)

    let inline uncurry10 f t =
        let (a, b, c, d, e, g, h, i, j, k) = t in f a b c d e g h i j k

    let inline curry11 f a b c d e g h i j k l =
        f(a, b, c, d, e, g, h, i, j, k, l)

    let inline uncurry11 f t =
        let (a, b, c, d, e, g, h, i, j, k, l) = t in f a b c d e g h i j k l


    let inline max a b =
        if b > a then
            b
        else
            a

    let inline min a b =
        if a > b then
            b
        else
            a

[<Extension>]
type Extensions() =
    [<Extension>]
    static member ToHexString(this: array<byte>) =
        "0x"
        + BitConverter
            .ToString(this)
            .Replace("-", "")
            .ToLowerInvariant()

module Async =
    let result = async.Return


    let map f value =
        async {
            let! v = value
            return! f v
        }

    let bind f xAsync =
        async {
            let! x = xAsync
            return! f x
        }

    let withTimeout timeoutMillis op =
        async {
            let! child = Async.StartChild(op, timeoutMillis)

            try
                let! result = child
                return Some result
            with
            | :? TimeoutException -> return None
        }

    let apply fAsync xAsync =
        async {
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

        let inline (>>=) m f =
            bind f m

        let inline (=<<) f m =
            bind f m

        let inline (<*>) f m =
            apply f m

        let inline (<!>) f m =
            map f m

        let inline ( *> ) m1 m2 =
            lift2 (fun _ x -> x) m1 m2

        let inline (<*) m1 m2 =
            lift2 (fun x _ -> x) m1 m2

    /// extend default async builder to enable awaiting task without Async.AwaitTask
    type AsyncBuilder with

        member __.Bind(t: Task<'T>, f: 'T -> Async<'R>) : Async<'R> =
            async.Bind(Async.AwaitTask t, f)

        member __.Bind(t: Task, f: unit -> Async<'R>) : Async<'R> =
            async.Bind(Async.AwaitTask t, f)

module Set =
    let first (predicate: 'a -> bool) (items: Set<'a>) =
        let res = items |> Set.filter predicate

        if (Seq.isEmpty res) then
            None
        else
            Some(res |> Set.toList |> (fun x -> x.[0]))

module Array =
    let skipBack (length: int) (a: array<'a>) =
        a.[0 .. a.Length - 1 - length]

module List =
    /// Map a Async producing function over a list to get a new Async using
    /// applicative style. ('a -> Async<'b>) -> 'a list -> Async<'b list>
    let rec traverseAsyncA f list =
        let (<*>) = Async.apply

        let cons head tail =
            head :: tail

        let initState = Async.result []

        let folder head tail =
            Async.result cons <*> (f head) <*> tail

        List.foldBack folder list initState

    /// Transform a "list<Async>" into a "Async<list>" and collect the results
    /// using apply.
    let sequenceAsyncA x =
        traverseAsyncA id x
