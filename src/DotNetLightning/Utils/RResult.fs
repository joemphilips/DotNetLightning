namespace DotNetLightning.Utils
open System.Text
open System.Collections.Generic

module RResult =
    [<RequireQualifiedAccess>]
    type RBad =
        | Message of string
        | Exception of exn
        | Object of obj
        | DescribedObject of string * obj

        member x.DescribeTo(sb: StringBuilder) =
            let inline app (t:string) = sb.Append t |> ignore

            match x with
            | Message msg -> app "Message: " ; app msg
            | Exception e -> app "Exception: " ; app e.Message
            | Object o -> app "Object: " ; sb.Append o |> ignore
            | DescribedObject (d, _) -> app "Object: "; sb.Append d |> ignore

    [<RequireQualifiedAccess>]
    type RBadTree =
        | Leaf of RBad
        | Fork of RBadTree * RBadTree

        member x.Visit (visitor: RBad -> bool): bool =
            let stack = Stack<RBadTree> 16
            let rec follow t =
                let inline pop () =
                    if stack.Count > 0 then
                        follow (stack.Pop ())
                    else
                        true
                match t with
                | Leaf v -> visitor v && pop ()
                | Fork (l, r) -> stack.Push r; follow l
            follow x

        member x.Flatten(): RBad[] =
            let result = ResizeArray 16
            x.Visit(fun v -> result.Add v; true) |> ignore
            result.ToArray()

        member x.Describe() : string =
            let sb = StringBuilder 16
            x.Visit
                ( fun rbad ->
                    if sb.Length > 0 then sb.Append "; " |> ignore
                    rbad.DescribeTo sb
                    true
                ) |> ignore
            sb.ToString()

        member x.Join o = Fork (x, o)

        (*
        static member op_Implicit(e: exn) =
            RBadTree.Leaf(RBad.Exception(e))

        static member op_Implicit(s: string) =
            RBadTree.Leaf(RBad.Message(s))
        *)

    [<Struct>]
    [<StructuredFormatDisplay("StructuredDisplayString")>]
    type RResult<'T> =
        | Good of good : 'T
        | Bad of bad : RBadTree

        member x.StructuredDisplayString =
            match x with
            | Good g -> sprintf "Good (%A)" g
            | Bad b -> sprintf "Bad ( %A" b

        override x.ToString() = x.StructuredDisplayString


    module RResult =
        exception DerefException of RBadTree

        // Monad
        let inline rreturn v = RResult.Good v
        let inline rbind (uf: 'T -> RResult<'U>) (t:  RResult<'T>) : RResult<'U> =
            match t with
            | RResult.Bad tbad -> RResult.Bad tbad
            | RResult.Good tgood -> uf tgood

        // Kleisli
        let inline rar f = f >> rreturn
        let inline rkleisli uf tf = fun v -> rbind uf (tf v)

        // Applicative
        let inline rpure f = rreturn f
        let inline rapply (t: RResult<'T>) (f: RResult<'T -> 'U>) : RResult<'U> =
            match f, t with
            | RResult.Bad   fbad  , RResult.Bad   tbad  -> RResult.Bad (fbad.Join tbad)
            | RResult.Bad   fbad  , _                   -> RResult.Bad fbad
            | _                   , RResult.Bad   tbad  -> RResult.Bad tbad
            | RResult.Good  fgood , RResult.Good  tgood -> rreturn (fgood tgood)

        // Functor
        let inline rmap (m : 'T -> 'U) (t: RResult<'T>): RResult<'U> =
            match t with
            | RResult.Bad   tbad  -> RResult.Bad tbad
            | RResult.Good  tgood -> rreturn (m tgood)

        // Lifts
        let inline rgood v = rreturn v
        let inline rbad b = RResult.Bad (RBadTree.Leaf b)
        let inline rmsg msg = rbad (RBad.Message msg)
        let inline rexn e = rbad (RBad.Exception e)


    // Misc

        let inline rdelay (tf : unit -> RResult<'T>) : RResult<'T> =
            tf ()

        let inline rthen uf t = rbind uf t

        let inline rdebug name (t : RResult<'T>) : RResult<'T> =
            match t with
            | RResult.Bad   tbad  -> printfn "BAD  - %s - %A" name tbad
            | RResult.Good  tgood -> printfn "GOOD - %s - %A" name tgood
            t

        let inline rbadBind (m : RBadTree -> RResult<'T>) (t : RResult<'T>) : RResult<'T> =
            match t with
            | RResult.Bad   tbad  -> (m tbad)
            | RResult.Good  _     -> t

        let inline rbadMap (m : RBadTree -> RBadTree) (t : RResult<'T>) : RResult<'T> =
            match t with
            | RResult.Bad   tbad  -> RResult.Bad (m tbad)
            | RResult.Good  _     -> t

        let inline rpickle (uf : 'T -> RResult<'U>) (bf : RBadTree -> RResult<'U>) (t : RResult<'T>) : RResult<'U> =
            match t with
            | RResult.Bad   tbad  -> bf tbad
            | RResult.Good  tgood -> uf tgood

        let inline rtoResult (t : RResult<'T>) =
            match t with
            | RResult.Good  good  -> Ok     good
            | RResult.Bad   bad   -> Error  bad

        // Exception aware combinators

        let inline rcatch (uf : 'T -> RResult<'U>) (t : RResult<'T>) : RResult<'U> =
            match t with
            | RResult.Bad   tbad  -> RResult.Bad tbad
            | RResult.Good  tgood ->
                try
                    uf tgood
                with
                | e -> rexn e

        let inline rcatchMap (m : 'T -> 'U) (t : RResult<'T>) : RResult<'U> =
            match t with
            | RResult.Bad   tbad  -> RResult.Bad tbad
            | RResult.Good  tgood ->
                try
                    rreturn (m tgood)
                with
                | e -> rexn e

        // Common combinators
        let inline rorElse (s : RResult<'T>) (f : RResult<'T>) : RResult<'T> =
            match f, s with
            | RResult.Good  _ , _                 -> f
            | _               , RResult.Good _    -> s
            | RResult.Bad fbad, RResult.Bad sbad  -> RResult.Bad (fbad.Join sbad)

        let inline rpair (u : RResult<'U>) (t : RResult<'T>) : RResult<'T*'U> =
            match t, u with
            | RResult.Good  tgood , RResult.Good  ugood -> rreturn (tgood, ugood)
            | RResult.Bad   tbad  , RResult.Good  _     -> RResult.Bad tbad
            | RResult.Good  _     , RResult.Bad   ubad  -> RResult.Bad ubad
            | RResult.Bad   tbad  , RResult.Bad   ubad  -> RResult.Bad (tbad.Join ubad)

        // Unpacking results
        let inline rderef (t : RResult<'T>) : 'T =
            match t with
            | RResult.Good  tgood -> tgood
            | RResult.Bad   tbad  -> raise (DerefException tbad)

        let inline rderefOr (bf : RBadTree -> 'T) (t : RResult<'T>) : 'T =
            match t with
            | RResult.Good  tgood -> tgood
            | RResult.Bad   tbad  -> bf tbad

        let inline runpack (onGood : 'T -> 'U) (onBad : RBadTree -> 'U) (t : RResult<'T>) : 'U =
            match t with
            | RResult.Good  tgood -> onGood tgood
            | RResult.Bad   tbad  -> onBad  tbad

        type Builder () =
            member inline x.Bind        (t, uf) = rbind   uf t
            member inline x.Delay       tf      = rdelay  tf
            member inline x.Return      v       = rreturn v
            member inline x.ReturnFrom  t       = t        : RResult<_>
            member inline x.Zero        ()      = rreturn LanguagePrimitives.GenericZero<_>

    let rresult = RResult.Builder ()


    type RResult<'T> with
        static member inline (>>=)  (x, uf) = RResult.rbind    uf x
        static member inline (<*>)  (x, t)  = RResult.rapply    t x
        static member inline (|>>)  (x, m)  = RResult.rmap      m x
        static member inline (<|>)  (x, s)  = RResult.rorElse   s x
        static member inline (~%%)  x       = RResult.rderef    x
        static member inline (%%)   (x, bf) = RResult.rderefOr bf x
