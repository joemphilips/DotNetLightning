namespace DotNetLightning.Utils

open ResultUtils
open ResultUtils.Portability

module Aether =
    type Lens<'a, 'b> =
        ('a -> 'b) * ('b -> 'a -> 'a)

    type Prism<'a, 'b> =
        ('a -> 'b option) * ('b -> 'a -> 'a)


    /// --- Morphisms ---

    type Isomorphism<'a, 'b> =
        ('a -> 'b) * ('b -> 'a)

    type Epimorphism<'a, 'b> =
        ('a -> 'b option) * ('b -> 'a)


    [<RequireQualifiedAccess>]
    module Compose =
        /// Static overloads of the composition function for lenses (>->).
        /// These functions do not generally need to be called directly,
        /// but will be used when calling Compose.optic
        type Lens =
            | Lens
            static member (>->) (Lens, (g2, s2): Lens<'b, 'c>) =
                fun ((g1, s1): Lens<'a, 'b>) ->
                    g1 >> g2,
                    (fun c a -> s1 (s2 c (g1 a)) a): Lens<'a, 'c>

            static member (>->) (Lens, (g2, s2): Prism<'b, 'c>) =
                fun ((g1, s1): Lens<'a, 'b>) ->
                    g1 >> g2,
                    (fun c a -> s1 (s2 c (g1 a)) a): Prism<'a, 'c>

            static member (>->) (Lens, (f, t): Isomorphism<'b, 'c>) =
                fun ((g, s): Lens<'a, 'b>) ->
                    g >> f,
                    (fun c a -> s (t c) a): Lens<'a, 'c>

            static member (>->) (Lens, (f, t): Epimorphism<'b, 'c>) =
                fun ((g, s): Lens<'a, 'b>) ->
                    g >> f,
                    (fun c a -> s (t c) a): Prism<'a, 'c>

        let inline lens l o =
            (Lens >-> o) l
        type Prism =
            | Prism
            static member (>?>) (Prism, (g2, s2): Lens<'b, 'c>) =
                fun ((g1, s1): Prism<'a, 'b>) ->
                    (fun a -> Option.map g2 (g1 a)),
                    (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a | _ -> a): Prism<'a, 'c>

            static member (>?>) (Prism, (g2, s2): Prism<'b, 'c>) =
                fun ((g1, s1): Prism<'a, 'b>) ->
                    (fun a -> Option.bind g2 (g1 a)),
                    (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a
                                                                     | _ -> a): Prism<'a, 'c>

            static member (>?>) (Prism, (f, t): Isomorphism<'b, 'c>) =
                fun ((g, s): Prism<'a, 'b>) ->
                    (fun a -> Option.map f (g a)),
                    (fun c a -> s (t c) a): Prism<'a, 'c>

            static member (>?>) (Prism, (f, t): Epimorphism<'b, 'c>) =
                fun ((g, s): Prism<'a, 'b>) ->
                    (fun a -> Option.bind f (g a)),
                    (fun c a -> s (t c) a): Prism<'a, 'c>

        let inline prism p o =
            (Prism >?> o) p

    /// Functions for using optics to operate on data structures, using the basic optic
    /// operation of get, set and map. The functions are overloaded to take either lenses
    /// or prisms, with the return type being inferred.
    [<RequireQualifiedAccess>]
    module Optic =
        /// Static overloads of the optic get function (^.). These functions do not generally
        /// need to be called directly, but will be used when calling Optic.get.
        type Get =
            | Get
            static member (^.) (Get, (g, _): Lens<'a, 'b>) =
                fun (a: 'a) -> g a: 'b
            static member (^.) (Get, (g, _): Prism<'a, 'b>) =
                fun (a: 'a) -> g a: 'b option

        let inline get optic target =
            (Get ^. optic) target

        type Set =
            | Set
            static member (^=) (Set, (_, s): Lens<'a, 'b>) =
                fun (b: 'b) -> s b: 'a -> 'a

            static member (^=) (Set, (_, s): Prism<'a, 'b>) =
                fun (b: 'b) ->
                    s b: 'a -> 'a

        /// Set a value using an optic.
        let inline set optic value =
            (Set ^= optic) value

        /// Static overloads of the optic map function (%=). These functions do not generally
        /// need to be called directly, but will be used when calling Optic.map.
        type Map =
            | Map
            static member (^%) (Map, (g, s): Lens<'a, 'b>) =
                fun (f: 'b -> 'b) ->
                    (fun a -> s (f (g a)) a): 'a -> 'a

            static member (^%) (Map, (g, s): Prism<'a, 'b>) =
                fun (f: 'b -> 'b) ->
                    (fun a -> Option.map f (g a) |> function | Some b -> s b a
                                                             | _ -> a): 'a -> 'a

        /// Modify a value using an optic.
        let inline map optic f =
            (Map ^% optic) f

    [<RequireQualifiedAccess>]
    module Lens =
        let ofIsomorphism ((f, t): Isomorphism<'a, 'b>): Lens<'a, 'b> =
            f, (fun b _ -> t b)

    [<RequireQualifiedAccess>]
    module Prism =
        let ofEpimorphism ((f, t): Epimorphism<'a, 'b>): Prism<'a, 'b> =
            f, (fun b _ -> t b)

   /// Various optics implemented for common types such as tuples,
    /// lists, and maps, along with an identity lens. (which is useful for composing
    /// a lens which has not specific "lensing" elements but is implicitly a chain)
    /// of one or more isomorphisms. Having an id lens enables the root composition.
    ///
     [<AutoOpen>]
    module Optics =
        /// Identity lens returning the original item regardless of modification
        let id_: Lens<'a, 'a> =
            id, (fun x _ -> x)

        /// Isomorphism between a boxed and unboxed type.
        let box_: Isomorphism<obj, 'a> =
            unbox<'a>, box

        /// Lens to first item of the tuple
        let fst_: Lens<'a * 'b, 'a> =
            fst,
            (fun a t -> a, snd t)

        let snd_: Lens<'a * 'b, 'b> =
            snd,
            (fun b t -> fst t, b)

        [<RequireQualifiedAccess>]
        module Array =
            /// Isomorphism to an list.
            let list_: Isomorphism<'v [], 'v list> =
                Array.toList,
                Array.ofList

        [<RequireQualifiedAccess>]
        module Choice =
            let choice1Of2_: Prism<Choice<_, _>, _> =
                (fun x ->
                    match x with
                    | Choice1Of2 v -> Some v
                    | _ -> None),
                (fun v x ->
                    match x with
                    | Choice1Of2 _ -> Choice1Of2 v
                    | _ -> x)

            /// Prism to Choice2Of2.
            let choice2Of2_: Prism<Choice<_, _>, _> =
                (fun x ->
                    match x with
                    | Choice2Of2 v -> Some v
                    | _ -> None),
                (fun v x ->
                    match x with
                    | Choice2Of2 _ -> Choice2Of2 v
                    | _ -> x)

        [<RequireQualifiedAccess>]
        module Result =
            let ok_: Prism<Result<_, _>, _> =
                (fun x ->
                    match x with
                    | Ok v -> Some v
                    | _ -> None),
                (fun v x ->
                    match x with
                    | Ok _ -> Ok v
                    | _ -> x)

                    /// Prism to Error.
            let error_: Prism<Result<_, _>, _> =
                (fun x ->
                    match x with
                    | Error v -> Some v
                    | _ -> None),
                (fun v x ->
                    match x with
                    | Error _ -> Error v
                    | _ -> x)


        [<RequireQualifiedAccess>]
        module List =
            let head_: Prism<'v list, 'v> =
                (function | h :: _ -> Some h | _ -> None),
                (fun v -> function | _ :: t -> v :: t | l -> l)

            /// Prism to an indexed position in a list.
            let pos_ (i: int): Prism<'v list, 'v> =
                (function | l when List.length l > i -> Some(List.item i l) | _ -> None),
                (fun v l -> List.mapi (fun i' x -> if i = i' then v else x) l)

            /// Prism to the tail of a list.
            let tail_: Prism<'v list, 'v list> =
                (function | _ :: t -> Some t
                          | _ -> None),
                (fun t ->
                    function | h :: _ -> h :: t
                             | [] -> [])

            /// Isomorphism to an array.
            let array_: Isomorphism<'v list, 'v []> =
                List.toArray,
                List.ofArray

        [<RequireQualifiedAccess>]
        module Map =
            let key_ (k: 'k): Prism<Map<'k, 'v>, 'v> =
                Map.tryFind k,
                (fun v x -> if Map.containsKey k x then Map.add k v x else x)

            let value_ (k: 'k): Lens<Map<'k, 'v>, 'v option> =
                Map.tryFind k,
                (fun v x -> match v with | Some v -> Map.add k v x | _ -> Map.remove k x)

            let array_: Isomorphism<Map<'k, 'v>, ('k * 'v) []> =
                Map.toArray,
                Map.ofArray

            let list_: Isomorphism<Map<'k, 'v>, ('k * 'v) list> =
                Map.toList,
                Map.ofList

        [<RequireQualifiedAccess>]
        module Option =
            /// Prism to the value in an Option.
            let value_: Prism<'v option, 'v> =
                id,
                (fun v ->
                    function | Some _ -> Some v
                             | None -> None)


    module Operators =
        /// Compose a lens with an optic or morphism
        let inline (>->) l o =
            Compose.lens l o

        /// Compose a prism with an optic or morphism.
        let inline (>?>) p o =
            Compose.prism p o

        /// Get a value using an optic.
        let inline (^.) target optic =
            Optic.get optic target

        /// Set a value using an optic.
        let inline (^=) value optic =
            Optic.set optic value

        /// Modify a value using an optic.
        let inline (^%) f optic =
            Optic.map optic f
