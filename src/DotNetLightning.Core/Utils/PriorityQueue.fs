namespace DotNetLightning.Utils

open System.Collections
open System.Collections.Generic

/// Based on FSharpx's PriorityQueue
/// refs: https://github.com/fsprojects/FSharpx.Collections/blob/master/src/FSharpx.Collections/PriorityQueue.fs
type IPriorityQueue<'T when 'T : comparison> =
    inherit IEnumerable
    inherit IEnumerable<'T>

    abstract member IsEmpty : bool with get
    abstract member Insert : 'T -> IPriorityQueue<'T>

    abstract member TryPeek : unit -> 'T option
    abstract member Peek: unit -> 'T
    abstract member Length: int
    abstract member TryPop : unit -> ('T * IPriorityQueue<'T>) option
    abstract member Pop : unit -> 'T * IPriorityQueue<'T>

type Heap<'T when 'T : comparison>(isDescending: bool, length: int, data: HeapData<'T>) =
    let mutable hashCode = None
    member internal this.HeapData = data
    member internal this.HeapLength = length

    member this.Length = length

    override this.GetHashCode() =
        match hashCode with
        | None ->
            let mutable hash = 1
            for x in this do
                hash <- 31 * hash * Unchecked.hash x
            hashCode <- Some hash
            hash
        | Some hash -> hash

    override this.Equals(other) =
        match other with
        | :? Heap<'T> as y ->
            if this.Length <> y.Length then false
            else
                if this.GetHashCode() <> y.GetHashCode() then false
                else Seq.forall2 (Unchecked.equals) this y
        | _ -> false

    static member private merge isDescending newLength (h1: HeapData<'T>) (h2: HeapData<'T>): Heap<'T> =
        match h1, h2 with
        | E, h -> Heap(isDescending, newLength, h)
        | h, E -> Heap(isDescending, newLength, h)
        | T(x, xs), T(y, ys) ->
            if isDescending then
                if x <= y then Heap(isDescending, newLength, T(y, h1 :: ys)) else Heap(isDescending, newLength, T(x, h2::xs))
            else
                if x <= y then Heap(isDescending, newLength, T(x, h2 :: xs)) else Heap(isDescending, newLength, T(y, h1::ys))

    static member private foldHeap nodeF leafV (h: HeapData<'T> list) =
        let rec loop (h: HeapData<'T> list) cont =
            match h with
            | T(a, h')::tl -> loop h' (fun lacc ->
                                   loop tl (fun racc -> cont (nodeF a lacc racc)))
            | _ -> cont leafV
        loop h (id)


    static member private inOrder (h: HeapData<'T> list) = (Heap.foldHeap (fun x l r acc -> l (x:: (r acc))) id h) []
    static member ofSeq (isDescending: bool) (s: 'T seq): Heap<'T> =
        if Seq.isEmpty s then Heap(isDescending, 0, E)
        else
            let len, h' =
                Seq.fold(fun (lnth, (h: 'T HeapData)) x ->
                    match h with
                    | E -> 1, T (x, [])
                    | T (y, ys) ->
                        if isDescending then
                            if x <= y then (lnth + 1), T(y, T(x, []):: ys) else (lnth + 1), T(x, T(y, ys)::[])
                        else
                            if x <= y then (lnth + 1), T(x, T(y, ys)::[]) else (lnth + 1), T(y, T(x, [])::ys))
                    (0, E) s
            Heap(isDescending, len, h')
    member this.Head() =
        match data with
        | E -> failwith "Heap is empty"
        | T(x, _) -> x
    member this.TryHead() =
        match data with
        | E -> None
        | T(x, _) -> Some (x)
    member this.Insert x =
        Heap.merge(isDescending) (length + 1) (T(x, [])) data

    member this.IsEmpty =
        match data with
        | E -> true
        | _ -> false

    member this.IsDescending = isDescending
    member this.Merge (xs: Heap<'T>) =
        if isDescending = xs.IsDescending then Heap.merge isDescending (length + xs.HeapLength) data xs.HeapData
        else failwith "heaps to merge have different sort orders"
    member this.TryMerge (xs: Heap<'T>) =
        if isDescending = xs.IsDescending then Heap.merge isDescending (length + xs.HeapLength) data xs.HeapData |> Some
        else None

    member this.Rev() =
        if isDescending then Heap<'T>.ofSeq false (this :> seq<'T>)
        else Heap<'T>.ofSeq true (this :> 'T seq)

    member this.Tail() =
        let mergeData (h1: HeapData<'T>) (h2: HeapData<'T>) : HeapData<'T> =
            match h1, h2 with
            | E, h -> h
            | h, E -> h
            | T(x, xs), T(y, ys) ->
                if isDescending then
                    if x <= y then T(y, h1::ys) else T(x, h2::xs)
                else
                    if x <= y then T(x, h2::xs) else T(y, h1::ys)
        match data with
        | E -> failwith "Heap is empty"
        | T (_x, xs) ->
            let combinePairs state item =
                match state with
                | Some p, l ->
                    (None, (mergeData item p)::l)
                | None, l ->
                    (Some item, l)

            let tail =
                xs
                |> List.fold combinePairs (None, [])
                |> function
                    | Some i, l -> i::l
                    | None, l -> l
                |> List.fold mergeData E
            Heap(isDescending, (length - 1), tail)
    member this.TryTail() =
        match data with
        | E -> None
        | _ -> Some(this.Tail())

    member this.Uncons() =
        match data with
        | E -> failwith "Heap is empty"
        | T(x, _xs) -> x, this.Tail()

    member this.TryUncons() =
        match data with
        | E -> None
        | T(x, _xs) -> Some(x, this.Tail())


    interface IEnumerable<'T> with
        member this.GetEnumerator() =
            let e =
                let listH = data :: []
                if isDescending
                then Heap.inOrder listH |> List.sort |> List.rev |> List.toSeq
                else Heap.inOrder listH |> List.sort |> List.toSeq
            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> IEnumerator

    interface IPriorityQueue<'T> with
        member this.Insert(element: 'T): IPriorityQueue<'T> = 
            this.Insert element :> IPriorityQueue<_>
        member this.IsEmpty: bool = this.IsEmpty
        member this.Length: int =  this.Length
        member this.Peek(): 'T =  this.Head()
        member this.TryPeek(): 'T option = this.TryHead()
        member this.Pop(): 'T * IPriorityQueue<'T> = 
            let elem, newHeap = this.Uncons()
            elem, (newHeap :> IPriorityQueue<_>)
        member this.TryPop(): ('T * IPriorityQueue<'T>) option = 
            match this.TryUncons() with
            | Some (elem, newHeap) -> Some(elem, newHeap :> IPriorityQueue<_>)
            | None -> None

and HeapData<'T when 'T : comparison> =
    | E
    | T of 'T * HeapData<'T> list

[<RequireQualifiedAccess>]
module Heap =   
    //pattern discriminator

    let (|Cons|Nil|) (h: Heap<'T>) = match h.TryUncons() with Some(a,b) -> Cons(a,b) | None -> Nil
  
    ///O(1). Returns a empty heap.
    let empty<'T when 'T : comparison> (isDescending: bool) = Heap<'T>(isDescending, 0, E)

    ///O(1) worst case. Returns the min or max element.
    let inline head (xs: Heap<'T>)  = xs.Head

    ///O(1) worst case. Returns option first min or max element.
    let inline tryHead (xs: Heap<'T>)  = xs.TryHead

    ///O(log n) amortized time. Returns a new heap with the element inserted.
    let inline insert x (xs: Heap<'T>) = xs.Insert x   

    ///O(1). Returns true if the heap has no elements.
    let inline isEmpty (xs: Heap<'T>) = xs.IsEmpty

    ///O(1). Returns true if the heap has max element at head.
    let inline isDescending (xs: Heap<'T>) = xs.IsDescending

    ///O(n). Returns the count of elememts.
    let inline length (xs: Heap<'T>) = xs.Length 

    ///O(log n) amortized time. Returns heap from merging two heaps, both must have same descending.
    let inline merge (xs: Heap<'T>) (ys: Heap<'T>) = xs.Merge ys

    ///O(log n) amortized time. Returns heap option from merging two heaps.
    let inline tryMerge (xs: Heap<'T>) (ys: Heap<'T>) = xs.TryMerge ys

    ///O(n log n). Returns heap, bool isDescending, from the sequence.
    let ofSeq isDescending s  = Heap<'T>.ofSeq isDescending s 
    
    ///O(n). Returns heap reversed.
    let inline rev (xs: Heap<'T>) = xs.Rev()

    ///O(log n) amortized time. Returns a new heap of the elements trailing the head.
    let inline tail (xs: Heap<'T>) = xs.Tail()

    ///O(log n) amortized time. Returns option heap of the elements trailing the head.
    let inline tryTail (xs: Heap<'T>) = xs.TryTail()

    ///O(n). Views the given heap as a sequence.
    let inline toSeq (xs: Heap<'T>) = xs :> seq<'T>

    ///O(log n) amortized time. Returns the head element and tail.
    let inline uncons (xs: Heap<'T>) = xs.Uncons()

    ///O(log n) amortized time. Returns option head element and tail.
    let inline tryUncons (xs: Heap<'T>) = xs.TryUncons()

[<RequireQualifiedAccess>]
module PriorityQueue =
    ///O(1). Returns a empty queue, with indicated ordering.
    let empty<'T when 'T : comparison> isDescending = Heap.empty isDescending :> IPriorityQueue<'T>

    ///O(1). Returns true if the queue has no elements.
    let inline isEmpty (pq:IPriorityQueue<'T>) = pq.IsEmpty

    ///O(log n) amortized time. Returns a new queue with the element added to the end.
    let inline insert element (pq:IPriorityQueue<'T>) = pq.Insert element

    ///O(1). Returns option first element.
    let inline tryPeek (pq:IPriorityQueue<'T>) = pq.TryPeek()

    ///O(1). Returns the first element.
    let inline peek (pq:IPriorityQueue<'T>) = pq.Peek()

    ///O(log n) amortized time. Returns the option first element and tail.
    let inline tryPop (pq:IPriorityQueue<'T>) = pq.TryPop()

    ///O(log n) amortized time. Returns the first element and tail.
    let inline pop (pq:IPriorityQueue<'T>) = pq.Pop()
