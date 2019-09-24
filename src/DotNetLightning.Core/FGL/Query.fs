namespace DotNetLightning.FGL

open DotNetLightning.Utils

type LPath<'V, 'Label> = LPath of LVertex<'V, 'Label> list
type LRTree<'V, 'Label> = LPath<'V, 'Label> list
type Path<'Vertex> = 'Vertex list

/// The weight for calculating the distance between edges.
/// It can be anything, but it must satisfy the monoid law.
type Weight = Weight of int with
    member this.Value = let (Weight i) = this in i
    static member Zero = Weight 0
    static member (+) (a: Weight, b: Weight) =
        a.Value + b.Value |> Weight

module Query =
    let private expand (d: Weight) (LPath p) (ctx: Context<_,_,_>) =
       let p: LVertex<_ ,Weight> list = p
       let (_,_,_, outs) = ctx
       outs |> List.map(fun (v, w) -> LPath((v, w + d)::p))
    let private mergeAll<'V when 'V : comparison>(p: LVertex<'V, Weight> list) (h: Heap<LPath<'V, Weight>>) ctx =
        let _, dist = p.Head
        let newPaths = (expand dist (LPath p) ctx)
        let folder = Heap.insert 
        newPaths |> List.fold (fun acc path -> folder path acc) h
        
    let rec dijkstraShortestPath(h: Heap<LPath<_, Weight>>) (g: Graph<_,_,_>): LRTree<_, Weight> =
        match g.IsEmpty with
        | true -> []
        | false ->
            match Heap.tryUncons h with
            | None -> []
            | Some (ledge, h') -> dijkstra ledge h' g
    and dijkstra (LPath p) (h') (g) =
        let (v, _) = p.Head
        match g |> Graph.tryDecompose v with
        | None, g -> dijkstraShortestPath h' g
        | Some(ctx), g' ->
            let r = dijkstraShortestPath (mergeAll p h' ctx) g'
            r
    let private getPath<'V when 'V : comparison>(v: 'V) (t: LRTree<'V, _>): Path<'V> =
        let vList =
            t
                |> List.choose(fun (LPath laveledVList) ->
                    let destinationV = laveledVList |> List.rev |> List.head |> fst
                    if (destinationV = v) then Some laveledVList else None)
                |> List.exactlyOne
        vList |> List.map(fst)
        
    let shortestPathTree<'V when 'V : comparison>(src: 'V) (g: Graph<_,_,_>): LRTree<'V, Weight> =
        let initialH = Heap.empty true |> Heap.insert (LPath([src, Weight.Zero]))
        dijkstraShortestPath (initialH) g
        
    let shortestPath<'V when 'V : comparison>(src: 'V) (dst: 'V) (g: Graph<_,_,_>): Path<'V> =
        getPath dst (shortestPathTree src g)
        
    let yenKShortestPath() =
        failwith "todo"
