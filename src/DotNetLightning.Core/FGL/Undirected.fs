namespace DotNetLightning.FGL.Undirected

open DotNetLightning.Utils.Aether
open DotNetLightning.FGL


module Vertices =
    (* Add and remove *)
    let add ((v, l) : LVertex<'V, 'L>) (g: Graph<'V, 'L, 'E>): Graph<'V, 'L, 'E> =
        Map.add v (Map.empty, l, Map.empty) g

    let addMany (vertices:LVertex<'V, 'L> list) (g: Graph<'V, 'L, 'E>) : Graph<'V, 'L, 'E> =
        List.fold ( fun g vertex -> add vertex g) g vertices

    /// Removes a vertex from a graph.
    let remove (v: 'V) (g: Graph<'V, 'L, 'E>): Graph<'V, 'L, 'E> =
        Graph.decompose v g |> snd

    let removeMany nList (g: Graph<'V, 'L, 'E>) : Graph<'V, 'L, 'E> =
        List.fold (fun g' v -> remove v g') g nList

    let neighbours (context: Context<'V, 'L, 'E>): 'V list =
        context |> fun (_, _, _, s) -> List.map fst s

    let degree (context: Context<'V, 'L, 'E>): int =
        context |> fun(p,_,_,s) -> List.length p + List.length s

    /// Evaluates the clustering coefficient of the vertex.
    let clusteringCoefficient (context: Context<'V, 'L, 'E>) (g: Graph<'V, 'L, 'E>) : float =
        context
        |> fun c ->
            if degree c < 2 then 0.
            else
                let add1IfInList acc x set =
                    if List.contains x set then acc + 1
                    else acc
                let neighboursNow = neighbours c
                let neighbourEdges =
                    List.fold(fun edgeAmount v' ->
                        Graph.getContext v' g
                        |> fun (p',_,_,_) ->
                            (p' |> List.fold(fun acc (x,_) -> add1IfInList acc x neighboursNow) 0) + edgeAmount
                        ) 0 neighboursNow
                let degree = List.length neighboursNow
                ((float neighbourEdges) / (float (degree * (degree - 1)))) / 2.


    let count (g: Graph<'V, 'L, 'E>): int = g.Count

    let tovertexList (g: Graph<'V, 'L, 'E>) : LVertex<'V, 'L> list =
        Map.toList g |> List.map(fun (v, (_,l,_)) -> v, l)

    /// Returns true, if vertex v is contained in the graph. Otherwise, it returns false.
    let contains v (g:Graph<'V, 'L, 'E>) : bool = Map.containsKey v g

    /// Lookup a labeled vertex in the graph. Raising KeyNotFoundException if no binding exists in the graph.
    let find (v: 'V) (g: Graph<'V, 'L, 'E>) : LVertex<'V, 'L> option =
        Map.tryFind v g |> Option.map (fun (_,l,_) -> v, l)

    /// Lookup a labeled vertex in the graph, returning a Some value if a binding exists an None if not.
    let tryFind (v: 'V) (g: Graph<'V, 'L, 'E>): LVertex<'V, 'L> option =
        Map.tryFind v g |> Option.map (fun (_,l,_) -> v, l)


    // ---- Iterative -----
    /// Maps the vertexlabels of the graph.
    let map (f: 'V -> 'L -> 'RLabel) (g: Graph<'V, 'L, 'E>): Graph<'V, 'RLabel, 'E> =
        g |> Map.map(fun vertex (p,l,s) -> p, f vertex l, s)

    /// Maps the vertexlabels of the raph. The mapping function also receives an ascending integer index.
    let mapi (f: (int -> 'V -> 'L -> 'RLabel)) (g: Graph<'V, 'L, 'E>) : Graph<'V, 'RLabel, 'E> =
        g
        |> Map.toArray
        |> Array.mapi(fun i (v, c) -> v, (i, c))
        |> Map.ofArray
        |> Map.map (fun vertex (i, (p,l,s)) -> p, f i vertex l , s)

    let iter (action: 'V -> 'L -> unit) (g: Graph<'V, 'L, 'E>): unit =
        g |> Graph.iterContexts (fun (_,v,l,_) -> action v l)

    let iteri (action: int -> 'Vertex -> 'Label -> unit) (g: Graph<'Vertex,'Label,'Edge>) : unit =
        let mutable i = 0
        g
        |> Map.iter (fun vertex (_, l, _) ->
            action i vertex l
            i <- i + 1)

    let fold (state: 'T) (folder: 'T -> 'Vertex -> 'Label -> 'T) (g: Graph<'Vertex,'Label,'Edge>) : 'T =
        g
        |> Graph.foldContexts state (fun acc (_, v, l, _) -> folder acc v l)


module Edges =
    let add ((v1, v2, edge): LEdge<'V, 'E>) (g: Graph<'V, 'L, 'E>) =
        let g1 =
            let composedPrism = Compose.prism (Map.key_ v1) (Lenses.msucc_)
            let adjListMapping = Map.add v2 edge
            (Optic.map composedPrism adjListMapping) g
        let composedPrism = Compose.prism (Map.key_ v2) Lenses.msucc_
        let adjListMapping = Map.add v1 edge
        (Optic.map composedPrism adjListMapping) g1

    ///Adds a list of labeled, undirected edges to the graph.
    let addMany (edges : list<LEdge<'Vertex,'Edge>>) (g: Graph<'Vertex,'Label,'Edge>) =
        List.fold (fun g e -> add e g) g edges

    ///Removes an edge from the graph.
    let remove ((v1, v2): Edge<'Vertex>) (g: Graph<'Vertex,'Label,'Edge>) =
        let g1 =
            let composedPrism = Compose.prism (Map.key_ v1) Lenses.msucc_
            let adjListMapping = Map.remove v2
            (Optic.map composedPrism adjListMapping) g
        let composedPrism = Compose.prism (Map.key_ v2) Lenses.msucc_
        let adjListMapping = Map.remove v1
        (Optic.map composedPrism adjListMapping) g1

    let removeMany (e: Edge<'V> list) (g: Graph<'V, 'L, 'E>) =
        List.fold (fun g e -> remove e g) g e

    (* Properties *)
    let count (g: Graph<'V, 'L, 'E>): int =
        Map.toArray g
        |> Array.fold (fun c (_, (_,_,s)) -> c + ((Map.toList s) |> List.length)) 0

    (* General *)
    let contains v1 v2 (g: Graph<'V, 'L, 'E>) : bool =
        Map.tryFind v1 g
        |> Option.bind(fun (_, _, s) -> Map.tryFind v2 s)
        |> Option.isSome

    let find (v1: 'V) (v2: 'V) (g: Graph<'V, 'L, 'E>) : LEdge<'V, 'E> =
        Map.find v1 g
        |> fun (_,_,s) -> Map.find v2 s
        |> fun e -> v1, v2, e

    ///Lookup a labeled edge in the graph, returning a Some value if a binding exists and None if not.
    let tryFind (v1:'Vertex) (v2:'Vertex) (g: Graph<'Vertex,'Label,'Edge>) : LEdge<'Vertex,'Edge> option =
            Map.tryFind v1 g
            |> Option.bind (fun (_, _, s) -> Map.tryFind v2 s)
            |> Option.map (fun e -> (v1,v2,e))

    (* Iterative *)

    ///Maps edgelabels of the graph.
    let map (mapping: 'V -> 'V -> 'E -> 'REdge) (g:Graph<'V,'L,'E>) : Graph<'V,'L,'REdge>=
            g
            |> Map.map (fun vertex (p, l, s) ->
                Map.map (fun pvertex edge -> mapping pvertex vertex edge) p,
                l,
                Map.map (fun svertex edge -> mapping vertex svertex edge) s)

    ///Performs a given function on every edge of the graph.
    let iter (action: 'V -> 'V -> 'E -> unit) (graph:Graph<'V,'L,'E>) : unit =
        let rec recurse g =
            match Graph.decomposeFirst g with
            | (Some c,g') ->
                c
                |> fun (_,v,_,s) ->
                    List.iter (fun (v',e) -> action v v' e) s
                    recurse g'
            | (None,_) -> ()
        recurse graph

    ///Performs a given function on every edge of the graph, which also receives an ascending integer index.
    let iteri (action: int -> 'Vertex -> 'Vertex -> 'Edge -> unit) (graph:Graph<'Vertex,'Label,'Edge>) : unit =
        let rec recurse i g =
            match Graph.decomposeFirst g with
            | (Some c,g') ->
                c
                |> fun (_,v,_,s) ->
                    List.iteri (fun j (v',e) -> action (j+i) v v' e) s
                    recurse (i+s.Length) g'
            | (None,_) -> ()
        recurse 0 graph

    let fold (folder : 'State -> 'Vertex -> 'Vertex -> 'Edge -> 'State) (state: 'State) (graph:Graph<'Vertex,'Label,'Edge>) : 'State =
        let rec recurse st g =
            match Graph.decomposeFirst g with
            | (Some c,g') ->
                c
                |> fun (_,v,_,s) ->
                    List.fold (fun state (v',e) -> folder state v v' e) st s
                    |> fun st ->
                        recurse st g'
            | (None,_) -> st
        recurse state graph

module Graph =
    let create vList eList : Graph<'V, 'L, 'E> =
        Graph.empty
        |> Vertices.addMany vList
        |> Edges.addMany eList

    ///Transforms a graph into a adjacency matrix of its edges. If there is no edge between two vertices, the noEdgeValue is used.
    let inline toAdjacencyMatrix (g:Graph<'Vertex,'Label,'Edge>) =
        //Create a hashmap of the vertices
        let hashMap = System.Collections.Generic.Dictionary<'Vertex,int>()
        let n =
            let rec loop g i=
                match Graph.decomposeFirst g with
                | Some (_,v,_,_),g ->
                    hashMap.[v] <- i
                    loop g (i+1)
                | None, _ -> i+1
            loop g 0
        //Create the matrix
        let adj : 'Edge [][] = Array.init n (fun _ ->
            Array.zeroCreate n)
        //Fill the matrix with values by using the hashmap as an index finder
        Edges.iter (fun v1 v2 e -> adj.[hashMap.Item v1].[hashMap.Item v2] <- e) g
        adj

    ///Transfroms a graph into a adjacency matrix, maps every edge using the projection.
    let inline toAdjacencyMatrixBy (projection : 'Edge -> 'REdge) (g:Graph<'Vertex,'Label,'Edge>) =
        //Create a hashmap of the vertices
        let hashMap = System.Collections.Generic.Dictionary<'Vertex,int>()
        let n =
            let rec loop g i=
                match Graph.decomposeFirst g with
                | Some (_,v,_,_),g ->
                    hashMap.[v] <- i
                    loop g (i+1)
                | None, _ -> i+1
            loop g 0
        //Create the matrix
        let adj : 'REdge [][] = Array.init n (fun _ ->
            Array.zeroCreate n)
        //Fill the matrix with values by using the hashmap as an index finder
        Edges.iter (fun v1 v2 e -> adj.[hashMap.Item v1].[hashMap.Item v2] <- projection e) g
        adj
