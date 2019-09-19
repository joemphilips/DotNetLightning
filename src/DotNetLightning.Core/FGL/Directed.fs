namespace DotNetLightning.Core.FGL

open DotNetLightning.Utils.Aether


module Vertices =
    let add ((v, l): LVertex<'V, 'L>) (g: Graph<'V, 'L, 'E>): Graph<'V, 'L, 'E> =
        Map.add v (Map.empty, l, Map.empty) g

    let addMany (vList: LVertex<'V, 'L> list) (g: Graph<'V, 'L, 'E>) : Graph<'V, 'L, 'E> =
        List.fold(fun g v -> add v g) g vList

    let remove (v: 'V) (g: Graph<'V, 'L, 'E>) : Graph<'V, 'L, 'E> =
        Graph.decompose v g |> snd

    let removeMany nList (g: Graph<'V, 'L, 'E>) : Graph<'V, 'L, 'E> =
        List.fold(fun g' v -> remove v g') g nList

    let predecessors (ctx: Context<'V, 'L, 'E>) : 'V list =
        ctx |> fun (p, _, _, _) -> List.map fst p

    let successors (ctx: Context<'V, 'L, 'E>) : 'V list =
        ctx |> fun (_,_,_,s) -> List.map fst s

    let neighbours (ctx: Context<'V, 'L, 'E>) : 'V list =
        ctx |> fun (p, _, _, s) -> (List.map fst p) @ (List.map fst s) |> List.distinct

    let inwardDegree (ctx: Context<'V, 'L, 'E>) : int =
        ctx |> fun (p, _, _, _) -> List.length p

    let outwardDegree (ctx: Context<'V, 'L, 'E>) : int =
        ctx |> fun (_, _, _, s) -> List.length s

    let degree (ctx: Context<'V, 'L, 'E>) : int =
        ctx |> fun (p, _, _, s) -> List.length p + List.length s

    let clusteringCoefficient (ctx: Context<'V, 'L, 'E>) (g: Graph<'V, 'L, 'E>): float =
        ctx
        |> fun c ->
            if degree c < 2 then 0.
            else
                let add1IfInList acc x set =
                    if List.contains x set then acc + 1
                    else acc
                let neighbours = neighbours c
                let neighbourEdges =
                    List.fold(fun edgeAmount v' ->
                        (Graph.getContext v' g
                        |> fun (p',_,_,_) ->
                            (p' |> List.fold (fun acc (x, _) -> add1IfInList acc x neighbours) 0)
                        ) + edgeAmount
                    ) 0 neighbours
                let degree = List.length neighbours
                ((float neighbourEdges) / (float (degree * (degree - 1)))) / 2.

    /// Get number of vertices
    let count (g: Graph<'V, 'L, 'E>) : int =
        g.Count

    let tovertexList (g: Graph<'V, 'L, 'E>) : LVertex<'V, 'L> list =
        Map.toList g |> List.map(fun (v, (_,l,_)) -> v, l)

    let contains v (g: Graph<'V, 'L, 'E>) : bool =
        Map.containsKey v g

    let find (v: 'V) (g: Graph<'V, 'L, 'E>) : LVertex<'V, 'L> =
        Map.find v g |> fun (_, l, _) -> v, l

    ///Lookup a labeled vertex in the graph, returning a Some value if a binding exists and None if not.
    let tryFind (v: 'Vertex) (g: Graph<'Vertex,'Label,'Edge>) : LVertex<'Vertex,'Label> option =
        Map.tryFind v g
        |> Option.map (fun (_, l, _) -> v, l)

    //Iterative

    ///Maps the vertexlabels of the graph.
    let map (mapping: 'Vertex -> 'Label -> 'RLabel) (g: Graph<'Vertex,'Label,'Edge>) : Graph<'Vertex,'RLabel,'Edge>=
        g
        |> Map.map (fun vertex (p, l, s) ->
            p, mapping vertex l, s)

    ///Maps the vertexlabels of the graph. The mapping function also receives an ascending integer index.
    let mapi (mapping: int -> 'Vertex -> 'Label -> 'RLabel) (g: Graph<'Vertex,'Label,'Edge>) : Graph<'Vertex,'RLabel,'Edge> =
        g
        |> Map.toArray
        |> Array.mapi (fun i (v,c) -> v,(i,c))
        |> Map.ofArray
        |> Map.map (fun vertex (i,(p, l, s)) ->
            p, mapping i vertex l, s)

    ///Performs a given function on every vertex and its label of a graph.
    let iter (action: 'Vertex -> 'Label -> unit) (g: Graph<'Vertex,'Label,'Edge>) : unit=
        g
        |> Graph.iterContexts (fun (_, v, l, _) -> action v l)

    let iteri (action: int -> 'Vertex -> 'Label -> unit) (g: Graph<'Vertex,'Label,'Edge>) : unit =
        let mutable i = 0
        g
        |> Map.iter (fun vertex (_, l, _) ->
            action i vertex l
            i <- i + 1)

    let fold (state: 'T) (folder: 'T -> 'Vertex -> 'Label -> 'T) (g: Graph<'Vertex,'Label,'Edge>) : 'T =
        g
        |> Graph.foldContexts state (fun acc (_, v, l, _) -> folder acc v l)

///Functions for edges of directed Graphs
module Edges =

    //Add and remove

    ///Adds a labeled, directed edge to the graph.
    let add ((v1, v2, edge): LEdge<'Vertex,'Edge>) (g: Graph<'Vertex,'Label,'Edge>) =
        let g1 =
            let composedPrism = Compose.prism (Map.key_ v1) Lenses.msucc_
            let adjListMapping = Map.add v2 edge
            (Optic.map composedPrism adjListMapping) g
        let composedPrism = Compose.prism (Map.key_ v2) Lenses.mpred_
        let adjListMapping = Map.add v1 edge
        (Optic.map composedPrism adjListMapping) g1

    ///Adds a list of labeled, directed edges to the graph.
    let addMany (edges : list<LEdge<'Vertex,'Edge>>) (g: Graph<'Vertex,'Label,'Edge>) =
        List.fold (fun g e -> add e g) g edges

    ///Removes an edge from the graph.
    let remove ((v1, v2): Edge<'Vertex>) (g: Graph<'Vertex,'Label,'Edge>) =
        let g1 =
            let composedPrism = Compose.prism (Map.key_ v1) Lenses.msucc_
            let adjListMapping = Map.remove v2
            (Optic.map composedPrism adjListMapping) g
        let composedPrism = Compose.prism (Map.key_ v2) Lenses.mpred_
        let adjListMapping = Map.remove v1
        (Optic.map composedPrism adjListMapping) g1

    ///Removes a list of edges from the graph.
    let removeMany (edges : list<Edge<'Vertex>>) (g: Graph<'Vertex,'Label,'Edge>) =
        List.fold (fun g e -> remove e g) g edges


    //Properties

    ///Evaluates the number of edges in the graph.
    let count (g: Graph<'Vertex,'Label,'Edge>) : int =
        Map.toArray g
        |> Array.fold (fun c (_,(_,_,s)) -> c + ((Map.toList s) |> List.length)) 0


    //General

    ///Returns true, if the edge from vertex v1 to vertex v2 is contained in the graph. Otherwise, it returns false.
    let contains v1 v2 (g: Graph<'Vertex,'Label,'Edge>) : bool =
        Map.tryFind v1 g
        |> Option.bind (fun (_, _, s) -> Map.tryFind v2 s)
        |> Option.isSome

    ///Lookup a labeled edge in the graph. Raising KeyNotFoundException if no binding exists in the graph.
    let find (v1:'Vertex) (v2:'Vertex) (g: Graph<'Vertex,'Label,'Edge>) : LEdge<'Vertex,'Edge>=
            Map.find v1 g
            |> fun (_, _, s) -> Map.find v2 s
            |> fun e -> (v1,v2,e)

    ///Lookup a labeled edge in the graph, returning a Some value if a binding exists and None if not.
    let tryFind (v1:'Vertex) (v2:'Vertex) (g: Graph<'Vertex,'Label,'Edge>) : LEdge<'Vertex,'Edge> option =
            Map.tryFind v1 g
            |> Option.bind (fun (_, _, s) -> Map.tryFind v2 s)
            |> Option.map (fun e -> (v1,v2,e))
    ///Transforms a directed graph to an undirected graph.
    let undirect (computeEdge: 'Edge -> 'Edge -> 'Edge) (g: Graph<'Vertex,'Label,'Edge>) : Graph<'Vertex,'Label,'Edge> =
        g
        |> Map.map (fun _ (p,l,s) ->
            Map.empty,
            l,
            Map.fold (fun s' v' e ->
                match Map.containsKey v' s' with
                |true ->
                    let lense = Map.key_ v'
                    (Optic.map lense (computeEdge e)) s'
                |false -> Map.add v' e s') s p
            )

    ///Reverses all edges in the graph.
    let rev (g: Graph<'Vertex,'Label,'Edge>) : (Graph<'Vertex,'Label,'Edge>)=
        Map.map (fun _ (p,l,s) -> (s,l,p)) g


    //Iterative

    ///Maps edgelabels of the graph.
    let map (mapping: 'Vertex -> 'Vertex -> 'Edge -> 'REdge) (g:Graph<'Vertex,'Label,'Edge>) : Graph<'Vertex,'Label,'REdge>=
            g
            |> Map.map (fun vertex (p, l, s) ->
                Map.map (fun pvertex edge -> mapping pvertex vertex edge) p,
                l,
                Map.map (fun svertex edge -> mapping vertex svertex edge) s)

    ///Performs a given function on every edge of the graph.
    let iter (action: 'Vertex -> 'Vertex -> 'Edge -> unit) (g:Graph<'Vertex,'Label,'Edge>) : unit =
        g
        |> Map.iter (fun vertex (_, _, s) ->
            Map.iter (fun svertex edge -> action vertex svertex edge) s)

    ///Performs a given function on every edge of the graph, which also receives an ascending integer index.
    let iteri (action: int -> 'Vertex -> 'Vertex -> 'Edge -> unit) (graph:Graph<'Vertex,'Label,'Edge>) : unit =
        let rec recurse i g =
            match Graph.decomposeFirst g with
            | (Some c,g') ->
                c
                |> fun (p,v,_,s) ->
                    let len = s.Length
                    List.iteri (fun j (v',e) -> action (j+i) v v' e) s
                    List.iteri (fun j (v',e) -> action (j+i+len) v' v e) p
                    recurse (i+len+p.Length) g'
            | (None,_) -> ()
        recurse 0 graph

    let fold (folder : 'State -> 'Vertex -> 'Vertex -> 'Edge -> 'State) (state: 'State) (graph:Graph<'Vertex,'Label,'Edge>) : 'State =
        let rec recurse st g =
            match Graph.decomposeFirst g with
            | (Some c,g') ->
                c
                |> fun (p,v,_,s) ->
                    List.fold (fun state (v',e) -> folder state v v' e) st s
                    |> fun st ->
                        List.fold (fun state (v',e) -> folder state v v' e) st p
                    |> fun st ->
                        recurse st g'
            | (None,_) -> st
        recurse state graph

module Graph =

    ///Creates a directed graph from a list of vertices and a list of edges
    let create vertices edges : Graph<'Vertex,'Label,'Edge> =
        Graph.empty
        |> Vertices.addMany vertices
        |> Edges.addMany edges

    ///Transforms a graph into an adjacency matrix of its edges.
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

module Models =
    /// Creates a directed graph ov vertexcount n and edge probability p
    let gilbert (nodeInitializer: int -> LVertex<'V, 'L>) (n: int) (p: float) : Graph<'V, 'L, int> =
        Models.gilbert Graph.empty nodeInitializer (Vertices.add) (fun (v1, _) (v2, _) g -> Edges.add (v1, v2, 1) g) n p

    let erdosRenyi (nodeInitializer : int -> LVertex<'V, 'L>) (v: int) (e: int) : Graph<'V, 'L, int> =
        Models.erdosRenyi Graph.empty nodeInitializer Vertices.add (fun (v1, _) (v2, _) g -> Edges.add (v1, v2, 1) g) v e
