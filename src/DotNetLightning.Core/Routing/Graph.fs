namespace DotNetLightning.Routing

open System
open System.Collections.Generic
open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open DotNetLightning.FGL
open DotNetLightning.FGL.Directed
open Graph
open NBitcoin

// Graph algorythms are based on eclair

module Graph =
    /// The cumulative weight of a set of edges (path in the graph).
    [<CustomComparison;StructuralEquality>]
    type RichWeight = {
        Weight: double
        Cost: LNMoney
        Length: int
        CLTV: BlockHeightOffset
    }
    with
        member this.CompareTo(other: RichWeight) =
            this.Weight.CompareTo(other.Weight)
            
        interface IComparable with
            member this.CompareTo(other:obj) =
                match other with
                | :? RichWeight as o -> this.CompareTo(o)
                | _ -> -1
                

    /// We use heuristics to calculate the weight of an edge based on channel age, cltv delta and capacity.
    /// We favor older channels, with bigger capacity and small cltv delta
    type WeightRatios = private {
        CLTVDeltaFactor: double
        AgeFactor: double
        CapacityFactor: double
    }
    with
        static member TryCreate(cltvDeltaFactor, ageFactor, capacityFactor) =
            let s = cltvDeltaFactor + ageFactor + capacityFactor
            if (s <= 0.|| 1. < s) then
                sprintf "sum of CLTVDeltaFactor + ageFactor + capacityFactor must in between 0 to 1. it was %f" s
                |> Error
            else
                {
                    CLTVDeltaFactor = cltvDeltaFactor
                    AgeFactor = ageFactor
                    CapacityFactor = capacityFactor
                } |> Ok
                
    [<CustomComparison;StructuralEquality>]           
    type WeightedNode = {
        Id: NodeId
        Weight: RichWeight
    }
        with
        member x.CompareTo(y: WeightedNode) =
            let weightCmp = x.Weight.CompareTo(y.Weight)
            if (weightCmp <> 0) then weightCmp else
            x.Id.Value.ToHex().CompareTo(y.Id.Value.ToHex())
        interface IComparable with
            member this.CompareTo(o: obj) =
                match o with
                | :? WeightedNode as wn -> this.CompareTo(wn)
                | _ -> -1
    
    type ChannelDesc = {
        ShortChannelId: ShortChannelId
        A: NodeId
        B: NodeId
    }
    
    type PublicChannel = private {
        Announcement:  ChannelAnnouncement
        FundingTxId: TxId
        Capacity: Money
        Update1Opt: ChannelUpdate option
        Update2Opt: ChannelUpdate option
    }
        with
        static member TryCreate (a) fundingTxId cap u1 u2 =
            {
                Announcement = a
                FundingTxId = fundingTxId
                Capacity = cap
                Update1Opt = u1
                Update2Opt = u2
            }
    type GraphLabel = { Desc: ChannelDesc; Update: ChannelUpdate }
        with
        member this.ToGraphEdge() =
            { Prev = this.Desc.A; Succ = this.Desc.B; Label = this }
        member this.AsTuple =
            this.ToGraphEdge().AsTuple
    and GraphEdge = { Prev: NodeId; Succ: NodeId; Label: GraphLabel } 
        with
        member this.AsTuple = (this.Prev, this.Succ, this.Label)
        
    type GraphVertex = LVertex<NodeId, unit>
    
    module GraphEdge =
        let hasZeroFee (l: GraphLabel) =
            let u = l.Update
            u.Contents.FeeBaseMSat = LNMoney.Zero && u.Contents.FeeProportionalMillionths = 0u
                
    
    [<CustomComparison;StructuralEquality>]
    type WeightedPath = {
        Path: GraphLabel seq
        Weight: RichWeight
    }
        with
        member x.CompareTo(y: WeightedPath)=
            x.Weight.CompareTo(y.Weight)
            
        interface IComparable with
            member this.CompareTo o =
                match o with
                | :? WeightedPath as x -> this.CompareTo(x)
                | _ -> -1
            
    
    /// The actual graph in Inductive Graph format
    /// refs: https://pdfs.semanticscholar.org/609d/73290697d27f40ff50fc045fd2e8d0034c9d.pdf
    /// This is unnecessary abstraction, ideally we should just use Graph type in FGL directly.
    /// It is here because we wanted to imitate the behavior of `DirectedGraph` type in eclair
    /// which uses (more simpler) adjacent list for storing graph object
    type DirectedLNGraph = private DirectedLNGraph of Graph<NodeId, unit, GraphLabel>
        with
        static member Create(vertices: NodeId list, edges: GraphLabel list) =
            Graph.create
                (vertices |> List.map(fun v -> v, ()))
                (edges |> List.map(fun e -> e.AsTuple))
            |> DirectedLNGraph
            
        static member Create(edges: GraphLabel list) =
            let vertices = edges |> List.collect(fun ({ Desc = d }) -> [d.A; d.B])
            DirectedLNGraph.Create(vertices, edges)
            
        static member Create() =
            Graph.empty |> DirectedLNGraph
            
        member this.Value = let (DirectedLNGraph v) = this in  v
        member this.ContainsEdge(a: NodeId, b: NodeId) =
            match this.Value |> Edges.tryFind a b with
            | None -> false
            | Some (_,_,_) -> true
        member this.ContainsEdge(desc: ChannelDesc) =
            this.ContainsEdge(desc.A, desc.B)
            
        member this.VertexSet() = this.Value |> Vertices.tovertexList
        member this.EdgeCount() = this.Value |> Edges.count
        
        member this.OutgoingEdgesOf(v: NodeId) =
            match Graph.tryGetContext v this.Value with
            | None -> List.empty
            | Some (_p, _v, _l, s) -> s |> List.map snd
            
        member this.IncomingEdgesOf(v: NodeId) =
            match Graph.tryGetContext v this.Value with
            | None -> List.empty
            | Some(p, _v, _l, _s) -> p |> List.map snd
            
        member this.RemoveEdge(desc: ChannelDesc): DirectedLNGraph =
            match this.Value |> Edges.tryFind desc.A desc.B with
            | None -> this
            | _ ->
                this.Value |> Edges.remove (desc.A, desc.B) |> DirectedLNGraph
        member this.RemoveEdges(descriptions: #seq<ChannelDesc>) =
            descriptions |> Seq.fold(fun (acc: DirectedLNGraph) d -> acc.RemoveEdge(d)) this
            
        member this.TryGetEdge(desc: ChannelDesc) =
            this.Value |> Edges.tryFind(desc.A) (desc.B)
            
        member this.TryGetEdge((_, _, e)) =
            this.TryGetEdge(e.Desc)
            
        member this.GetEdgesBetween(keyA: NodeId, keyB: NodeId) =
            match this.Value |> Graph.tryGetContext keyB with
            | None -> List.empty
            | Some ctx ->
                let (predecessor, _v, _l, _successor) = ctx
                predecessor |> List.choose(fun (_v, e) -> if e.Desc.A = keyA then Some (e) else None)
                
        member this.ContainsVertex v =
            this.Value |> Vertices.contains v
        member this.GetIncomingEdgesOf(keyB: NodeId) =
            match this.Value |> Graph.tryGetContext keyB with
            | None -> List.empty
            | Some (p, _v, _l, _s) ->
                p |> List.map snd
                
        member this.GetOutGoingEdgesOf (keyA: NodeId) =
            match this.Value |> Graph.tryGetContext keyA with
            | None -> List.empty
            | Some (_p, _v, _l, s) -> s |> List.map snd
            
              
        member this.RemoveVertex(key: NodeId) =
            this.GetIncomingEdgesOf key
            |> Seq.map(fun (e) -> e.Desc)
            |> this.RemoveEdges
            |> fun (DirectedLNGraph g) -> g |> Vertices.remove(key)
            |> DirectedLNGraph
            
        member this.AddVertex(key: NodeId) =
            this.Value |> Vertices.add (key, ()) |> DirectedLNGraph
            
        member this.AddEdge(l) =
            let vertIn = l.Desc.A
            let vertOut = l.Desc.B
            this.Value
            |> (if (this.ContainsVertex vertIn) then id else Vertices.add (vertIn, ()))
            |> (if (this.ContainsVertex vertOut) then id else Vertices.add (vertOut, ()))
            |> Edges.add (vertIn, vertOut, l)
            |> DirectedLNGraph
                
        member this.AddEdge({ Label = l }) =
            this.AddEdge(l)
        member this.AddEdge(p, s, l) =
            this.AddEdge({Prev = p; Succ = s; Label = l})
            
        member this.AddEdges(edges: seq<ChannelDesc * ChannelUpdate>) =
            edges
            |> Seq.fold
                    (fun (acc: DirectedLNGraph) (desc, update) ->
                        acc.AddEdge({ Desc = desc; Update = update }))
                    this
                
    module private RoutingHeuristics =
        let BLOCK_TIME_TWO_MONTHS = 8640us |> BlockHeightOffset
        let CAPACITY_CHANNEL_LOW = LNMoney.Satoshis(1000L)
        let CAPACITY_CHANNEL_HIGH = DotNetLightning.Channel.ChannelConstants.MAX_FUNDING_SATOSHIS.Satoshi |> LNMoney.Satoshis
        
        [<Literal>]
        let CLTV_LOW = 9L
        [<Literal>]
        let CLTV_HIGH = 2016
        
        let normalize(v, min, max): double =
            if (v <= min) then 0.00001 else
            if (v > max) then 0.99999 else
            (v - min) / (max - min)
        
    let private nodeFee(baseFee: LNMoney, proportionalFee: int64, paymentAmount: LNMoney) =
        baseFee.MilliSatoshi + ((paymentAmount.MilliSatoshi * proportionalFee) / 1000000L) |> LNMoney
        
    /// This forces channel_update(s) with fees = 0 to have a minimum of 1msat for the baseFee. Note that
    /// the update is not being modified and the result of the route computation will still have the update
    /// with fees=0 which is what will be used to build the onion.
    let private edgeFeeCost (edge: GraphLabel, amountWithFees: LNMoney) =
        if (GraphEdge.hasZeroFee(edge)) then amountWithFees + nodeFee(LNMoney.One, 0L, amountWithFees) else
        let ({Update = update}) = edge
        amountWithFees + nodeFee(update.Contents.FeeBaseMSat, (int64 update.Contents.FeeProportionalMillionths), amountWithFees)
        
    /// Computes the compound weight for the given @param edge, the weight is cumulative
    let private edgeWeight
        (edge: GraphLabel)
        (prev: RichWeight)
        (isNeighborTarget: bool)
        (currentBlockHeight: BlockHeight)
        (weightRatios: WeightRatios option): RichWeight =
        match weightRatios with
        | None ->
            let edgeCost = if (isNeighborTarget) then prev.Cost else edgeFeeCost(edge, prev.Cost)
            { RichWeight.Cost = edgeCost; Length = prev.Length + 1; CLTV = prev.CLTV; Weight = edgeCost.MilliSatoshi |> double }
        | Some wr ->
            let ({ Update = update; Desc = desc }) = edge
            let channelBlockHeight = desc.ShortChannelId.BlockHeight.Value
            // every edge is weighted by funding block height where older blocks add less weight,
            let ageFactor =
                RoutingHeuristics.normalize(channelBlockHeight |> double,
                                            (currentBlockHeight - RoutingHeuristics.BLOCK_TIME_TWO_MONTHS).Value |> double,
                                            currentBlockHeight.Value |> double)
            // Every edge is weighted by channel capacity, larger channels and less weight
            let edgeMaxCapacity =
                update.Contents.HTLCMaximumMSat |> Option.defaultValue (RoutingHeuristics.CAPACITY_CHANNEL_LOW)
            let capFactor =
                1. - RoutingHeuristics.normalize(edgeMaxCapacity.MilliSatoshi |> double,
                                                 RoutingHeuristics.CAPACITY_CHANNEL_LOW.MilliSatoshi |> double,
                                                 RoutingHeuristics.CAPACITY_CHANNEL_HIGH.MilliSatoshi |> double)
            // Every edge is weighted by cltv-delta value, normalized.
            let channelCLTVDelta = update.Contents.CLTVExpiryDelta
            let cltvFactor =
                RoutingHeuristics.normalize(channelCLTVDelta.Value |> double,
                                            RoutingHeuristics.CLTV_LOW |> double,
                                            RoutingHeuristics.CLTV_HIGH |> double)
            let edgeCost = if (isNeighborTarget) then prev.Cost else edgeFeeCost(edge, prev.Cost)
            
            let factor = (cltvFactor * wr.CLTVDeltaFactor) + (ageFactor * wr.AgeFactor) + (capFactor * wr.CapacityFactor)
            let edgeWeight =
                if (isNeighborTarget) then prev.Weight else
                prev.Weight + prev.Weight + (edgeCost.MilliSatoshi |> double) + factor
            { RichWeight.Cost = edgeCost; Length = prev.Length + 1; Weight = edgeWeight; CLTV = prev.CLTV + channelCLTVDelta }
        
    /// Calculates the total cost of a path (amount + fees),
    /// direct channels with the source will have a cost of 0 (pay no fees)
    let pathWeight
        (path: GraphLabel seq)
        (amount: LNMoney)
        (isPartial: bool)
        (currentBlockHeight: BlockHeight)
        (wr: WeightRatios option) =
        path
        |> Seq.skip(if isPartial then 0 else 1)
        |> Seq.fold
            (fun (acc: RichWeight) (edge: GraphLabel) ->
                edgeWeight(edge) (acc) (false) (currentBlockHeight) (wr)
            )
            { RichWeight.Cost = amount
              Weight = 0.
              Length = 0
              CLTV = BlockHeightOffset.Zero }
            
    open System.Linq
    /// Finds the shortest path in the graph, uses a modified version of Dijkstra's algorithm that computes
    /// the shortest path from the target to the source (this is because we )
    ///
    /// <param name="g"> The graph on which will be performed the search </param>
    /// <param name="sourceNode"> The starting node of the path we're looking for </param>
    /// <param name="targetNode"> The destination node of the path </param>
    /// <param name="ignoredEdges"> a list of edges we do not want to consider </param>
    /// <param name="extraEdges"> a list of extra edges we want to consider but are not currently in the graph </param>
    /// <param name="wr"> an object containing the ratios used to 'weight' edges when searching for the shortest path </param>
    /// <param name="currentBlockHeight"> the height of the chain tip (latest block) </param>
    /// <param name="boundaries"> a predicate function that can used to impose limits </param>
    let dijkstraShortestPath
        (g: DirectedLNGraph)
        (sourceNode: NodeId)
        (targetNode: NodeId)
        (ignoredEdges: Set<ChannelDesc>)
        (ignoredVertices: Set<NodeId>)
        (extraEdges: Set<GraphLabel>)
        (initialWeight: RichWeight)
        (boundaries: RichWeight -> bool)
        (currentBlockHeight: BlockHeight)
        (wr: WeightRatios option) : GraphLabel seq =
        // The graph does not contain source/destination nodes
        if (not <| g.ContainsVertex sourceNode) then Seq.empty else
        if (not <| g.ContainsVertex targetNode && (not <| extraEdges.IsEmpty) && not <| extraEdges.Any(fun x -> x.Desc.B = targetNode)) then Seq.empty else
            
        let maxMapSize = 100 // conservative estimation to avoid over allocating memory
        let weight = Dictionary<NodeId, RichWeight>(maxMapSize)
        let prev = Dictionary<NodeId, GraphLabel>(maxMapSize)
        // TODO: mutable and while loop is ugly. Refactor.
        let mutable vertexQueue =
            // Ideally, we should use Fibonacci heap for the sake of performance,
            // but to make things easy, we just use regular heap.
            // isDescending is false, which means root of the heap is the smallest value is the smallest value.
            Heap.empty<WeightedNode>(false)
            |> PriorityQueue.insert({ WeightedNode.Id = targetNode; Weight = initialWeight })
        
        weight.Add(targetNode, initialWeight)
        let mutable targetFound = false
        while (not <| vertexQueue.IsEmpty && not <| targetFound) do
            let current, vq = vertexQueue.Pop()
            vertexQueue <- vq
            if (current.Id = sourceNode) then
                targetFound <- true
            else
                // build the neighbors with optional extra edges
                let currentNeighbors =
                    if extraEdges.IsEmpty then
                        g.GetIncomingEdgesOf(current.Id) |> List.toSeq
                     else
                         let extraNeighbors =
                             extraEdges
                             |> Seq.filter(fun x -> x.Desc.B = current.Id)
                         // the resulting set must have only one element per shortChannelId
                         let incoming =
                             g.GetIncomingEdgesOf(current.Id)
                             |> Seq.filter(fun (e) -> not <| extraEdges.Any(fun x -> x.Desc.ShortChannelId = e.Desc.ShortChannelId))
                         seq {yield! incoming; yield! extraNeighbors}
                let currentWeight =
                    match weight.TryGetValue current.Id with
                    | true, t -> t
                    | _ -> failwithf "Unreachable! Failed to get value %A" current.Id
                    
                currentNeighbors
                |> Seq.iter ( fun edge ->
                    let neighbor = edge.Desc.A
                    let newMinimumKnownWeight =
                        edgeWeight (edge)
                                   (currentWeight)
                                   (initialWeight.Length = 0 && neighbor = sourceNode)
                                   (currentBlockHeight)
                                   (wr)
                    if (edge.Update.Contents.HTLCMaximumMSat |> Option.forall(fun x -> newMinimumKnownWeight.Cost <= x)) &&
                       newMinimumKnownWeight.Cost >= edge.Update.Contents.HTLCMinimumMSat &&
                       boundaries (newMinimumKnownWeight) &&
                       not <| ignoredEdges.Contains(edge.Desc) && not <| ignoredVertices.Contains(neighbor) then
                       let neighborCost =
                           match weight.TryGetValue(neighbor) with
                           | true, s -> s
                           | false, _ ->
                               { RichWeight.Cost = LNMoney.MaxValue
                                 Weight = Double.MaxValue
                                 Length = Int32.MaxValue
                                 CLTV = BlockHeightOffset.MaxValue }
                       if (newMinimumKnownWeight.Weight < neighborCost.Weight) then
                           prev.Add(neighbor, edge)
                           vertexQueue <- vertexQueue |> PriorityQueue.insert({ WeightedNode.Id = neighbor; Weight = newMinimumKnownWeight })
                    ()
                )
        match targetFound with
        | false -> Seq.empty
        | true ->
            let edgePath = ResizeArray()
            let mutable found, current = prev.TryGetValue(sourceNode)
            while found do
                edgePath.Add(current)
                let f, c = prev.TryGetValue(current.Desc.B)
                found <- f
                current <- c
            edgePath :> seq<_>
        
        
        
    let yenKShortestPaths (g: DirectedLNGraph)
                          (sourceNode: NodeId)
                          (targetNode: NodeId)
                          (amount: LNMoney)
                          (ignoredEdges: Set<ChannelDesc>)
                          (ignoredVertices: Set<NodeId>)
                          (extraEdges: Set<GraphLabel>)
                          (pathsToFind: int)
                          (wr: WeightRatios option)
                          (currentBlockHeight: BlockHeight)
                          (boundaries: RichWeight -> bool): WeightedPath seq =
        let mutable allSpurPathsFound = false
        // Stores tha shortest paths
        let shortestPaths = ResizeArray<WeightedPath>()
        // Stores the candidates for k(K+1) shortest paths
        // we instantiate by isDescending=false, so `Pop` should return the lowest cost
        let candidates = Heap.empty true
        
        // find the shortest path, k = 0
        let initialWeight = { RichWeight.Cost = amount;
                              Weight = 0.
                              Length = 0
                              CLTV = BlockHeightOffset.Zero }
        let shortestPath =
            dijkstraShortestPath g
                                 sourceNode
                                 targetNode
                                 ignoredEdges
                                 ignoredVertices
                                 extraEdges
                                 initialWeight
                                 boundaries
                                 currentBlockHeight
                                 wr
        shortestPaths.Add(
            { WeightedPath.Path = shortestPath
              Weight = pathWeight(shortestPath) (amount) false currentBlockHeight wr }
            )
        if (shortestPaths.Count = 0) then Seq.empty else
        for k in 1..pathsToFind do
            if (not <| allSpurPathsFound) then
                for i in 0..(shortestPaths.[k - 1].Path.Count() - 1) do
                    let prevShortestPath = shortestPaths.[k - 1].Path
                    // select the spur node as the i-th element of the k-the previous shortest path (k - 1)
                    let spurEdge = prevShortestPath |> Seq.item i
                    // select the sub-path from the source to the spur node of the k-th previous shortest path
                    let rootPathEdges =
                        if (i = 0) then prevShortestPath |> Seq.head |> List.singleton else
                        prevShortestPath |> Seq.take i |> Seq.toList
                    let rootPathWeight =
                        pathWeight
                            (rootPathEdges)
                            (amount)
                            true
                            currentBlockHeight
                            wr
                            
                    // links to be removed that are part of the previous shortest path and which share the
                    // same root path
                    let edgesToIgnore =
                        seq {
                            for weightedPath in shortestPaths do
                                if (i = 0 &&
                                        (weightedPath.Path |> Seq.head |> List.singleton = rootPathEdges ||
                                         weightedPath.Path |> Seq.take i |> Seq.toList = rootPathEdges)) then
                                    yield (weightedPath.Path |> Seq.item i).Desc
                        }
                        
                    // remove any link that can lead back to the previous vertex to avoid going back from where
                    // we arrived(previous iteration)
                    let returningEdges =
                        rootPathEdges
                        |> List.tryLast
                        |> Option.map(fun last -> g.GetEdgesBetween(last.Desc.B, last.Desc.A))
                        |> Option.toList
                        |> Seq.collect id
                        |> Seq.map(fun x -> x.Desc)
                        
                    // find the "spur" path, a sub-path going from the spur edge to the target avoiding previously
                    // found sub-paths
                    let spurPath =
                        let ignoredE = ignoredEdges |> Set.union (Set(edgesToIgnore)) |> Set.union(Set(returningEdges))
                        dijkstraShortestPath
                            (g)
                            spurEdge.Desc.A
                            targetNode
                            ignoredE
                            ignoredVertices
                            extraEdges
                            rootPathWeight
                            boundaries
                            currentBlockHeight
                            wr
                            
                    // if there wasn't a path the spur will be empty
                    if (spurPath.Count() <> 0) then
                        // candidate k-shortest path is made of the rootPath and the new spurPath
                        let totalPath =
                            if (rootPathEdges |> List.head).Desc.A = (spurPath |> Seq.head).Desc.A then
                                // if the heads are the same node, drop it from the rootPath
                                seq { for i in (rootPathEdges |> List.tail) do yield i; yield! spurPath }
                            else
                                seq { for i in rootPathEdges do yield i; yield! spurPath }
                        let candidatePath = { WeightedPath.Path = totalPath; Weight = pathWeight(totalPath) (amount) false currentBlockHeight wr }
                        if (boundaries(candidatePath.Weight) &&
                            not <| shortestPaths.Contains(candidatePath) &&
                            not <| (candidates |> Seq.exists((=)candidatePath))) then
                            candidates.Insert(candidatePath) |> ignore
                if (candidates.IsEmpty) then
                    // handles the case of having exhausted all possible spur paths and it's impossible to
                    // reach the target from the source
                    allSpurPathsFound <- true
                else
                    let (best, _) = candidates |> PriorityQueue.pop
                    shortestPaths.Add(best)
        shortestPaths :> seq<_>
