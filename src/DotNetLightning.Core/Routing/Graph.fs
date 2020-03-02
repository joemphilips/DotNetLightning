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
        ShotChannelId: ShortChannelId
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
    module GraphStructure =
        type GraphLabel = { Desc: ChannelDesc; Update: ChannelUpdate }
        type GraphVertex = LVertex<NodeId, unit>
        
        type GraphEdge = { Prev: NodeId; Succ: NodeId; Label: GraphLabel } 
            with
            member this.AsTuple = (this.Prev, this.Succ, this.Label)
            
        module GraphEdge =
            let hasZeroFee ({ Label = l }) =
                let u = l.Update
                u.Contents.FeeBaseMSat = LNMoney.Zero && u.Contents.FeeProportionalMillionths = 0u
                
    open GraphStructure
    
    [<CustomComparison;StructuralEquality>]
    type WeightedPath = {
        Path: GraphEdge list
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
            
    
    type DirectedLNGraph = private DirectedLNGraph of Graph<NodeId, unit, GraphLabel>
        with
        static member Create(vertices: (NodeId * _) list) (edges: GraphEdge list) =
            Graph.empty
            |> Directed.Vertices.addMany vertices
            |> Directed.Edges.addMany (edges |> List.map(fun e -> e.AsTuple))
            |> DirectedLNGraph
            
        member this.Value = let (DirectedLNGraph v) = this in  v
        member this.ContainsEdge(desc: ChannelDesc) =
            match this.Value |> Edges.tryFind desc.A desc.B with
            | None -> false
            | Some (_,_,_) -> true
            
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
            | None -> Seq.empty
            | Some ctx ->
                let (_predecessor, _v, _l, successor) = ctx
                successor |> Seq.choose(fun (_v, e) -> if e.Desc.A = keyA then Some (e) else None)
                
        member this.ContainsVertex v =
            this.Value |> Vertices.contains v
        member this.GetIncomingEdgesOf(keyB: NodeId) =
            match this.Value |> Graph.tryGetContext keyB with
            | None -> Seq.empty
            | Some (p, _v, _l, _s) ->
                p |> Seq.ofList
              
        member this.RemoveVertex(key: NodeId) =
            this.GetIncomingEdgesOf key
            |> Seq.map(fun (_v, e) -> e.Desc)
            |> this.RemoveEdges
            |> fun (DirectedLNGraph g) -> g |> Vertices.remove(key)
            |> DirectedLNGraph
            
        member this.AddVertex(key: NodeId) =
            this.Value |> Vertices.add (key, ()) |> DirectedLNGraph
            
        member this.AddEdge({ Label = edge } as e) =
            let vertIn = edge.Desc.A
            let vertOut = edge.Desc.B
            if this.ContainsEdge(edge.Desc) then
                this.RemoveEdge(edge.Desc).AddEdge(e)
            else
                this.Value
                |> Edges.add (vertIn, vertOut, edge)
                |> DirectedLNGraph
                
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
    let private edgeFeeCost (edge: GraphEdge, amountWithFees: LNMoney) =
        if (GraphEdge.hasZeroFee(edge)) then amountWithFees + nodeFee(LNMoney.One, 0L, amountWithFees) else
        let (_,_, {Update = update}) = edge.AsTuple
        amountWithFees + nodeFee(update.Contents.FeeBaseMSat, (int64 update.Contents.FeeProportionalMillionths), amountWithFees)
        
    /// Computes the compound weight for the given @param edge, the weight is cumulative
    let private edgeWeight
        (edge: GraphEdge)
        (prev: RichWeight)
        (isNeighborTarget: bool)
        (currentBlockHeight: BlockHeight)
        (weightRatios: WeightRatios option): RichWeight =
        match weightRatios with
        | None ->
            let edgeCost = if (isNeighborTarget) then prev.Cost else edgeFeeCost(edge, prev.Cost)
            { RichWeight.Cost = edgeCost; Length = prev.Length + 1; CLTV = prev.CLTV; Weight = edgeCost.MilliSatoshi |> double }
        | Some wr ->
            let (_,_, { Update = update; Desc = desc }) =  edge.AsTuple
            let channelBlockHeight = desc.ShotChannelId.BlockHeight.Value
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
        (path: GraphEdge seq)
        (amount: LNMoney)
        (isPartial: bool)
        (currentBlockHeight: BlockHeight)
        (wr: WeightRatios option) =
        path
        |> Seq.skip(if isPartial then 0 else 1)
        |> Seq.fold
            (fun (acc: RichWeight) (edge: GraphEdge) ->
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
        (wr: WeightRatios option) : GraphEdge seq =
        // The graph does not contain source/destination nodes
        if (not <| g.ContainsVertex sourceNode) then Seq.empty else
        if (not <| g.ContainsVertex targetNode && (not <| extraEdges.IsEmpty) && not <| extraEdges.Any(fun x -> x.Desc.B = targetNode)) then Seq.empty else
            
        let maxMapSize = 100 // conservative estimation to avoid over allocating memory
        let weight = Dictionary<NodeId, RichWeight>(maxMapSize)
        let prev = Dictionary<NodeId, GraphLabel>(maxMapSize)
        let vertexQueue = Fibonac
        failwith ""
        
    let yenKShortestPaths (DirectedLNGraph g)
                          (sourceNode: NodeId)
                          (targetNode: NodeId)
                          (amount: LNMoney)
                          (ignoredEdges: Set<ChannelDesc>)
                          (extraEdges: Set<GraphLabel>)
                          (pathsToFind: int)
                          (wr: WeightRatios option)
                          (currentBlockHeight: BlockHeight)
                          (boundaries: RichWeight -> bool): WeightedPath list =
        let mutable allSpurPathFound = false
        // Stores tha shortest paths
        let shortestPaths = ResizeArray<WeightedPath>()
        // Stores the candidates for k (K+1) shortest paths
        // should be sorted by path cost
        let candidates = Heap.empty
        
        // find the shortest path, k = 0
        let initialWeight = { RichWeight.Cost = amount;
                              Weight = 0.
                              Length = 0
                              CLTV = BlockHeightOffset.Zero }
        let shortestPath = dijkstraShortestPath
        failwith ""
        
