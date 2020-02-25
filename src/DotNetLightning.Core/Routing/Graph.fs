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
    
    [<CustomComparison;StructuralEquality>]
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
    module private GraphStructure =
        type GraphEdge = { Desc: ChannelDesc; Update: ChannelUpdate }
        type DirectedGraph =  private {
            Vertices: Map<NodeId, GraphEdge list>
        }
        with
        member private this.AddEdge(edge: GraphEdge) =
            let vertexIn = edge.Desc
            failwith ""
        member this.AddEdge(d: ChannelDesc, u: ChannelUpdate) =
            this.AddEdge({ Desc = d; Update =u })
        member this.AddEdges(edges: (ChannelDesc * ChannelUpdate) seq) =
            edges |> Seq.fold(fun (acc: DirectedGraph) (c, u) -> acc.AddEdge(c, u)) this
    open GraphStructure
    
    [<CustomComparison;StructuralEquality>]
    type WeightedPath = {
        Path: GraphEdge list
        Weight: RichWeight
    }
        with
        member x.CompareTo(y: WeightedPath)=
            x.Weight.CompareTo(y.Weight)
    
    type DirectedLNGraph = DirectedLNGraph of Graph<NodeId, string, GraphEdge>
        with
        static member Create(vertices: (NodeId * _) list) (edges: GraphEdge) =
            let h = Directed.Vertices.addMany vertices
            let e = Directed.Edges.addMany
            failwith ""
    
    let makeGraph(descAndUpdates: SortedDictionary<ShortChannelId, PublicChannel>): DirectedLNGraph =
        let h1 =
            descAndUpdates
            |> Seq.map(fun kvp -> let update, desc = kvp.Key, kvp.Value in desc, { GraphEdge.Desc = desc; Update = update })
        let h = Seq.concat[h1] |> Map.ofSeq
        DirectedLNGraph.Create(h)
        
    let private dijkstraShortestPath(DirectedLNGraph g)
                                    (sourceNode: NodeId)
                                    (targetNode: NodeId)
                                    (ignoredEdges: Set<ChannelDesc>)
                                    (extraEdges: Set<GraphEdge>)
                                    (initialWeight: RichWeight)
                                    (boundaries: RichWeight -> bool)
                                    (currentBlockHeight: BlockHeight)
                                    (wr: WeightRatios option): GraphEdge list =
        if (not <| (g |> Vertices.contains sourceNode)) then List.empty else
        if (not <| (g |> Vertices.contains targetNode) &&
            not <| extraEdges.IsEmpty &&
            (extraEdges |> Set.exists(fun x -> x.Desc.B = targetNode))) then List.empty else
        
        let mutable maxMapSize = 100
        let weight: Map<NodeId, RichWeight> = Map.empty
        let prev = Map.empty
        let vertexQueue = Heap.empty true
        
        let weight = weight |> Map.add targetNode initialWeight
        let vertexQueue = vertexQueue |> Heap.insert({WeightedNode.Id = targetNode; Weight = initialWeight})
        let mutable targetFound = false
        let rec loop (targetFound) vertexQueue =
            let current, vertexQueue = vertexQueue |> Heap.uncons
            if (current.Id = sourceNode) then
                // finish loop
                ()
            else
                let currentNeighbors =
                    match extraEdges.IsEmpty with
                    | true -> g |> Graph.getContext current.Id |> Vertices.predecessors
                    | false ->
                        let extraNeighbors = extraEdges
                        failwith ""
                ()
        (*
        while (not vertexQueue.IsEmpty && not targetFound) do
            let current, vertexQueue = vertexQueue |> Heap.uncons
            if (current.Id = sourceNode) then
                ()
            failwith ""
        *)
        loop false vertexQueue
        failwith ""
        
    let yenKShortestPaths (DirectedLNGraph g)
                          (sourceNode: NodeId)
                          (targetNode: NodeId)
                          (amount: LNMoney)
                          (ignoredEdges: Set<ChannelDesc>)
                          (extraEdges: Set<GraphEdge>)
                          (pathsToFind: int)
                          (wr: WeightRatios option)
                          (currentBlockHeight: BlockHeight)
                          (boundaries: RichWeight -> bool): WeightedPath list =
        let mutable allSpurPathFound = false
        // Stores tha shortest paths
        let shortestPaths = new ResizeArray<WeightedPath>()
        // Stores the candidates for k (K+1) shortest paths
        // should be sorted by path cost
        let candidates = Heap.empty
        
        // find the shortest path, k = 0
        let initialWeight = { RichWeight.Cost = amount;
                              Weight = 0.
                              Length = 0
                              CLTV = 0 }
        let shortestPath = dijkstraShortestPath
        failwith ""
        
