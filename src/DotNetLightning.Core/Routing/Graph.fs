namespace DotNetLightning.Routing

open System
open System.Collections.Generic
open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open DotNetLightning.FGL
open DotNetLightning.FGL.Directed
open DotNetLightning.LN

type WeightRatios = unit


module Graph =
    [<CustomComparison;StructuralEquality>]
    type RichWeight = {
        Weight: double
        Cost: LNMoney
        Length: int
        CLTV: int
    }
    with
        member this.CompareTo(other: RichWeight) =
            this.Weight.CompareTo(other.Weight)
            
        interface IComparable with
            member this.CompareTo(other:obj) =
                match other with
                | :? RichWeight as o -> this.CompareTo(o)
                | _ -> -1
                

    type WeightRatios = private {
        CLTVDeltaFactor: double
        AgeFactor: double
        CapacityFactor: double
    }
    with
        static member Create(cltvDeltaFactor, ageFactor, capacityFactor) =
            let s = cltvDeltaFactor + ageFactor + capacityFactor
            if (s <= 0.|| 1. < s) then
                sprintf "sum of CLTVDeltaFactor + ageFactor + capacityFactor must in between 0 to 1. it was %f" s
                |> RResult.rmsg 
            else
                {
                    CLTVDeltaFactor = cltvDeltaFactor
                    AgeFactor = ageFactor
                    CapacityFactor = capacityFactor
                } |> Good
                
    type WeightedNode = {
        Id: NodeId
        Weight: RichWeight
    }
    
    [<CustomComparison;StructuralEquality>]
    type ChannelDesc = {
        ShotChannelId: ShortChannelId
        ChannelUpdate: ChannelUpdate
    }
        with
        member this.CompareTo(o: ChannelDesc) =
            (this.ShotChannelId :> IComparable).CompareTo(o.ShotChannelId)
            
        interface IComparable with
            member this.CompareTo(o: obj) =
                match o with
                | :? ChannelDesc as e -> this.CompareTo(e)
                | _ -> -1

    type GraphEdge = NodeId * NodeId * ChannelDesc
    type WeightedGraphEdge = NodeId * NodeId * RichWeight
                
    type WeightedPath = WeightedGraphEdge list
    
    type DirectedLNGraph = DirectedLNGraph of Graph<NodeId, string, GraphEdge>
        with
        static member Create(vertices: (NodeId * _) list) (edges: GraphEdge) =
            let h = Directed.Vertices.addMany vertices
            let e = Directed.Edges.addMany
            failwith ""
    
    let makeGraph(descAndUpdates: Map<ChannelDesc, ChannelUpdate>): DirectedLNGraph =
        let h1 =
            descAndUpdates
            |> Map.toSeq
            |> Seq.map(fun (desc, update) -> desc.B, { GraphEdge.Desc = desc; Update = update })
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
        
