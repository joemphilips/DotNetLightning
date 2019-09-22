namespace DotNetLightning.Core.Routing

open System
open System.Collections.Generic
open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open DotNetLightning.FGL
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
    
    [<StructuralComparison;StructuralEquality>]
    type ChannelDesc = {
        ShortChannelId: ShortChannelId
        A: NodeId
        B: NodeId
    }

    [<CustomComparison;StructuralEquality>]
    type GraphEdge = {
        Desc: ChannelDesc
        Update: ChannelUpdate
    }
    with
        member this.CompareTo(o: GraphEdge) =
            (this.Desc :> IComparable).CompareTo(o.Desc)
            
        interface IComparable with
            member this.CompareTo(o: obj) =
                match o with
                | :? GraphEdge as e -> this.CompareTo(e)
                | _ -> -1
                
    type WeightedPath = {
        Path: GraphEdge list
        Weight: RichWeight
    }
    
    type DirectedGraph = Graph<NodeId, string, GraphEdge>
    let private dijkstraShortestPath(g: DirectedGraph)
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
        let vertexQueue = SortedSet()
        
        let w = weight |> Map.add targetNode initialWeight
        vertexQueue
        failwith ""
        
    let yenKShortestPaths (graph: DirectedGraph)
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
        let candidates = PriorityQueue
        
        // find the shortest path, k = 0
        let initialWeight = { RichWeight.Cost = amount;
                              Weight = 0.
                              Length = 0
                              CLTV = 0 }
        let shortestPath = dijkstraShortestPath
        failwith ""