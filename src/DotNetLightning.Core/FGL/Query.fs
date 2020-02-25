namespace DotNetLightning.FGL

open System
open DotNetLightning.Utils
    
module BellmanFordTest =
    type Edge = {
        Source: int
        Destination: int
        Weight: int
    }
    
    type Graph = {
        VerticesCount: int
        EdgesCount: int
        Edges: Edge array
    }
    type BellmanFordError = NegativeCycleDetected

    let bellmanFord(g: Graph) (source:int) =
        let verticesCount = g.VerticesCount
        let edgesCount = g.EdgesCount
        let distance = Array.zeroCreate verticesCount
        for i in 0..verticesCount - 1 do
            distance.[i] <- Int32.MaxValue
        distance.[source] <- 0
        for i in 1 ..verticesCount - 1 do
            for j in 0 .. edgesCount-1 do
                let u = g.Edges.[j].Source
                let v = g.Edges.[j].Destination
                let weight = g.Edges.[j].Weight
                if (distance.[u] <> Int32.MaxValue && distance.[u] + weight < distance.[v]) then
                    distance.[v] <- distance.[u] + weight
        let mutable errorFound = false
        for i in 0..edgesCount - 1 do
            let u = g.Edges.[i].Source
            let v = g.Edges.[i].Destination
            let weight = g.Edges.[i].Weight
            if distance.[u] <> Int32.MaxValue && distance.[u] + weight < distance.[v] then
                errorFound <- true
            else ()
        if errorFound then Error(NegativeCycleDetected) else
        (distance, verticesCount) |> Ok
