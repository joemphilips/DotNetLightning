namespace DotNetLightning.Routing

open System
open System.Collections.Generic
open DotNetLightning.Utils
open DotNetLightning.Serialization.Msgs
open NBitcoin

open ResultUtils
open ResultUtils.Portability

// Graph algorithms are based on eclair

module Graph =
    /// The cumulative weight of a set of edges (path in the graph).
    [<CustomComparison; CustomEquality>]
    type RichWeight =
        {
            Weight: double
            Cost: LNMoney
            Length: int
            CLTV: BlockHeightOffset16
        }

        override this.GetHashCode() =
            this.Weight.GetHashCode()

        member this.Equals(o: RichWeight) =
            this.Weight.Equals(o.Weight)

        override this.Equals(o: obj) =
            match o with
            | :? RichWeight as other -> this.Equals(other)
            | _ -> false

        member this.CompareTo(other: RichWeight) =
            this.Weight.CompareTo(other.Weight)

        interface IComparable with
            member this.CompareTo(other: obj) =
                match other with
                | :? RichWeight as o -> this.CompareTo(o)
                | _ -> -1


    /// We use heuristics to calculate the weight of an edge based on channel age, cltv delta and capacity.
    /// We favor older channels, with bigger capacity and small cltv delta
    type WeightRatios =
        private
            {
                CLTVDeltaFactor: double
                AgeFactor: double
                CapacityFactor: double
            }

        static member TryCreate(cltvDeltaFactor, ageFactor, capacityFactor) =
            let s = cltvDeltaFactor + ageFactor + capacityFactor

            if (s <= 0. || 1. < s) then
                sprintf
                    "sum of CLTVDeltaFactor + ageFactor + capacityFactor must in between 0 to 1. it was %f"
                    s
                |> Error
            else
                {
                    CLTVDeltaFactor = cltvDeltaFactor
                    AgeFactor = ageFactor
                    CapacityFactor = capacityFactor
                }
                |> Ok

    [<CustomComparison; CustomEquality>]
    type WeightedNode =
        {
            Id: NodeId
            Weight: RichWeight
        }

        member this.Equals(o: WeightedNode) =
            this.Id.Equals(o.Id)

        override this.Equals(o: obj) =
            match o with
            | :? WeightedNode as other -> this.Equals(other)
            | _ -> false

        override this.GetHashCode() =
            let mutable num = 0

            num <-
                -1640531527
                + this.Id.GetHashCode()
                + ((num <<< 6) + (num >>> 2))

            num

        interface IEquatable<WeightedNode> with
            member this.Equals o =
                this.Equals o

        member this.CompareTo(other: WeightedNode) =
            let weightCmp = this.Weight.CompareTo other.Weight

            if (weightCmp <> 0) then
                weightCmp
            else
                this
                    .Id
                    .Value
                    .ToHex()
                    .CompareTo(other.Id.Value.ToHex())

        interface IComparable with
            member this.CompareTo(o: obj) =
                match o with
                | :? WeightedNode as wn -> this.CompareTo(wn)
                | _ -> -1

    type ChannelDesc =
        {
            ShortChannelId: ShortChannelId
            A: NodeId
            B: NodeId
        }

    type PublicChannel =
        private
            {
                Announcement: UnsignedChannelAnnouncementMsg
                FundingTxId: TxId
                Capacity: Money
                Update1Opt: option<UnsignedChannelUpdateMsg>
                Update2Opt: option<UnsignedChannelUpdateMsg>
            }

        member this.Ann = this.Announcement

        static member Create(a, fundingTxId, cap, u1, u2) =
            {
                Announcement = a
                FundingTxId = fundingTxId
                Capacity = cap
                Update1Opt = u1
                Update2Opt = u2
            }

    type GraphLabel =
        {
            Desc: ChannelDesc
            Update: UnsignedChannelUpdateMsg
        }

        static member Create(d, u) =
            {
                Desc = d
                Update = u
            }

    module GraphEdge =
        let hasZeroFee(l: GraphLabel) =
            let u = l.Update
            u.FeeBaseMSat = LNMoney.Zero && u.FeeProportionalMillionths = 0u


    [<CustomComparison; CustomEquality>]
    type WeightedPath =
        {
            Path: seq<GraphLabel>
            Weight: RichWeight
        }

        override this.GetHashCode() =
            let mutable num = 0

            num <-
                -1640531527
                + this.Weight.GetHashCode()
                + ((num <<< 6) + (num >>> 2))

            for i in this.Path do
                num <-
                    -1640531527 + i.GetHashCode() + ((num <<< 6) + (num >>> 2))

            num

        member this.Equals(o: WeightedPath) =
            this.Path.Equals(o.Path) && this.Weight.Equals(o.Weight)

        override this.Equals o =
            match o with
            | :? WeightedPath as other -> this.Equals other
            | _ -> false

        interface IEquatable<WeightedPath> with
            member this.Equals o =
                this.Equals o

        member this.CompareTo(other: WeightedPath) =
            this.Weight.CompareTo other.Weight

        interface IComparable with
            member this.CompareTo o =
                match o with
                | :? WeightedPath as x -> this.CompareTo(x)
                | _ -> -1

    let internal getDesc
        (
            u: UnsignedChannelUpdateMsg,
            ann: UnsignedChannelAnnouncementMsg
        ) : ChannelDesc =
        let isNode1 = (u.ChannelFlags &&& 1uy) = 0uy

        let a, b =
            if isNode1 then
                ann.NodeId1, ann.NodeId2
            else
                ann.NodeId2, ann.NodeId1

        {
            ShortChannelId = u.ShortChannelId
            A = a
            B = b
        }

    [<StructuredFormatDisplay("{PrettyPrint}")>]
    type DirectedLNGraph =
        private
        | DirectedLNGraph of Map<NodeId, list<GraphLabel>>

        static member Create() =
            Map.empty |> DirectedLNGraph

        member this.Value = let (DirectedLNGraph v) = this in v

        member this.AddVertex(key: NodeId) =
            match this.Value |> Map.tryFind key with
            | None -> this.Value |> Map.add key [] |> DirectedLNGraph
            | Some _ -> this

        member this.AddEdge
            ({
                 Desc = d
             }: GraphLabel as e)
            =
            let vertIn = d.A
            let vertOut = d.B

            if (this.ContainsEdge(d)) then
                this.RemoveEdge(d).AddEdge e
            else
                let withVertices = this.AddVertex(vertIn).AddVertex(vertOut)

                withVertices.Value
                |> Map.add (vertOut) (e :: withVertices.Value.[vertOut])
                |> DirectedLNGraph

        member this.AddEdge(desc, update) =
            this.AddEdge(
                {
                    Desc = desc
                    Update = update
                }
            )

        member this.AddEdges
            (edges: seq<ChannelDesc * UnsignedChannelUpdateMsg>)
            =
            edges
            |> Seq.fold
                (fun (acc: DirectedLNGraph) (desc, update) ->
                    acc.AddEdge(
                        {
                            Desc = desc
                            Update = update
                        }
                    )
                )
                this

        member this.AddEdges(edges: seq<GraphLabel>) =
            edges |> Seq.map(fun e -> e.Desc, e.Update) |> this.AddEdges

        member this.ContainsEdge
            ({
                 A = a
                 B = b
                 ShortChannelId = id
             })
            =
            match this.Value |> Map.tryFind b with
            | None -> false
            | Some adj ->
                adj
                |> List.exists(fun {
                                       Desc = d
                                   } -> d.ShortChannelId = id && d.A = a
                )


        member this.VertexSet() =
            this.Value |> Seq.map(fun kvp -> kvp.Key) |> Seq.toList

        member this.EdgeSet() =
            this.Value |> Seq.collect(fun kvp -> kvp.Value) |> Seq.toList

        member this.OutgoingEdgesOf(v: NodeId) =
            this.EdgeSet()
            |> List.filter(fun {
                                   Desc = d
                               } -> d.A = v
            )

        member this.IncomingEdgesOf(v: NodeId) =
            this.Value |> Map.tryFind v |> Option.defaultValue []

        member this.RemoveEdge
            ({
                 B = b
             }: ChannelDesc as desc)
            : DirectedLNGraph =
            match this.ContainsEdge(desc) with
            | true ->
                this.Value
                |> Map.add
                    (b)
                    (this.Value
                     |> Map.find b
                     |> List.filter(fun x -> x.Desc <> desc))
                |> DirectedLNGraph
            | false -> this

        member this.RemoveEdges(descriptions: #seq<ChannelDesc>) =
            descriptions
            |> Seq.fold (fun (acc: DirectedLNGraph) d -> acc.RemoveEdge(d)) this

        member this.TryGetEdge(desc: ChannelDesc) =
            this.Value
            |> Map.tryFind(desc.B)
            |> Option.bind(fun adj ->
                adj
                |> List.tryFind(fun e ->
                    e.Desc.ShortChannelId = desc.ShortChannelId
                    && e.Desc.A = desc.A
                )
            )

        member this.TryGetEdge
            ({
                 Desc = d
             })
            =
            this.TryGetEdge(d)

        member this.GetEdgesBetween(keyA: NodeId, keyB: NodeId) =
            match this.Value |> Map.tryFind keyB with
            | None -> List.empty
            | Some adj -> adj |> List.filter(fun e -> e.Desc.A = keyA)

        member this.ContainsVertex v =
            this.Value |> Map.containsKey v

        member this.RemoveVertex(key: NodeId) =
            let ds = this.IncomingEdgesOf(key) |> List.map(fun x -> x.Desc)
            this.RemoveEdges(ds).Value |> Map.remove key |> DirectedLNGraph

        /// This is the recommended way of creating the network graph.
        /// We don't include private channels: they would bloat the graph without providing any value(if they are
        /// private they probably don't want to be involved in routing other people's payments).
        /// The only private channels we know are ours: we should check them to see if our destination can be reached
        /// in a single hop via private channel before using the public network graph
        static member MakeGraph
            (channels: Map<ShortChannelId, PublicChannel>)
            : DirectedLNGraph =
            let result = Dictionary<NodeId, list<GraphLabel>>()

            let addDescToDict(desc: ChannelDesc, u: UnsignedChannelUpdateMsg) =
                let previousV =
                    result.TryGetValue(desc.B)
                    |> function
                        | true, v -> v
                        | false, _ -> List.empty

                result.AddOrReplace(
                    desc.B,
                    GraphLabel.Create(desc, u) :: previousV
                )

                match result.TryGetValue desc.A with
                | false, _ -> result.Add(desc.A, List.empty) |> ignore
                | true, _ -> ()

            channels
            |> Map.iter(fun _k channel ->
                channel.Update1Opt
                |> Option.iter(fun u1 ->
                    let desc1 = getDesc(u1, channel.Announcement)
                    addDescToDict(desc1, u1)
                )

                channel.Update2Opt
                |> Option.iter(fun u2 ->
                    let desc2 = getDesc(u2, channel.Announcement)
                    addDescToDict(desc2, u2)
                )
            )

            DirectedLNGraph(result |> Seq.map(|KeyValue|) |> Map.ofSeq)

        member this.PrettyPrint =
            this.Value
            |> Seq.fold
                (fun acc kvp ->
                    let (v, adj) = kvp.Key, kvp.Value

                    sprintf
                        "%s[%A]: %A\n"
                        acc
                        (v.Value.ToHex().[0..6])
                        (adj
                         |> List.map(fun x ->
                             ("--> " + x.Desc.B.Value.ToHex().[0..6])
                         ))
                )
                ""

    module internal RoutingHeuristics =
        let BLOCK_TIME_TWO_MONTHS = 8640us |> BlockHeightOffset16
        let CAPACITY_CHANNEL_LOW = LNMoney.Satoshis(1000L)

        let CAPACITY_CHANNEL_HIGH =
            DotNetLightning.Channel.ChannelConstants.MAX_FUNDING_SATOSHIS.Satoshi
            |> LNMoney.Satoshis

        [<Literal>]
        let CLTV_LOW = 9L

        [<Literal>]
        let CLTV_HIGH = 2016

        let normalize(v, min, max) : double =
            if (v <= min) then
                0.00001
            else if (v > max) then
                0.99999
            else
                (v - min) / (max - min)

    let internal nodeFee
        (
            baseFee: LNMoney,
            proportionalFee: int64,
            paymentAmount: LNMoney
        ) =
        baseFee + ((paymentAmount * proportionalFee) / 1000000L)

    /// This forces channel_update(s) with fees = 0 to have a minimum of 1msat for the baseFee. Note that
    /// the update is not being modified and the result of the route computation will still have the update
    /// with fees=0 which is what will be used to build the onion.
    let private edgeFeeCost(edge: GraphLabel, amountWithFees: LNMoney) =
        if (GraphEdge.hasZeroFee(edge)) then
            amountWithFees + nodeFee(LNMoney.One, 0L, amountWithFees)
        else
            let ({
                     Update = update
                 }) =
                edge

            amountWithFees
            + nodeFee(
                update.FeeBaseMSat,
                (int64 update.FeeProportionalMillionths),
                amountWithFees
            )

    /// Computes the compound weight for the given @param edge, the weight is cumulative
    let private edgeWeight
        (edge: GraphLabel)
        (prev: RichWeight)
        (isNeighborTarget: bool)
        (currentBlockHeight: BlockHeight)
        (weightRatios: option<WeightRatios>)
        : RichWeight =
        match weightRatios with
        | None ->
            let edgeCost =
                if isNeighborTarget then
                    prev.Cost
                else
                    edgeFeeCost(edge, prev.Cost)

            {
                RichWeight.Cost = edgeCost
                Length = prev.Length + 1
                CLTV = prev.CLTV + edge.Update.CLTVExpiryDelta
                Weight = edgeCost.MilliSatoshi |> double
            }
        | Some wr ->
            let ({
                     Update = update
                     Desc = desc
                 }) =
                edge

            let channelBlockHeight = desc.ShortChannelId.BlockHeight.Value
            // every edge is weighted by funding block height where older blocks add less weight,
            let ageFactor =
                RoutingHeuristics.normalize(
                    channelBlockHeight |> double,
                    (currentBlockHeight
                     - RoutingHeuristics.BLOCK_TIME_TWO_MONTHS)
                        .Value
                    |> double,
                    currentBlockHeight.Value |> double
                )
            // Every edge is weighted by channel capacity, larger channels and less weight
            let edgeMaxCapacity =
                update.HTLCMaximumMSat
                |> Option.defaultValue(RoutingHeuristics.CAPACITY_CHANNEL_LOW)

            let capFactor =
                1.
                - RoutingHeuristics.normalize(
                    edgeMaxCapacity.MilliSatoshi |> double,
                    RoutingHeuristics.CAPACITY_CHANNEL_LOW.MilliSatoshi
                    |> double,
                    RoutingHeuristics.CAPACITY_CHANNEL_HIGH.MilliSatoshi
                    |> double
                )
            // Every edge is weighted by cltv-delta value, normalized.
            let channelCLTVDelta = update.CLTVExpiryDelta

            let cltvFactor =
                RoutingHeuristics.normalize(
                    channelCLTVDelta.Value |> double,
                    RoutingHeuristics.CLTV_LOW |> double,
                    RoutingHeuristics.CLTV_HIGH |> double
                )

            let edgeCost =
                if isNeighborTarget then
                    prev.Cost
                else
                    edgeFeeCost(edge, prev.Cost)

            let factor =
                (cltvFactor * wr.CLTVDeltaFactor)
                + (ageFactor * wr.AgeFactor)
                + (capFactor * wr.CapacityFactor)

            let edgeWeight =
                if isNeighborTarget then
                    prev.Weight
                else
                    prev.Weight + ((edgeCost.MilliSatoshi |> double) * factor)

            {
                RichWeight.Cost = edgeCost
                Length = prev.Length + 1
                Weight = edgeWeight
                CLTV = prev.CLTV + channelCLTVDelta
            }

    /// Calculates the total cost of a path (amount + fees),
    /// direct channels with the source will have a cost of 0 (pay no fees)
    let pathWeight
        (path: seq<GraphLabel>)
        (amount: LNMoney)
        (isPartial: bool)
        (currentBlockHeight: BlockHeight)
        (wr: option<WeightRatios>)
        =
        let zero =
            {
                RichWeight.Cost = amount
                Weight = 0.
                Length = 0
                CLTV = BlockHeightOffset16.Zero
            }

        if path |> Seq.isEmpty then
            zero
        else
            path
            |> Seq.skip(
                if isPartial then
                    0
                else
                    1
            )
            |> Seq.fold
                (fun (acc: RichWeight) (edge: GraphLabel) ->
                    edgeWeight (edge) (acc) (false) (currentBlockHeight) (wr)
                )
                zero

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
        (wr: option<WeightRatios>)
        : seq<GraphLabel> =
        // The graph does not contain source/destination nodes
        if (not <| g.ContainsVertex sourceNode) then
            Seq.empty
        else if (not <| g.ContainsVertex targetNode
                 && (not <| extraEdges.IsEmpty)
                 && not <| extraEdges.Any(fun x -> x.Desc.B = targetNode)) then
            Seq.empty
        else

            let maxMapSize = 100 // conservative estimation to avoid over allocating memory
            let weight = Dictionary<NodeId, RichWeight>(maxMapSize)
            let prev = Dictionary<NodeId, GraphLabel>(maxMapSize)
            // TODO: mutable and while loop is ugly. Refactor.
            let mutable vertexQueue =
                // Ideally, we should use Fibonacci heap for the sake of performance,
                // but to make things easy, we just use regular heap.
                // isDescending is false, which means root of the heap is the smallest value,
                // so that we can pop the min-cost node with constant time.
                Heap.empty<WeightedNode>(false)
                |> PriorityQueue.insert(
                    {
                        WeightedNode.Id = targetNode
                        Weight = initialWeight
                    }
                )

            weight.Add(targetNode, initialWeight)
            let mutable targetFound = false

            while (not <| vertexQueue.IsEmpty && not <| targetFound) do
                let current, _vq = vertexQueue.Pop()
                vertexQueue <- _vq

                if (current.Id = sourceNode) then
                    targetFound <- true
                else
                    // build the neighbors with optional extra edges
                    let currentNeighbors =
                        if extraEdges.IsEmpty then
                            g.IncomingEdgesOf(current.Id) |> List.toSeq
                        else
                            let extraNeighbors =
                                extraEdges
                                |> Seq.filter(fun x -> x.Desc.B = current.Id)
                            // the resulting set must have only one element per shortChannelId
                            let incoming =
                                g.IncomingEdgesOf(current.Id)
                                |> Seq.filter(fun e ->
                                    not
                                    <| extraEdges.Any(fun x ->
                                        x.Desc.ShortChannelId = e.Desc.ShortChannelId
                                    )
                                )

                            seq {
                                yield! incoming
                                yield! extraNeighbors
                            }

                    let currentWeight =
                        match weight.TryGetValue current.Id with
                        | true, t -> t
                        | _ ->
                            failwithf
                                "Unreachable! Failed to get value %A \n from %A"
                                current.Id
                                (weight |> Seq.map(|KeyValue|))

                    currentNeighbors
                    |> Seq.iter(fun edge ->
                        let neighbor = edge.Desc.A

                        let newMinimumKnownWeight =
                            edgeWeight
                                (edge)
                                (currentWeight)
                                (initialWeight.Length = 0
                                 && neighbor = sourceNode)
                                (currentBlockHeight)
                                (wr)

                        if (edge.Update.HTLCMaximumMSat
                            |> Option.forall(fun x ->
                                newMinimumKnownWeight.Cost <= x
                            ))
                           && newMinimumKnownWeight.Cost
                              >= edge.Update.HTLCMinimumMSat
                           && boundaries(newMinimumKnownWeight)
                           && (not <| ignoredEdges.Contains(edge.Desc))
                           && (not <| ignoredVertices.Contains(neighbor)) then
                            let neighborCost =
                                match weight.TryGetValue(neighbor) with
                                | true, s -> s
                                | false, _ ->
                                    {
                                        RichWeight.Cost = LNMoney.MaxValue
                                        Weight = Double.MaxValue
                                        Length = Int32.MaxValue
                                        CLTV = BlockHeightOffset16.MaxValue
                                    }
                            // if this neighbor has a shorter distance than previously known
                            if (newMinimumKnownWeight.Weight < neighborCost.Weight) then
                                // update the visiting tree
                                prev.AddOrReplace(neighbor, edge)
                                // update the queue
                                vertexQueue <-
                                    vertexQueue
                                    |> PriorityQueue.insert(
                                        {
                                            WeightedNode.Id = neighbor
                                            Weight = newMinimumKnownWeight
                                        }
                                    )
                                // update the minimum known distance array
                                weight.AddOrReplace(
                                    neighbor,
                                    newMinimumKnownWeight
                                )

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



    let yenKShortestPaths
        (g: DirectedLNGraph)
        (sourceNode: NodeId)
        (targetNode: NodeId)
        (amount: LNMoney)
        (ignoredEdges: Set<ChannelDesc>)
        (ignoredVertices: Set<NodeId>)
        (extraEdges: Set<GraphLabel>)
        (pathsToFind: int)
        (wr: option<WeightRatios>)
        (currentBlockHeight: BlockHeight)
        (boundaries: RichWeight -> bool)
        : ResizeArray<WeightedPath> =
        let mutable allSpurPathsFound = false
        // Stores tha shortest paths
        let shortestPaths = ResizeArray<WeightedPath>()
        // Stores the candidates for k(K+1) shortest paths
        // we instantiate by isDescending=false, so `Pop` should return the lowest cost element
        let mutable candidates = Heap.empty false

        // find the shortest path, k = 0
        let initialWeight =
            {
                RichWeight.Cost = amount
                Weight = 0.
                Length = 0
                CLTV = BlockHeightOffset16.Zero
            }

        let shortestPath =
            dijkstraShortestPath
                g
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
            {
                WeightedPath.Path = shortestPath |> Seq.toList
                Weight =
                    pathWeight
                        (shortestPath)
                        (amount)
                        false
                        currentBlockHeight
                        wr
            }
        )

        if ((shortestPath.Count()) = 0) then
            ResizeArray()
        else
            for k in 1 .. (pathsToFind - 1) do
                if (not <| allSpurPathsFound) then
                    let edgeNum = shortestPaths.[k - 1].Path.Count()

                    for i in 0 .. (edgeNum - 1) do
                        let prevShortestPath = shortestPaths.[k - 1].Path
                        // select the spur node as the i-th element of the k-the previous shortest path (k - 1)
                        let spurEdge = prevShortestPath |> Seq.item i
                        // select the sub-path from the source to the spur node of the k-th previous shortest path
                        let rootPathEdges =
                            if (i = 0) then
                                prevShortestPath |> Seq.head |> List.singleton
                            else
                                prevShortestPath |> Seq.truncate i |> Seq.toList

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
                                    if (weightedPath.Path |> Seq.isEmpty) then
                                        ()
                                    else if (i = 0
                                             && (weightedPath.Path
                                                 |> Seq.head
                                                 |> List.singleton = rootPathEdges))
                                            || (weightedPath.Path
                                                |> Seq.truncate i
                                                |> Seq.toList = rootPathEdges) then
                                        yield
                                            (weightedPath.Path |> Seq.item i)
                                                .Desc
                                    else
                                        yield! []
                            }

                        // remove any link that can lead back to the previous vertex to avoid going back from where
                        // we arrived(previous iteration)
                        let returningEdges =
                            rootPathEdges
                            |> List.tryLast
                            |> Option.map(fun last ->
                                g.GetEdgesBetween(last.Desc.B, last.Desc.A)
                            )
                            |> Option.toList
                            |> Seq.collect id
                            |> Seq.map(fun x -> x.Desc)

                        // find the "spur" path, a sub-path going from the spur edge to the target avoiding previously
                        // found sub-paths
                        let spurPath =
                            let ignoredE =
                                ignoredEdges
                                |> Set.union(Set(edgesToIgnore))
                                |> Set.union(Set(returningEdges))

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
                            |> List.ofSeq

                        // if there wasn't a path the spur will be empty
                        if (spurPath.Count() <> 0) then
                            // candidate k-shortest path is made of the rootPath and the new spurPath
                            let totalPath =
                                if rootPathEdges.Head.Desc.A = (spurPath
                                                                |> Seq.head)
                                    .Desc
                                    .A then
                                    // if the heads are the same node, drop it from the rootPath
                                    let t =
                                        List.concat
                                            [
                                                (rootPathEdges |> List.tail)
                                                spurPath
                                            ]

                                    t
                                else
                                    List.concat [ rootPathEdges; spurPath ]

                            let candidatePath =
                                {
                                    WeightedPath.Path = totalPath
                                    Weight =
                                        pathWeight
                                            totalPath
                                            amount
                                            false
                                            currentBlockHeight
                                            wr
                                }

                            if (boundaries(candidatePath.Weight)
                                && (not <| shortestPaths.Contains(candidatePath))
                                && (not
                                    <| (candidates
                                        |> Seq.exists((=) candidatePath)))) then
                                candidates <- candidates.Insert(candidatePath)

                if candidates.IsEmpty then
                    // handles the case of having exhausted all possible spur paths and it's impossible to
                    // reach the target from the source
                    allSpurPathsFound <- true
                else
                    let (best, c) = candidates.Uncons()
                    candidates <- c
                    shortestPaths.Add(best)

            shortestPaths
