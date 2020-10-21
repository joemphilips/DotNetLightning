module GraphTests


open DotNetLightning.Utils
open DotNetLightning.Routing
open DotNetLightning.Routing.Graph
open DotNetLightning.Serialization.Msgs
open NBitcoin
open Expecto
open NBitcoin.DataEncoders

let hex = Encoders.Hex

module Constants =
    let ascii = System.Text.ASCIIEncoding.ASCII
    let signMessageWith (privKey: Key) (msgHash: string) =
        let msgBytes = msgHash |> ascii.GetBytes
        privKey.SignCompact(msgBytes |> uint256, false) |> fun d -> LNECDSASignature.FromBytesCompact(d, true)
    let DEFAULT_AMOUNT_MSAT = LNMoney.MilliSatoshis(10000000L)
    let DEFAULT_ROUTE_PARAMS = { RouteParams.Randomize = false
                                 MaxFeeBase = LNMoney.MilliSatoshis(21000L)
                                 MaxFeePCT = 0.03
                                 RouteMaxCLTV = 2016us |> BlockHeightOffset16
                                 RouteMaxLength = 6
                                 Ratios = None }
    let privKey1 = new Key(hex.DecodeData("0101010101010101010101010101010101010101010101010101010101010101"))
    
    let DUMMY_SIG = signMessageWith privKey1 "01010101010101010101010101010101"
    
/// Taken from eclair-core
let pks =
    [
        "02999fa724ec3c244e4da52b4a91ad421dc96c9a810587849cd4b2469313519c73"; //a
        "03f1cb1af20fe9ccda3ea128e27d7c39ee27375c8480f11a87c17197e97541ca6a"; //b
        "0358e32d245ff5f5a3eb14c78c6f69c67cea7846bdf9aeeb7199e8f6fbb0306484"; //c
        "029e059b6780f155f38e83601969919aae631ddf6faed58fe860c72225eb327d7c"; //d
        "02f38f4e37142cc05df44683a83e22dea608cf4691492829ff4cf99888c5ec2d3a"; //e
        "03fc5b91ce2d857f146fd9b986363374ffe04dc143d8bcd6d7664c8873c463cdfc"; //f
        "03864ef025fde8fb587d989186ce6a4a186895ee44a926bfc370e2c366597a3f8f"; //g
    ]
    |> List.map (hex.DecodeData >> PubKey >> NodeId)
let a, b, c, d, e, f, g = pks.[0], pks.[1], pks.[2], pks.[3], pks.[4], pks.[5], pks.[6]


let makeUpdateCore(shortChannelId: ShortChannelId,
                    nodeid1: NodeId,
                    nodeid2: NodeId,
                    feeBase: LNMoney,
                    feeProportionalMillions: uint32,
                    minHtlc: LNMoney option,
                    maxHtlc: LNMoney option,
                    cltvDelta: BlockHeightOffset16 option
                    ): (ChannelDesc * UnsignedChannelUpdateMsg) =
    let minHtlc = Option.defaultValue Constants.DEFAULT_AMOUNT_MSAT minHtlc
    let cltvDelta = Option.defaultValue (BlockHeightOffset16(0us)) cltvDelta
    let desc = { ChannelDesc.ShortChannelId = shortChannelId
                 A = nodeid1
                 B = nodeid2 }
    let update =
        { UnsignedChannelUpdateMsg.MessageFlags =
              match maxHtlc with Some _ -> 1uy | _ -> 0uy
          ChannelFlags = 0uy
          ChainHash = Network.RegTest.GenesisHash
          ShortChannelId = shortChannelId
          Timestamp = 0u
          CLTVExpiryDelta = cltvDelta
          HTLCMinimumMSat = minHtlc
          FeeBaseMSat = feeBase
          FeeProportionalMillionths = feeProportionalMillions
          HTLCMaximumMSat = maxHtlc }
    desc, update
    
let makeUpdate (shortChannelId: uint64,
                nodeid1: NodeId,
                nodeid2: NodeId,
                feeBase: LNMoney,
                feeProportionalMillions: uint32,
                minHtlc: LNMoney option,
                maxHtlc: LNMoney option,
                cltvDelta: BlockHeightOffset16 option ): (ChannelDesc * UnsignedChannelUpdateMsg) =
     makeUpdateCore(shortChannelId |> ShortChannelId.FromUInt64, nodeid1, nodeid2, feeBase, feeProportionalMillions, minHtlc, maxHtlc, cltvDelta)
let makeUpdate2 (s, a, b, feeBase, feeProp, minHTLC, maxHTLC, cltvDelta) =
    makeUpdateCore(ShortChannelId.ParseUnsafe(s), a, b, feeBase, feeProp, minHTLC, maxHTLC, cltvDelta)
    
let makeUpdateSimple (shortChannelId, a, b) =
    makeUpdate(shortChannelId, a, b, LNMoney.Zero, 0u, None, None, None)
let makeTestGraph(): DirectedLNGraph =
    let updates =
        [
            makeUpdateSimple(1UL, a, b)
            makeUpdateSimple(2UL, b, c)
            makeUpdateSimple(3UL, a, d)
            makeUpdateSimple(4UL, d, c)
            makeUpdateSimple(5UL, c, e)
            makeUpdateSimple(6UL, b, e)
        ]
    DirectedLNGraph.Create().AddEdges(updates)
    
let descFromNodes(id, a: NodeId, b: NodeId) =
    { ShortChannelId = ShortChannelId.FromUInt64(uint64 id); A = a; B = b }
[<Tests>]
let graphTests =
    testList "GraphTests from eclair" [
        testCase "Instantiate a graph, with vertices and then add edges" <| fun _ ->
            let g =
                DirectedLNGraph.Create()
                    .AddVertex(a)
                    .AddVertex(b)
                    .AddVertex(c)
                    .AddVertex(d)
                    .AddVertex(e)
            Expect.isTrue(g.ContainsVertex(a) && g.ContainsVertex(e)) ""
            Expect.equal (g.VertexSet().Length) 5 ""
            let otherGraph = g.AddVertex(a)
            Expect.equal (otherGraph.VertexSet().Length) 5 ""
            let descAB, updateAB = makeUpdate(1UL, a, b, LNMoney.Zero, 0u, None, None, None)
            let descBC, updateBC = makeUpdate(2UL, b, c, LNMoney.Zero, 0u, None, None, None)
            let descAD, updateAD = makeUpdate(3UL, a, d, LNMoney.Zero, 0u, None, None, None)
            let descDC, updateDC = makeUpdate(4UL, d, c, LNMoney.Zero, 0u, None, None, None)
            let descCE, updateCE = makeUpdate(5UL, c, e, LNMoney.Zero, 0u, None, None, None)
            let graphWithEdges =
                g
                    .AddEdge({ Update = updateAB; Desc = descAB })
                    .AddEdge({ Update = updateBC; Desc = descBC })
                    .AddEdge({ Update = updateAD; Desc = descAD })
                    .AddEdge({ Update = updateDC; Desc = descDC })
                    .AddEdge({ Update = updateCE; Desc = descCE })
            Expect.equal (graphWithEdges.OutgoingEdgesOf(a).Length) 2 ""
            Expect.equal (graphWithEdges.OutgoingEdgesOf(b).Length) 1 ""
            Expect.equal (graphWithEdges.OutgoingEdgesOf(c).Length) 1 ""
            Expect.equal (graphWithEdges.OutgoingEdgesOf(d).Length) 1 ""
            Expect.equal (graphWithEdges.OutgoingEdgesOf(e).Length) 0 ""
            Expect.isEmpty (graphWithEdges.OutgoingEdgesOf(f)) ""
            
            let withRemovedEdges = graphWithEdges.RemoveEdge(descAD)
            Expect.equal (withRemovedEdges.OutgoingEdgesOf(d).Length) 1 ""
            
        testCase "instantiate a graph adding edges only" <| fun _ ->
            let labelAB =
                makeUpdateSimple(1UL, a, b)
                |> fun (a, b) -> { Desc = a; Update = b }
            let labelBC = makeUpdateSimple(2UL, b, c) |> fun (a, b) -> { Desc = a; Update = b }
            let labelAD = makeUpdateSimple(3UL, a, d) |> fun (a, b) -> { Desc = a; Update = b }
            let labelDC = makeUpdateSimple(4UL, d, c) |> fun (a, b) -> { Desc = a; Update = b }
            let labelCE = makeUpdateSimple(5UL, c, e) |> fun (a, b) -> { Desc = a; Update = b }
            let labelBE = makeUpdateSimple(6UL, b, e) |> fun (a, b) -> { Desc = a; Update = b }
            let g =
                DirectedLNGraph.Create()
                    .AddEdge(labelAB)
                    .AddEdge(labelBC)
                    .AddEdge(labelAD)
                    .AddEdge(labelDC)
                    .AddEdge(labelCE)
                    .AddEdge(labelBE)
            Expect.equal (g.VertexSet().Length) 5 ""
            Expect.equal (g.OutgoingEdgesOf(c).Length) 1 ""
            Expect.equal (g.IncomingEdgesOf(c).Length) 2 ""
            Expect.equal (g.EdgeSet().Length) 6 ""
            
        testCase "containsEdge should return true if the graph contains that edge, false otherwise" <| fun _ ->
            let updates =
                seq {
                    makeUpdateSimple(1UL, a, b)
                    makeUpdateSimple(2UL, b, c)
                    makeUpdateSimple(3UL, c, d)
                    makeUpdateSimple(4UL, d, e)
                }
                
            let graph = DirectedLNGraph.Create().AddEdges(updates)
            
            Expect.isTrue  (graph.ContainsEdge(descFromNodes(1, a, b))) ""
            Expect.isTrue  (graph.ContainsEdge(descFromNodes(2, b, c))) ""
            Expect.isTrue  (graph.ContainsEdge(descFromNodes(3, c, d))) ""
            Expect.isTrue  (graph.ContainsEdge(descFromNodes(4, d, e))) ""
            // same with channel desc
            Expect.isTrue  (graph.ContainsEdge({ ChannelDesc.ShortChannelId = ShortChannelId.FromUInt64(4UL); A = d; B = e })) ""
            Expect.isFalse (graph.ContainsEdge({ ChannelDesc.ShortChannelId = ShortChannelId.FromUInt64(4UL); A = a; B = g })) ""
            Expect.isFalse (graph.ContainsEdge(descFromNodes(50, a, e))) ""
            Expect.isFalse (graph.ContainsEdge(descFromNodes(66, c, f))) ""
            
        testCase "Should remove a set of edges" <| fun _ ->
            let graph: DirectedLNGraph = makeTestGraph()
            let (descBE, _) = makeUpdateSimple(6UL, b, e)
            let (descCE, _) = makeUpdateSimple(5UL, c, e)
            let (descAD, _) = makeUpdateSimple(3UL, a, d)
            let (descDC, _) = makeUpdateSimple(4UL, d, c)
            Expect.equal (graph.EdgeSet().Length) 6 ""
            Expect.isTrue (graph.ContainsEdge(descBE)) ""
            
            let withRemovedEdge = graph.RemoveEdge(descBE)
            Expect.equal (withRemovedEdge.EdgeSet().Length) 5 ""
            
            let withRemovedList = graph.RemoveEdges(seq { descAD; descDC })
            Expect.equal (withRemovedList.EdgeSet().Length) 4 ""
            
            let withoutAnyIncomingEdgeInE = graph.RemoveEdges(seq { descBE; descCE })
            Expect.isTrue (withoutAnyIncomingEdgeInE.ContainsVertex(e)) ""
            Expect.isEmpty (withoutAnyIncomingEdgeInE.OutgoingEdgesOf(e)) ""
            
        testCase "should get an edge given two vertices" <| fun _ ->
            let updates = seq { makeUpdateSimple(1UL, a, b); makeUpdateSimple(2UL, b, c) }
            let graph = DirectedLNGraph.Create().AddEdges(updates)
            let edgesAB = graph.GetEdgesBetween(a, b)
            Expect.equal (edgesAB.Length) 1 ""
            Expect.equal (edgesAB.Head.Desc.A) a ""
            Expect.equal (edgesAB.Head.Desc.B) b ""
            
            let bIncoming = graph.IncomingEdgesOf(b)
            Expect.equal bIncoming.Length 1 ""
            Expect.contains (bIncoming |> List.map (fun x -> x.Desc.A)) a ""
            Expect.contains (bIncoming |> List.map (fun x -> x.Desc.B)) b ""
            
            let bOutgoing = graph.OutgoingEdgesOf b
            Expect.equal bOutgoing.Length 1 ""
            Expect.contains (bOutgoing |> List.map(fun x -> x.Desc.A)) b ""
            Expect.contains (bOutgoing |> List.map(fun x -> x.Desc.B)) c ""
            ()
            
        testCase "there can be multiple edges between the same vertices" <| fun _ ->
            let graph: DirectedLNGraph = makeTestGraph()
            // A --> B, A --> D
            Expect.equal (graph.OutgoingEdgesOf(a).Length) 2 ""
            
            // add a new edge a --> b but with different channel update and a different ShortChannelId
            let newEdgeForNewChannel =
                makeUpdate(15UL, a, b, LNMoney.MilliSatoshis(20L), 0u, None, None, None)
                |> GraphLabel.Create
            let mutatedGraph = graph.AddEdge(newEdgeForNewChannel)
            
            Expect.equal (mutatedGraph.OutgoingEdgesOf(a).Length) 3 ""
            
            // if the ShortChannelId is the same we replace the edge and the update
            // this edge have an update with a different 'feeBaseMSat'
            let edgeForTheSameChannel =
               makeUpdate(15UL, a, b, LNMoney.MilliSatoshis(30L), 0u, None, None, None)
               |> GraphLabel.Create
            let mutatedGraph2 = mutatedGraph.AddEdge(edgeForTheSameChannel)
            Expect.equal (mutatedGraph2.OutgoingEdgesOf a).Length 3 ""
            Expect.equal (mutatedGraph2.GetEdgesBetween(a, b).Length) 2 ""
            Expect.equal
                (mutatedGraph2.TryGetEdge(edgeForTheSameChannel.Desc).Value.Update.FeeBaseMSat)
                (LNMoney.MilliSatoshis(30L)) ""
            
        testCase "remove a vertex with incoming edges and check those edges are removed too" <| fun _ ->
            let graph = makeTestGraph()
            Expect.equal  (graph.VertexSet().Length) 5 ""
            Expect.isTrue (graph.ContainsVertex(e)) ""
            Expect.isTrue (graph.ContainsEdge(descFromNodes(5, c, e))) ""
            Expect.isTrue (graph.ContainsEdge(descFromNodes(6, b, e))) ""
            
            let withoutE = graph.RemoveVertex(e)
            Expect.equal (withoutE.VertexSet().Length) 4 ""
            Expect.isFalse (withoutE.ContainsEdge(descFromNodes(5, c, e))) ""
            Expect.isFalse (withoutE.ContainsEdge(descFromNodes(6, b, e))) ""
    ]
