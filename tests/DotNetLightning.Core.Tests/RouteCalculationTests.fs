module RouteCalculationTests

open NBitcoin
open NBitcoin.DataEncoders
open Expecto
open ResultUtils

open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Routing
open DotNetLightning.Routing.Graph

open DotNetLightning.Payment
open DotNetLightning.Routing.RouterPrimitives
open DotNetLightning.Utils
open DotNetLightning.Utils.Primitives
open DotNetLightning.Utils.Primitives
open DotNetLightning.Utils.Primitives
open DotNetLightning.Utils.Primitives
open GraphTests
open GraphTests.Constants

let hex = Encoders.Hex

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

let hops2Ids (route: seq<ChannelHop>) =
    route |> Seq.map(fun hop -> hop.LastUpdateValue.ShortChannelId.ToBytes() |> fun x -> NBitcoin.Utils.ToUInt64(x, false))

let hops2Edges (route: ChannelHop seq) =
    route
    |> Seq.map(fun h ->
        { GraphLabel.Desc =
            { ShortChannelId = h.LastUpdateValue.ShortChannelId
              A = h.NodeIdValue
              B = h.NodeIdValue }
          Update = h.LastUpdateValue })
[<Tests>]
let tests = ftestList "Route Calculation" [
    let calculateRouteSimple routeParams =
        let updates = [
                makeUpdate(1UL, a, b, LNMoney.MilliSatoshis(1L), 10u, None, None, BlockHeightOffset.One |> Some)
                makeUpdate(2UL, b, c, LNMoney.MilliSatoshis(1L), 10u, None, None, BlockHeightOffset.One |> Some)
                makeUpdate(3UL, c, d, LNMoney.MilliSatoshis(1L), 10u, None, None, BlockHeightOffset.One |> Some)
                makeUpdate(4UL, d, e, LNMoney.MilliSatoshis(1L), 10u, None, None, BlockHeightOffset.One |> Some)
            ]
        
        let g = DirectedLNGraph.Create().AddEdges(updates)
        let route =
            Routing.findRoute (g) (a) (e) DEFAULT_AMOUNT_MSAT 1 (Set.empty) (Set.empty) (Set.empty) routeParams (BlockHeight 400000u)
            |> Result.deref
        Expect.sequenceEqual (route |> hops2Ids) ([1UL; 2UL; 3UL; 4UL]) ""
    testCase "Calculate simple route" <| fun _ ->
        calculateRouteSimple  DEFAULT_ROUTE_PARAMS
        
    testCase "Check fee against max pct properly" <| fun _ ->
        // fee is acceptable if it is either
        // - below our maximum fee base
        // - below our maximum fraction of the paid amount
        
        // here we have a maximum fee base of 1 msat, and all our updates have a base fee of 10 msat
        // so our fee will always be above the base fee, and we will always check that it is below our maximum percentage
        // of the amount being paid
        calculateRouteSimple { DEFAULT_ROUTE_PARAMS with MaxFeeBase = LNMoney.One }
        
    testCase "Calculate the shortest path (with correct fees)" <| fun _ ->
        let amount = LNMoney.MilliSatoshis(10000L)
        let expectedCost = 10007L |> LNMoney.MilliSatoshis
        let updates = [
            makeUpdate(1UL, a, b, LNMoney.One, 200u, Some(LNMoney.Zero), None, None)
            makeUpdate(4UL, a, e, LNMoney.One, 200u, Some(LNMoney.Zero), None, None)
            makeUpdate(2UL, b, c, LNMoney.One, 300u, Some(LNMoney.Zero), None, None)
            makeUpdate(3UL, c, d, LNMoney.One, 400u, Some(LNMoney.Zero), None, None)
            makeUpdate(3UL, e, f, LNMoney.One, 400u, Some(LNMoney.Zero), None, None)
            makeUpdate(3UL, f, d, LNMoney.One, 100u, Some(LNMoney.Zero), None, None)
        ]
        let graph = DirectedLNGraph.Create().AddEdges(updates)
        let route =
            Routing.findRoute graph a d amount 1 (Set.empty) (Set.empty) (Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
            |> Result.deref
        let totalCost = Graph.pathWeight(hops2Edges(route)) (amount) false BlockHeight.Zero None |> fun x -> x.Cost
        Expect.sequenceEqual (hops2Ids(route)) [4UL; 5UL; 6UL] ""
        
        /// Now channel 5 could route the amount (10000) but not the amount + fees (10007)
        let (desc, update) = makeUpdate(5UL, e, f, LNMoney.One, 400u, Some(LNMoney.Zero), Some(LNMoney.MilliSatoshis(10005L)), None)
        let graph1 = graph.AddEdge(desc, update)
        let route1 =
            Routing.findRoute(graph1) a d amount 1  (Set.empty) (Set.empty) (Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
            |> Result.deref
        Expect.sequenceEqual (hops2Ids(route1)) [1UL; 2UL; 3UL] ""
        
    testCase "calculate route considering the direct channel pays no fees" <| fun _ ->
        let updates = [
            makeUpdate(1UL, a, b, LNMoney.MilliSatoshis(5L), 0u, None, None, None)
            makeUpdate(2UL, a, d, LNMoney.MilliSatoshis(15L), 0u, None, None, None)
            makeUpdate(3UL, b, c, LNMoney.MilliSatoshis(5L), 0u, None, None, None)
            makeUpdate(4UL, c, d, LNMoney.MilliSatoshis(5L), 0u, None, None, None)
            makeUpdate(5UL, d, e, LNMoney.MilliSatoshis(5L), 0u, None, None, None)
        ]
        let g = DirectedLNGraph.Create().AddEdges(updates)
        let route =
            Routing.findRoute(g) a e DEFAULT_AMOUNT_MSAT 1 (Set.empty) (Set.empty) (Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
            |> Result.deref
        Expect.sequenceEqual (hops2Ids(route)) [2UL;5UL] ""
        
    testCase "Calculate simple route (add and remove edges)" <| fun _ ->
        let updates = [
            makeUpdateSimple(1UL, a, b)
            makeUpdateSimple(2UL, b, c)
            makeUpdateSimple(3UL, c, d)
            makeUpdateSimple(4UL, d, e)
        ]
        let g = DirectedLNGraph.Create().AddEdges(updates)
        let route1 =
            Routing.findRoute g a e DEFAULT_AMOUNT_MSAT 1 (Set.empty)(Set.empty)(Set.empty)DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
            |> Result.deref
            
        Expect.sequenceEqual (hops2Ids(route1)) [1UL; 2UL; 3UL; 4UL;] ""
        
        let graphWithRemovedEdge = g.RemoveEdge({ ChannelDesc.ShortChannelId = ShortChannelId.FromUInt64(3UL); A = c; B = d })
        
        let route2 =
            Routing.findRoute(graphWithRemovedEdge) a e DEFAULT_AMOUNT_MSAT 1 (Set.empty) (Set.empty)(Set.empty)DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
        Expect.isError (route2) ""
        
    testCase "calculate the shortest path (select direct channel)" <| fun _ ->
        let updates = [
            makeUpdate(1UL, a, b, LNMoney.Zero, 0u, None, None, None)
            makeUpdate(4UL, a, d, LNMoney.MilliSatoshis(50L), 0u, None, None, None)
            makeUpdate(2UL, b, c, LNMoney.MilliSatoshis(0L), 0u, None, None, None)
            makeUpdate(3UL, c, d, LNMoney.MilliSatoshis(0L), 0u, None, None, None)
        ]
        let graph = DirectedLNGraph.Create().AddEdges(updates)
        let route =
            Routing.findRoute(graph) a d DEFAULT_AMOUNT_MSAT 2 (Set.empty) (Set.empty) (Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
            |> Result.deref
        Expect.sequenceEqual (hops2Ids(route)) [4UL] ""
        
    let (f, g, h, i) = (
        "02999fa724ec3c244e4da52b4a91ad421dc96c9a810587849cd4b2469313519c73" |> (hex.DecodeData >> PubKey >> NodeId), // F source
        "03f1cb1af20fe9ccda3ea128e27d7c39ee27375c8480f11a87c17197e97541ca6a" |> (hex.DecodeData >> PubKey >> NodeId), // G
        "0358e32d245ff5f5a3eb14c78c6f69c67cea7846bdf9aeeb7199e8f6fbb0306484" |> (hex.DecodeData >> PubKey >> NodeId), // H
        "029e059b6780f155f38e83601969919aae631ddf6faed58fe860c72225eb327d7c" |> (hex.DecodeData >> PubKey >> NodeId) // I target
        )
    testCase "find a route using channels with hltcMaximumMsat close to the payment amount" <| fun _ ->
        let updates = [
            makeUpdate(1UL, f, g, LNMoney.One, 0u, None, None, None)
            makeUpdate(2UL, g, h, LNMoney.One, 0u, None, Some(DEFAULT_AMOUNT_MSAT + LNMoney.MilliSatoshis(50L)), None)
            makeUpdate(3UL, h, i, LNMoney.One, 0u, None, None, None)
        ]
        let graph = DirectedLNGraph.Create().AddEdges(updates)
        let route =
            Routing.findRoute graph f i DEFAULT_AMOUNT_MSAT 1 (Set.empty)(Set.empty)(Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
            |> Result.deref
        Expect.sequenceEqual (hops2Ids(route)) [1UL; 2UL; 3UL] ""
        
    testCase "find a route using channels with htlcMinimumMsat close to the payment amount" <| fun _ ->
        let updates = [
            makeUpdate(1UL, f, g, LNMoney.One, 0u, None, None, None)
            makeUpdate(2UL, g, h, LNMoney.One, 0u, Some(DEFAULT_AMOUNT_MSAT + LNMoney.MilliSatoshis(50L)), None, None)
            makeUpdate(3UL, h, i, LNMoney.One, 0u, None, None, None)
        ]
        let graph = DirectedLNGraph.Create().AddEdges(updates)
        let route =
            Routing.findRoute graph f i DEFAULT_AMOUNT_MSAT 1 (Set.empty)(Set.empty)(Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
        Expect.isError (route) ""
        
    testCase "if there are multiple channels between the same node, select the cheapest" <| fun _ ->
        let updates = [
            makeUpdate(1UL, f, g, LNMoney.Zero, 0u, None, None, None)
            makeUpdate(2UL, g, h, LNMoney.MilliSatoshis(5L), 5u, None, None, None) // expensive g -> h channel
            makeUpdate(6UL, g, h, LNMoney.MilliSatoshis(0L), 0u, None, None, None) // cheap     g -> h channel
            makeUpdate(3UL, h, i, LNMoney.MilliSatoshis(0L), 0u, None, None, None)
        ]
        let graph = DirectedLNGraph.Create().AddEdges(updates)
        let route =
            Routing.findRoute graph f i DEFAULT_AMOUNT_MSAT 1 (Set.empty)(Set.empty)(Set.empty)DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
            |> Result.deref
        Expect.sequenceEqual(hops2Ids(route)) [1UL;6UL;3UL] ""
        
    testCase "Calculate longer but cheaper route" <| fun _ ->
        let updates = [
            makeUpdateSimple(1UL, a, b)
            makeUpdateSimple(2UL, b, c)
            makeUpdateSimple(3UL, c, d)
            makeUpdateSimple(4UL, d, e)
            makeUpdate(5UL, b, e, LNMoney.MilliSatoshis(10L), 10u, None, None, None)
        ]
        let g = DirectedLNGraph.Create().AddEdges(updates)
        let route =
            Routing.findRoute g a e DEFAULT_AMOUNT_MSAT 1 (Set.empty) (Set.empty) (Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
            |> Result.deref
        Expect.sequenceEqual (hops2Ids(route)) [1UL;2UL;3UL;4UL;] ""
    testCase "no local channels" <| fun _ ->
        let updates = [
            makeUpdateSimple(2UL, b, c)
            makeUpdateSimple(4UL, d, e)
        ]
        
        let g = DirectedLNGraph.Create().AddEdges(updates)
        let route = Routing.findRoute(g) a e DEFAULT_AMOUNT_MSAT 1 (Set.empty)(Set.empty)(Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
        Expect.isError route ""
        
    testCase "route not found (source OR target node not connected)" <| fun _ ->
        let updates = [
            makeUpdateSimple(2UL, b, c)
            makeUpdateSimple(4UL, c, d)
        ]
        let g = DirectedLNGraph.Create().AddEdges(updates).AddVertex(a).AddVertex(e)
        Expect.isError(Routing.findRoute g a d DEFAULT_AMOUNT_MSAT 1 (Set.empty)(Set.empty)(Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))) ""
        Expect.isError(Routing.findRoute g b e DEFAULT_AMOUNT_MSAT 1 (Set.empty)(Set.empty)(Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))) ""
        
    testCase "route not found (amount too high OR too low)" <| fun _ ->
        let highAmount = DEFAULT_AMOUNT_MSAT * 10
        let lowAmount = DEFAULT_AMOUNT_MSAT / 10
        let updatesHi = [
            makeUpdateSimple(1UL, a, b)
            makeUpdate(2UL, b, c, LNMoney.Zero, 0u, None, Some(DEFAULT_AMOUNT_MSAT), None)
            makeUpdateSimple(3UL, c, d)
        ]
        let updatesLow = [
            makeUpdateSimple(1UL, a, b)
            makeUpdate(2UL, b, c, LNMoney.Zero, 0u, Some(DEFAULT_AMOUNT_MSAT), None, None)
            makeUpdateSimple(3UL, c, d)
        ]
        
        let gHigh = DirectedLNGraph.Create().AddEdges(updatesHi)
        let gLow = DirectedLNGraph.Create().AddEdges(updatesLow)
        
        Expect.isError (Routing.findRoute gHigh a d highAmount 1 (Set.empty)(Set.empty)(Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))) ""
        Expect.isError (Routing.findRoute gLow a d lowAmount 1 (Set.empty)(Set.empty)(Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))) ""
        
    testCase "route to self" <| fun _ ->
        let updates = [
            makeUpdateSimple(1UL, a, b)
            makeUpdateSimple(2UL, b, c)
            makeUpdateSimple(3UL, c, d)
        ]
        
        let g = DirectedLNGraph.Create().AddEdges(updates)
        let route =
            Routing.findRoute g a a DEFAULT_AMOUNT_MSAT 1 (Set.empty)(Set.empty)(Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
        Expect.isError route ""
        
    testCase "route to immediate neighbor" <| fun _ ->
        let updates = [
            makeUpdateSimple(1UL, a, b)
            makeUpdateSimple(2UL, b, c)
            makeUpdateSimple(3UL, c, d)
            makeUpdateSimple(4UL, d, e)
        ]
        
        let g = DirectedLNGraph.Create().AddEdges(updates)
        let route =
            Routing.findRoute(g) a b DEFAULT_AMOUNT_MSAT 1 (Set.empty)(Set.empty)(Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
            |> Result.deref
        Expect.sequenceEqual (hops2Ids(route)) [1UL] ""
        
    testCase "directed graph" <| fun _ ->
        let updates = [
            makeUpdateSimple(1UL, a, b)
            makeUpdateSimple(2UL, b, c)
            makeUpdateSimple(3UL, c, d)
            makeUpdateSimple(4UL, d, e)
        ]
        // a -> e works, e -> a fails
        let g = DirectedLNGraph.Create().AddEdges(updates)
        let route1 =
            Routing.findRoute g a e DEFAULT_AMOUNT_MSAT 1 (Set.empty)(Set.empty)(Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
            |> Result.deref
            
        Expect.sequenceEqual (hops2Ids(route1)) [1UL; 2UL; 3UL; 4UL] ""
        let route2 =
            Routing.findRoute g e e DEFAULT_AMOUNT_MSAT 1 (Set.empty)(Set.empty)(Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
        Expect.isError route2 ""
        
    testCase "calculate route and return metadata" <| fun _ ->
        let uab =
            { UnsignedChannelUpdate.ChainHash = Network.RegTest.GenesisHash
              ShortChannelId = ShortChannelId.FromUInt64(1UL)
              Timestamp = 0u
              MessageFlags = 0uy
              ChannelFlags = 0uy
              CLTVExpiryDelta = BlockHeightOffset(1us)
              HTLCMinimumMSat = LNMoney.MilliSatoshis(42L)
              FeeBaseMSat = LNMoney.MilliSatoshis(2500L)
              FeeProportionalMillionths = 140u
              HTLCMaximumMSat = None }
        let uba =
            { UnsignedChannelUpdate.ChainHash = Network.RegTest.GenesisHash
              ShortChannelId = ShortChannelId.FromUInt64(1UL)
              Timestamp = 1u
              MessageFlags = 0uy
              ChannelFlags = 1uy
              CLTVExpiryDelta = BlockHeightOffset(1us)
              HTLCMinimumMSat = LNMoney.MilliSatoshis(43L)
              FeeBaseMSat = LNMoney.MilliSatoshis(2501L)
              FeeProportionalMillionths = 141u
              HTLCMaximumMSat = None }
        let ubc =
            { UnsignedChannelUpdate.ChainHash = Network.RegTest.GenesisHash
              ShortChannelId = ShortChannelId.FromUInt64(2UL)
              Timestamp = 1u
              MessageFlags = 0uy
              ChannelFlags = 0uy
              CLTVExpiryDelta = BlockHeightOffset(1us)
              HTLCMinimumMSat = LNMoney.MilliSatoshis(44L)
              FeeBaseMSat = LNMoney.MilliSatoshis(2502L)
              FeeProportionalMillionths = 142u
              HTLCMaximumMSat = None }
        let ucb =
            { UnsignedChannelUpdate.ChainHash = Network.RegTest.GenesisHash
              ShortChannelId = ShortChannelId.FromUInt64(2UL)
              Timestamp = 1u
              MessageFlags = 0uy
              ChannelFlags = 1uy
              CLTVExpiryDelta = BlockHeightOffset(1us)
              HTLCMinimumMSat = LNMoney.MilliSatoshis(45L)
              FeeBaseMSat = LNMoney.MilliSatoshis(2503L)
              FeeProportionalMillionths = 143u
              HTLCMaximumMSat = None }
        let ucd =
            { UnsignedChannelUpdate.ChainHash = Network.RegTest.GenesisHash
              ShortChannelId = ShortChannelId.FromUInt64(3UL)
              Timestamp = 1u
              MessageFlags = 1uy
              ChannelFlags = 0uy
              CLTVExpiryDelta = BlockHeightOffset(1us)
              HTLCMinimumMSat = LNMoney.MilliSatoshis(46L)
              FeeBaseMSat = LNMoney.MilliSatoshis(2504L)
              FeeProportionalMillionths = 144u
              HTLCMaximumMSat = Some(LNMoney.MilliSatoshis(500000000L)) }
        let udc =
            { UnsignedChannelUpdate.ChainHash = Network.RegTest.GenesisHash
              ShortChannelId = ShortChannelId.FromUInt64(3UL)
              Timestamp = 1u
              MessageFlags = 0uy
              ChannelFlags = 1uy
              CLTVExpiryDelta = BlockHeightOffset(1us)
              HTLCMinimumMSat = LNMoney.MilliSatoshis(47L)
              FeeBaseMSat = LNMoney.MilliSatoshis(2505L)
              FeeProportionalMillionths = 145u
              HTLCMaximumMSat = None }
        let ude =
            { UnsignedChannelUpdate.ChainHash = Network.RegTest.GenesisHash
              ShortChannelId = ShortChannelId.FromUInt64(4UL)
              Timestamp = 1u
              MessageFlags = 0uy
              ChannelFlags = 0uy
              CLTVExpiryDelta = BlockHeightOffset(1us)
              HTLCMinimumMSat = LNMoney.MilliSatoshis(48L)
              FeeBaseMSat = LNMoney.MilliSatoshis(2506L)
              FeeProportionalMillionths = 146u
              HTLCMaximumMSat = None }
        let ued =
            { UnsignedChannelUpdate.ChainHash = Network.RegTest.GenesisHash
              ShortChannelId = ShortChannelId.FromUInt64(4UL)
              Timestamp = 1u
              MessageFlags = 0uy
              ChannelFlags = 1uy
              CLTVExpiryDelta = BlockHeightOffset(1us)
              HTLCMinimumMSat = LNMoney.MilliSatoshis(49L)
              FeeBaseMSat = LNMoney.MilliSatoshis(2507L)
              FeeProportionalMillionths = 147u
              HTLCMaximumMSat = None }
        let updates =
            Map.empty
            |> Map.add ({ChannelDesc.ShortChannelId = ShortChannelId.FromUInt64(1UL); A = a; B = b}) uab
            |> Map.add ({ChannelDesc.ShortChannelId = ShortChannelId.FromUInt64(1UL); A = b; B = a}) uba
            |> Map.add ({ChannelDesc.ShortChannelId = ShortChannelId.FromUInt64(2UL); A = b; B = c}) ubc
            |> Map.add ({ChannelDesc.ShortChannelId = ShortChannelId.FromUInt64(2UL); A = c; B = b}) ucb
            |> Map.add ({ChannelDesc.ShortChannelId = ShortChannelId.FromUInt64(3UL); A = c; B = d}) ucd
            |> Map.add ({ChannelDesc.ShortChannelId = ShortChannelId.FromUInt64(3UL); A = d; B = c}) udc
            |> Map.add ({ChannelDesc.ShortChannelId = ShortChannelId.FromUInt64(4UL); A = d; B = e}) ude
            |> Map.add ({ChannelDesc.ShortChannelId = ShortChannelId.FromUInt64(4UL); A = e; B = d}) ued
            
        let g = DirectedLNGraph.Create().AddEdges(updates |> Map.toSeq)
        let hops =
            Routing.findRoute g a e DEFAULT_AMOUNT_MSAT 1 (Set.empty)(Set.empty)(Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
            |> Result.deref
        let e = [
            (ChannelHop.Create(a, b, uab))
            (ChannelHop.Create(b, c, ubc))
            (ChannelHop.Create(c, d, ucd))
            (ChannelHop.Create(d, e, ude))
        ]
        Expect.sequenceEqual hops e ""
        
    testCase "convert extra hops to assisted channels" <| fun _ ->
        let extraHop1 = { ExtraHop.NodeId = a
                          ShortChannelId = ShortChannelId.FromUInt64(1UL)
                          FeeBase = LNMoney.MilliSatoshis(12L)
                          FeeProportionalMillionths = 10000u
                          CLTVExpiryDelta = BlockHeightOffset(12us) }
        let extraHop2 = { ExtraHop.NodeId = b
                          ShortChannelId = ShortChannelId.FromUInt64(2UL)
                          FeeBase = LNMoney.MilliSatoshis(200L)
                          FeeProportionalMillionths = 0u
                          CLTVExpiryDelta = BlockHeightOffset(22us) }
        let extraHop3 = { ExtraHop.NodeId = c
                          ShortChannelId = ShortChannelId.FromUInt64(3UL)
                          FeeBase = LNMoney.MilliSatoshis(150L)
                          FeeProportionalMillionths = 0u
                          CLTVExpiryDelta = BlockHeightOffset(32us) }
        let extraHop4 = { ExtraHop.NodeId = d
                          ShortChannelId = ShortChannelId.FromUInt64(4UL)
                          FeeBase = LNMoney.MilliSatoshis(50L)
                          FeeProportionalMillionths = 0u
                          CLTVExpiryDelta = BlockHeightOffset(42us) }
        let extraHops = [ extraHop1; extraHop2; extraHop3; extraHop4 ]
        let amount = LNMoney.Satoshis(900L) // below RoutingHeuristics.CAPACITY_CHANNEL_LOW
        let acs = Routing.toAssistedChannels e amount extraHops |> Map.ofSeq
        Expect.equal  (acs.[extraHop4.ShortChannelId]) ({ AssistedChannel.ExtraHop = extraHop4; NextNodeId = e; HTLCMaximum = (LNMoney.MilliSatoshis(1050L)) }) ""
        Expect.equal  (acs.[extraHop3.ShortChannelId]) ({ AssistedChannel.ExtraHop = extraHop3; NextNodeId = d; HTLCMaximum = (LNMoney.MilliSatoshis(1200L)) }) ""
        Expect.equal  (acs.[extraHop2.ShortChannelId]) ({ AssistedChannel.ExtraHop = extraHop2; NextNodeId = c; HTLCMaximum = (LNMoney.MilliSatoshis(1400L)) }) ""
        Expect.equal  (acs.[extraHop1.ShortChannelId]) ({ AssistedChannel.ExtraHop = extraHop1; NextNodeId = b; HTLCMaximum = (LNMoney.MilliSatoshis(1426L)) }) ""
        
    testCase "blacklist routes" <| fun _ ->
        let updates = [
            makeUpdateSimple(1UL, a, b)
            makeUpdateSimple(2UL, b, c)
            makeUpdateSimple(3UL, c, d)
            makeUpdateSimple(4UL, d, e)
        ]
        let g = DirectedLNGraph.Create().AddEdges(updates)
        let ignoredE = Set.singleton({ ShortChannelId = ShortChannelId.FromUInt64(3UL); A = c; B = d })
        let route1 = Routing.findRoute(g) a e DEFAULT_AMOUNT_MSAT 1 (Set.empty) (ignoredE) (Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
        Expect.isError (route1) ""
        
        // verify that we left the graph untouched
        Expect.isTrue(g.ContainsEdge(makeUpdateSimple(3UL, c, d) |> fst)) ""
        Expect.isTrue(g.ContainsVertex(c)) ""
        Expect.isTrue(g.ContainsVertex(d)) ""
        
        // make sure we can find a route without the blacklist
        let route2 =
            Routing.findRoute g a e DEFAULT_AMOUNT_MSAT 1 (Set.empty) (Set.empty) (Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
            |> Result.deref
        Expect.sequenceEqual(hops2Ids(route2)) [1UL; 2UL; 3UL; 4UL] ""
        
    testCase "route to a destination that is not in the graph (with assisted routes)" <| fun _ ->
        let updates = [
            makeUpdate(1UL, a, b, LNMoney.MilliSatoshis(10L), 10u, None, None, None)
            makeUpdate(2UL, b, c, LNMoney.MilliSatoshis(10L), 10u, None, None, None)
            makeUpdate(2UL, c, d, LNMoney.MilliSatoshis(10L), 10u, None, None, None)
        ]
        let g = DirectedLNGraph.Create().AddEdges(updates)
        let route =
            Routing.findRoute(g) a e DEFAULT_AMOUNT_MSAT 1 (Set.empty)(Set.empty)(Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
        Expect.isError(route) "there should be no e node in the graph"
        
        // now we add the missing edge to reach the destination
        let (extraDesc, extraUpdate) = makeUpdate(4UL, d, e, LNMoney.MilliSatoshis(5L), 5u, None, None, None)
        let extraGraphEdges = Set.singleton({ GraphLabel.Desc =extraDesc; Update = extraUpdate })
        let route1 =
            Routing.findRoute g a e DEFAULT_AMOUNT_MSAT 1 (extraGraphEdges) (Set.empty)(Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight(400000u))
            |> Result.deref
        Expect.sequenceEqual(hops2Ids(route1)) [1UL; 2UL; 3UL; 4UL] ""
        
    testCase "Verify that extra hops takes precedence over known channels" <| fun _ ->
        let updates = []
        ()
]
