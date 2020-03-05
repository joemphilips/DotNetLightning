module RouteCalculationTests

open NBitcoin
open NBitcoin.DataEncoders
open Expecto

open DotNetLightning.Routing
open DotNetLightning.Routing.Graph
open DotNetLightning.Utils
open DotNetLightning.Utils
open DotNetLightning.Utils.Primitives
open GraphTests
open GraphTests.Constants
open ResultUtils

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

[<Tests>]
let tests = testList "Route Calculation" [
    testCase "Calculate simple route" <| fun _ ->
        let updates = [
                makeUpdate(1UL, a, b, LNMoney.MilliSatoshis(1L), 10u, None, None, BlockHeightOffset.One |> Some)
                makeUpdate(2UL, b, c, LNMoney.MilliSatoshis(1L), 10u, None, None, BlockHeightOffset.One |> Some)
                makeUpdate(3UL, c, d, LNMoney.MilliSatoshis(1L), 10u, None, None, BlockHeightOffset.One |> Some)
                makeUpdate(4UL, d, e, LNMoney.MilliSatoshis(1L), 10u, None, None, BlockHeightOffset.One |> Some)
            ]
        
        let g = DirectedLNGraph.Create().AddEdges(updates)
        let route =
            Routing.findRoute (g) (a) (e) DEFAULT_AMOUNT_MSAT 1 (Set.empty) (Set.empty) (Set.empty) DEFAULT_ROUTE_PARAMS (BlockHeight 400000u)
            |> Result.deref
        Expect.sequenceEqual (route |> hops2Ids) ([1UL; 2UL; 3UL; 4UL]) ""
]

