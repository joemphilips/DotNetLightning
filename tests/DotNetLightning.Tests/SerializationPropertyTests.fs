module SerializationPropertyTests
open Expecto
open DotNetLightning.Serialize.Msgs
open Generators

let config =
    { FsCheckConfig.defaultConfig with
            arbitrary = [ typeof<P2PMsgGenerators> ]
        }

[<Tests>]
let tests =
    testList "SerializationPropertyTest" [
        testPropertyWithConfig config "init" <| fun (msg: Init) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "ping" <| fun (msg: Ping) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "pong" <| fun (msg: Pong) ->
            Expect.equal (msg.Clone()) (msg) ""

        testPropertyWithConfig config "open_channel" <| fun (msg: OpenChannel) ->
            Expect.equal (msg.Clone()) (msg) ""
    ]