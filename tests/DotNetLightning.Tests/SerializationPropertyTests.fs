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
        testPropertyWithConfig config "ser" <| fun (msg: Init) ->
            Expect.equal (msg.Clone()) (msg) ""
    ]