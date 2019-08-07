module Channel.WaitForAcceptChannelStateTests

open Expecto
open Expecto.Logging
open Expecto.Logging.Message
open DotNetLightning.LN
open DotNetLightning.Serialize.Msgs

let logger = Log.create "Sphinx tests"
let logCore = eventX >> logger.info 
let log level = logCore


[<Tests>]
let tests =
    testList "" [
        testCase "" <| fun _ ->
            let aliceChannel = ChannelTestUtils.Constants.alice
            let aliceInit = { Init.GlobalFeatures = aliceChannel
                              LocalFeatures = failwith "Not Implemented" }
            ()
    ]