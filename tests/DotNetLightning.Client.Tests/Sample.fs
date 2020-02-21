module Tests

open System
open Expecto

open NBitcoin
open DotNetLightning.Client
open DotNetLightning.Client.CLightning

[<Tests>]
let tests =
  testList "c-lightning client" [
      testCase "can get info" <| fun _ ->
          ()
  ]
