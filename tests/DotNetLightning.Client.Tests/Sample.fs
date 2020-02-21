module Tests

open System
open Expecto

open NBitcoin
open DotNetLightning.Client
open DotNetLightning.Client.CLightning

[<Tests>]
let tests =
  testList "c-lightning client" [
      let client = CLightningClient(Uri(""), Network.Main)
      testCase "can get info" <| fun _ ->
          ()
  ]
