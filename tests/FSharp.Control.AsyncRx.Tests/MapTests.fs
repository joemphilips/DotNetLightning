module Tests

open Expecto
open FSharp.Control.Tasks
open FSharp.Control

[<Tests>]
let tests =
  testList "Map tests" [
    testAsync "Test map async" {
      let mapper x = task {
        return x * 10
      }
      
      // let xs = AsyncRx.single 42 |> AsyncRx.mapAsync
      Expect.equal "" "" ""
    }
  ]
