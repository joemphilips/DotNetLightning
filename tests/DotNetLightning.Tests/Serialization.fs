module Serialization
open DotNetLightning.Utils.Msgs
open DotNetLightning.Utils.Primitives

open Expecto
open NBitcoin
open DotNetLightning.Utils
open System.IO.Pipelines
open System.IO

[<Tests>]
let tests =
  testList "SerializationTest" [
    testCase "Channel reestablish no secret" <| fun _ ->
      let cid = ChannelId (uint256([|4; 0; 0; 0; 0; 0; 0; 0; 5; 0; 0; 0; 0; 0; 0; 0; 6; 0; 0; 0; 0; 0; 0; 0; 7; 0; 0; 0; 0; 0; 0; 0|] |> Array.map((uint8)))) 
      let cr = {
          ChannelId = cid
          NextLocalCommitmentNumber = 3UL
          NextRemoteCommitmentNumber = 3UL
          DataLossProtect = None
          }
      let p = Pipe()
      use outputStream = new MemoryStream(p.Writer.GetMemory().ToArray())
      let actual = NetworkSerializer.Serialze outputStream cr
      let expected =
          [|4; 0; 0; 0; 0; 0; 0; 0; 5; 0; 0; 0; 0; 0; 0; 0; 6; 0; 0; 0; 0; 0; 0; 0; 7; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 3; 0; 0; 0; 0; 0; 0; 0; 4|] 
          |> Array.map(uint8)
      Expect.equal expected cr
      ()
  ]
