module ClaimReceivedHTLCTests


open System.Text
open System.Text.Unicode
open Expecto
open NBitcoin
open NBitcoin.Crypto
open NBitcoin.DataEncoders

let hex = Encoders.Hex
let utf8 = UTF8Encoding()
let base58 = Encoders.Base58
let n = Network.TestNet

let alice =
    let commitKey = "cVuzKWCszfvjkoJyUasvsrRdECriz8hSd1BDinRNzytwnXmX7m1g"  |> fun d ->  Key.Parse(d, n)
    let finalKey = "cRUfvpbRtMSqCFD1ADdvgPn5HfRLYuHCFYAr2noWnaRDNger2AoA" |> fun d -> Key.Parse(d, n)
    let r = Hashes.SHA256("This is Alice's R" |> utf8.GetBytes)
    let revokeCommit = "Alice foo" |> utf8.GetBytes
    {|
      CommitKey = commitKey
      FinalKey = finalKey
      CommitPubKey = commitKey.PubKey
      finalPubKey = finalKey.PubKey
      R = r
      RHash = Hashes.SHA256(r)
      RevokeCommit = revokeCommit
      RevokeCommitHash = Hashes.SHA256(revokeCommit)
     |}

[<Tests>]
let tests =
    testList "Claim Received HTLC Tests" [
        testCase "Should accept accept_channel" <| fun _ ->
            ()
    ]
