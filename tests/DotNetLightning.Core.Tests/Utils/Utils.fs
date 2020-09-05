[<AutoOpen>]
module Utils

open System
open Expecto

let CheckArrayEqual (actual: 'a array) (expected: 'a array) =
    let mutable index = 0
    try
        for offset in seq { for x in 1..Int32.MaxValue do if x % 50 = 0 then yield x} do
            index <- offset
            Expect.equal actual.[offset..(offset + 50)] expected.[offset..(offset + 50)] (sprintf "failed in %d ~ %d" offset (offset + 50))
    with
    | :? IndexOutOfRangeException -> 
        try
            Expect.equal actual.[(actual.Length - 50)..(actual.Length - 1)] expected.[(actual.Length - 50)..(actual.Length - 1)] (sprintf "failed in last 50 of actual: %d (expected length was %d)" (actual.Length) (expected.Length))
            Expect.equal actual expected ""
        with
        | :? IndexOutOfRangeException ->
            Expect.equal actual.[(expected.Length - 50)..(expected.Length - 1)] expected.[(expected.Length - 50)..(expected.Length - 1)] (sprintf "failed in last 50 of expected: %d (actual length was %d)" (expected.Length) (actual.Length))
            Expect.equal actual expected ""