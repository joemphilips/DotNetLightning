module TLVSerialize

open System
open System.IO
open System.Text.Json

open Expecto

open DotNetLightning.Serialization

[<Tests>]
let bigSizeVarIntTests =
    let hex = NBitcoin.DataEncoders.HexEncoder()
    let dataPath1 = Path.Join(AppDomain.CurrentDomain.BaseDirectory, "../../..", ("Data/bolt1-bigsize.json"))
    let testData1 = dataPath1 |> File.ReadAllText |> JsonDocument.Parse

    testList "BigSize encoding tests" [
        let successTestCases =
            testData1.RootElement.EnumerateArray()
            |> Seq.choose(fun x -> match x.TryGetProperty("exp_error") with true, _ -> None | false, _ -> Some x)
        for v in successTestCases do
            yield testCase (v.GetProperty("name").GetString()) <| fun _ ->
                use wms = new MemoryStream()
                use writer = new LightningWriterStream(wms)
                let value = v.GetProperty("value").GetUInt64()
                let b = v.GetProperty("bytes").GetString() |> hex.DecodeData
                let actualBytes =
                    writer.WriteBigSize(value)
                    wms.ToArray()
                Expect.equal (actualBytes) b "failed to write"
                
                use rms = new MemoryStream(b)
                use reader = new LightningReaderStream(rms)
                let actualValue = reader.ReadBigSize()
                Expect.equal actualValue value "failed to read"
                
        let failureTestCases =
            testData1.RootElement.EnumerateArray()
            |> Seq.choose(fun x -> match x.TryGetProperty("exp_error") with true, _ -> Some(x) | false, _ -> None)
            
        for v in failureTestCases do
            yield testCase (v.GetProperty("name").GetString()) <| fun _ ->
                use wms = new MemoryStream()
                use writer = new LightningWriterStream(wms)
                
                let b = v.GetProperty("bytes").GetString() |> hex.DecodeData
                use rms = new MemoryStream(b)
                use reader = new LightningReaderStream(rms)
                let isEOFError = v.GetProperty("exp_error").GetString().Contains("EOF")
                if (isEOFError) then
                    Expect.throwsT<System.IO.EndOfStreamException>
                        (fun _ -> reader.ReadBigSize() |> ignore) "should throw EOF exception"
                else
                    Expect.throwsT<FormatException>
                        (fun _ -> reader.ReadBigSize() |> ignore) "should throw Format exception"
                        
                        
        yield testProperty "Should encode-decode" <| fun (v: uint64) ->
            use wms = new MemoryStream()
            use writer = new LightningWriterStream(wms)
            writer.WriteBigSize(v)
            
            use rms = new MemoryStream(wms.ToArray())
            use reader = new LightningReaderStream(rms)
            let actual = reader.ReadBigSize()
            Expect.equal actual v ""
    ]
    
let bolt4Tests2 =
    // let dataPath1 = Path.Join(AppDomain.CurrentDomain.BaseDirectory, "../../..", ("Data/bolt04/onion-test-multi-frame.json"))
    // let testData1 = dataPath1 |> File.ReadAllText |> JsonDocument.Parse
    testList "bolt04 test vectors" [
        testCase "" <| fun  _ ->
            ()
    ]
