namespace DotNetLightning.ClnRpc.Tests

open System.Text.Json
open DotNetLightning.ClnRpc
open Generators
open Xunit
open FsCheck
open FsCheck.Xunit
open NBitcoin
open DotNetLightning.ClnRpc.SystemTextJsonConverters
open NewtonsoftJsonConverters.NewtonsoftJsonHelpers

type SerializationTests() =

    do
        Arb.register<PrimitiveGenerators>() |> ignore

    member private this.Roundtrip<'T>(v: 'T) =
        // System.Text.Json
        let opts = JsonSerializerOptions()
        opts.AddDNLJsonConverters(Network.RegTest)
        let v2 =
            JsonSerializer.Serialize(v, opts)
            |> fun o -> JsonSerializer.Deserialize<'T>(o, opts)
        Assert.Equal(v, v2)
        
        // Newtonsoft.Json
        let opts = Newtonsoft.Json.JsonSerializerSettings()
        opts.AddDNLJsonConverters(Network.RegTest)
        let v2 =
            Newtonsoft.Json.JsonConvert.SerializeObject(v, opts)
            |> fun o -> Newtonsoft.Json.JsonConvert.DeserializeObject<'T>(o, opts)
        Assert.Equal(v, v2)
        ()

    [<Property>]
    [<Trait("PropTest", "PropTest")>]
    member this.ListchannelsRequest(v: Requests.ListchannelsRequest) =
        this.Roundtrip<_>(v)
        
    //[<Property>]
    //[<Trait("PropTest", "PropTest")>]
    //member this.ListchannelsResponse(v: Responses.ListchannelsResponse) =
      //  this.Roundtrip<_>(v)
