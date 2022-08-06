[<AutoOpen>]
module DotNetLightning.ClnRpc.Tests.Helpers

open System.Collections.Generic
open System.IO
open System.IO.Pipelines
open System.Text
open System.Text.Json
open DotNetLightning.ClnRpc.Plugin
open DotNetLightning.Serialization


let utf8 = UTF8Encoding.UTF8

let inline flatten(s: string) =
    s |> JsonSerializer.Deserialize<JsonDocument> |> JsonSerializer.Serialize

let initStr =
    $"""
  {{
    "id": 0,
    "method": "init",
    "jsonrpc": "2.0",
    "params": {{
      "options": {{
        "greeting": "World",
        "number": [0]
      }},
      "configuration": {{
        "lightning-dir": "/home/user/.lightning/testnet",
        "rpc-file": "lightning-rpc",
        "startup": true,
        "network": "testnet",
        "feature_set": {{
            "init": "02aaa2",
            "node": "8000000002aaa2",
            "channel": "",
            "invoice": "028200"
        }},
        "proxy": {{
            "type": "ipv4",
            "address": "127.0.0.1",
            "port": 9050
        }},
        "torv3-enabled": true,
        "always_use_proxy": false
      }}
    }}
  }}
  """

let initB = initStr |> flatten |> utf8.GetBytes

let initConfigDTO =
    {
        LightningInitConfigurationDTO.Network = "testnet"
        LightningDir = "/path/to/lightning_dir"
        RpcFile = "lightning-rpc"
        Startup = true
        FeatureSet =
            {
                FeatureSetDTO.Channel = None
                Init = None
                Node = "02aaa2" |> FeatureBits.ParseHexUnsafe |> Some
                Invoice = "02aaa2" |> FeatureBits.ParseHexUnsafe |> Some
            }
        Proxy =
            {
                ProxyDTO.Address = "localhost"
                Ty = "ipv4"
                Port = 1000
            }
        TorV3Enabled = true
        AlwaysUseProxy = false
    }

let initOptionsDTO = Dictionary<string, obj>()

let initDTO =
    {
        Configuration = initConfigDTO
        Options = initOptionsDTO
    }

let newlineB = "\n\n" |> utf8.GetBytes

let setupRawStream<'T when 'T :> PluginServerBase>
    (
        p: 'T,
        msgs: byte [] seq,
        ct
    ) =
    task {
        let outStream =
            let outS = new MemoryStream()

            // fsharplint:disable
            PipeWriter
                .Create(outS, StreamPipeWriterOptions(leaveOpen = false))
                .AsStream()
        // fsharplint:enable

        let buf =
            Array.concat
                [
                    for m in msgs do
                        yield! [ m; newlineB ]
                ]

        let inStream =
            let inMemStream = new MemoryStream(buf)

            // fsharplint:disable
            PipeReader
                .Create(inMemStream, StreamPipeReaderOptions(leaveOpen = false))
                .AsStream()
        // fsharplint:enable

        let! _listener = p.StartAsync(outStream, inStream, ct)
        inStream.Flush()
        return p
    }

open NBitcoin
open DotNetLightning.ClnRpc.SystemTextJsonConverters
open DotNetLightning.ClnRpc.NewtonsoftJsonConverters
open Xunit

let internal serializationTestRoundtrip<'T> v =
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

    let v3 =
        Newtonsoft.Json.JsonConvert.SerializeObject(v, opts)
        |> fun o -> Newtonsoft.Json.JsonConvert.DeserializeObject<'T>(o, opts)

    Assert.Equal(v, v3)
    Assert.Equal(v2, v3)
    ()
