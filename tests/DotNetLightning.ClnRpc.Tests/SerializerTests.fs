namespace DotNetLightning.ClnRpc.Tests

open System
open System.IO
open System.Net
open System.Text.Json
open System.Threading.Tasks
open DotNetLightning.ClnRpc
open DotNetLightning.ClnRpc.Plugin
open DotNetLightning.ClnRpc.NewtonsoftJsonConverters
open DotNetLightning.ClnRpc.SystemTextJsonConverters
open DotNetLightning.Serialization
open NBitcoin
open Xunit

type SerializerTests() =
    [<Fact>]
    member this.DTOBindings() =
        let config =
            """
            {
                "lightning-dir": "/home/user/.lightning/testnet",
                "rpc-file": "lightning-rpc",
                "startup": true,
                "network": "testnet",
                "feature_set": {
                    "init": "02aaa2",
                    "node": "8000000002aaa2",
                    "invoice": "028200"
                },
                "proxy": {
                    "type": "ipv4",
                    "address": "127.0.0.1",
                    "port": 9050
                },
                "torv3-enabled": true,
                "always_use_proxy": false
            }
            """

        let settings = Newtonsoft.Json.JsonSerializerSettings()
        settings.AddDNLJsonConverters(Network.RegTest)

        let res =
            Newtonsoft.Json.JsonConvert.DeserializeObject<LightningInitConfigurationDTO>(
                config,
                settings
            )

        let expected =
            {
                LightningInitConfigurationDTO.LightningDir =
                    "/home/user/.lightning/testnet"
                RpcFile = "lightning-rpc"
                Startup = true
                Network = "testnet"
                FeatureSet =
                    {
                        FeatureSetDTO.Channel = None
                        Init = "02aaa2" |> FeatureBits.ParseHexUnsafe |> Some
                        Node =
                            "8000000002aaa2"
                            |> FeatureBits.ParseHexUnsafe
                            |> Some
                        Invoice = "028200" |> FeatureBits.ParseHexUnsafe |> Some
                    }
                Proxy =
                    {
                        Ty = "ipv4"
                        Address = "127.0.0.1"
                        Port = 9050
                    }
                TorV3Enabled = true
                AlwaysUseProxy = false
            }

        Assert.Equal(expected, res)

        // newtonsoft.json roundtrip
        let serializedExpected =
            Newtonsoft.Json.JsonConvert.SerializeObject(expected, settings)

        let serializedActual =
            Newtonsoft.Json.JsonConvert.SerializeObject(res, settings)

        Assert.Equal(serializedExpected, serializedActual)

        // SystemTextJson roundtrip
        let options = System.Text.Json.JsonSerializerOptions()
        options.AddDNLJsonConverters(Network.RegTest)

        let serializedExpected =
            System.Text.Json.JsonSerializer.Serialize(expected, options)

        let serializedActual =
            System.Text.Json.JsonSerializer.Serialize(res, options)

        Assert.Equal(serializedExpected, serializedActual)

    [<Fact>]
    member this.DeserializeListpeersResponse() =
        let data =
            Path.Join(
                AppDomain.CurrentDomain.BaseDirectory,
                "../../..",
                "Data/listpeers.response.json"
            )
            |> File.ReadAllText

        let data1 =
            let opts = JsonSerializerOptions()
            opts.AddDNLJsonConverters(Network.RegTest)
            JsonSerializer.Deserialize<Responses.ListpeersResponse>(data, opts)

        Assert.NotNull(data1)
        ()

    [<Fact>]
    member this.DeserializeListchannelsResponse() =
        let data =
            Path.Join(
                AppDomain.CurrentDomain.BaseDirectory,
                "../../..",
                "Data/listchannels.response.json"
            )
            |> File.ReadAllText

        let data1 =
            let opts = JsonSerializerOptions()
            opts.AddDNLJsonConverters(Network.RegTest)

            JsonSerializer.Deserialize<Responses.ListchannelsResponse>(
                data,
                opts
            )

        Assert.NotNull(data1)

    [<Fact>]
    member this.SerializeListchannelsRequest() =
        let req =
            {
                Requests.ListchannelsRequest.ShortChannelId = None
                Requests.ListchannelsRequest.Source = None
                Requests.ListchannelsRequest.Destination =
                    // we cannot specify both channel id and destination id
                    Some(
                        PubKey
                            "02bd27450207ab7abad315d5817dc0727ec90b14b5dc09f66fb9ec3f11bb1a71df"
                    )
            }

        let data1 =
            let opts = JsonSerializerOptions()
            opts.AddDNLJsonConverters(Network.RegTest)
            JsonSerializer.Serialize(req, opts)

        let transport = new MemoryStream() :> Stream

        let cli =
            ClnClient(
                Network.RegTest,
                getTransport = Func<_, _>(fun _ -> transport |> Task.FromResult)
            )

        task {
            let! resp = cli.ListChannelsAsync(req)
            Assert.NotNull(data1)
            Assert.NotNull(resp)
            Assert.Null(resp.Channels)
        }
