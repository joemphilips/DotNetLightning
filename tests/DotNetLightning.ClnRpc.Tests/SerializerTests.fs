namespace DotNetLightning.ClnRpc.Tests

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
