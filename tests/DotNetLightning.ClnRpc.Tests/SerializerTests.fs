namespace DotNetLightning.ClnRpc.Tests

open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Text.Json
open System.Threading.Tasks
open DotNetLightning.ClnRpc
open DotNetLightning.ClnRpc.Plugin
open DotNetLightning.ClnRpc.NewtonsoftJsonConverters
open DotNetLightning.ClnRpc.Requests
open DotNetLightning.ClnRpc.Responses
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

    [<Fact>]
    member this.SerializeGetrouteRequest_MustIgnoreNoneValue() =
        let req =
            {
                GetrouteRequest.Id =
                    PubKey
                        "02c1e1e97d7f1bb9aa7ec2c899e13c4dcbe3c08971620bd11cdf36e1addd812985"
                Msatoshi = 10000L<msat>
                Riskfactor = 10UL // no big reason for this value
                Cltv = None // req.Invoice.MinFinalCLTVExpiryDelta.Value |> int64 |> Some
                Fromid = None
                Fuzzpercent = None
                Exclude = None
                Maxhops = None
            }

        let opts = JsonSerializerOptions()

        let data1 =
            opts.AddDNLJsonConverters(Network.RegTest)
            JsonSerializer.SerializeToDocument(req, opts)

        Assert.Throws<KeyNotFoundException>(
            Func<obj>(fun _ -> data1.RootElement.GetProperty("exclude"))
        )
        |> ignore

        Assert.Throws<KeyNotFoundException>(
            Func<obj>(fun _ -> data1.RootElement.GetProperty("cltv"))
        )
        |> ignore

        let jObj =
            let opts = Newtonsoft.Json.JsonSerializerSettings()
            opts.AddDNLJsonConverters(Network.RegTest)
            let ser = Newtonsoft.Json.JsonSerializer.Create(opts)
            Newtonsoft.Json.Linq.JToken.FromObject(req, ser)

        Assert.Null(jObj.Root.["exclude"])
        Assert.Null(jObj.Root.["cltv"])
        ()

    [<Fact>]
    member this.SerializeListPays() =
        let req =
            {
                ListpaysRequest.Bolt11 =
                    "lnbcrt500u1p305fnmpp5vzsjps8uptzedfmrw8jsuw37m4mdlyjjua0qfzceph3a0nz7rtfqdql2djkuepqw3hjqsj5gvsxzerywfjhxuccqzptxqrrsssp5fak5cm2c3r5wtezcflfg6cs3psrp4kczvp4wly66h85y4m4hsrds9qyyssqqxemaw5w9r6hteaxmmhvqe4nkv654nyk88gahjt5mxfjjzkj945xe6frwuavv8u0fzwcst0mvrxj8nxlj3qad9dxgzv8rg9dup3r5kcqnwpqjk"
                    |> Some
                PaymentHash = None
                Status = ListpaysStatus.PENDING |> Some
            }

        let opts = JsonSerializerOptions()

        let data1 =
            opts.AddDNLJsonConverters(Network.RegTest)
            JsonSerializer.SerializeToDocument(req, opts)

        Assert.Equal(
            "pending",
            data1
                .RootElement
                .GetProperty("status")
                .GetString()
        )
