namespace DotNetLightning.ClnRpc.Tests

open System
open System.Collections.Generic
open System.IO
open System.IO.Pipelines
open System.IO.Pipes
open System.Runtime.InteropServices
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open DotNetLightning.ClnRpc.Plugin
open DotNetLightning.ClnRpc.NewtonsoftJsonConverters
open DotNetLightning.ClnRpc.Plugin
open DotNetLightning.Serialization
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.VisualStudio.Threading
open NBitcoin
open Newtonsoft.Json.Linq
open StreamJsonRpc
open Xunit

type IPluginServer1 =
    inherit IPluginServer
    abstract member Test1: name: string -> unit
    abstract member Test2Async: name: string -> Task<obj>
    abstract member Subsc1: subscArg: string -> unit

module PluginServer1 =
    [<Literal>]
    let Test1Desc = "This is a description for test1 method"

    [<Literal>]
    let Test1LongDesc = "long description for test1 method"

    [<Literal>]
    let Test2Desc = "This is a description for test2 method"

    [<Literal>]
    let Test2LongDesc = "long description for test2 method"

    [<Literal>]
    let Test3Desc = "This is a description for test3 method"

    [<Literal>]
    let Test3LongDesc = "long description for test3 method"

    [<Literal>]
    let Test3DeprecationMsg = "test3 is deprecated"

    [<Literal>]
    let Test1Topic1 = "test1 topic1"

    [<Literal>]
    let Test1Topic2 = "test1 topic2"

type PluginServer1(?dynamic: bool) =
    inherit PluginServerBase
        (
            [
                PluginServer1.Test1Topic1
                PluginServer1.Test1Topic2
            ],
            (defaultArg dynamic false)
        )

    member val Initialized = false with get, set

    member val Name = null with get, set
    member val Test2Name = null with get, set
    member val InitTask = TaskCompletionSource() with get, set

    member val Test1Task = TaskCompletionSource() with get, set

    override this.InitCore(configuration, cliOptions) =
        // do nothing
        this.Initialized <- true
        this.InitTask.SetResult()
        ()

    [<PluginJsonRpcMethod("test1",
                          PluginServer1.Test1Desc,
                          PluginServer1.Test1LongDesc)>]
    member this.Test1
        (
            name,
            [<OptionalArgument;
              DefaultParameterValue("default_value_for_optional")>] optionalArg: string
        ) =
        this.Name <- name
        this.Test1Task.SetResult()
        ()

    [<PluginJsonRpcMethod("test2",
                          PluginServer1.Test2Desc,
                          PluginServer1.Test2LongDesc)>]
    member this.Test2Async(name) =
        task {
            this.Test2Name <- name
            return () |> box
        }

    [<Obsolete(PluginServer1.Test3DeprecationMsg)>]
    [<PluginJsonRpcMethod("test3",
                          PluginServer1.Test3Desc,
                          PluginServer1.Test3LongDesc)>]
    member this.DeprecatedMethod() =
        ()

    [<PluginJsonRpcSubscription("subsc1")>]
    member this.Subsc1(subscArg: string) =
        ()

    interface IPluginServer1 with
        member this.Test1(name) =
            this.Test1(name)

        member this.Subsc1(subscArg: string) =
            this.Subsc1(subscArg)

        member this.Test2Async(name: string) =
            this.Test2Async(name)


[<AutoOpen>]
module TestHelpers =
    [<Literal>]
    let PipeName = "SamplePipe1"

    let struct (serverPipe, clientPipe) =
        Nerdbank.Streams.FullDuplexStream.CreatePair()

    let inline getClientProxy<'T when 'T: not struct>() =
        let pipe = clientPipe

        let handler =
            let formatter = new JsonMessageFormatter()

            formatter.JsonSerializer.Converters.AddDNLJsonConverters(
                Network.RegTest
            )

            new NewLineDelimitedMessageHandler(pipe, pipe, formatter)

        task { return JsonRpc.Attach<'T>(handler) }

    let inline createRpcServer<'T when 'T: not struct>(server: 'T) =
        task {
            while true do
                let pipe = serverPipe

                let handler =
                    let formatter = new JsonMessageFormatter()

                    formatter.JsonSerializer.Converters.AddDNLJsonConverters(
                        Network.RegTest
                    )

                    new NewLineDelimitedMessageHandler(pipe, pipe, formatter)

                let rpc = new JsonRpc(handler)
                rpc.AddLocalRpcTarget<'T>(server, JsonRpcTargetOptions())
                rpc.StartListening()
                do! rpc.Completion
        }

type PluginTests() =

    [<Fact>]
    member this.Test_GetManifestWithTypedStream() =
        task {
            let server = PluginServer1()
            let getClientTask = getClientProxy<IPluginServer1>()
            let _server = createRpcServer<IPluginServer1>(server)
            let! client = getClientTask
            let! manifest = client.GetManifest(false, obj())

            let expectedManifest =
                {
                    Manifest.Dynamic = false
                    Options = server.Options
                    RPCMethods =
                        [
                            {
                                RPCMethod.Description = PluginServer1.Test1Desc
                                Name = "test1"
                                Usage = "name [optionalArg]"
                                Deprecated = false
                                LongDescription = PluginServer1.Test1LongDesc
                            }
                            {
                                RPCMethod.Description = PluginServer1.Test2Desc
                                Name = "test2"
                                Usage = "name"
                                Deprecated = false
                                LongDescription = PluginServer1.Test2LongDesc
                            }
                            {
                                RPCMethod.Description =
                                    PluginServer1.Test3Desc
                                    + $" (this rpc is deprecated: {PluginServer1.Test3DeprecationMsg})"
                                Name = "test3"
                                Usage = ""
                                Deprecated = true
                                LongDescription =
                                    PluginServer1.Test3LongDesc
                                    + $" (this rpc is deprecated: {PluginServer1.Test3DeprecationMsg})"
                            }
                        ]
                    Subscriptions = [ "subsc1" ]
                    Hooks = []
                    Notifications =
                        [
                            {
                                Method = PluginServer1.Test1Topic1
                            }
                            {
                                Method = PluginServer1.Test1Topic2
                            }
                        ]
                    FeatureBits = server.FeatureBits
                }

            Assert.Equal(expectedManifest.Dynamic, manifest.Dynamic)

            Assert.Equal<PluginOptions>(
                expectedManifest.Options,
                manifest.Options
            )

            Assert.Equal<RPCMethod>(
                expectedManifest.RPCMethods,
                manifest.RPCMethods
            )

            Assert.Equal<string>(
                expectedManifest.Subscriptions,
                manifest.Subscriptions
            )

            Assert.Equal<obj>(expectedManifest.Hooks, manifest.Hooks)

            Assert.Equal<NotificationsDTO>(
                expectedManifest.Notifications,
                manifest.Notifications
            )

            Assert.Equal(expectedManifest.FeatureBits, manifest.FeatureBits)
            let! _ = client.Init(initConfigDTO, initOptionsDTO)
            Assert.True(server.Initialized)
            let arg = "this is \"name\" argument"
            client.Test1(arg)
            do! server.Test1Task.Task
            Assert.Equal(arg, server.Name)
            ()
        }


    [<Fact>]
    member this.Test_ServerWithRawStdinOutStream() =
        task {
            let req =
                """{ "id": 1, "jsonrpc": "2.0", "method": "test1", "params": { "name": "World" } }"""
                |> utf8.GetBytes

            use cts = new CancellationTokenSource()
            cts.CancelAfter(2000)

            let! p = setupRawStream(PluginServer1(), [ initB; req ], cts.Token)

            do! p.InitTask.Task
            Assert.True(p.Initialized)
            do! p.Test1Task.Task
            Assert.Equal("World", p.Name)
        }

    [<Fact>]
    member this.CanSendNotificationToCLightning() =
        task {
            use cts = new CancellationTokenSource()
            cts.CancelAfter(2000)

            let! p = setupRawStream(PluginServer1(), [ initB ], cts.Token)

            let msg =
                $"this is notification msg for {nameof(this.CanSendNotificationToCLightning)}"

            use outStream = new MemoryStream(Array.zeroCreate 65535)

            p.GetClientOutputStream <-
                Func<_, _>(fun ct -> task { return outStream :> _ })

            do! p.SendNotification(PluginServer1.Test1Topic1, msg)

            let actual =
                utf8
                    .GetString(outStream.ToArray())
                    .TrimEnd([| '\x00' |])

            let expected =
                $"""
{{
    "method": "{PluginServer1.Test1Topic1}",
    "jsonrpc": "2.0",
    "params": "{msg}"
}}
"""

            Assert.Equal(expected |> flatten, actual |> flatten)
            ()
        }

type BogusPlugin() =
    inherit PluginServerBase()

    member val InitTaskCompletionSource = TaskCompletionSource()

    override this.InitCore(_conf, _opts) =
        failwith "InitCore fails"
        this.InitTaskCompletionSource.SetResult()
        ()

type ExceptionTest() =
    [<Fact>]
    member this.ServerTerminatesWhenInitCoreThrowsAnException() =
        task {
            let req =
                """{ "id": 1, "jsonrpc": "2.0", "method": "test1", "params": { "name": "World" } }"""
                |> utf8.GetBytes

            use cts = new CancellationTokenSource()
            cts.CancelAfter(2000)
            let p = BogusPlugin()

            let! _e =
                Assert.ThrowsAsync<PluginInitializationException>(fun () ->
                    task {
                        let! p = setupRawStream(p, [| initB; req |], cts.Token)
                        do! p.InitTaskCompletionSource.Task
                    }
                    :> Task
                )

            ()
        }
