namespace DotNetLightning.ClnRpc.Plugin

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.InteropServices
open System.Threading
open System.Threading.Tasks
open DotNetLightning.ClnRpc
open DotNetLightning.Serialization
open DotNetLightning.Utils
open Microsoft.VisualStudio.Threading
open NBitcoin
open NBitcoin.JsonConverters
open StreamJsonRpc

type internal O = OptionalArgumentAttribute
type internal D = DefaultParameterValueAttribute

/// <summary>
/// Marker attribute for the rpc method handler.
/// It is mostly the same with `StreamJsonRpc.JsonRpcMethodAttribute`
/// Using this instead of `StreamJsonRpc.JsonRpcMethodAttribute`
/// will affect the startup behavior of the plugin,
/// so that c-lightning will expose the rpc endpoint to users
/// and the call to c-lightning will be bypassed to the plugin.
/// An argument `description` and `longDescription` is a message that users
/// will see in a `help` call to c-lightning.
/// </summary>
[<AttributeUsage(AttributeTargets.Method)>]
type PluginJsonRpcMethodAttribute
    (
        name,
        [<O; D(null)>] description: string,
        [<O; D(null)>] longDescription: string
    ) =
    inherit JsonRpcMethodAttribute(name)
    member val Description = description |> Option.ofObj
    member val LongDescription = longDescription |> Option.ofObj

/// <summary>
/// Marker attribute for the rpc handler to be an subscription handler.
/// Using this instead of `PluginJsonRpcMethod` will affect the behavior of the
/// `getmanifest`, so that c-lightning does not expose the method to users.
/// Instead, c-lightning will send a notification topic to the handler with
/// this attribute.
/// pass the topic name of an argument to this attribute.
/// </summary>
[<AttributeUsage(AttributeTargets.Method)>]
type PluginJsonRpcSubscriptionAttribute(topic) =
    inherit JsonRpcMethodAttribute(topic)
    member val Topic = topic

type PluginInitializationStatus =
    | NotYet = 0
    | InitializedSuccessfully = 1
    | Failed = 2

/// abstract base class for your c-lightning plugin.
/// It talks to c-lightning through stdin/stdout.
/// It will automatically create methods necessary for working as a plugin for
/// you.
///
/// ## 1. rpc method exposure
///
/// If you want to handle an rpc call from users,
/// you must implement the rpc handler, and annotate the handler with
/// `PluginJsonRpcMethod`, which is a class that inherits from
/// `StreamJsonRpc.JsonRpcMethod` It will automatically be exposed as an
/// additional rpc method for c-lightning. The behavior of argument binding etc
/// can be found at
/// [`StreamJsonRpc`'s document](https://github.com/microsoft/vs-streamjsonrpc/tree/main/doc)
///
/// IMPORTANT: plugin will use stdin/stdout for communicating with c-lightning,
/// so you must **never write to stdout in your application's code** and to
/// not corrupt the json rpc messages, you must not return
/// the value from rpc handlers (i.e. those with `JsonRpcMethod` attribute)
/// concurrently. The easiest way to achieve this is to get the async semaphore
/// in your method. In F#, that is...
///
/// ```fsharp
/// use _ = this.AsyncSemaphore.EnterAsync()
/// ```
/// or in C#,
///
/// ```csharp
/// using var _ = this.AsyncSemaphore.EnterAsync()
/// ```
///
/// ## 2. Subscribing to the topic.
///
/// If you want to subscribe to a notification from c-lightning
/// (or other plugins), You must implement it with
/// `PluginJsonRpcSubscription` instead of
/// `PluginJsonRpcMethod`.
/// By using it, the response value is ignored by c-lightning, and the
/// method is not exposed to an user.
///
/// ## 3. sending notification to c-lightning
///
/// plugins can send notification to c-lightning,
/// Use `SendNotification` method for it.
/// But before that, you must register the notification topics with constructor
/// argument to `PluginServerBase`, since c-lightning must know the list of
/// possible topics beforehand.
///
/// ## 4. other features
///
/// For other features, e.g. taking cli options, validating c-lightning init
/// message, etc, see corresponding abstract properties or methods.
///
/// Hooks are currently unsupported since it is a quite advanced feature.
/// Please send a PR if you really want it.
[<AbstractClass>]
type PluginServerBase
    (
        [<O; D(null)>] notificationTopics: seq<string>,
        [<O; D(false)>] dynamic
    ) =
    let semaphore = new AsyncSemaphore(1)

    /// <summary>
    /// When the c-lightning gets ready, it will send you `init` rpc call.
    /// You probably want to validate the init msg and abort the plugin
    /// when the value passed in is not the one expected.
    /// You may also want to store the configuration object as your need.
    /// </summary>
    abstract member InitCore:
        configuration: LightningInitConfigurationDTO *
        cliOptions: Dictionary<string, obj> ->
            unit

    /// <summary>
    /// Semaphore to assure the thread safety when writing to the stdout.
    /// </summary>
    member val AsyncSemaphore = semaphore

    /// <summary>
    /// Plugins can overwrite the lightning node feature bits.
    /// </summary>
    abstract member FeatureBits: FeatureSetDTO option

    override this.FeatureBits = None

    /// <summary>
    /// CLI options that plugin takes from users
    /// (via `lightningd`'s initialization options).
    /// </summary>
    abstract member Options: PluginOptions seq

    override this.Options = seq []

    /// <summary>
    /// The status of how we have processed the `init` message from c-lightning
    /// </summary>
    member val InitializationStatus =
        PluginInitializationStatus.NotYet with get, set

    member val Network = Network.RegTest with get, set

    member this.GetStdoutClient() =
        let getStdoutStream(_ct: CancellationToken) =
            Task.FromResult(Console.OpenStandardOutput())

        ClnClient(
            this.Network,
            getTransport = Func<_, _>(getStdoutStream),
            jsonLibrary = JsonLibraryType.Newtonsoft
        )

    /// <summary>
    /// Send notification to the c-lightning, the `topic` must be
    /// registered in constructor parameter of `PluginServerBase`
    /// </summary>
    member this.SendNotification(topic: string, arg: 'T) =
        task {
            if notificationTopics |> Seq.contains topic |> not then
                raise
                <| ArgumentException(
                    $"topic {topic} is not part of {nameof(notificationTopics)}"
                )
            else
                use _ = this.AsyncSemaphore.EnterAsync()

                do!
                    this
                        .GetStdoutClient()
                        .SendCommandAsync(topic, arg, noReturn = true)
        }

    [<JsonRpcMethod("init")>]
    member this.Init
        (
            configuration: LightningInitConfigurationDTO,
            options: Dictionary<string, obj>
        ) : Task<obj> =
        task {
            use! _releaser = semaphore.EnterAsync()

            try
                this.InitCore(configuration, options)

                this.InitializationStatus <-
                    PluginInitializationStatus.InitializedSuccessfully

                return () |> box
            with
            | x ->
                this.InitializationStatus <- PluginInitializationStatus.Failed
                return raise <| AggregateException("Error in ValidateInit", x)
        }

    [<JsonRpcMethod("getmanifest")>]
    member this.GetManifest
        (
            [<O; D(false)>] ``allow-deprecated-apis``: bool,
            [<O; D(null)>] _otherparams: obj
        ) : Task<Manifest> =
        task {
            use! _releaser = semaphore.EnterAsync()
            let _ = ``allow-deprecated-apis``

            let rpcMethodInfo, subscriptionMethodInfo =
                let equalStr a b =
                    String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

                this
                    .GetType()
                    .GetMethods(
                        BindingFlags.Public
                        ||| BindingFlags.Instance
                        ||| BindingFlags.DeclaredOnly
                    )
                |> Seq.filter(fun m ->
                    not <| m.IsSpecialName
                    && not <| (equalStr "init" m.Name)
                    && not <| (equalStr "getmanifest" m.Name)
                )
                |> Seq.toList
                |> List.partition(fun methodInfo ->
                    methodInfo.GetCustomAttribute(
                        typeof<PluginJsonRpcSubscriptionAttribute>
                    )
                    |> isNull
                )

            return
                {
                    Options = this.Options
                    RPCMethods =
                        rpcMethodInfo
                        |> List.map(fun methodInfo ->
                            let attr =
                                methodInfo.GetCustomAttribute<PluginJsonRpcMethodAttribute>
                                    ()

                            assert (attr |> box |> isNull |> not)

                            {
                                Name = attr.Name
                                Description = attr.Description
                                LongDescription = attr.LongDescription
                                Usage =
                                    let argSpec = methodInfo.GetParameters()

                                    let numDefaults =
                                        argSpec
                                        |> Seq.filter(fun s -> s.HasDefaultValue
                                        )
                                        |> Seq.length

                                    let keywordArgsStartIndex =
                                        argSpec.Length - numDefaults

                                    let args =
                                        argSpec
                                        |> Seq.filter(fun s ->
                                            let comp v =
                                                not
                                                <| String.Equals(
                                                    s.Name,
                                                    v,
                                                    StringComparison.OrdinalIgnoreCase
                                                )

                                            comp "plugin" && comp "request"
                                        )
                                        |> Seq.mapi(fun i s ->
                                            if i < keywordArgsStartIndex then
                                                // positional arguments
                                                s.Name
                                            else
                                                // keyword arguments
                                                $"[{s.Name}]"
                                        )

                                    String.Join(' ', args)
                            }
                        )
                    Notifications =
                        notificationTopics
                        |> Seq.toList
                        |> List.map(fun topic ->
                            {
                                NotificationsDTO.Method = topic
                            }
                        )
                    Subscriptions =
                        subscriptionMethodInfo
                        |> Seq.choose(fun m ->
                            let attr =
                                m.GetCustomAttribute<PluginJsonRpcSubscriptionAttribute>
                                    ()

                            Some <| attr.Topic
                        )
                    Hooks = []
                    Dynamic = dynamic
                    FeatureBits = this.FeatureBits
                }
        }


    /// Start listening to the server, returns a task when finish processing
    /// "init" message from c-lightning.
    member this.StartAsync() =
        if Environment.GetEnvironmentVariable("LIGHTNINGD_PLUGIN") <> "1" then
            failwith
                $"{nameof(this.StartAsync)} must not be called when you are not running a binary as a c-lightning plugin"
        else
            let formatter = new JsonMessageFormatter()

            let handler =
                new NewLineDelimitedMessageHandler(
                    Console.OpenStandardOutput(),
                    Console.OpenStandardInput(),
                    formatter
                )

            let rpc = new JsonRpc(handler)
            rpc.AddLocalRpcTarget(this, JsonRpcTargetOptions())
            rpc.StartListening()

            backgroundTask {
                while this.InitializationStatus = PluginInitializationStatus.NotYet do
                    ()

                if this.InitializationStatus = PluginInitializationStatus.Failed then
                    failwith "Initialization Failed."
                else
                    ()
            }
