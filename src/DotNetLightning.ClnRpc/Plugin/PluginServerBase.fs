namespace DotNetLightning.ClnRpc.Plugin

open System
open System.Collections.Generic
open System.IO
open System.IO.Pipelines
open System.Reflection
open System.Runtime.InteropServices
open System.Threading
open System.Threading.Tasks
open DotNetLightning.ClnRpc
open DotNetLightning.ClnRpc.NewtonsoftJsonConverters
open DotNetLightning.ClnRpc.Plugin
open DotNetLightning.Utils
open Microsoft.VisualStudio.Threading
open NBitcoin
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
    member val Description = description
    member val LongDescription = longDescription

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

/// interface for plugin server
/// It might be useful when you want to call "init"/"getmanifest" in typed client.
type IPluginServer =
    abstract member Init:
        configuration: LightningInitConfigurationDTO *
        options: Dictionary<string, obj> ->
            Task<obj>

    abstract member GetManifest:
        ``allow-deprecated-apis``: bool * _otherparams: obj -> Task<Manifest>

exception PluginInitializationException of Exception

[<AllowNullLiteral>]
type PluginJsonRpc(handler: IJsonRpcMessageHandler) =
    inherit JsonRpc(handler)

    override this.IsFatalException(ex) =
        match ex with
        | :? PluginInitializationException -> true
        | _ -> ``base``.IsFatalException(ex)

type GetClientOutputStream = Func<CancellationToken, Task<Stream>>

/// <summary>
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
/// so you must **never write naively to stdout in your application's code**
/// otherwise json rpc messages will be corrupted.
///
/// If you really want to do it, say for logging, use the same
/// synchronized stream for both plugin and your code. i.e.
///
/// ```fsharp
/// let outputStream = Console.OpenStandardOutput() |> Stream.Synchronized
/// let logger = MyCustomLogger(outputStream) // log to output stream
/// let plugin =
///     MyPlugin().StartAsync(outputStream, Console.OpenStandardInput())
/// ```
///
/// Or just use stderr (e.g. from `Console.Error`)
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
/// If you set `ObsoleteAttribute` to methods, those will not be shown to users
/// unless they specify "allow-deprecated-apis" to true.
///
/// Hooks are currently unsupported since it is a quite advanced feature.
/// Please send a PR if you really want it.
/// </summary>
[<AbstractClass>]
type PluginServerBase
    /// <param name="notificationTopics">The list of topic names that might be notified to c-lightning by <see cref="PluginServerBase.SendNotification" /></param>
    /// <param name="dynamic">Is it ok for this plugin to terminate when the c-lightning sends "stop" call. set <c>false</c> if the plugin is critical for an user and it should never stop.</param>
    /// <param name="logDebug"> custom logging function for debug message on startup. </param>
    (notificationTopics: seq<string>, dynamic, logDebug: string -> unit) =

    let mutable getClientStream: GetClientOutputStream =
        Func<_, _>(fun ct -> Console.OpenStandardOutput() |> Task.FromResult)

    let mutable jsonRpc = null
    do logDebug $"PluginServerBase initialized"
    new(dynamic) = PluginServerBase(Seq.empty, dynamic, ignore<string>)
    new() = PluginServerBase(Seq.empty, true, ignore<string>)
    new(topics) = PluginServerBase(topics, true, ignore<string>)
    new(topics, dynamic) = PluginServerBase(topics, dynamic, ignore<string>)
    new(topics, logger) = PluginServerBase(topics, true, logger)
    new(logger) = PluginServerBase(Seq.empty, true, logger)

    /// <summary>
    /// When the c-lightning gets ready, it will send you `init` rpc call.
    /// You probably want to validate the init msg and abort the plugin
    /// when the value passed in is not the one expected.
    /// You may also want to store the configuration object as your need.
    /// If this method throws exception, the plugin shuts down.
    /// </summary>
    abstract member InitCore:
        configuration: LightningInitConfigurationDTO *
        cliOptions: Dictionary<string, obj> ->
            unit


    /// <summary>
    /// If this value is false, c-lightning will never try to stop this plugin.
    /// default: false
    /// </summary>
    member this.Dynamic = dynamic

    /// <summary>
    /// Semaphore to assure the thread safety when writing to the stdout.
    /// </summary>
    member val AsyncSemaphore = new AsyncSemaphore(1)

    /// <summary>
    /// Plugins can set its own feature bits,
    /// this value will be passed to c-lightning through `getmanifest`
    /// and it will be announced to other nodes in several ways.
    /// </summary>
    abstract member FeatureBits: FeatureSetDTO with get, set

    default val FeatureBits = Unchecked.defaultof<_> with get, set

    /// <summary>
    /// You can add optional JsonConverters for your own type with this property.
    /// </summary>
    member val JsonConverters: seq<Newtonsoft.Json.JsonConverter> =
        null with get, set

    member this.JsonRpc
        with get (): PluginJsonRpc = jsonRpc
        and private set v = jsonRpc <- v

    /// <summary>
    /// CLI options that plugin takes from users
    /// (via `lightningd`'s initialization options).
    /// </summary>
    abstract member Options: PluginOptions seq with get, set

    default val Options = seq [] with get, set

    /// <summary>
    /// The status of how we have processed the `init` message from c-lightning
    /// </summary>
    member val InitializationStatus =
        PluginInitializationStatus.NotYet with get, set

    member val Network = Network.RegTest with get, set

    /// <summary>
    /// output stream for client to send notification.
    /// Default is STDOUT. you can replace your own in test.
    /// </summary>
    member val GetClientOutputStream: GetClientOutputStream =
        getClientStream with get, set

    member this.GetStdoutClient() =
        let cli =
            ClnClient(
                this.Network,
                getTransport = this.GetClientOutputStream,
                jsonLibrary = JsonLibraryType.Newtonsoft
            )

        if this.JsonConverters |> isNull |> not then
            for c in this.JsonConverters do
                cli.NewtonSoftJsonConverters.Add(c)

        cli

    /// <summary>
    /// Send notification to the c-lightning, the `topic` must be
    /// registered in constructor parameter of `PluginServerBase`
    /// </summary>
    member this.SendNotification
        (
            topic: string,
            arg: 'T,
            [<Optional; DefaultParameterValue(CancellationToken())>] cancellationToken: CancellationToken
        ) =
        task {
            if notificationTopics |> Seq.contains topic |> not then
                raise
                <| ArgumentException(
                    $"topic {topic} is not part of {nameof(notificationTopics)}"
                )
            else
                do!
                    this
                        .GetStdoutClient()
                        .SendNotification(topic, arg, cancellationToken)
        }

    [<JsonRpcMethod("init")>]
    member this.Init
        (
            configuration: LightningInitConfigurationDTO,
            options: Dictionary<string, obj>
        ) : Task<obj> =
        task {

            try
                this.InitCore(configuration, options)

                this.InitializationStatus <-
                    PluginInitializationStatus.InitializedSuccessfully

                return () |> box
            with
            | x ->
                logDebug $"Failed to start plugin. {x}"

                this.InitializationStatus <- PluginInitializationStatus.Failed
                return raise(x |> PluginInitializationException)
        }

    [<JsonRpcMethod("getmanifest")>]
    member this.GetManifest
        (
            [<O; D(false)>] ``allow-deprecated-apis``: bool,
            [<O; D(null)>] _otherparams: obj // for future compatibility
        ) : Task<Manifest> =
        task {

            try
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
                    |> Seq.choose(fun methodInfo ->
                        let subscA =
                            methodInfo.GetCustomAttribute<PluginJsonRpcSubscriptionAttribute>
                                ()

                        let methodA =
                            methodInfo.GetCustomAttribute<PluginJsonRpcMethodAttribute>
                                ()

                        if subscA |> box |> isNull |> not
                           || methodA |> box |> isNull |> not then
                            Some(subscA, methodA, methodInfo)
                        else
                            None
                    )
                    |> Seq.toList
                    |> List.partition(fun (subscA, _, _) ->
                        subscA |> box |> isNull
                    )

                return
                    {
                        Options =
                            if ``allow-deprecated-apis`` then
                                this.Options
                            else
                                this.Options
                                |> Seq.filter(fun opts -> not <| opts.Deprecated
                                )
                        RPCMethods =
                            rpcMethodInfo
                            |> List.map(fun (_, attr, methodInfo) ->
                                assert (attr |> box |> isNull |> not)

                                let obsoleteAttr =
                                    methodInfo.GetCustomAttribute<ObsoleteAttribute>
                                        ()

                                let isDeprecated = obsoleteAttr |> isNull |> not

                                {
                                    Name = attr.Name
                                    Deprecated = isDeprecated
                                    Description =
                                        attr.Description
                                        + if isDeprecated then
                                              $" (this rpc is deprecated: {obsoleteAttr.Message})"
                                          else
                                              String.Empty
                                    LongDescription =
                                        attr.LongDescription
                                        + if isDeprecated then
                                              $" (this rpc is deprecated: {obsoleteAttr.Message})"
                                          else
                                              String.Empty
                                    Usage =
                                        let argSpec = methodInfo.GetParameters()

                                        let numDefaults =
                                            argSpec
                                            |> Seq.filter(fun s ->
                                                s.HasDefaultValue
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
                            |> Seq.choose(fun (attr, _, _methodInfo) ->
                                Some <| attr.Topic
                            )
                        Hooks = []
                        Dynamic = dynamic
                        FeatureBits = this.FeatureBits
                    }
            with
            | ex ->
                logDebug
                    $"error while running getmanifest. This should never happen {ex}"

                return raise ex
        }

    interface IPluginServer with
        member this.Init(configuration, options) =
            this.Init(configuration, options)

        member this.GetManifest(``allow-deprecated-apis``, _otherparams) =
            this.GetManifest(``allow-deprecated-apis``, _otherparams)


    /// <summary>
    /// Start listening as a rpc server, returns a task when finish processing
    /// "init" message from c-lightning.
    ///
    /// You can inject streams other than STDIN/STDOUT with parameters.
    /// This feature might be useful for testing.
    /// </summary>
    member this.StartAsync
        (
            writer: PipeWriter,
            reader: PipeReader,
            cancellationToken: CancellationToken
        ) =
#if !DEBUG
        if Environment.GetEnvironmentVariable("LIGHTNINGD_PLUGIN") <> "1" then
            failwith
                $"{nameof(this.StartAsync)} must not be called when you are not running a binary as a c-lightning plugin"
        else
#endif
            let formatter = new JsonMessageFormatter()

            formatter.JsonSerializer.NullValueHandling <-
                Newtonsoft.Json.NullValueHandling.Ignore

            // add user-defined converters first so that user can
            // override the converter
            if this.JsonConverters |> isNull |> not then
                for c in this.JsonConverters do
                    formatter.JsonSerializer.Converters.Add(c)

            formatter.JsonSerializer.Converters.AddDNLJsonConverters(
                this.Network
            )

            let handler =
                new NewLineDelimitedMessageHandler(writer, reader, formatter)

            handler.NewLine <- NewLineDelimitedMessageHandler.NewLineStyle.Lf

            let rpc = new PluginJsonRpc(handler)
            rpc.AddLocalRpcTarget(this, JsonRpcTargetOptions())
            rpc.StartListening()
            this.JsonRpc <- rpc

            // usually this never completes until the transport is disconnected.
            // But when the plugin throws an error while initialization,
            // this is the only task which completes.
            let completionTask = this.JsonRpc.Completion

            // completes when the initialization process goes successfully.

            let initializationTask =
                backgroundTask {
                    do! Task.Yield() // necessary for prevent hanging

                    while this.InitializationStatus
                          <> PluginInitializationStatus.InitializedSuccessfully
                          && (cancellationToken.IsCancellationRequested |> not) do
                        ()
                }
                :> Task

            task {
                try
                    let! t =
                        Task.WhenAny(
                            [
                                completionTask
                                initializationTask
                                Task.Delay(-1, cancellationToken)
                            ]
                        )

                    do! t.WithCancellation(cancellationToken)
                with
                | :? EndOfStreamException ->
                    // Stream closed, it probably means that c-lightning
                    // has shutdown
                    ()

                return rpc
            }

    member this.StartAsync
        (
            outStream: Stream,
            inStream: Stream,
            ct: CancellationToken
        ) =
        let outStream =
            if outStream |> isNull then
                Console.OpenStandardOutput() |> Stream.Synchronized
            else
                outStream

        this.GetClientOutputStream <-
            Func<_, _>(fun _ -> outStream |> Task.FromResult)

        let inStream =
            if inStream |> isNull then
                Console.OpenStandardInput()
            else
                inStream

        // fsharplint:disable
        let writer =
            PipeWriter.Create(
                outStream,
                StreamPipeWriterOptions(leaveOpen = true)
            )

        let reader =
            PipeReader.Create(
                inStream,
                StreamPipeReaderOptions(leaveOpen = true)
            )
        // fsharplint:enable

        this.StartAsync(writer, reader, ct)

    member this.StartAsync(outStream: Stream, inStream: Stream) =
        this.StartAsync(outStream, inStream, CancellationToken.None)

    member this.StartAsync(pipeWriter: PipeWriter, pipeReader: PipeReader) =
        this.StartAsync(pipeWriter, pipeReader, CancellationToken.None)

    member this.StartAsync() =
        let o: Stream = null
        let i: Stream = null
        this.StartAsync(o, i, CancellationToken.None)

    member this.StartAsync(cancellationToken) =
        let o: Stream = null
        let i: Stream = null
        this.StartAsync(o, i, cancellationToken)
