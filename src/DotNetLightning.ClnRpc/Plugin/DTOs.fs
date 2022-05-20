namespace DotNetLightning.ClnRpc.Plugin

open System.Runtime.Serialization
open DotNetLightning.ClnRpc.NewtonsoftJsonConverters
open DotNetLightning.Serialization
open DotNetLightning.Utils
open NBitcoin.JsonConverters
open Newtonsoft.Json.Converters

/// <namespacedoc>
///     <summary>
///         "DotNetLightning.ClnRpc.Plugin" contains a utility to build your own
///         c-lightning plugins.
///         All you have to do is
///         1. write a class which inherits from <c>PluginServerBase</c>.
///         2. instantiate and run <c>StartAsync</c> method.
///         3. Compile as a single binary and put it into c-lightning's plugin directory.
///
///         there is also "PluginLogger" type which can be used for logging
///         in plugin.
///     </summary>
/// </namespacedoc>
/// <exclude />
module NamespaceDocDummy =
    ()

type PluginOptType =
    | [<EnumMember(Value = "string")>] String = 0
    | [<EnumMember(Value = "int")>] Int = 1
    | [<EnumMember(Value = "bool")>] Bool = 2
    | [<EnumMember(Value = "flag")>] Flag = 3

[<CLIMutable>]
type RPCMethod =
    {
        [<Newtonsoft.Json.JsonProperty "name">]
        Name: string

        [<Newtonsoft.Json.JsonProperty "usage">]
        Usage: string

        [<Newtonsoft.Json.JsonProperty "description">]
        Description: string

        [<Newtonsoft.Json.JsonProperty "long_description">]
        LongDescription: string

        [<Newtonsoft.Json.JsonProperty "deprecated">]
        Deprecated: bool
    }

[<CLIMutable>]
type PluginOptions =
    {
        [<Newtonsoft.Json.JsonProperty "name">]
        Name: string

        [<Newtonsoft.Json.JsonProperty "default">]
        Default: obj

        [<Newtonsoft.Json.JsonProperty "description">]
        Description: string

        [<Newtonsoft.Json.JsonProperty "type">]
        [<Newtonsoft.Json.JsonConverter(typeof<StringEnumConverter>)>]
        OptionType: PluginOptType

        [<Newtonsoft.Json.JsonProperty "multi">]
        Multi: bool

        [<Newtonsoft.Json.JsonProperty "deprecated">]
        Deprecated: bool
    }

[<CLIMutable>]
type FeatureSetDTO =
    {
        [<Newtonsoft.Json.JsonProperty "init">]
        Init: FeatureBits option

        [<Newtonsoft.Json.JsonProperty "node">]
        Node: FeatureBits option

        [<Newtonsoft.Json.JsonProperty "channel">]
        Channel: FeatureBits option

        [<Newtonsoft.Json.JsonProperty "invoice">]
        Invoice: FeatureBits option
    }

[<CLIMutable>]
type NotificationsDTO =
    {
        [<Newtonsoft.Json.JsonProperty "method">]
        Method: string
    }

[<CLIMutable>]
type Manifest =
    {
        [<Newtonsoft.Json.JsonProperty "options">]
        Options: PluginOptions seq

        [<Newtonsoft.Json.JsonProperty "rpcmethods">]
        RPCMethods: RPCMethod seq

        [<Newtonsoft.Json.JsonProperty "subscriptions">]
        Subscriptions: string seq

        [<Newtonsoft.Json.JsonProperty "hooks">]
        Hooks: obj seq

        [<Newtonsoft.Json.JsonProperty "dynamic">]
        Dynamic: bool

        [<Newtonsoft.Json.JsonProperty "notifications">]
        Notifications: NotificationsDTO seq

        [<Newtonsoft.Json.JsonProperty "featurebits">]
        FeatureBits: FeatureSetDTO
    }

[<CLIMutable>]
type ProxyDTO =
    {
        [<Newtonsoft.Json.JsonProperty "type">]
        Ty: string

        [<Newtonsoft.Json.JsonProperty "address">]
        Address: string

        [<Newtonsoft.Json.JsonProperty "port">]
        Port: int
    }

[<CLIMutable>]
type LightningInitConfigurationDTO =
    {
        [<Newtonsoft.Json.JsonProperty "lightning-dir">]
        LightningDir: string

        [<Newtonsoft.Json.JsonProperty "rpc-file">]
        RpcFile: string

        [<Newtonsoft.Json.JsonProperty "startup">]
        Startup: bool

        [<Newtonsoft.Json.JsonProperty "network">]
        Network: string

        [<Newtonsoft.Json.JsonProperty "feature_set">]
        FeatureSet: FeatureSetDTO

        [<Newtonsoft.Json.JsonProperty "proxy">]
        Proxy: ProxyDTO

        [<Newtonsoft.Json.JsonProperty "torv3-enabled">]
        TorV3Enabled: bool

        [<Newtonsoft.Json.JsonProperty "always_use_proxy">]
        AlwaysUseProxy: bool
    }

open System.Collections.Generic

[<CLIMutable>]
type InitDTO =
    {
        [<Newtonsoft.Json.JsonProperty "configuration">]
        Configuration: LightningInitConfigurationDTO

        [<Newtonsoft.Json.JsonProperty "options">]
        Options: Dictionary<string, obj>
    }
