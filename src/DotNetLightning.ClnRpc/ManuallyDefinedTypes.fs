namespace DotNetLightning.ClnRpc

open DotNetLightning.ClnRpc.SystemTextJsonConverters
open System.Text.Json.Serialization
open DotNetLightning.Utils
open NBitcoin

type RouteHop =
    {
        [<JsonPropertyName "id">]
        [<JsonConverter(typeof<PubKeyJsonConverter>)>]
        [<Newtonsoft.Json.JsonConverter(typeof<NewtonsoftJsonConverters.PubKeyJsonConverter>)>]
        Id: PubKey

        [<JsonPropertyName "scid">]
        [<JsonConverter(typeof<ShortChannelIdJsonConverter>)>]
        [<Newtonsoft.Json.JsonConverter(typeof<NewtonsoftJsonConverters.ShortChannelIdJsonConverter>)>]
        Scid: ShortChannelId

        [<JsonPropertyName "feebase">]
        [<JsonConverter(typeof<MSatJsonConverter>)>]
        [<Newtonsoft.Json.JsonConverter(typeof<NewtonsoftJsonConverters.MSatJsonConverter>)>]
        Feebase: int64<msat>

        [<JsonPropertyName "feeprop">]
        Feeprop: uint32

        [<JsonPropertyName "expirydelta">]
        Expirydelta: uint16
    }

type Routehint =
    {
        [<JsonPropertyName "hops">]
        Hops: RouteHop []
    }

type RoutehintList =
    {
        [<JsonPropertyName "hints">]
        Hints: Routehint []
    }
