namespace DotNetLightning.ClnRpc.SystemTextJsonConverters

open System.Text.Json

open System.Runtime.CompilerServices
open NBitcoin

[<Extension; AbstractClass; Sealed>]
type ClnSharpClientHelpers =
    [<Extension>]
    static member internal AddDNLJsonConverters
        (
            this: JsonSerializerOptions,
            n: Network
        ) =
        this._AddDNLJsonConverters(n)
        DotNetLightning.ClnRpc.AddJsonConverters.addEnumConverters(this)
