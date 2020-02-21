namespace DotNetLightning.Infrastructure

open System.Text.Json

/// Defines possible serialization formats for non-p2p object serialization
/// e.g. for saving in storage
type SupportedCodec =
    | SystemTextJson of SystemTextJsonCodec

and SystemTextJsonCodec = {
    Writer: Utf8JsonWriter
    Reader: JsonSerializer
}
