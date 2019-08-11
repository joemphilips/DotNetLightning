namespace DotNetLightning.Infrastructure

open System.Text.Json

type SupportedCodec =
    | SystemTextJson of SystemTextJsonCodec

and SystemTextJsonCodec = {
    Writer: Utf8JsonWriter
    Reader: JsonSerializer
}