namespace DotNetLightning.Serialize

open System

[<Flags>]
type ShortChannelIdEncoding =
    | SortedPlain = 0uy
    | ZLib = 1uy
