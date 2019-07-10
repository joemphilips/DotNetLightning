namespace DotNetLightning.Utils

type LogLevel =
    | Off
    | Error
    | Warn
    | Info
    | Debug
    | Trace

type Logger = string * LogLevel -> unit