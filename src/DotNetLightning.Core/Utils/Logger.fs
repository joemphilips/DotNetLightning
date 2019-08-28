namespace DotNetLightning.Utils

type LogLevel =
    | Critical
    | Error
    | Warn
    | Info
    | Debug
    | Trace

type Logger = LogLevel -> string -> unit