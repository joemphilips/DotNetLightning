namespace DotNetLightning.Utils

type LogLevel =
    | Off
    | Error
    | Warn
    | Info
    | Debug
    | Trace

type Logger = LogLevel -> string -> unit