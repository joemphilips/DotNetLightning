namespace DotNetLightning.Infrastructure

open Microsoft.Extensions.Logging

module Logger =
    let fromMicrosoftLogger (l: ILogger): DotNetLightning.Utils.Logger =
        fun (loglevel) ->
            let log msg = 
                match loglevel with
                | DotNetLightning.Utils.LogLevel.Critical ->
                    l.LogCritical(msg)
                | DotNetLightning.Utils.LogLevel.Error ->
                    l.LogError(msg)
                | DotNetLightning.Utils.Warn
                    -> l.LogWarning(msg)
                | DotNetLightning.Utils.Info ->
                    l.LogInformation(msg)
                | DotNetLightning.Utils.Debug ->
                    l.LogDebug(msg)
                | DotNetLightning.Utils.Trace -> l.LogTrace(msg)
            log


