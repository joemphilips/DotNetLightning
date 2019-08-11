namespace DotNetLightning.Infrastructure

open System

[<AutoOpen>]
module Constants =
    let [<Literal>] HOME_DIRECTORY_NAME = ".dotnetlightning"

    let homePath =
        if (Environment.OSVersion.Platform = PlatformID.Unix) || (Environment.OSVersion.Platform = PlatformID.MacOSX) then
            Environment.GetEnvironmentVariable("HOME")
        else
            Environment.ExpandEnvironmentVariables("%HOMEDRIVE%%HOMEPATH%")
