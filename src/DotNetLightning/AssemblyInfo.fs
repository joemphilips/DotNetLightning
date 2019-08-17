module AssemblyInfo

open System.Reflection
open System.Runtime.CompilerServices

[<assembly:AssemblyTitleAttribute("DotNetLightning")>]
[<assembly:AssemblyProductAttribute("DotNetLightning")>]
[<assembly:AssemblyVersionAttribute("0.1.0")>]
[<assembly:AssemblyFileVersionAttribute("0.1.0")>]
[<assembly:AssemblyInformationalVersionAttribute("0.1.0")>]
[<assembly:AssemblyMetadataAttribute("ReleaseChannel", "release")>]
[<assembly:AssemblyDescriptionAttribute("Lightning Network Daemon for DotNet")>]
[<assembly: InternalsVisibleTo("DotNetLightning.Tests")>]
[<assembly: System.Runtime.CompilerServices.InternalsVisibleTo("DotNetLightning.Infrastructure")>]

do ()

module internal AssemblyVersionInformation =
    [<Literal>]
    let AssemblyTitle = "DotNetLightning"

    [<Literal>]
    let AssemblyProduct = "DotNetLightning"

    [<Literal>]
    let AssemblyVersion = "0.1.0"

    [<Literal>]
    let AssemblyFileVersion = "0.1.0"

    [<Literal>]
    let AssemblyInformationalVersion = "0.1.0"

    [<Literal>]
    let AssemblyMetadata_ReleaseChannel = "release"

