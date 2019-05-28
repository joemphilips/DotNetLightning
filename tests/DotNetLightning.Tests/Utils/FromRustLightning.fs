namespace DotNetLightning.Tests.Utils
open DotNetLightning.Chain

type Node = {
    ChainMonitor: ChainWatchInterfaceUtil
}

[<AutoOpen>]
module FromRustLN =
    let createNetwork (nodeCount: uint32) =
        ()