namespace DotNetLightning.Infrastructure

open System
open System.IO
open NBitcoin
open System.Collections.Generic


type DotNetLightningDefaultSettings = {
    DefaultDataDirectory: string
    DefaultCookieFile: string
    DefaultP2PPort: int
    DefaultRPCPort: int
    DefaultUrl: Uri
}

module DotNetLightningDefaultSettings =
    let getSettings =
        let settings = Dictionary<NetworkType, DotNetLightningDefaultSettings>()
        for nType in [NetworkType.Mainnet; NetworkType.Testnet; NetworkType.Regtest] do
            let defaultDir = getDataDirectory(nType)
            let defaultP2PPort = 9735
            let defaultRPCPort = 19735
            let s = { DefaultDataDirectory = defaultDir
                      DefaultCookieFile = Path.Combine(defaultDir, ".cookie")
                      DefaultP2PPort = defaultP2PPort
                      DefaultRPCPort = defaultRPCPort
                      DefaultUrl = Uri(sprintf "http://127.0.0.1:%d/" defaultRPCPort, UriKind.Absolute) }
            settings.Add(nType, s)

        fun (nType: NetworkType) -> settings.[nType]