namespace DotNetLightning.Infrastructure

open System.IO
open NBitcoin


type DotNetLightningNetwork(nType: NetworkType, nSet: INetworkSet) =
    
    member val CryptoCode = nSet.CryptoCode.ToUpperInvariant() with get
    member val NBitcoinNetwork = nSet.GetNetwork(nType) with get
    member val DefaultSettings = DotNetLightningDefaultSettings.getSettings(nType)
    member val NetworkType = nType
    
    new () = DotNetLightningNetwork(Network.Main.NetworkType, Bitcoin.Instance)
    
    override this.ToString() =
        sprintf "%s-%s" this.CryptoCode this.NetworkType.DataDirName
    member this.DataDirPath(root: string) =
        Path.Join(root, this.CryptoCode, nType.DataDirName)
