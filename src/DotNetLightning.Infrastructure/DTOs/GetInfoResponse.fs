namespace DotNetLightning.Infrastructure.DTOs

open DotNetLightning.Serialize
open DotNetLightning.Utils.Primitives


type GetInfoResponse = {
    /// Number of peers that this daemon knows
    NumPeers: int
    
    /// number of channels that is currently in the process of opening
    NumPendingChannels: int
    /// Number of open channels
    NumActiveChannels: int
    /// Number of channels already closed or in the process of closing, or has abnormal state
    NumInactiveChannels: int
    BlockHeight: uint32
    Alias: string
    Network: string
    SyncedToGraph: bool
    NodeId: string
    ChainSynchronizationProgress: float
    Features: FeatureBit
    Version: string
    Uris: string []
    BlockHash: string
    Color: RGB
    Chains: string[]
    BestHeaderTimestamp: string
}
