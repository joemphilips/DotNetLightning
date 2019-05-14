namespace DotNetLightning.Chain
open NBitcoin
open DotNetLightning.Utils


type ChainError =
    | NotSupported
    | NotWatched
    | UnknownTx of Transaction


type IChainListener =
    abstract member BlockConnected: (BlockHeader * Blockheight)
    abstract member BlockDisconnected: BlockHeader -> bool

type IChainWatcher =
    abstract member IntallWatchTx: (uint256 * Script) -> bool
    abstract member InstallWatchOutPoint: (OutPoint * Script) -> bool
    abstract member WatchAllTxn: unit -> bool
    abstract member RegisterListener: IChainListener -> bool

type ConfirmationTarget =
    | Background
    | Normal
    | HighPriority

type IFeeEstimator =
    abstract member GetEstSatPer1000Weight: (ConfirmationTarget) -> FeeRate