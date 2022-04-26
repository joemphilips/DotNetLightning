namespace DotNetLightning.Chain

open System
open NBitcoin
open DotNetLightning.Utils
open Microsoft.Extensions.Logging


type ChainError =
    | NotSupported
    | NotWatched
    | UnknownTx of Transaction

/// Id for specific on-chain information source
type BlockChainInstanceId = BlockChainInstanceId of string

/// We want transaction index number for channel id and such.
/// So not using NBitcoin.Block directly
type BlockContent = BlockHeader * BlockHeight * list<(uint32 * Transaction)>

type RawOnChainEvent =
    | BlockConnected of chainId: BlockChainInstanceId * content: BlockContent
    | BlockDisconnected of chainId: BlockChainInstanceId * BlockHeader

type OnChainEvent =
    | BlockConnected of content: BlockContent
    /// value is a list of blocks which has disappeared from the blockchain.
    | BlockDisconnected of header: list<BlockContent>


type IBroadCaster =
    abstract member BroadCastTransaction: (Transaction) -> Async<TxId>

type ConfirmationTarget =
    | Background
    | Normal
    | HighPriority

type IFeeEstimator =
    abstract member GetEstSatPer1000Weight: (ConfirmationTarget) -> FeeRatePerKw
