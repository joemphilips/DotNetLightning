namespace DotNetLightning.Chain

open System
open NBitcoin
open DotNetLightning.Utils

#nowarn "0044" // "This construct is deprecated" warning

[<Obsolete>]
type ChainError =
    | NotSupported
    | NotWatched
    | UnknownTx of Transaction

[<Obsolete>]
type BlockChainInstanceId = BlockChainInstanceId of string

[<Obsolete>]
type BlockContent = BlockHeader * BlockHeight * list<(uint32 * Transaction)>

[<Obsolete>]
type RawOnChainEvent =
    | BlockConnected of chainId: BlockChainInstanceId * content: BlockContent
    | BlockDisconnected of chainId: BlockChainInstanceId * BlockHeader

[<Obsolete>]
type OnChainEvent =
    | BlockConnected of content: BlockContent
    /// value is a list of blocks which has disappeared from the blockchain.
    | BlockDisconnected of header: list<BlockContent>


[<Obsolete>]
type IBroadCaster =
    abstract member BroadCastTransaction: (Transaction) -> Async<TxId>
