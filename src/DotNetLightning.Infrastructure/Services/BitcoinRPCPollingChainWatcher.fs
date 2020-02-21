namespace DotNetLightning.Infrastructure.Services

open CustomEventAggregator
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Threading.Tasks
open DotNetLightning.Chain
open DotNetLightning.Infrastructure
open DotNetLightning.Infrastructure.Interfaces
open DotNetLightning.Utils.Primitives
open Microsoft.Extensions.Hosting
open NBitcoin
open NBitcoin.RPC
open FSharp.Control.Tasks

type BitcoinRPCPollingChainListener(rpc: RPCClient, eventAggregator: IEventAggregator, maxBlockNum) =
    let _chainPollingInterval = TimeSpan.FromMilliseconds 5000.
    let _maxBlockSize = defaultArg maxBlockNum 5000 // block size to remember.
    let _rpc = rpc
    let _eventAggregator = eventAggregator
    
type BitcoinRPCPollingChainWatcher(rpc: RPCClient,
                                   eventAggregator: IEventAggregator,
                                   network: DotNetLightningNetwork,
                                   repo: IRepository,
                                   ?maxBlockNum: int) =
    inherit BackgroundService()
    let _chainPollingInterval = TimeSpan.FromMilliseconds 5000.
    let _maxBlockSize = defaultArg maxBlockNum 5000 // block size to remember.
    let _rpc = rpc
    let _eventAggregator = eventAggregator
    let _knownBlocks = LinkedList<Block>()
    override this.ExecuteAsync(ct) =
        unitTask {
            while (not ct.IsCancellationRequested) do
                do! Async.Sleep _chainPollingInterval.Milliseconds
                if (not ct.IsCancellationRequested) then
                    let! blockHash = _rpc.GetBestBlockHashAsync()
                    let! b = _rpc.GetBlockAsync(blockHash)
                    if _knownBlocks.Count = _maxBlockSize then
                        _knownBlocks.RemoveFirst()
                        
                    if (_knownBlocks.Last.Value <> b) then
                        _knownBlocks.AddLast b |> ignore
                        _eventAggregator.Publish(OnChainEvent.BlockConnected)
                    ()
        }

    interface IChainWatcher with
        member this.InstallWatchTx(txId: TxId, scriptPubKey: Script): bool =
            failwith ""
            
        member this.InstallWatchOutPoint (outPoint: OutPoint, scriptPubKey: Script): bool =
            failwith ""
        member this.WatchAllTxn() =
            failwith ""
        member this.RegisterListener(listener: IChainListener) =
            failwith ""
            
        member this.CurrentTip = failwith ""
        
        member this.StartAsync(ct) =
            failwith ""
        member this.StopAsync(ct) =
            failwith ""
            
        member val Network = network
