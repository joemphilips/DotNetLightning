namespace DotNetLightning.Infrastructure

open DotNetLightning.Chain
open DotNetLightning.Utils
open DotNetLightning.Utils.Primitives
open NBitcoin.RPC

type BitcoindBroadCaster(client: RPCClient) =
    let _client = client
    interface IBroadCaster with
        member this.BroadCastTransaction(tx) = async {
                let! uintTxId = _client.SendRawTransactionAsync(tx) |> Async.AwaitTask
                return TxId uintTxId
            }

