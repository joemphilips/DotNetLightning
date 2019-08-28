namespace DotNetLightning.Utils

open NBitcoin

module NBitcoinExtensions =
    module FeeRate =
        let FeePerKWeight (feeRate: FeeRate) =
            Money.Satoshis(feeRate.FeePerK.Satoshi / 4L)

    type NBitcoin.Transaction with
        member this.GetTxId() = TxId (this.GetHash())

    type Money with
        member this.ToLNMoney() = LNMoney.Satoshis(this.Satoshi)

    type OutPoint with
        member this.ToChannelId(): ChannelId =
            let mutable res = this.Clone().Hash.ToBytes()
            res.[30] <- res.[30] ^^^ (uint8 (this.N >>> 8) &&& 0xffuy)
            res.[31] <- res.[31] ^^^ (uint8 (this.N >>> 0) &&& 0xffuy)
            res  |> uint256 |> ChannelId

    type PSBT with
        member this.GetMatchingSig(pubkey: PubKey) =
            this.Inputs
            |> Seq.collect (fun i -> i.PartialSigs)
            |> Seq.choose(fun kv -> if kv.Key = pubkey then Some kv.Value else None)
            |> Seq.exactlyOne

        member this.GetTxId() =
            this.GetGlobalTransaction().GetTxId()
