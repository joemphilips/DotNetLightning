namespace DotNetLightning.Infrastructure

open NBitcoin
open System.Collections.Generic

module DotNetLightningNetworkProvider =
    let private createCache networkType =
        let d = Dictionary<_,_>()
        let _ =
            let n = DotNetLightningNetwork(networkType, NBitcoin.Bitcoin.Instance)
            d.Add(NBitcoin.Bitcoin.Instance.CryptoCode, n)
        let _ =
            let n = DotNetLightningNetwork(networkType, NBitcoin.Altcoins.Monacoin.Instance)
            d.Add(NBitcoin.Altcoins.Monacoin.Instance.CryptoCode, n)
        let _ =
            let n = DotNetLightningNetwork(networkType, NBitcoin.Altcoins.Litecoin.Instance)
            d.Add(NBitcoin.Altcoins.Litecoin.Instance.CryptoCode, n)
        d
    let private cacheMainnet =
        createCache NetworkType.Mainnet
        
    let private cacheTestnet =
        createCache NetworkType.Testnet
        
    let private cacheRegtest =
        createCache NetworkType.Regtest
       
    let getNetwork networkType =
        let cache =
            match networkType with
            | NetworkType.Mainnet -> cacheMainnet
            | NetworkType.Testnet -> cacheTestnet
            | NetworkType.Regtest -> cacheRegtest
            | n -> failwithf "Unreachable! network type %A is not supported" n
        fun (cryptoCode: string) ->
            match cache.TryGetValue(cryptoCode) with
            | true, s -> s
            | false, _ -> failwithf "Unreachable! cryptoCode %s is not supported" cryptoCode
            
    let getAll() =
        seq {
            yield cacheMainnet
            yield cacheTestnet
            yield cacheRegtest
        }
        |> Seq.concat
        |> Seq.map(fun kv -> kv.Value)
