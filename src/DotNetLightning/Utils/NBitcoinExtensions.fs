namespace DotNetLightning.Utils
open NBitcoin
open NBitcoin.Crypto

module NBitcoinExtensions =
    module FeeRate =
        let FeePerKWeight (feeRate: FeeRate) =
            Money.Satoshis(feeRate.FeePerK.Satoshi / 4L)

    type ECDSASignature with
        member this.ToBytesCompact() =
            Array.append (this.R.ToByteArray()) (this.S.ToByteArray())