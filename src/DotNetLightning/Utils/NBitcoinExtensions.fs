namespace DotNetLightning.Utils
open NBitcoin
open NBitcoin.Crypto

module NBitcoinExtensions =
    module FeeRate =
        let FeePerKWeight (feeRate: FeeRate) =
            Money.Satoshis(feeRate.FeePerK.Satoshi / 4L)

    type ECDSASignature with

        /// ** Description **
        ///
        /// Bitcoin Layer 1 forces (by consensus) DER encoding for the signatures.
        /// This is not optimal, but remaining as a rule since changing consensus is not easy.
        /// However in layer2, there are no such rules. So we use more optiomal serialization by
        /// This function.
        ///
        /// **Output**
        ///
        /// (serialized R value + S value) in byte array.
        member this.ToBytesCompact() =
            Array.append (this.R.ToByteArray()) (this.S.ToByteArray())