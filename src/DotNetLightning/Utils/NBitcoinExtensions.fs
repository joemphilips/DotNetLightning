namespace DotNetLightning.Utils
open NBitcoin
open NBitcoin.Crypto
open NBitcoin.Crypto
open System.Numerics

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

    type ECDSASignature with

        /// ** Description **
        ///
        /// Bitcoin Layer 1 forces (by consensus) DER encoding for the signatures.
        /// This is not optimal, but remaining as a rule since changing consensus is not easy.
        /// However in layer2, there are no such rules. So we use more optimal serialization by
        /// This function.
        /// Note it does not include the recovery id. so its always 64 bytes
        ///
        /// **Output**
        ///
        /// (serialized R value + S value) in byte array.
        member this.ToBytesCompact() =
            Array.append (NBitcoin.Utils.BigIntegerToBytes(this.R, 32)) (NBitcoin.Utils.BigIntegerToBytes(this.S, 32))

        static member FromBytesCompact(bytes: byte[], ?withRecId: bool) =
            let withRecId = defaultArg withRecId false
            if withRecId && bytes.Length <> 65 then
                invalidArg "bytes" "ECDSASignature specified to have recovery id, but it was not 65 bytes length"
            else if not withRecId && bytes.Length <> 64 then
                invalidArg "bytes" "ECDSASignature was not specified to have recovery id, but it was not 64 bytes length."
            else
                let data = if withRecId then bytes.[1..] else bytes
                let r = NBitcoin.BouncyCastle.Math.BigInteger(data.[0..31])
                let s = NBitcoin.BouncyCastle.Math.BigInteger(data.[32..63])
                ECDSASignature(r, s)