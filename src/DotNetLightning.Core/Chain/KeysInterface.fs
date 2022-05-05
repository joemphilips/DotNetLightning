namespace DotNetLightning.Chain

open System
open NBitcoin

#nowarn "0044" // "This construct is deprecated" warning

[<Obsolete>]
type StaticOutput =
    {
        outPoint: OutPoint
        output: TxOut
    }

[<Obsolete>]
type DynamicOutputP2WSH =
    {
        outPoint: OutPoint
        key: Key
        witnessScript: Script
        toSelfDelay: uint16
        output: TxOut
    }

[<Obsolete>]
type DynamicOutputP2WPKH =
    {
        /// Output spendable by user wallet
        outpoint: OutPoint
        /// localkey = payment_basepoint_secret + SHA256(per_commitment_point || payment_basepoint)
        key: Key
        /// The output which is reference by the given outpoint
        output: TxOut
    }

[<Obsolete>]
type SpendableOutputDescriptor =
    | StaticOutput of StaticOutput
    | DynamicOutputP2WSH of DynamicOutputP2WSH
    | DynamicOutputP2WPKH of DynamicOutputP2WPKH
