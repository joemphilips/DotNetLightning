namespace DotNetLightning.Chain

open System
open NBitcoin

#nowarn "0044" // "This construct is deprecated" warning

[<Obsolete>]
type StaticOutput =
    {
        OutPoint: OutPoint
        Output: TxOut
    }

[<Obsolete>]
type DynamicOutputP2WSH =
    {
        OutPoint: OutPoint
        Key: Key
        WitnessScript: Script
        ToSelfDelay: uint16
        Output: TxOut
    }

[<Obsolete>]
type DynamicOutputP2WPKH =
    {
        /// Output spendable by user wallet
        Outpoint: OutPoint
        /// localkey = payment_basepoint_secret + SHA256(per_commitment_point || payment_basepoint)
        Key: Key
        /// The output which is reference by the given outpoint
        Output: TxOut
    }

[<Obsolete>]
type SpendableOutputDescriptor =
    | StaticOutput of StaticOutput
    | DynamicOutputP2WSH of DynamicOutputP2WSH
    | DynamicOutputP2WPKH of DynamicOutputP2WPKH
