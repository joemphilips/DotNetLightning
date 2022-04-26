namespace DotNetLightning.Chain

open System.Collections.Concurrent
open System.Text
open System.Threading
open NBitcoin
open NBitcoin.Crypto
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions

/// OutPoint
type StaticOutput =
    {
        outPoint: OutPoint
        output: TxOut
    }

/// Outpoint commits to p2wsh
/// P2WSH should be spend by the following witness
/// `<local_delayedsig> 0 <witnessScript>` (with input nSequence set to self_delay)
/// Outputs from HTLC-Success/Timeout tx/commitment tx
type DynamicOutputP2WSH =
    {
        outPoint: OutPoint
        key: Key
        witnessScript: Script
        toSelfDelay: uint16
        output: TxOut
    }

/// Outpoint commits to a P2WPKH
/// P2WPKH should be spend by the following witness.
/// `<local_sig> <local_pubkey>`
/// Outputs to_remote from a commitment tx
type DynamicOutputP2WPKH =
    {
        /// Output spendable by user wallet
        outpoint: OutPoint
        /// localkey = payment_basepoint_secret + SHA256(per_commitment_point || payment_basepoint)
        key: Key
        /// The output which is reference by the given outpoint
        output: TxOut
    }

/// When on-chain outputs are created by DotNetLightning an event is generated which informs the user thereof.
/// This enum describes the format of the output and provides the OutPoint.
type SpendableOutputDescriptor =
    | StaticOutput of StaticOutput
    | DynamicOutputP2WSH of DynamicOutputP2WSH
    | DynamicOutputP2WPKH of DynamicOutputP2WPKH
