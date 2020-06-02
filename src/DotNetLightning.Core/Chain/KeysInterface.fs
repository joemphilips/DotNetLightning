namespace DotNetLightning.Chain

open System.Collections.Concurrent
open System.Text
open System.Threading
open NBitcoin
open NBitcoin.Crypto
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Crypto

/// OutPoint
type StaticOutput = {
    outPoint: OutPoint
    output: TxOut
}

/// Outpoint commits to p2wsh
/// P2WSH should be spend by the following witness
/// `<local_delayedsig> 0 <witnessScript>` (with input nSequence set to self_delay)
/// Outputs from HTLC-Success/Timeout tx/commitment tx
type DynamicOutputP2WSH = {
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
type DynamicOutputP2WPKH = {
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

/// Set of lightning keys needed to operate a channel as describe in BOLT 3
type ChannelKeys = {
    FundingKey: Key
    RevocationBaseKey: Key
    PaymentBaseKey: Key
    DelayedPaymentBaseKey: Key
    HTLCBaseKey: Key
    CommitmentSeed: CommitmentSeed
}

    with
        member this.ToChannelPubKeys() =
            {
                FundingPubKey = this.FundingKey.PubKey
                RevocationBasePubKey = this.RevocationBaseKey.PubKey
                PaymentBasePubKey = this.PaymentBaseKey.PubKey
                DelayedPaymentBasePubKey = this.DelayedPaymentBaseKey.PubKey
                HTLCBasePubKey = this.HTLCBaseKey.PubKey
            }

/// In usual operation we should not hold secrets on memory. So only hold pubkey
and ChannelPubKeys =  {
    FundingPubKey: PubKey
    RevocationBasePubKey: PubKey
    PaymentBasePubKey: PubKey
    DelayedPaymentBasePubKey: PubKey
    HTLCBasePubKey: PubKey
}


/// Interface to describe an object which can get user secrets and key material.
type IKeysRepository =
    /// Secret key for node_id a.k.a network_key
    abstract member GetNodeSecret: unit -> Key
    abstract member GetDestinationScript: unit -> Script
    abstract member GetShutdownPubKey: unit -> PubKey
    /// Get a new set of ChannelKeys for per-channel secrets. These MUST be unique even if you
    /// restarted with some stale data.
    abstract member GetChannelKeys: inbound:bool -> ChannelKeys

    /// Must add signature to the PSBT *And* return the signature
    abstract member GetSignatureFor: psbt: PSBT * pubKey: PubKey -> TransactionSignature * PSBT
    /// Must add signature to the PSBT *And* return the signature
    abstract member GenerateKeyFromBasePointAndSign: psbt: PSBT * pubkey: PubKey * basePoint: PubKey -> TransactionSignature * PSBT
    /// Must add signature to the PSBT *And* return the signature
    abstract member GenerateKeyFromRemoteSecretAndSign: psbt: PSBT * pubKey: PubKey * remoteSecret : Key -> TransactionSignature * PSBT

/// `InMemoryChannelKeys` in rust-lightning.
///
/// `nodeSecret` is the node-wide master key which is also used for
/// transport-level encryption. The channel's keys are derived from
/// `nodeSecret` via BIP32 key derivation where `channelIndex` is the child
/// index used to derive the channel's master key.
type DefaultKeyRepository(nodeSecret: ExtKey, channelIndex: int) =
    let _utf8 = Encoding.UTF8
    let channelMasterKey = nodeSecret.Derive(channelIndex, true)

    let destinationKey = channelMasterKey.Derive(1, true).PrivateKey
    let shutdownKey = channelMasterKey.Derive(2, true).PrivateKey
    let commitmentSeed = channelMasterKey.Derive(3, true).PrivateKey |> CommitmentSeed

    let fundingKey = channelMasterKey.Derive(4, true).PrivateKey
    let fundingPubKey = fundingKey.PubKey

    let revocationBaseKey = channelMasterKey.Derive(5, true).PrivateKey
    let revocationBasePubKey = revocationBaseKey.PubKey

    let paymentBaseKey = channelMasterKey.Derive(6, true).PrivateKey
    let paymentBasePubKey = paymentBaseKey.PubKey

    let delayedPaymentBaseKey = channelMasterKey.Derive(7, true).PrivateKey
    let delayedPaymentBasePubKey = delayedPaymentBaseKey.PubKey

    let htlcBaseKey = channelMasterKey.Derive(8, true).PrivateKey
    let htlcBasePubKey = htlcBaseKey.PubKey

    let basepointToSecretMap = ConcurrentDictionary<PubKey, Key>()
    do
        basepointToSecretMap.TryAdd(fundingPubKey, fundingKey) |> ignore
        basepointToSecretMap.TryAdd(revocationBasePubKey, revocationBaseKey) |> ignore
        basepointToSecretMap.TryAdd(paymentBasePubKey, paymentBaseKey) |> ignore
        basepointToSecretMap.TryAdd(delayedPaymentBasePubKey, delayedPaymentBaseKey) |> ignore
        basepointToSecretMap.TryAdd(htlcBasePubKey, htlcBaseKey) |> ignore

    member this.NodeSecret = nodeSecret
    member this.DestinationScript = destinationKey.PubKey.WitHash.ScriptPubKey
    member this.ShutDownKey = shutdownKey
    member this.ShutDownPubKey = shutdownKey.PubKey
    member this.CommitmentSeed = commitmentSeed
    member this.FundingKey = fundingKey
    member this.FundingPubKey = fundingPubKey
    member this.RevocationBaseKey = revocationBaseKey
    member this.RevocationBasePubKey = revocationBasePubKey
    member this.PaymentBaseKey = paymentBaseKey
    member this.PaymentBasePubKey = paymentBasePubKey
    member this.DelayedPaymentBaseKey = delayedPaymentBaseKey
    member this.DelayedPaymentBasePubKey = delayedPaymentBasePubKey
    member this.HtlcBaseKey = htlcBaseKey
    member this.HtlcBasePubKey = htlcBasePubKey

    member val BasepointToSecretMap = basepointToSecretMap

    member this.GetChannelKeys() =
        {
            FundingKey = this.FundingKey
            RevocationBaseKey = this.RevocationBaseKey
            PaymentBaseKey = this.PaymentBaseKey
            DelayedPaymentBaseKey = this.DelayedPaymentBaseKey
            HTLCBaseKey = this.HtlcBaseKey
            CommitmentSeed = this.CommitmentSeed
        }
    interface IKeysRepository with
        // TODO: Update
        member this.GetChannelKeys(_inbound): ChannelKeys =
            this.GetChannelKeys()
        member this.GetDestinationScript() =
            this.DestinationScript
        member this.GetShutdownPubKey(): PubKey =
            this.ShutDownPubKey
        member this.GetNodeSecret() =
            this.NodeSecret.PrivateKey

        member this.GetSignatureFor(psbt, pubkey) =
            let priv = this.BasepointToSecretMap.TryGet pubkey
            psbt.SignWithKeys(priv) |> ignore
            match psbt.GetMatchingSig(pubkey) with
            | Some signature -> (signature, psbt)
            | None -> failwithf "Failed to get signature for %A. by pubkey(%A). This should never happen" psbt pubkey

        member this.GenerateKeyFromBasePointAndSign(psbt, pubkey, basePoint) =
            use ctx = CryptoUtils.impl.newSecp256k1()
            let basepointSecret: Key = this.BasepointToSecretMap.TryGet pubkey
            let priv2 = Generators.derivePrivKey ctx (basepointSecret)  basePoint 
            psbt.SignWithKeys(priv2) |> ignore
            match psbt.GetMatchingSig(priv2.PubKey) with
            | Some signature -> (signature, psbt)
            | None ->
                failwithf
                    "failed to get signature for %A . \n input pubkey was: (%A).\n and basepoint was(%A)"
                    psbt pubkey basePoint

        member this.GenerateKeyFromRemoteSecretAndSign(_psbt, _pubkey, _remoteSecret) =
            failwith "Not implemented: DefaultKeyRepository::GenerateKeyFromRemoteSecretAndSign"
