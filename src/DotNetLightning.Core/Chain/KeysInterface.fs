namespace DotNetLightning.Chain
open System.Collections.Concurrent
open System.Collections.Concurrent
open System.Text
open NBitcoin
open NBitcoin.Crypto
open DotNetLightning.Utils.Primitives
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Crypto
open NBitcoin.Crypto

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
    CommitmentSeed: uint256
}

    with
        member this.ToChannelPubKeys() =
            {
                FundingPubKey = this.FundingKey.PubKey
                RevocationBasePubKey = this.RevocationBaseKey.PubKey
                PaymentBasePubKey = this.PaymentBaseKey.PubKey
                DelayedPaymentBasePubKey = this.DelayedPaymentBaseKey.PubKey
                HTLCBasePubKey = this.HTLCBaseKey.PubKey
                CommitmentSeed = this.CommitmentSeed
            }

/// In usual operation we should not hold secrets on memory. So only hold pubkey
and ChannelPubKeys =  {
    FundingPubKey: PubKey
    RevocationBasePubKey: PubKey
    PaymentBasePubKey: PubKey
    DelayedPaymentBasePubKey: PubKey
    HTLCBasePubKey: PubKey
    CommitmentSeed: uint256
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
    /// Get a secret for constructing onion packet
    abstract member GetSessionKey: unit -> Key

    /// Must add signature to the PSBT *And* return the signature
    abstract member GetSignatureFor: psbt: PSBT * pubKey: PubKey -> TransactionSignature * PSBT
    /// Must add signature to the PSBT *And* return the signature
    abstract member GenerateKeyFromBasePointAndSign: psbt: PSBT * pubkey: PubKey * basePoint: PubKey -> TransactionSignature * PSBT
    /// Must add signature to the PSBT *And* return the signature
    abstract member GenerateKeyFromRemoteSecretAndSign: psbt: PSBT * pubKey: PubKey * remoteSecret : Key -> TransactionSignature * PSBT

/// `KeyManager` in rust-lightning
type DefaultKeyRepository(seed: uint256) =
    let masterKey = ExtKey(seed.ToBytes())
    let destinationKey = masterKey.Derive(1, true)
    let _lockObj = obj()
    let mutable _childIndex = 0
    let _utf8 = Encoding.UTF8
    member this.NodeSecret = masterKey.Derive(0, true)
    member this.DestinationScript = destinationKey.PrivateKey.PubKey.WitHash.ScriptPubKey
    member this.ShutDownPubKey = masterKey.Derive(2, true).PrivateKey.PubKey
    member this.ChannelMasterKey = masterKey.Derive(3, true)
    member this.SessionMasterKey = masterKey.Derive(4, true)
    member this.ChannelIdMasterKey = masterKey.Derive(5, true)
    member val SessionChildIndex = 0u with get, set
    member val BasepointToSecretMap = ConcurrentDictionary<PubKey, Key>() with get, set
    member this.GetChannelKeys() =
        lock _lockObj (fun _ -> _childIndex <- _childIndex + 1)
        let childPrivKey = masterKey.Derive(_childIndex, true)
        let seed = Hashes.SHA256(childPrivKey.ToBytes())
        let commitmentSeed = Array.append seed ("commitment seed" |> _utf8.GetBytes) |> Hashes.SHA256
        let fundingKey = Array.concat[| seed; commitmentSeed; ("funding pubkey" |> _utf8.GetBytes) |] |> Hashes.SHA256
        let revocationBaseKey = Array.concat[| seed; fundingKey; ("revocation base key" |> _utf8.GetBytes) |] |> Hashes.SHA256
        let paymentBaseKey = Array.concat[| seed; revocationBaseKey; ("payment base key" |> _utf8.GetBytes) |] |> Hashes.SHA256
        let delayedPaymentBaseKey = Array.concat[| seed; paymentBaseKey; ("delayed payment base key" |> _utf8.GetBytes) |] |> Hashes.SHA256
        let htlcBaseKey = Array.concat[| seed; delayedPaymentBaseKey; ("htlc base key" |> _utf8.GetBytes) |] |> Hashes.SHA256
        let keys = [ fundingKey; revocationBaseKey; paymentBaseKey; delayedPaymentBaseKey; htlcBaseKey ] |> List.map Key
        let basepointAndSecrets = keys |> List.map(fun k -> k.PubKey, k)
        basepointAndSecrets
        |> List.iter(fun (k ,v) ->
            this.BasepointToSecretMap.TryAdd(k, v) |> ignore)
        {
            FundingKey = keys.[0]
            RevocationBaseKey = keys.[1]
            PaymentBaseKey = keys.[2]
            DelayedPaymentBaseKey = keys.[3]
            HTLCBaseKey = keys.[4]
            CommitmentSeed = seed |> uint256
        }
    interface IKeysRepository with
        // TODO: Update
        member this.GetChannelKeys(_inbound): ChannelKeys =
            this.GetChannelKeys()
        member this.GetDestinationScript() =
            this.DestinationScript
        member this.GetSessionKey(): Key = 
            this.SessionMasterKey.PrivateKey
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
            use ctx = new Secp256k1Net.Secp256k1()
            let basepointSecret: Key = this.BasepointToSecretMap.TryGet pubkey
            let priv2 = Generators.derivePrivKey ctx (basepointSecret)  basePoint 
            psbt.SignWithKeys(priv2) |> ignore
            match psbt.GetMatchingSig(priv2.PubKey) with
            | Some signature -> (signature, psbt)
            | None ->
                failwithf
                    "failed to get signature for %A . \n input pubkey was: (%A).\n and basepoint was(%A)"
                    psbt pubkey basePoint

        member this.GenerateKeyFromRemoteSecretAndSign(psbt, pubkey, remoteSecret) =
            failwith ""
