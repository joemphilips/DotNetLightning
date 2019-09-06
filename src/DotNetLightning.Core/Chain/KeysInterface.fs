namespace DotNetLightning.Chain
open NBitcoin
open NBitcoin.Crypto
open DotNetLightning.Utils.Primitives
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
    /// Get a unique temporary channel id. Channel will be refered to by this until the funding TX is
    /// created, at which point they will use the outpoint in the funding TX.
    abstract member GetChannelId: unit -> ChannelId

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
    member this.NodeSecret = masterKey.Derive(0, true)
    member this.DestinationScript = destinationKey.PrivateKey.PubKey.Hash.ScriptPubKey
    member this.ShutDownPubKey = masterKey.Derive(2, true).PrivateKey.PubKey
    member this.ChannelMasterKey = masterKey.Derive(3, true)
    member this.SessionMasterKey = masterKey.Derive(4, true)
    member this.ChannelIdMasterKey = masterKey.Derive(5, true)
    member val ChannelIdChildIndex = 0u with get, set
    member val SessionChildIndex = 0u with get, set
    member val BasepointToSecretMap = Map.empty with get, set
    interface IKeysRepository with
        member this.GetChannelId(): ChannelId = 
            let idx = this.ChannelIdChildIndex
            let childPrivKey  = this.ChannelIdMasterKey.Derive((int)idx, true)
            this.ChannelIdChildIndex <- this.ChannelIdChildIndex + 1u
            ChannelId (uint256(Hashes.SHA256(childPrivKey.ToBytes())))
        // TODO: Update
        member this.GetChannelKeys(_inbound): ChannelKeys = 
            let seed = uint256(RandomUtils.GetBytes(32))
            let keys = [ for _ in 0..4 -> Key() ]
            let basepointAndSecrets = keys |> List.map(fun k -> k.PubKey, k)
            this.BasepointToSecretMap <- basepointAndSecrets |> Map.ofList
            {
                FundingKey = keys.[0]
                RevocationBaseKey = keys.[1]
                PaymentBaseKey = keys.[2]
                DelayedPaymentBaseKey = keys.[3]
                HTLCBaseKey = keys.[4]
                CommitmentSeed = seed
            }
        member this.GetDestinationScript() =
            this.DestinationScript
        member this.GetSessionKey(): Key = 
            this.SessionMasterKey.PrivateKey
        member this.GetShutdownPubKey(): PubKey = 
            this.ShutDownPubKey
        member this.GetNodeSecret() =
            this.NodeSecret.PrivateKey

        member this.GetSignatureFor(psbt, pubkey) =
            let priv = this.BasepointToSecretMap |> Map.find pubkey
            psbt.SignWithKeys(priv) |> ignore
            (psbt.GetMatchingSig(priv.PubKey), psbt)

        member this.GenerateKeyFromBasePointAndSign(psbt, pubkey, basePoint) =
            use ctx = new Secp256k1Net.Secp256k1()
            let basepointSecret: Key = this.BasepointToSecretMap |> Map.find pubkey
            let priv2 = Generators.derivePrivKey ctx (basepointSecret)  basePoint 
            psbt.SignWithKeys(priv2) |> ignore
            (psbt.GetMatchingSig(priv2.PubKey), psbt)

        member this.GenerateKeyFromRemoteSecretAndSign(psbt, pubkey, remoteSecret) =
            failwith ""
