namespace DotNetLightning
open Microsoft.Extensions.Logging
open NBitcoin
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


/// Interface to describe an object which can get user secrets and key material.
type IKeysRepository =
    /// Secret key for node_id a.k.a network_key
    abstract member GetNodeSecret: unit -> Key
    abstract member GetDestinationScript: unit -> Script
    abstract member GetShutdownPubKey: unit -> PubKey
    /// Get a new set of ChannelKeys for per-channel secrets. These MUST be unique even if you
    /// restarted with some stale data.
    abstract member GetChannelKeys: unit -> ChannelKeys
    /// Get a secret for constructing onion packet
    abstract member GetSessionKey: unit -> Key
    /// Get a unique temporary channel id. Channel will be refered to by this until the funding TX is
    /// created, at which point they will use the outpoint in the funding TX.
    abstract member GetChannelId: unit -> uint256


/// `KeyManager` in rust-lightning
type DefaultKeyRepository(seed: uint256, network: Network, logger: ILogger) =
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
    interface IKeysRepository with
        member this.GetChannelId(): uint256 = 
            let idx = this.ChannelIdChildIndex
            let childPrivKey  = this.ChannelIdMasterKey.Derive((int)idx, true)
            this.ChannelIdChildIndex <- this.ChannelIdChildIndex + 1u
            uint256(Hashes.SHA256(childPrivKey.ToBytes()))
        // TODO: Update
        member this.GetChannelKeys(): ChannelKeys = 
            let seed = uint256(RandomUtils.GetBytes(32))
            {
                FundingKey = Key ()
                RevocationBaseKey = Key()
                PaymentBaseKey = Key()
                DelayedPaymentBaseKey = Key()
                HTLCBaseKey = Key()
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
