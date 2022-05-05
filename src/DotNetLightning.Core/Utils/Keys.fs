namespace DotNetLightning.Utils

open System
open NBitcoin

open ResultUtils
open ResultUtils.Portability

/// Type wrapper to annotate that this pubkey is used for locking the funding
/// txo with 2-of-2 multisig.
/// Which is exchanged in `open_channel` to create funding tx.
type FundingPubKey =
    | FundingPubKey of PubKey

    member this.RawPubKey() : PubKey =
        let (FundingPubKey pubKey) = this
        pubKey

    member this.ToBytes() : array<byte> =
        this.RawPubKey().ToBytes()

    override this.ToString() =
        this.RawPubKey().ToString()

/// Type wrapper to annotate that this privkey corresponds to the pubkey which
/// is locking the funding txo.
type FundingPrivKey =
    | FundingPrivKey of Key

    member this.RawKey() : Key =
        let (FundingPrivKey key) = this
        key

    member this.FundingPubKey() : FundingPubKey =
        FundingPubKey(this.RawKey().PubKey)

/// Type wrapper for `revocation_basepoint`. Which is exchange in channel
/// creation, and used later to create `revocationpubkey` see [bolt03](https://github.com/lightning/bolts/blob/master/03-transactions.md)
/// For the detail.
type RevocationBasepoint =
    | RevocationBasepoint of PubKey

    member this.RawPubKey() : PubKey =
        let (RevocationBasepoint pubKey) = this
        pubKey

    member this.ToBytes() : array<byte> =
        this.RawPubKey().ToBytes()

    override this.ToString() =
        this.RawPubKey().ToString()

/// The secret which corresponds to `RevocationBasepoint`.
type RevocationBasepointSecret =
    | RevocationBasepointSecret of Key

    member this.RawKey() : Key =
        let (RevocationBasepointSecret key) = this
        key

    member this.RevocationBasepoint() : RevocationBasepoint =
        RevocationBasepoint(this.RawKey().PubKey)

    member this.ToBytes() : array<byte> =
        this.RawKey().ToBytes()

/// PubKey which is necessary for each new commitment tx.
/// Which can be derived from `RevocationBasepoint` and `PerCommitmentPoint`
/// The corresponding privkey (i.e. `RevocationPrivKey`) can be derived when
/// The counterparty reveals the `per_commitment_secret` Thus ensures the revocation
/// Of the commitment. (Which is a crucial step for LN payment.)
/// See [bolt03](https://github.com/lightning/bolts/blob/master/03-transactions.md)
/// For more detail.
type RevocationPubKey =
    | RevocationPubKey of PubKey

    member this.RawPubKey() : PubKey =
        let (RevocationPubKey pubKey) = this
        pubKey

    static member FromBytes(bytes: array<byte>) : RevocationPubKey =
        RevocationPubKey <| PubKey bytes

    member this.ToBytes() : array<byte> =
        this.RawPubKey().ToBytes()

    override this.ToString() =
        this.RawPubKey().ToString()

/// Privkey corresponds to `RevocationPubKey`.
/// Which is necessary in a dirty-close (that is, when the counterparty
/// tries to cheat by publishing an old commitment tx.)
/// This can be derived from `PerCommitmentSecret`, thus your node doesn't have
/// to remember this. Instead, use `PerCommitmentSecretStore` to remember
/// `PerCommitmentSecret` and derive this from it.
type RevocationPrivKey =
    | RevocationPrivKey of Key

    member this.RawKey() : Key =
        let (RevocationPrivKey key) = this
        key

    member this.ToBytes() : array<byte> =
        this.RawKey().ToBytes()

/// elliptic curve point (a.k.a. pubkey) to derive `remotepubkey`.
/// This is static for each channel and it is negotiated
/// during channel opening.
/// We don't want to use same pubkeys for each commitments more than once,
/// so the key derivation needs new `PerCommitmentPoint` for each HTLCs.
/// see [bolt03](https://github.com/lightning/bolts/blob/master/03-transactions.md)
/// for more detail.
///
/// In case of `option_static_remotekey` feature bit is
/// negotiated, this corresponds to `remotepubkey` which the public key that
/// counterparty gets paid to when the channel closes.
type PaymentBasepoint =
    | PaymentBasepoint of PubKey

    member this.RawPubKey() : PubKey =
        let (PaymentBasepoint pubKey) = this
        pubKey

    member this.ToBytes() : array<byte> =
        this.RawPubKey().ToBytes()

    override this.ToString() =
        this.RawPubKey().ToString()

/// secret which corresponds to `PaymentBasepoint`
/// This is not part of the p2p msg, and it must be kept secret for whole
/// channel lifetime.
type PaymentBasepointSecret =
    | PaymentBasepointSecret of Key

    member this.RawKey() : Key =
        let (PaymentBasepointSecret key) = this
        key

    member this.PaymentBasepoint() : PaymentBasepoint =
        PaymentBasepoint(this.RawKey().PubKey)

/// The pubkey for `to_local` or `to_remote` output scriptpubkey.
/// Usually rotated for each new HTLC, but it corresponds directly to
/// `PaymentBasepoint` in case that `option_static_remotekey` feature bit is
/// negotiated. Thus static for whole channel lifetime.
type PaymentPubKey =
    | PaymentPubKey of PubKey

    member this.RawPubKey() : PubKey =
        let (PaymentPubKey pubKey) = this
        pubKey

    member this.ToBytes() : array<byte> =
        this.RawPubKey().ToBytes()

    override this.ToString() =
        this.RawPubKey().ToString()

/// Private key corresponds to `PaymentPubKey`. Usually not part of the p2p
/// messaging. and it is only for spending the funds after channel closure.
type PaymentPrivKey =
    | PaymentPrivKey of Key

    member this.RawKey() : Key =
        let (PaymentPrivKey key) = this
        key

    member this.PaymentPubKey() : PaymentPubKey =
        PaymentPubKey <| this.RawKey().PubKey

/// elliptic curve point (a.k.a. pubkey) to derive `[remote|local]_delayedpubkey`.
/// This is static for each channel and it is negotiated
/// during channel opening.
/// We don't want to use same pubkeys for each commitments more than once,
/// so the key derivation needs new `PerCommitmentPoint` for each HTLCs.
/// see [bolt03](https://github.com/lightning/bolts/blob/master/03-transactions.md)
/// for more detail.
type DelayedPaymentBasepoint =
    | DelayedPaymentBasepoint of PubKey

    member this.RawPubKey() : PubKey =
        let (DelayedPaymentBasepoint pubKey) = this
        pubKey

    member this.ToBytes() : array<byte> =
        this.RawPubKey().ToBytes()

    override this.ToString() =
        this.RawPubKey().ToString()

/// secret which corresponds to `DelayedPaymentBasepoint`
/// This is not part of the p2p msg, and it must be kept secret for whole
/// channel lifetime.
type DelayedPaymentBasepointSecret =
    | DelayedPaymentBasepointSecret of Key

    member this.RawKey() : Key =
        let (DelayedPaymentBasepointSecret key) = this
        key

    member this.DelayedPaymentBasepoint() : DelayedPaymentBasepoint =
        DelayedPaymentBasepoint(this.RawKey().PubKey)

/// The pubkey used for `to_self` output in htlc-success/htlc-timeout tx.
type DelayedPaymentPubKey =
    | DelayedPaymentPubKey of PubKey

    member this.RawPubKey() : PubKey =
        let (DelayedPaymentPubKey pubKey) = this
        pubKey

    static member FromBytes(bytes: array<byte>) : DelayedPaymentPubKey =
        DelayedPaymentPubKey <| PubKey bytes

    member this.ToBytes() : array<byte> =
        this.RawPubKey().ToBytes()

    override this.ToString() =
        this.RawPubKey().ToString()

/// The private key which corresponds to `DelayedPaymentPubKey`.
type DelayedPaymentPrivKey =
    | DelayedPaymentPrivKey of Key

    member this.RawKey() : Key =
        let (DelayedPaymentPrivKey key) = this
        key

    member this.DelayedPaymentPubKey() : DelayedPaymentPubKey =
        DelayedPaymentPubKey <| this.RawKey().PubKey

/// elliptic curve point (a.k.a. pubkey) to derive `[remote|local]_htlcpubkey`.
/// This is static for each channel and it is negotiated
/// during channel opening.
/// We don't want to use same pubkeys for each commitments more than once,
/// so the key derivation needs new `PerCommitmentPoint` for each HTLCs.
/// see [bolt03](https://github.com/lightning/bolts/blob/master/03-transactions.md)
/// for more detail.
type HtlcBasepoint =
    | HtlcBasepoint of PubKey

    member this.RawPubKey() : PubKey =
        let (HtlcBasepoint pubKey) = this
        pubKey

    member this.ToBytes() : array<byte> =
        this.RawPubKey().ToBytes()

    override this.ToString() =
        this.RawPubKey().ToString()

/// secret which corresponds to `DelayedPaymentBasepoint`
/// This is not part of the p2p msg, and it must be kept secret for whole
/// channel lifetime.
type HtlcBasepointSecret =
    | HtlcBasepointSecret of Key

    member this.RawKey() : Key =
        let (HtlcBasepointSecret key) = this
        key

    member this.HtlcBasepoint() : HtlcBasepoint =
        HtlcBasepoint(this.RawKey().PubKey)

/// The pubkey used in htlc output to ensure that no one else other
/// than the 2-party can have chance to claim the htlc.
type HtlcPubKey =
    | HtlcPubKey of PubKey

    member this.RawPubKey() : PubKey =
        let (HtlcPubKey pubKey) = this
        pubKey

    member this.ToBytes() : array<byte> =
        this.RawPubKey().ToBytes()

    override this.ToString() =
        this.RawPubKey().ToString()

/// The private key which corresponds to `HtlcPubKey`.
type HtlcPrivKey =
    | HtlcPrivKey of Key

    member this.RawKey() : Key =
        let (HtlcPrivKey key) = this
        key

    member this.HtlcPubKey() : HtlcPubKey =
        HtlcPubKey <| this.RawKey().PubKey

/// Private key which corresponds to `NodeId` 1-by-1.
/// Which is necessary for ECDH key exchange for transport encryption.
type NodeSecret =
    | NodeSecret of Key

    member this.RawKey() : Key =
        let (NodeSecret key) = this
        key

    member this.NodeId() : NodeId =
        NodeId(this.RawKey().PubKey)

/// set of static pubkeys for each channel.
/// corresponds to `ChannelPrivKeys` in case of our own keys.
type ChannelPubKeys =
    {
        FundingPubKey: FundingPubKey
        RevocationBasepoint: RevocationBasepoint
        PaymentBasepoint: PaymentBasepoint
        DelayedPaymentBasepoint: DelayedPaymentBasepoint
        HtlcBasepoint: HtlcBasepoint
    }

/// set of pubkeys which get rotated for every commitment in the channel.
/// Generated from `ChannelPubKeys` and `PerCommitmentPoint`
type CommitmentPubKeys =
    {
        RevocationPubKey: RevocationPubKey
        PaymentPubKey: PaymentPubKey
        DelayedPaymentPubKey: DelayedPaymentPubKey
        HtlcPubKey: HtlcPubKey
    }

/// Instead of sending actual pubkeys necessary for commitment,
/// Lighting Nodes send `per_commitment_point` for each commitment
/// which enables to derive all other pubkeys.
/// This is a type to annotate such commitment point, an actual value is
/// elliptic curve point (a.k.a. public key).
type PerCommitmentPoint =
    | PerCommitmentPoint of PubKey

    member this.RawPubKey() : PubKey =
        let (PerCommitmentPoint pubKey) = this
        pubKey

    static member BytesLength: int = PubKey.BytesLength

    static member FromBytes(bytes: array<byte>) : PerCommitmentPoint =
        PerCommitmentPoint <| PubKey bytes

    member this.ToBytes() : array<byte> =
        this.RawPubKey().ToBytes()

/// 48-bit incrementing counter for each commitment transaction
/// This is set to commitment tx locktime
/// (in obscured form), see [bolt03-transactions](https://github.com/lightning/bolts/blob/master/03-transactions.md)
/// for more detail.
#if !NoDUsAsStructs
[<Struct>]
#endif
type CommitmentNumber =
    | CommitmentNumber of UInt48

    member this.Index() =
        let (CommitmentNumber index) = this
        index

    override this.ToString() =
        sprintf
            "%012x (#%i)"
            (this.Index().UInt64)
            (UInt48.MaxValue - this.Index()).UInt64

    static member LastCommitment: CommitmentNumber =
        CommitmentNumber UInt48.Zero

    static member FirstCommitment: CommitmentNumber =
        CommitmentNumber UInt48.MaxValue

    member this.PreviousCommitment() : CommitmentNumber =
        CommitmentNumber(this.Index() + UInt48.One)

    member this.NextCommitment() : CommitmentNumber =
        CommitmentNumber(this.Index() - UInt48.One)

/// An actual value set to commitment tx locktime.
#if !NoDUsAsStructs
[<Struct>]
#endif
type ObscuredCommitmentNumber =
    | ObscuredCommitmentNumber of UInt48

    member this.ObscuredIndex() : UInt48 =
        let (ObscuredCommitmentNumber obscuredIndex) = this
        obscuredIndex

    override this.ToString() =
        sprintf "%012x" (this.ObscuredIndex().UInt64)

/// `per_commitment_secret` in bolt.
/// By revealing this value to counterparty, you can revoke the commitment.
/// And thus complete the state transition in LN channel.
type PerCommitmentSecret =
    | PerCommitmentSecret of Key

    member this.RawKey() : Key =
        let (PerCommitmentSecret key) = this
        key

    static member BytesLength: int = Key.BytesLength

    static member FromBytes(bytes: array<byte>) : PerCommitmentSecret =
        PerCommitmentSecret <| new Key(bytes)

    member this.ToBytes() : array<byte> =
        this.RawKey().ToBytes()

    member this.PerCommitmentPoint() : PerCommitmentPoint =
        PerCommitmentPoint <| this.RawKey().PubKey

/// Seed to generate all other `PerCommitmentSecret`.
/// The generation algorithm is described in [bolt03](https://github.com/lightning/bolts/blob/master/03-transactions.md)
type CommitmentSeed =
    | CommitmentSeed of PerCommitmentSecret

    member this.LastPerCommitmentSecret() =
        let (CommitmentSeed lastPerCommitmentSecret) = this
        lastPerCommitmentSecret

/// Set of lightning keys needed to operate a channel as describe in BOLT 3
type ChannelPrivKeys =
    {
        FundingPrivKey: FundingPrivKey
        RevocationBasepointSecret: RevocationBasepointSecret
        PaymentBasepointSecret: PaymentBasepointSecret
        DelayedPaymentBasepointSecret: DelayedPaymentBasepointSecret
        HtlcBasepointSecret: HtlcBasepointSecret
        CommitmentSeed: CommitmentSeed
    }

    member this.ToChannelPubKeys() : ChannelPubKeys =
        {
            FundingPubKey = this.FundingPrivKey.FundingPubKey()
            RevocationBasepoint =
                this.RevocationBasepointSecret.RevocationBasepoint()
            PaymentBasepoint = this.PaymentBasepointSecret.PaymentBasepoint()
            DelayedPaymentBasepoint =
                this.DelayedPaymentBasepointSecret.DelayedPaymentBasepoint()
            HtlcBasepoint = this.HtlcBasepointSecret.HtlcBasepoint()
        }

/// This is the node-wide master key which is also used for
/// transport-level encryption. The channel's keys can be derived from
/// this via BIP32 key derivation.
/// It is basically `NodeSecret` + "ChainCode (an entropy for deriving the children)"
type NodeMasterPrivKey =
    | NodeMasterPrivKey of ExtKey

    member this.RawExtKey() : ExtKey =
        let (NodeMasterPrivKey extKey) = this
        extKey

    member this.NodeSecret() : NodeSecret =
        NodeSecret(this.RawExtKey().PrivateKey)

    member this.NodeId() : NodeId =
        this.NodeSecret().NodeId()
