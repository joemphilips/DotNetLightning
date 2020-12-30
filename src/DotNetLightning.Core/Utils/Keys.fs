namespace DotNetLightning.Utils

open System
open NBitcoin

open ResultUtils
open ResultUtils.Portability

type FundingPubKey =
    | FundingPubKey of PubKey
    with
    member this.RawPubKey(): PubKey =
        let (FundingPubKey pubKey) = this
        pubKey

    member this.ToBytes(): array<byte> =
        this.RawPubKey().ToBytes()

    override this.ToString() =
        this.RawPubKey().ToString()

type FundingPrivKey =
    | FundingPrivKey of Key
    with
    member this.RawKey(): Key =
        let (FundingPrivKey key) = this
        key

    member this.FundingPubKey(): FundingPubKey =
        FundingPubKey(this.RawKey().PubKey)

type RevocationBasepoint =
    | RevocationBasepoint of PubKey
    with
    member this.RawPubKey(): PubKey =
        let (RevocationBasepoint pubKey) = this
        pubKey

    member this.ToBytes(): array<byte> =
        this.RawPubKey().ToBytes()

    override this.ToString() =
        this.RawPubKey().ToString()

type RevocationBasepointSecret =
    | RevocationBasepointSecret of Key
    with
    member this.RawKey(): Key =
        let (RevocationBasepointSecret key) = this
        key

    member this.RevocationBasepoint(): RevocationBasepoint =
        RevocationBasepoint(this.RawKey().PubKey)

    member this.ToBytes(): array<byte> =
        this.RawKey().ToBytes()

type RevocationPubKey =
    | RevocationPubKey of PubKey
    with
    member this.RawPubKey(): PubKey =
        let (RevocationPubKey pubKey) = this
        pubKey

    static member FromBytes(bytes: array<byte>): RevocationPubKey =
        RevocationPubKey <| PubKey bytes

    member this.ToBytes(): array<byte> =
        this.RawPubKey().ToBytes()

    override this.ToString() =
        this.RawPubKey().ToString()

type RevocationPrivKey =
    | RevocationPrivKey of Key
    with
    member this.RawKey(): Key =
        let (RevocationPrivKey key) = this
        key

    member this.ToBytes(): array<byte> =
        this.RawKey().ToBytes()

type PaymentBasepoint =
    | PaymentBasepoint of PubKey
    with
    member this.RawPubKey(): PubKey =
        let (PaymentBasepoint pubKey) = this
        pubKey

    member this.ToBytes(): array<byte> =
        this.RawPubKey().ToBytes()

    override this.ToString() =
        this.RawPubKey().ToString()

type PaymentBasepointSecret =
    | PaymentBasepointSecret of Key
    with
    member this.RawKey(): Key =
        let (PaymentBasepointSecret key) = this
        key

    member this.PaymentBasepoint(): PaymentBasepoint =
        PaymentBasepoint(this.RawKey().PubKey)

type PaymentPubKey =
    | PaymentPubKey of PubKey
    with
    member this.RawPubKey(): PubKey =
        let (PaymentPubKey pubKey) = this
        pubKey

    member this.ToBytes(): array<byte> =
        this.RawPubKey().ToBytes()

    override this.ToString() =
        this.RawPubKey().ToString()

type PaymentPrivKey =
    | PaymentPrivKey of Key
    with
    member this.RawKey(): Key =
        let (PaymentPrivKey key) = this
        key

    member this.PaymentPubKey(): PaymentPubKey =
        PaymentPubKey <| this.RawKey().PubKey

type DelayedPaymentBasepoint =
    | DelayedPaymentBasepoint of PubKey
    with
    member this.RawPubKey(): PubKey =
        let (DelayedPaymentBasepoint pubKey) = this
        pubKey

    member this.ToBytes(): array<byte> =
        this.RawPubKey().ToBytes()

    override this.ToString() =
        this.RawPubKey().ToString()

type DelayedPaymentBasepointSecret =
    | DelayedPaymentBasepointSecret of Key
    with
    member this.RawKey(): Key =
        let (DelayedPaymentBasepointSecret key) = this
        key

    member this.DelayedPaymentBasepoint(): DelayedPaymentBasepoint =
        DelayedPaymentBasepoint(this.RawKey().PubKey)

type DelayedPaymentPubKey =
    | DelayedPaymentPubKey of PubKey
    with
    member this.RawPubKey(): PubKey =
        let (DelayedPaymentPubKey pubKey) = this
        pubKey

    static member FromBytes(bytes: array<byte>): DelayedPaymentPubKey =
        DelayedPaymentPubKey <| PubKey bytes

    member this.ToBytes(): array<byte> =
        this.RawPubKey().ToBytes()

    override this.ToString() =
        this.RawPubKey().ToString()

type DelayedPaymentPrivKey =
    | DelayedPaymentPrivKey of Key
    with
    member this.RawKey(): Key =
        let (DelayedPaymentPrivKey key) = this
        key

    member this.DelayedPaymentPubKey(): DelayedPaymentPubKey =
        DelayedPaymentPubKey <| this.RawKey().PubKey

type HtlcBasepoint =
    | HtlcBasepoint of PubKey
    with
    member this.RawPubKey(): PubKey =
        let (HtlcBasepoint pubKey) = this
        pubKey

    member this.ToBytes(): array<byte> =
        this.RawPubKey().ToBytes()

    override this.ToString() =
        this.RawPubKey().ToString()

type HtlcBasepointSecret =
    | HtlcBasepointSecret of Key
    with
    member this.RawKey(): Key =
        let (HtlcBasepointSecret key) = this
        key

    member this.HtlcBasepoint(): HtlcBasepoint =
        HtlcBasepoint(this.RawKey().PubKey)

type HtlcPubKey =
    | HtlcPubKey of PubKey
    with
    member this.RawPubKey(): PubKey =
        let (HtlcPubKey pubKey) = this
        pubKey

    member this.ToBytes(): array<byte> =
        this.RawPubKey().ToBytes()

    override this.ToString() =
        this.RawPubKey().ToString()

type HtlcPrivKey =
    | HtlcPrivKey of Key
    with
    member this.RawKey(): Key =
        let (HtlcPrivKey key) = this
        key

    member this.HtlcPubKey(): HtlcPubKey =
        HtlcPubKey <| this.RawKey().PubKey

type NodeSecret =
    | NodeSecret of Key
    with
    member this.RawKey(): Key =
        let (NodeSecret key) = this
        key

    member this.NodeId(): NodeId =
        NodeId (this.RawKey().PubKey)

/// In usual operation we should not hold secrets on memory. So only hold pubkey
type ChannelPubKeys = {
    FundingPubKey: FundingPubKey
    RevocationBasepoint: RevocationBasepoint
    PaymentBasepoint: PaymentBasepoint
    DelayedPaymentBasepoint: DelayedPaymentBasepoint
    HtlcBasepoint: HtlcBasepoint
}

type CommitmentPubKeys = {
    RevocationPubKey: RevocationPubKey
    PaymentPubKey: PaymentPubKey
    DelayedPaymentPubKey: DelayedPaymentPubKey
    HtlcPubKey: HtlcPubKey
}

type PerCommitmentPoint =
    | PerCommitmentPoint of PubKey
    with
    member this.RawPubKey(): PubKey =
        let (PerCommitmentPoint pubKey) = this
        pubKey

    static member BytesLength: int = PubKey.BytesLength

    static member FromBytes(bytes: array<byte>): PerCommitmentPoint =
        PerCommitmentPoint <| PubKey bytes

    member this.ToBytes(): array<byte> =
        this.RawPubKey().ToBytes()

#if !NoDUsAsStructs
[<Struct>]
#endif
type CommitmentNumber =
    | CommitmentNumber of UInt48
    with
    member this.Index() =
        let (CommitmentNumber index) = this
        index

    override this.ToString() =
        sprintf "%012x (#%i)" (this.Index().UInt64) (UInt48.MaxValue - this.Index()).UInt64

    static member LastCommitment: CommitmentNumber =
        CommitmentNumber UInt48.Zero

    static member FirstCommitment: CommitmentNumber =
        CommitmentNumber UInt48.MaxValue

    member this.PreviousCommitment(): CommitmentNumber =
        CommitmentNumber(this.Index() + UInt48.One)

    member this.NextCommitment(): CommitmentNumber =
        CommitmentNumber(this.Index() - UInt48.One)

#if !NoDUsAsStructs
[<Struct>]
#endif
type ObscuredCommitmentNumber =
    | ObscuredCommitmentNumber of UInt48
    with
    member this.ObscuredIndex(): UInt48 =
        let (ObscuredCommitmentNumber obscuredIndex) = this
        obscuredIndex

    override this.ToString() =
        sprintf "%012x" (this.ObscuredIndex().UInt64)

type PerCommitmentSecret =
    | PerCommitmentSecret of Key
    with
    member this.RawKey(): Key =
        let (PerCommitmentSecret key) = this
        key

    static member BytesLength: int = Key.BytesLength

    static member FromBytes(bytes: array<byte>): PerCommitmentSecret =
        PerCommitmentSecret <| new Key(bytes)

    member this.ToBytes(): array<byte> =
        this.RawKey().ToBytes()

    member this.PerCommitmentPoint(): PerCommitmentPoint =
        PerCommitmentPoint <| this.RawKey().PubKey

type CommitmentSeed =
    | CommitmentSeed of PerCommitmentSecret
    with
    member this.LastPerCommitmentSecret() =
        let (CommitmentSeed lastPerCommitmentSecret) = this
        lastPerCommitmentSecret

/// Set of lightning keys needed to operate a channel as describe in BOLT 3
type ChannelPrivKeys = {
    FundingPrivKey: FundingPrivKey
    RevocationBasepointSecret: RevocationBasepointSecret
    PaymentBasepointSecret: PaymentBasepointSecret
    DelayedPaymentBasepointSecret: DelayedPaymentBasepointSecret
    HtlcBasepointSecret: HtlcBasepointSecret
    CommitmentSeed: CommitmentSeed
} with
    member this.ToChannelPubKeys(): ChannelPubKeys =
        {
            FundingPubKey = this.FundingPrivKey.FundingPubKey()
            RevocationBasepoint = this.RevocationBasepointSecret.RevocationBasepoint()
            PaymentBasepoint = this.PaymentBasepointSecret.PaymentBasepoint()
            DelayedPaymentBasepoint = this.DelayedPaymentBasepointSecret.DelayedPaymentBasepoint()
            HtlcBasepoint = this.HtlcBasepointSecret.HtlcBasepoint()
        }

/// This is the node-wide master key which is also used for
/// transport-level encryption. The channel's keys are derived from
/// this via BIP32 key derivation where `channelIndex` is the child
/// index used to derive the channel's master key.
type NodeMasterPrivKey =
    | NodeMasterPrivKey of ExtKey
    with
    member this.RawExtKey(): ExtKey =
        let (NodeMasterPrivKey extKey) = this
        extKey

    member this.NodeSecret(): NodeSecret =
        NodeSecret (this.RawExtKey().PrivateKey)

    member this.NodeId(): NodeId =
        this.NodeSecret().NodeId()

