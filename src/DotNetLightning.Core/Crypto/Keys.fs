namespace DotNetLightning.Crypto

open System
open NBitcoin
open NBitcoin.Crypto

open DotNetLightning.Utils

// FIXME: Should the [<Struct>]-annotated types here be changed to records or single-constructor discriminated unions? 
    
/// Set of lightning keys needed to operate a channel as describe in BOLT 3
type ChannelKeys = {
    FundingKey: Key
    RevocationBaseKey: Key
    PaymentBaseKey: Key
    DelayedPaymentBaseKey: Key
    HTLCBaseKey: Key
    CommitmentSeed: CommitmentSeed
} with
    member this.ToChannelPubKeys(): ChannelPubKeys =
        {
            FundingPubKey = this.FundingKey.PubKey
            RevocationBasePubKey = this.RevocationBaseKey.PubKey
            PaymentBasePubKey = this.PaymentBaseKey.PubKey
            DelayedPaymentBasePubKey = this.DelayedPaymentBaseKey.PubKey
            HTLCBasePubKey = this.HTLCBaseKey.PubKey
        }

/// In usual operation we should not hold secrets on memory. So only hold pubkey
and ChannelPubKeys = {
    FundingPubKey: PubKey
    RevocationBasePubKey: PubKey
    PaymentBasePubKey: PubKey
    DelayedPaymentBasePubKey: PubKey
    HTLCBasePubKey: PubKey
}

and [<Struct>] PerCommitmentSecret(key: Key) =
    member this.RawKey(): Key =
        key

    static member BytesLength: int = Key.BytesLength

    static member FromBytes(bytes: array<byte>): PerCommitmentSecret =
        PerCommitmentSecret <| new Key(bytes)

    member this.ToBytes(): array<byte> =
        this.RawKey().ToBytes()

    member this.DeriveChild (thisCommitmentNumber: CommitmentNumber)
                            (childCommitmentNumber: CommitmentNumber)
                                : Option<PerCommitmentSecret> =
        if thisCommitmentNumber.Subsumes childCommitmentNumber then
            let commonBits = thisCommitmentNumber.Index.TrailingZeros()
            let index = childCommitmentNumber.Index
            let mutable secret = this.ToBytes()
            for bit in (commonBits - 1) .. -1 .. 0 do
                if (index >>> bit) &&& UInt48.One = UInt48.One then
                    let byteIndex = bit / 8
                    let bitIndex = bit % 8
                    secret.[byteIndex] <- secret.[byteIndex] ^^^ (1uy <<< bitIndex)
                    secret <- Hashes.SHA256 secret
            Some <| PerCommitmentSecret(new Key(secret))
        else
            None

    member this.PerCommitmentPoint(): PerCommitmentPoint =
        PerCommitmentPoint <| this.RawKey().PubKey

and [<Struct>] PerCommitmentPoint(pubKey: PubKey) =
    member this.RawPubKey(): PubKey =
        pubKey

    static member BytesLength: int = PubKey.BytesLength

    static member FromBytes(bytes: array<byte>): PerCommitmentPoint =
        PerCommitmentPoint <| PubKey bytes

    member this.ToBytes(): array<byte> =
        this.RawPubKey().ToBytes()

and [<Struct>] CommitmentSeed(lastPerCommitmentSecret: PerCommitmentSecret) =
    new(key: Key) =
        CommitmentSeed(PerCommitmentSecret key)

    member this.LastPerCommitmentSecret = lastPerCommitmentSecret

    member this.DerivePerCommitmentSecret (commitmentNumber: CommitmentNumber): PerCommitmentSecret =
        let res =
            this.LastPerCommitmentSecret.DeriveChild
                CommitmentNumber.LastCommitment
                commitmentNumber
        match res with
        | Some perCommitmentSecret -> perCommitmentSecret
        | None ->
            failwith
                "The final per commitment secret should be able to derive the \
                commitment secret for all prior commitments. This is a bug."

    member this.DerivePerCommitmentPoint (commitmentNumber: CommitmentNumber): PerCommitmentPoint =
        let perCommitmentSecret = this.DerivePerCommitmentSecret commitmentNumber
        perCommitmentSecret.PerCommitmentPoint()

