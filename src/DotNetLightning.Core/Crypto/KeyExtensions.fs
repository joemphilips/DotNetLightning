namespace DotNetLightning.Crypto

open System
open NBitcoin
open NBitcoin.Crypto

open DotNetLightning.Utils
open DotNetLightning.Core.Utils.Extensions

open ResultUtils
open ResultUtils.Portability

[<AutoOpen>]
module NBitcoinArithmethicExtensions =
    /// The functions in this module may fail but it does not return Result and just throw Exception in
    /// case of failure. Why? because the error case is very, very unlikely to happen.
    /// (e.g. two keys we operate were complement of each other.)
    /// It is interesting to consider if it is possible for the attacker to intentionally make this happen,
    /// but it is way beyond my skills, so we just ignore for now.

    let Secp256k1 = CryptoUtils.impl.newSecp256k1()

    type Key with
        static member Mul(lhs: Key, rhs: Key): Key =
            let lhsBytes = lhs.ToBytes()
            let rhsBytes = rhs.ToBytes()
            let tweak = ReadOnlySpan(lhsBytes)
            match Secp256k1.PrivateKeyTweakMultiply(tweak, rhsBytes.AsSpan()) with
            | true -> new Key(rhsBytes)
            | false -> failwith "failed to multiply Keys"

        static member Add(lhs: Key, rhs: Key): Key =
            let lhsBytes = lhs.ToBytes()
            let rhsBytes = rhs.ToBytes()
            let tweak = ReadOnlySpan(lhsBytes)
            match Secp256k1.PrivateKeyTweakAdd(tweak, rhsBytes.AsSpan()) with
            | true -> new Key(rhsBytes)
            | false -> failwithf "failed to add Keys"

    type PubKey with
        member private this.ExpandToBytes(): array<byte> =
            let compressedPubKeyBytes = this.ToBytes()
            let compressedPubKeyBytesSpan = ReadOnlySpan(compressedPubKeyBytes)
            match Secp256k1.PublicKeyParse(compressedPubKeyBytesSpan) with
            | true, expandedPubKeyBytes -> expandedPubKeyBytes
            | false, _ -> failwithf "Failed  to expand public key %A" this

        static member private CompressFromBytes(expandedPubKeyBytes: array<byte>): PubKey =
            let expandedPubKeyBytesSpan = ReadOnlySpan(expandedPubKeyBytes)
            match Secp256k1.PublicKeySerializeCompressed(expandedPubKeyBytesSpan) with
            | true, compressedPubKeyBytes -> PubKey compressedPubKeyBytes
            | false, _ -> failwith "Failed to compress pubkey"

        static member Mul(pubKey: PubKey, key: Key): PubKey =
            let keyBytes = key.ToBytes()
            let pubKeyBytes = pubKey.ExpandToBytes()
            let tweak = ReadOnlySpan(keyBytes)
            match Secp256k1.PublicKeyTweakMultiply(tweak, pubKeyBytes.AsSpan()) with
            | true -> PubKey.CompressFromBytes pubKeyBytes
            | false -> failwith "failed to multiplying PubKey by Key"

        static member Add(lhs: PubKey, rhs: PubKey): PubKey =
            let lhsBytes = lhs.ToBytes()
            let rhsBytes = rhs.ToBytes()
            match Secp256k1.PublicKeyCombine(lhsBytes.AsSpan(), rhsBytes.AsSpan()) with
            | true, result -> PubKey.CompressFromBytes result
            | false, _ -> failwith "failed to add PubKeys"

[<AutoOpen>]
module KeyExtensions =
    type PerCommitmentPoint with
        member this.DerivePrivKey (basepointSecret: Key): Key =
            let basepointBytes =
                let basepoint = basepointSecret.PubKey
                basepoint.ToBytes()
            let perCommitmentPointBytes = this.ToBytes()
            let tweak =
                Key.FromHashOf <| Array.append perCommitmentPointBytes basepointBytes
            Key.Add(basepointSecret, tweak)

        member this.DerivePubKey (basepoint: PubKey): PubKey =
            let basepointBytes = basepoint.ToBytes()
            let perCommitmentPointBytes = this.ToBytes()
            let tweak =
                Key.FromHashOf <| Array.append perCommitmentPointBytes basepointBytes
            PubKey.Add(basepoint, tweak.PubKey)

        member this.DerivePaymentPrivKey (paymentBasepointSecret: PaymentBasepointSecret)
                                             : PaymentPrivKey =
            PaymentPrivKey <|
                this.DerivePrivKey (paymentBasepointSecret.RawKey())

        member this.DerivePaymentPubKey (paymentBasepoint: PaymentBasepoint)
                                            : PaymentPubKey =
            PaymentPubKey <|
                this.DerivePubKey (paymentBasepoint.RawPubKey())

        member this.DeriveDelayedPaymentPrivKey (delayedPaymentBasepointSecret: DelayedPaymentBasepointSecret)
                                                    : DelayedPaymentPrivKey =
            DelayedPaymentPrivKey <|
                this.DerivePrivKey (delayedPaymentBasepointSecret.RawKey())

        member this.DeriveDelayedPaymentPubKey (delayedPaymentBasepoint: DelayedPaymentBasepoint)
                                                   : DelayedPaymentPubKey =
            DelayedPaymentPubKey <|
                this.DerivePubKey (delayedPaymentBasepoint.RawPubKey())

        member this.DeriveHtlcPrivKey (htlcBasepointSecret: HtlcBasepointSecret)
                                          : HtlcPrivKey =
            HtlcPrivKey <|
                this.DerivePrivKey (htlcBasepointSecret.RawKey())

        member this.DeriveHtlcPubKey (htlcBasepoint: HtlcBasepoint)
                                         : HtlcPubKey =
            HtlcPubKey <|
                this.DerivePubKey (htlcBasepoint.RawPubKey())

        member this.DeriveRevocationPubKey (revocationBasepoint: RevocationBasepoint)
                               : RevocationPubKey =
            let revocationBasepointBytes = revocationBasepoint.ToBytes()
            let perCommitmentPointBytes = this.ToBytes()
            let revocationBasepointTweak =
                Key.FromHashOf <| Array.append revocationBasepointBytes perCommitmentPointBytes
            let perCommitmentPointTweak =
                Key.FromHashOf <| Array.append perCommitmentPointBytes revocationBasepointBytes

            RevocationPubKey <| PubKey.Add(
                PubKey.Mul(revocationBasepoint.RawPubKey(), revocationBasepointTweak),
                PubKey.Mul(this.RawPubKey(), perCommitmentPointTweak)
            )

        member this.DeriveCommitmentPubKeys (channelPubKeys: ChannelPubKeys)
                                                : CommitmentPubKeys = {
            RevocationPubKey =
                this.DeriveRevocationPubKey channelPubKeys.RevocationBasepoint
            PaymentPubKey =
                this.DerivePaymentPubKey channelPubKeys.PaymentBasepoint
            DelayedPaymentPubKey =
                this.DeriveDelayedPaymentPubKey channelPubKeys.DelayedPaymentBasepoint
            HtlcPubKey =
                this.DeriveHtlcPubKey channelPubKeys.HtlcBasepoint
        }

    type CommitmentNumber with
        static member ObscureFactor (isFunder: bool)
                                    (localPaymentBasepoint: PaymentBasepoint)
                                    (remotePaymentBasepoint: PaymentBasepoint)
                                        : UInt48 =
            let pubKeysHash =
                if isFunder then
                    let ba =
                        Array.concat
                            (seq [ yield localPaymentBasepoint.ToBytes(); yield remotePaymentBasepoint.ToBytes() ])
                    Hashes.SHA256 ba
                else
                    let ba =
                        Array.concat
                            (seq [ yield remotePaymentBasepoint.ToBytes(); yield localPaymentBasepoint.ToBytes() ])
                    Hashes.SHA256 ba
            UInt48.FromBytesBigEndian pubKeysHash.[26..]

        member this.Subsumes(other: CommitmentNumber): bool =
            let trailingZeros = this.Index().TrailingZeros()
            (this.Index() >>> trailingZeros) = (other.Index() >>> trailingZeros)

        member this.PreviousUnsubsumed(): Option<CommitmentNumber> =
            let trailingZeros = this.Index().TrailingZeros()
            let prev = this.Index().UInt64 + (1UL <<< trailingZeros)
            if prev > UInt48.MaxValue.UInt64 then
                None
            else
                Some <| CommitmentNumber(UInt48.FromUInt64 prev)

        member this.Obscure (isFunder: bool)
                            (localPaymentBasepoint: PaymentBasepoint)
                            (remotePaymentBasepoint: PaymentBasepoint)
                                : ObscuredCommitmentNumber =
            let obscureFactor =
                CommitmentNumber.ObscureFactor
                    isFunder
                    localPaymentBasepoint
                    remotePaymentBasepoint
            ObscuredCommitmentNumber((UInt48.MaxValue - this.Index()) ^^^ obscureFactor)

    type ObscuredCommitmentNumber with
        member this.LockTime: LockTime =
            Array.concat [| [| 0x20uy |]; this.ObscuredIndex().GetBytesBigEndian().[3..] |]
            |> System.UInt32.FromBytesBigEndian
            |> LockTime

        member this.Sequence: Sequence =
            Array.concat [| [| 0x80uy |]; this.ObscuredIndex().GetBytesBigEndian().[..2] |]
            |> System.UInt32.FromBytesBigEndian
            |> Sequence

        static member TryFromLockTimeAndSequence (lockTime: LockTime)
                                                 (sequence: Sequence)
                                                     : Option<ObscuredCommitmentNumber> =
            let lockTimeBytes = lockTime.Value.GetBytesBigEndian()
            let sequenceBytes = sequence.Value.GetBytesBigEndian()
            if lockTimeBytes.[0] <> 0x20uy || sequenceBytes.[0] <> 0x80uy then
                None
            else
                Array.concat [| sequenceBytes.[1..]; lockTimeBytes.[1..] |]
                |> UInt48.FromBytesBigEndian
                |> ObscuredCommitmentNumber
                |> Some

        member this.Unobscure (isFunder: bool)
                              (localPaymentBasepoint: PaymentBasepoint)
                              (remotePaymentBasepoint: PaymentBasepoint)
                                  : CommitmentNumber =
            let obscureFactor =
                CommitmentNumber.ObscureFactor
                    isFunder
                    localPaymentBasepoint
                    remotePaymentBasepoint
            CommitmentNumber(UInt48.MaxValue - (this.ObscuredIndex() ^^^ obscureFactor))

    type PerCommitmentSecret with
        member this.DeriveChild (thisCommitmentNumber: CommitmentNumber)
                                (childCommitmentNumber: CommitmentNumber)
                                    : Option<PerCommitmentSecret> =
            if thisCommitmentNumber.Subsumes childCommitmentNumber then
                let commonBits = thisCommitmentNumber.Index().TrailingZeros()
                let index = childCommitmentNumber.Index()
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

        member this.DeriveRevocationPrivKey (revocationBasepointSecret: RevocationBasepointSecret)
                                                : RevocationPrivKey =
            let revocationBasepointBytes =
                let revocationBasepoint = revocationBasepointSecret.RevocationBasepoint()
                revocationBasepoint.ToBytes()
            let perCommitmentPointBytes =
                let perCommitmentPoint = this.PerCommitmentPoint()
                perCommitmentPoint.ToBytes()
            let revocationBasepointSecretTweak =
                Key.FromHashOf <| Array.append revocationBasepointBytes perCommitmentPointBytes
            let perCommitmentSecretTweak =
                Key.FromHashOf <| Array.append perCommitmentPointBytes revocationBasepointBytes

            RevocationPrivKey <| Key.Add(
                Key.Mul(revocationBasepointSecret.RawKey(), revocationBasepointSecretTweak),
                Key.Mul(this.RawKey(), perCommitmentSecretTweak)
            )

    type CommitmentSeed with
        member this.DerivePerCommitmentSecret (commitmentNumber: CommitmentNumber): PerCommitmentSecret =
            let res =
                this.LastPerCommitmentSecret().DeriveChild
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

    /// Set of lightning keys needed to operate a channel as describe in BOLT 3
    type ChannelPrivKeys with
        member this.SignWithFundingPrivKey (psbt: PSBT)
                                               : TransactionSignature * PSBT =
            let fundingPubKey = this.FundingPrivKey.FundingPubKey()
            psbt.SignWithKeys(this.FundingPrivKey.RawKey()) |> ignore
            match psbt.GetMatchingSig(fundingPubKey.RawPubKey()) with
            | Some signature -> (signature, psbt)
            | None -> failwithf "Failed to get signature for %A with funding pub key (%A). This should never happen" psbt fundingPubKey

        member this.SignHtlcTx (psbt: PSBT)
                               (perCommitmentPoint: PerCommitmentPoint)
                                   : TransactionSignature * PSBT =
            let htlcPrivKey = perCommitmentPoint.DeriveHtlcPrivKey this.HtlcBasepointSecret
            let htlcPubKey = htlcPrivKey.HtlcPubKey()
            psbt.SignWithKeys(htlcPrivKey.RawKey()) |> ignore
            match psbt.GetMatchingSig(htlcPubKey.RawPubKey()) with
            | Some signature -> (signature, psbt)
            | None ->
                failwithf
                    "failed to get htlc signature for %A. with htlc pubkey (%A) and perCommitmentPoint (%A)"
                    psbt htlcPubKey perCommitmentPoint

    /// This is the node-wide master key which is also used for
    /// transport-level encryption. The channel's keys are derived from
    /// this via BIP32 key derivation where `channelIndex` is the child
    /// index used to derive the channel's master key.
    type NodeMasterPrivKey with
        member this.ChannelPrivKeys (channelIndex: int): ChannelPrivKeys =
            let channelMasterKey = this.RawExtKey().Derive(channelIndex, true)

            // TODO: make use of these keys or remove them
            //let destinationKey = channelMasterKey.Derive(1, true).PrivateKey
            //let shutdownKey = channelMasterKey.Derive(2, true).PrivateKey
            let commitmentSeed =
                channelMasterKey.Derive(3, true).PrivateKey
                |> PerCommitmentSecret
                |> CommitmentSeed

            let fundingPrivKey =
                channelMasterKey.Derive(4, true).PrivateKey |> FundingPrivKey

            let revocationBasepointSecret =
                channelMasterKey.Derive(5, true).PrivateKey |> RevocationBasepointSecret

            let paymentBasepointSecret =
                channelMasterKey.Derive(6, true).PrivateKey |> PaymentBasepointSecret

            let delayedPaymentBasepointSecret =
                channelMasterKey.Derive(7, true).PrivateKey |> DelayedPaymentBasepointSecret

            let htlcBasepointSecret =
                channelMasterKey.Derive(8, true).PrivateKey |> HtlcBasepointSecret
            {
                FundingPrivKey = fundingPrivKey
                RevocationBasepointSecret = revocationBasepointSecret
                PaymentBasepointSecret = paymentBasepointSecret
                DelayedPaymentBasepointSecret = delayedPaymentBasepointSecret
                HtlcBasepointSecret = htlcBasepointSecret
                CommitmentSeed = commitmentSeed
            }

