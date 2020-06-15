namespace DotNetLightning.Crypto
open NBitcoin
open NBitcoin.Crypto
open DotNetLightning.Utils
open System

/// The functions in this module may fail but it does not return Result and just throw Exception in
/// case of failure. Why? because the error case is very, very unlikely to happen.
/// (e.g. two keys we operate were complement of each other.)
/// It is interesting to consider if it is possible for the attacker to intentionally make this happen,
/// but it is way beyond my skills, so we just ignore for now.
module Generators =

    let private derivePubKeyFromPrivKey (secp256k1: ISecp256k1) (privKey: byte[]) =
        let tmp = ReadOnlySpan(privKey)
        match secp256k1.PublicKeyCreate(tmp) with
        | true, pubkeyByte -> pubkeyByte
        | false, _  -> failwithf "Failed to derive  pubkey from %A" privKey

    let private compressePubKey (secp256k1: ISecp256k1) (pubkey: byte[]) =
        let tmp = ReadOnlySpan(pubkey)
        match secp256k1.PublicKeySerializeCompressed(tmp) with
        | true, compressedPubkeyBytes -> compressedPubkeyBytes
        | false, _ -> failwith "Failed to compress pubkey"

    let private expandPubKey (secp256k1: ISecp256k1) (pubkey: byte[]) =
        let tmp = ReadOnlySpan(pubkey)
        match secp256k1.PublicKeyParse(tmp) with
        | true, uncompressedPubKey -> uncompressedPubKey
        | false, _ -> failwithf "Failed  to parse public key %A" pubkey

    let private combinePubKey (secp256k1: ISecp256k1) (a: byte[]) (b: byte[]) =
        match secp256k1.PublicKeyCombine(a.AsSpan(), b.AsSpan()) with
        | true, result -> result
        | false, _ -> failwithf "Failed to combine public key %A and %A" a b

    /// mutable b by adding a to it
    let private combinePrivKey (secp256k1: ISecp256k1) (a: byte[]) (b: byte[]) =
        let tweak = ReadOnlySpan(a)
        match secp256k1.PrivateKeyTweakAdd(tweak, b.AsSpan()) with
        | true -> b
        | false -> failwithf "Failed to add private key (%A) to private key (%A)" a b

    let private multiplyPrivateKey (secp256k1: ISecp256k1) (a: Key) (b: byte[]) =
        let tweak = ReadOnlySpan(a.ToBytes())
        match secp256k1.PrivateKeyTweakMultiply(tweak, b.AsSpan()) with
        | true -> b
        | false -> failwith "Not implemented: Generators::multiplyPrivateKey"

    let private multiplyPublicKey (secp256k1: ISecp256k1) (secret: byte[]) (pubkey: byte[]) =
        let tweak = ReadOnlySpan(secret)
        match secp256k1.PublicKeyTweakMultiply(tweak, pubkey.AsSpan()) with
        | true -> pubkey
        | false -> failwith "Not implemented: Generators::multiplyPublicKey"


    /// Compute `baseSecret + Sha256(perCommitmentPoint || basePoint) * G`
    let derivePrivKey (ctx: ISecp256k1) (baseSecret: Key)  (perCommitmentPoint: PubKey) =
        Array.append (perCommitmentPoint.ToBytes()) (baseSecret.PubKey.ToBytes())
        |> Hashes.SHA256
        |> combinePrivKey ctx (baseSecret.ToBytes())
        |> fun h -> new Key(h)

    let derivePubKey (ctx: ISecp256k1) (basePoint: PubKey) (perCommitmentPoint: PubKey) =
        let basePointBytes = basePoint.ToBytes()
        Array.append (perCommitmentPoint.ToBytes())(basePointBytes)
        |> Hashes.SHA256
        |> derivePubKeyFromPrivKey ctx
        |> combinePubKey ctx (basePointBytes)
        |> compressePubKey ctx
        |> PubKey

    let revocationPubKey (ctx: ISecp256k1) (basePoint: PubKey) (perCommitmentPoint: PubKey) =
        let perCommitmentPointB = perCommitmentPoint.ToBytes()
        let basePointB = basePoint.ToBytes()
        let a = Array.append (basePointB) (perCommitmentPointB)
                |> Hashes.SHA256
        let b = Array.append (perCommitmentPoint.ToBytes()) (basePoint.ToBytes())
                |> Hashes.SHA256
        // below is just doing this
        // basePoint.Multiply(a).Add(perCommitmentPoint.Multiply(b))
        (expandPubKey ctx basePointB, expandPubKey ctx perCommitmentPointB)
        |> fun (baseP, perCommitP) ->
            (multiplyPublicKey ctx a baseP, multiplyPublicKey ctx b perCommitP)
        ||> combinePubKey ctx
        |> compressePubKey ctx
        |> PubKey

    let revocationPrivKey (ctx: ISecp256k1) (secret: Key) (perCommitmentSecret: Key) =
        let a =
            Array.append (secret.PubKey.ToBytes()) (perCommitmentSecret.PubKey.ToBytes())
            |> Hashes.SHA256
        let b =
            Array.append (perCommitmentSecret.PubKey.ToBytes()) (secret.PubKey.ToBytes())
            |> Hashes.SHA256
        (multiplyPrivateKey ctx secret a, multiplyPrivateKey ctx perCommitmentSecret b)
        ||> combinePrivKey ctx
        |> fun h -> new Key(h)