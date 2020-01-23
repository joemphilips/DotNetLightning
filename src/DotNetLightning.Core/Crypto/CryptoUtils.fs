namespace DotNetLightning.Crypto

open System
open NBitcoin // For e.g. uint256

#if BouncyCastle
open Org.BouncyCastle.Crypto.Parameters
open Org.BouncyCastle.Crypto.Macs // For Poly1305
#else
open Secp256k1Net
#endif

type CryptoError =
    | BadMac
    | InvalidErrorPacketLength of expected: int * actual: int
    | InvalidPublicKey of byte[]
    | InvalidMessageLength of int
    | FailedToParseErrorPacket of packet: byte[] * sharedSecrets: (byte[] * PubKey) list
    
module Secret =
    let FromKeyPair(pub: PubKey, priv: Key) =
        NBitcoin.Crypto.Hashes.SHA256 <| pub.GetSharedPubkey(priv).ToBytes()

type ISecp256k1 =
    inherit IDisposable
    abstract member PublicKeyCreate: privateKeyInput: ReadOnlySpan<byte> -> bool * byte[]
    abstract member PublicKeySerializeCompressed: publicKey: ReadOnlySpan<byte> -> bool * byte[]
    abstract member PublicKeyParse: serializedPublicKey: ReadOnlySpan<byte> -> bool * publicKeyOutput: byte[]
    abstract member PublicKeyCombine: inputPubKey1: Span<byte> * inputPubKey2: Span<byte> -> bool * pubkeyOutput: byte[]
    abstract member PrivateKeyTweakAdd: tweak: ReadOnlySpan<byte> * privateKeyToMutate: Span<byte> -> bool
    abstract member PrivateKeyTweakMultiply: tweak: ReadOnlySpan<byte> * privKeyToMutate: Span<byte> -> bool
    abstract member PublicKeyTweakMultiply: tweak: ReadOnlySpan<byte> * pubKeyToMutate: Span<byte> -> bool

type ICryptoImpl =
    abstract member decryptWithAD: nonce: uint64 * key: uint256 * ad: byte[] * cipherText: ReadOnlySpan<byte> -> Result<byte[], CryptoError>
    abstract member encryptWithAD: nonce: uint64 * key: uint256 * ad: ReadOnlySpan<byte> * plainText: ReadOnlySpan<byte> -> byte[]
    /// This is used for filler generation in onion routing (BOLT 4)
    abstract member encryptWithoutAD: nonce: uint64 * key: byte[] * plainText: ReadOnlySpan<byte> -> byte[]
    abstract member newSecp256k1: unit -> ISecp256k1

#if !BouncyCastle
module Sodium =
    let internal getNonce (n: uint64) =
        let nonceBytes = ReadOnlySpan(Array.concat[| Array.zeroCreate 4; BitConverter.GetBytes n |]) // little endian
        NSec.Cryptography.Nonce(nonceBytes, 0)

    let internal chacha20AD = NSec.Cryptography.ChaCha20Poly1305.ChaCha20Poly1305
    let internal chacha20 = NSec.Experimental.ChaCha20.ChaCha20

    type internal SodiumSecp256k1() =
        let instance = new Secp256k1()
        interface IDisposable with
            member this.Dispose() = instance.Dispose()
        interface ISecp256k1 with
            member this.PublicKeyCreate privKey = instance.PublicKeyCreate privKey
            member this.PublicKeySerializeCompressed publicKey = instance.PublicKeySerialize (publicKey, Flags.SECP256K1_EC_COMPRESSED)
            member this.PublicKeyParse serializedPublicKey = instance.PublicKeyParse serializedPublicKey
            member this.PublicKeyCombine (pubkey1, pubkey2) = instance.PublicKeyCombine (pubkey1, pubkey2)
            member this.PrivateKeyTweakAdd (tweak, privKeyToMutate) = instance.PrivateKeyTweakAdd (tweak, privKeyToMutate)
            member this.PrivateKeyTweakMultiply (tweak, privKeyToMutate) = instance.PrivateKeyTweakMultiply (tweak, privKeyToMutate)
            member this.PublicKeyTweakMultiply (tweak, publicKeyToMutate) = instance.PublicKeyTweakMultiply (tweak, publicKeyToMutate)

    type CryptoImpl() =
        interface ICryptoImpl with
            member this.newSecp256k1() =
                new SodiumSecp256k1() :> ISecp256k1
            member this.decryptWithAD(n: uint64, key: uint256, ad: byte[], cipherText: ReadOnlySpan<byte>): Result<byte[], CryptoError> =
                let nonce = getNonce n
                let keySpan = ReadOnlySpan (key.ToBytes())
                let adSpan = ReadOnlySpan ad
                let blobF = NSec.Cryptography.KeyBlobFormat.RawSymmetricKey
                let chachaKey = NSec.Cryptography.Key.Import(chacha20AD, keySpan, blobF)
                match chacha20AD.Decrypt(chachaKey, &nonce, adSpan, cipherText) with
                | true, plainText -> Ok plainText
                | false, _ -> Error(BadMac)

            member this.encryptWithoutAD(n: uint64, key: byte[], plainText: ReadOnlySpan<byte>) =
                let nonce = getNonce n
                let keySpan = ReadOnlySpan key
                let blobF = NSec.Cryptography.KeyBlobFormat.RawSymmetricKey
                use chachaKey = NSec.Cryptography.Key.Import(chacha20, keySpan, blobF)
                let res = chacha20.XOr(chachaKey, &nonce, plainText)
                res

            member this.encryptWithAD(n: uint64, key: uint256, ad: ReadOnlySpan<byte>, plainText: ReadOnlySpan<byte>) =
                let nonce = getNonce n
                let keySpan = ReadOnlySpan (key.ToBytes())
                let blobF = NSec.Cryptography.KeyBlobFormat.RawSymmetricKey
                use chachaKey = NSec.Cryptography.Key.Import(chacha20AD, keySpan, blobF)
                chacha20AD.Encrypt(chachaKey, &nonce, ad, plainText)
#else
type internal Op = Mul | Add

type internal BouncySecp256k1() =
    let hex = NBitcoin.DataEncoders.HexEncoder()
    let parameters: Org.BouncyCastle.Asn1.X9.X9ECParameters = Org.BouncyCastle.Asn1.Sec.SecNamedCurves.GetByName "secp256k1"
    let ecParams = ECDomainParameters(parameters.Curve, parameters.G, parameters.N, parameters.H)
    let bcBigint (x: byte[]) = Org.BouncyCastle.Math.BigInteger(1, x)
    let tweakKey (op: Op) (tweak: ReadOnlySpan<byte>) (keyToMutate: Span<byte>) =
        let k = bcBigint <| keyToMutate.ToArray()
        let tweakInt = bcBigint <| tweak.ToArray()
        let tweaked = match op with
                      | Mul -> k.Multiply tweakInt
                      | Add -> k.Add tweakInt
        tweaked.Mod(parameters.N).ToByteArrayUnsigned().AsSpan().CopyTo keyToMutate
        true
    interface IDisposable with
        member this.Dispose() = ()
    interface ISecp256k1 with
        member this.PublicKeyCreate privKey =
            let privInt = bcBigint <| privKey.ToArray()
            true, ecParams.G.Multiply(privInt).GetEncoded true
        member this.PublicKeySerializeCompressed publicKey =
            let p = parameters.Curve.DecodePoint <| publicKey.ToArray()
            true, p.GetEncoded true
        member this.PublicKeyParse serializedPubKey =
            let p = parameters.Curve.DecodePoint <| serializedPubKey.ToArray()
            true, p.GetEncoded true
        member this.PublicKeyCombine (pubkey1, pubkey2) =
            let p1 = parameters.Curve.DecodePoint <| pubkey1.ToArray()
            let p2 = parameters.Curve.DecodePoint <| pubkey2.ToArray()
            true, p1.Add(p2).Normalize().GetEncoded true
        member this.PrivateKeyTweakAdd (tweak, privKeyToMutate) =
            tweakKey Add tweak privKeyToMutate
        member this.PrivateKeyTweakMultiply (tweak, privKeyToMutate) =
            tweakKey Mul tweak privKeyToMutate
        member this.PublicKeyTweakMultiply (tweak, publicKeyToMutate) =
            let p = parameters.Curve.DecodePoint <| publicKeyToMutate.ToArray()
            let tweakInt = bcBigint <| tweak.ToArray()
            let tweaked = p.Multiply tweakInt
            tweaked.Normalize().GetEncoded(true).AsSpan().CopyTo publicKeyToMutate
            true

module BouncyCastle =
    type internal Mode = Encrypt | Decrypt

    let internal encryptOrDecrypt (mode: Mode) (inp: byte[]) (key: byte[]) (nonce: byte[]) (skip1block: bool): byte[] =
        let eng = Org.BouncyCastle.Crypto.Engines.ChaCha7539Engine()
        eng.Init((mode = Encrypt), ParametersWithIV(KeyParameter key, nonce))
        let out = Array.zeroCreate inp.Length
        if skip1block then
            let dummy = Array.zeroCreate 64
            eng.ProcessBytes(Array.zeroCreate 64, 0, 64, dummy, 0)
        eng.ProcessBytes(inp, 0, inp.Length, out, 0)
        out

    let internal pad (mac: Poly1305) (length: int): unit =
        match length % 16 with
        | 0 -> ()
        | n ->
            let padding = Array.zeroCreate <| 16 - n
            mac.BlockUpdate(padding, 0, padding.Length)

    let internal writeLE (mac: Poly1305) (length: int): unit =
        let serialized = BitConverter.GetBytes(uint64 length)
        if not BitConverter.IsLittleEndian then
            Array.Reverse serialized
        mac.BlockUpdate(serialized, 0, 8)

    let internal writeSpan (mac: Poly1305) (span: ReadOnlySpan<byte>): unit =
        let byteArray = span.ToArray()
        mac.BlockUpdate(byteArray, 0, byteArray.Length)

    let internal calcMac key nonce ciphertext ad: byte[] =
        let mac = Poly1305()
        let polyKey = encryptOrDecrypt Encrypt (Array.zeroCreate 32) key nonce false
        mac.Init <| KeyParameter polyKey
        writeSpan mac ad
        pad mac ad.Length
        mac.BlockUpdate(ciphertext, 0, ciphertext.Length)
        pad mac ciphertext.Length
        writeLE mac ad.Length
        writeLE mac ciphertext.Length
        let tag: byte[] = Array.zeroCreate 16
        let macreslen = mac.DoFinal(tag, 0)
        assert (macreslen = 16)
        tag

    type CryptoImpl() =
        interface ICryptoImpl with
            member this.newSecp256k1() = BouncySecp256k1() :> ISecp256k1
            member this.encryptWithAD(n: uint64, key: uint256, ad: ReadOnlySpan<byte>, plainText: ReadOnlySpan<byte>) =
                let key = key.ToBytes()
                let nonce = Array.concat [| Array.zeroCreate 4; BitConverter.GetBytes n |]
                let plainTextBytes = plainText.ToArray()
                let ciphertext = encryptOrDecrypt Encrypt plainTextBytes key nonce true
                let tag = calcMac key nonce ciphertext ad
                Array.concat [| ciphertext; tag |]

            member this.decryptWithAD(n: uint64, key: uint256, ad: byte[], ciphertext: ReadOnlySpan<byte>) =
                if ciphertext.Length < 16 then
                    CryptoError.InvalidMessageLength ciphertext.Length |> Error
                else
                    let key = key.ToBytes()
                    let nonce = Array.concat[| Array.zeroCreate 4; BitConverter.GetBytes n |]
                    let ciphertextWithoutMac = ciphertext.Slice(0, ciphertext.Length - 16).ToArray()
                    let macToValidate = ciphertext.Slice(ciphertext.Length - 16).ToArray()
                    let correctMac = calcMac key nonce ciphertextWithoutMac (ReadOnlySpan ad)
                    if correctMac <> macToValidate then
                        Error(BadMac)
                    else
                        let plaintext = encryptOrDecrypt Decrypt ciphertextWithoutMac key nonce true
                        Ok plaintext

            member this.encryptWithoutAD(n: uint64, key: byte[], plainText: ReadOnlySpan<byte>) =
                let nonce = Array.concat [| Array.zeroCreate 4; BitConverter.GetBytes n |]
                encryptOrDecrypt Encrypt (plainText.ToArray()) key nonce false

#endif

module CryptoUtils =
#if BouncyCastle
    let impl = BouncyCastle.CryptoImpl() :> ICryptoImpl
#else
    let impl = Sodium.CryptoImpl() :> ICryptoImpl
#endif
