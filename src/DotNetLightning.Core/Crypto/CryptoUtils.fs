namespace DotNetLightning.Crypto

open System
open NBitcoin // For e.g. uint256

open Org.BouncyCastle.Crypto.Parameters
open Org.BouncyCastle.Crypto.Macs // For Poly1305

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

module CryptoUtils =
    let impl = BouncyCastle.CryptoImpl() :> ICryptoImpl
