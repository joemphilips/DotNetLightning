namespace DotNetLightning.Crypto

open System
open NBitcoin
open NBitcoin.Crypto

open DotNetLightning.Utils

module internal CryptoUtils =
    module internal SharedSecret =
        let FromKeyPair(pub: PubKey, priv: Key) =
            Hashes.SHA256 (pub.GetSharedPubkey(priv).ToBytes())

    let internal getNonce (n: uint64) =
        let nonceBytes = ReadOnlySpan(Array.concat[| Array.zeroCreate 4; BitConverter.GetBytes(n) |]) // little endian
        NSec.Cryptography.Nonce(nonceBytes, 0)

    let chacha20AD = NSec.Cryptography.AeadAlgorithm.ChaCha20Poly1305
    let chacha20 = NSec.Experimental.StreamCipherAlgorithm.ChaCha20

    let internal decryptWithAD(n: uint64, key: uint256, ad: byte[], cipherText: ReadOnlySpan<byte>): RResult<byte[]> =
        let nonce = getNonce n
        let keySpan = ReadOnlySpan(key.ToBytes())
        let adSpan = ReadOnlySpan(ad)
        let blobF = NSec.Cryptography.KeyBlobFormat.RawSymmetricKey
        let chachaKey = NSec.Cryptography.Key.Import(chacha20AD, keySpan, blobF)
        match chacha20AD.Decrypt(chachaKey, &nonce, adSpan, cipherText) with
        | true, plainText -> Good plainText
        | false, _ -> RResult.rmsg "Failed to decrypt with AD. Bad Mac"

    /// This is used for filler generation in onion routing (BOLT 4)
    let internal encryptWithoutAD(n: uint64, key: byte[], plainText: ReadOnlySpan<byte>) =
        let nonce = getNonce n
        let keySpan = ReadOnlySpan(key)
        let blobF = NSec.Cryptography.KeyBlobFormat.RawSymmetricKey
        use chachaKey = NSec.Cryptography.Key.Import(chacha20, keySpan, blobF)
        let res = chacha20.XOr(chachaKey, &nonce, plainText)
        res

    let internal encryptWithAD(n: uint64, key: uint256, ad: ReadOnlySpan<byte>, plainText: ReadOnlySpan<byte>) =
        let nonce = getNonce n
        let keySpan = ReadOnlySpan(key.ToBytes())
        let blobF = NSec.Cryptography.KeyBlobFormat.RawSymmetricKey
        use chachaKey = NSec.Cryptography.Key.Import(chacha20AD, keySpan, blobF)
        chacha20AD.Encrypt(chachaKey, &nonce, ad, plainText)
