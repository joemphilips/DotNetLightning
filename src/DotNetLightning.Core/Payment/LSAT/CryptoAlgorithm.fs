namespace DotNetLightning.Payment.LSAT

open Macaroons
open System
open System.Security.Cryptography
open NBitcoin

#if BouncyCastle
type BouncyCastleCryptoAlgorithm() =
    inherit CryptoAlgorithm()
    override this.Encrypt(_key, _plaintext) =
        raise <| NotSupportedException("Encryption/Decryption for third party caveats are not supported yet for pure C# build.")
        
    override this.Decrypt(_key, _cipherText) =
        raise <| NotSupportedException("Encryption/Decryption for third party caveats are not supported yet for pure C# build.")

module CryptoAlgorithm =
    /// Run this function if function if you want to use third party caveat feature. Otherwise it is not necessary.
    /// If you try to add/verify 3rd party caveats without doing this, it will throw Exception.
    [<CompiledName("InitMacaroon")>]
    let initMacaroon() =
        Macaroon.Crypto <- (BouncyCastleCryptoAlgorithm() :> CryptoAlgorithm)
    
#else
type SecretBox = NSec.Experimental.Sodium.NaclXSalsa20Poly1305
type SecretBoxCryptoAlgorithm(?useRandomNonce: bool) =
    inherit CryptoAlgorithm()
    let useRandomNonce = Option.defaultValue false useRandomNonce
    
    [<Literal>]
    let SECRET_BOX_NONCE_BYTES = 24
    
    let secretBox = SecretBox()
    
    let getNonce () =
        let mutable arr = Array.zeroCreate SECRET_BOX_NONCE_BYTES
        if useRandomNonce then
            arr <- RandomUtils.GetBytes(SECRET_BOX_NONCE_BYTES)
        arr
    
    member val UseRandomNonce = useRandomNonce with get, set
    
    override this.Encrypt(key, plainText) =
        let n = getNonce()
        let nonce = NSec.Cryptography.Nonce(ReadOnlySpan(n), 0)
        use encryptionKey =
            let keySpan = ReadOnlySpan(key)
            let blobF = NSec.Cryptography.KeyBlobFormat.RawSymmetricKey;
            NSec.Cryptography.Key.Import(secretBox, keySpan, blobF)
        let macAndCipherText = secretBox.Encrypt(encryptionKey, &nonce, ReadOnlySpan(plainText));
        let result = Array.zeroCreate (n.Length + macAndCipherText.Length)
        Array.blit n 0 result 0 SECRET_BOX_NONCE_BYTES
        Array.blit macAndCipherText 0 result n.Length macAndCipherText.Length
        result
        
    override this.Decrypt(key, nonceAndMacAndCipherText) =
        let nonceBytes = Array.zeroCreate(SECRET_BOX_NONCE_BYTES)
        Buffer.BlockCopy(nonceAndMacAndCipherText, 0, nonceBytes, 0,nonceBytes.Length);
        let nonce = NSec.Cryptography.Nonce(ReadOnlySpan(nonceBytes), 0);
        let macAndCipherText = Array.zeroCreate(nonceAndMacAndCipherText.Length - nonceBytes.Length)
        Buffer.BlockCopy(nonceAndMacAndCipherText, nonceBytes.Length, macAndCipherText, 0, macAndCipherText.Length);
        let keySpan = new ReadOnlySpan<byte>(key);
        let blobF = NSec.Cryptography.KeyBlobFormat.RawSymmetricKey;
        use encryptionKey = NSec.Cryptography.Key.Import(secretBox, keySpan, blobF);
        match (secretBox.Decrypt(encryptionKey, &nonce, ReadOnlySpan(macAndCipherText))) with
        | true, plaintext -> plaintext
        | false, _ ->
            raise <| CryptographicException("Failed to decode data")

module CryptoAlgorithm =
    [<CompiledName("InitMacaroon")>]
    /// Run this function if function if you want to use third party caveat feature. Otherwise it is not necessary.
    /// If you try to add/verify 3rd party caveats without doing this, it will throw Exception.
    let initMacaroon() =
        Macaroon.Crypto <- (SecretBoxCryptoAlgorithm() :> CryptoAlgorithm)

#endif
