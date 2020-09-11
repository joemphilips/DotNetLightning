using System;
using System.Security.Cryptography;
using NBitcoin;

namespace Macaroons
{
    #if BouncyCastle
    public class SecretBoxCryptoAlgorithm: CryptoAlgorithm
    {
        public SecretBoxCryptoAlgorithm(bool _ = true) {}
        
        public override byte[] Encrypt(byte[] key, byte[] plainText)
        {
            throw new NotImplementedException("Use non-bouncycastle build for third party caveat");
        }

        public override byte[] Decrypt(byte[] key, byte[] nonceAndMacAndCipherText)
        {
            throw new NotImplementedException("Use non-bouncycastle build for third party caveat");
        }
    }
    #else
    public class SecretBoxCryptoAlgorithm: CryptoAlgorithm
    {
        private readonly bool _useRandomNonce;
        private const int SECRET_BOX_NONCE_BYTES = 24;
        private static NSec.Experimental.Sodium.NaclXSalsa20Poly1305 _secretBox = new NSec.Experimental.Sodium.NaclXSalsa20Poly1305();

        private byte[] GetNonce() => _useRandomNonce
            ? RandomUtils.GetBytes(SECRET_BOX_NONCE_BYTES)
            : new byte[SECRET_BOX_NONCE_BYTES];

        public SecretBoxCryptoAlgorithm(bool useRandomNonce = true)
        {
            _useRandomNonce = useRandomNonce;
        }

        public override byte[] Encrypt(byte[] key, byte[] plainText)
        {
            var n = GetNonce();
            var nonce = new NSec.Cryptography.Nonce(n, 0);
            using var encryptionKey = NSec.Cryptography.Key.Import(_secretBox, key,
                NSec.Cryptography.KeyBlobFormat.RawSymmetricKey);
            var macAndCipherText = _secretBox.Encrypt(encryptionKey, nonce, plainText);
            var result = new byte[n.Length + macAndCipherText.Length];
            n.CopyTo(result, 0);
            macAndCipherText.AsSpan().CopyTo(result.AsSpan(n.Length, macAndCipherText.Length));
            return result;
        }

        public override unsafe byte[] Decrypt(byte[] key, byte[] nonceAndMacAndCipherText)
        {
            Span<byte> nonceBytes = stackalloc byte[SECRET_BOX_NONCE_BYTES];
            nonceAndMacAndCipherText.AsSpan().Slice(0, nonceBytes.Length).CopyTo(nonceBytes);
            var nonce = new NSec.Cryptography.Nonce(nonceBytes, 0);
            Span<byte> macAndCipherText = stackalloc byte[nonceAndMacAndCipherText.Length - nonceBytes.Length];
            nonceAndMacAndCipherText.AsSpan().Slice(nonceBytes.Length).CopyTo(macAndCipherText);
            var keySpan = key.AsSpan();
            var blobF = NSec.Cryptography.KeyBlobFormat.RawSymmetricKey;
            using var encryptionKey = NSec.Cryptography.Key.Import(_secretBox, keySpan, blobF);
            if (_secretBox.Decrypt(encryptionKey, nonce, macAndCipherText, out var plaintext))
                return plaintext;
            
            throw new CryptographicException("Failed to decrypt data");
        }
    }
    #endif
}