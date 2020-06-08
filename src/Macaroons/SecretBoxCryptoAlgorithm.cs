using System;
using System.Security.Cryptography;
using NBitcoin;
using NSec.Experimental.Sodium;
using SecretBox = NSec.Experimental.Sodium.NaclXSalsa20Poly1305;

namespace Macaroons
{
    public class SecretBoxCryptoAlgorithm : CryptoAlgorithm
    {
        protected bool UseRandomNonce { get; } = false;
        public const int SECRET_BOX_NONCE_BYTES = 24;
        
        public SecretBoxCryptoAlgorithm() {}
        public SecretBoxCryptoAlgorithm(bool useRandomNonce)
        {
            UseRandomNonce = useRandomNonce;
        }
        
        private NaclSecretBoxAlgorithm SecretBox = new SecretBox();

        private byte[] GetNonce()
        {
            byte[] arr = new byte[SECRET_BOX_NONCE_BYTES];
            // We usually should use randomize nonce. But because original C implementation and its tests
            // assumes non-random nonce, so made it configurable like this.
            if (UseRandomNonce)
            {
                arr = RandomUtils.GetBytes(SECRET_BOX_NONCE_BYTES);
            }
            return arr;
        }
        public byte[] EncryptWithAD(byte[] key, ReadOnlySpan<byte> plainText)
        {
            var nonceBytes = GetNonce();
            var nonce = new NSec.Cryptography.Nonce(nonceBytes, 0);
            var keySpan = new ReadOnlySpan<byte>(key);
            var blobF = NSec.Cryptography.KeyBlobFormat.RawSymmetricKey;
            using var encryptionKey = NSec.Cryptography.Key.Import(SecretBox, keySpan, blobF);
            var macAndCipherText = SecretBox.Encrypt(encryptionKey, in nonce, plainText);
            
            var result = new byte[nonceBytes.Length + macAndCipherText.Length];
            Buffer.BlockCopy(nonceBytes, 0, result, 0, SECRET_BOX_NONCE_BYTES);
            Buffer.BlockCopy(macAndCipherText, 0, result, nonceBytes.Length, macAndCipherText.Length);
            return result;
        }
        
        public byte[] DecryptWithAD(byte[] key, byte[] nonceAndMacAndCipherText)
        {
            var nonceBytes = new byte[SECRET_BOX_NONCE_BYTES];
            Buffer.BlockCopy(nonceAndMacAndCipherText, 0, nonceBytes, 0,nonceBytes.Length);
            var nonce = new NSec.Cryptography.Nonce(nonceBytes, 0);
            
            var macAndCipherText = new byte[nonceAndMacAndCipherText.Length - nonceBytes.Length];
            Buffer.BlockCopy(nonceAndMacAndCipherText, nonceBytes.Length, macAndCipherText, 0, macAndCipherText.Length);
            
            var keySpan = new ReadOnlySpan<byte>(key);
            var blobF = NSec.Cryptography.KeyBlobFormat.RawSymmetricKey;
            using var encryptionKey = NSec.Cryptography.Key.Import(SecretBox, keySpan, blobF);
            if (SecretBox.Decrypt(encryptionKey, in nonce, macAndCipherText, out var plaintext))
            {
                return plaintext;
            }
            throw new CryptographicException("Failed to decode data");
        }
        
        public override byte[] Encrypt(byte[] key, byte[] plainText)
        {
            return EncryptWithAD(key, plainText.AsSpan());
        }

        public override byte[] Decrypt(byte[] key, byte[] cipherText)
        {
            return DecryptWithAD(key, cipherText);
        }
    }
}