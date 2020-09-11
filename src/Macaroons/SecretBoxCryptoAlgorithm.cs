using System;
using System.Diagnostics;
using System.Linq;
using System.Security.Cryptography;
using NBitcoin;

#if BouncyCastle
using Org.BouncyCastle.Crypto.Macs;
using Org.BouncyCastle.Crypto.Parameters;
using Org.BouncyCastle.Crypto.Engines;

namespace Macaroons
{
    public class ChaCha20Poly1305CryptoAlgorithm : CryptoAlgorithm
    {
        private const int SECRET_BOX_NONCE_BYTES = 12;

        public bool UseRandomNonce {get; set;} = true;
        private byte[] GetNonce() =>
            UseRandomNonce ? RandomUtils.GetBytes(SECRET_BOX_NONCE_BYTES): new byte[SECRET_BOX_NONCE_BYTES];

        private byte[] EncryptOrDecrypt(bool isEncrypt, byte[] input, byte[] key, byte[] nonce, bool skip1Block)
        {
            var eng = new ChaCha7539Engine();
            var keyParam = new ParametersWithIV(new KeyParameter(key), nonce);
            eng.Init(isEncrypt, keyParam);
            var output = new byte[input.Length];
            if (skip1Block)
            {
                var dummy = new byte[64];
                eng.ProcessBytes(new byte[64], 0, 64, dummy, 0);
            }

            eng.ProcessBytes(input, 0, input.Length, output, 0);
            return output;
        }

        private void Pad(Poly1305 mac, int length)
        {
            var n = length % 16;
            if (n != 0)
            {
                var padding = new byte[16 - n];
                mac.BlockUpdate(padding, 0, padding.Length);
            }
        }

        private void WriteLE(Poly1305 mac, int length)
        {
            var s = Utils.ToBytes((ulong) length, true);
            mac.BlockUpdate(s, 0, 8);
        }

        private byte[] CalcMac(byte[] key, byte[] nonce, byte[] cipherText, byte[] ad)
        {
            var mac = new Poly1305();
            var polyKey = EncryptOrDecrypt(true, new byte[32], key, nonce, false);
            mac.Init(new KeyParameter(polyKey));
            mac.BlockUpdate(ad, 0, ad.Length);
            Pad(mac, ad.Length);
            mac.BlockUpdate(cipherText, 0, cipherText.Length);
            Pad(mac, cipherText.Length);
            WriteLE(mac, ad.Length);
            WriteLE(mac, cipherText.Length);
            var tag = new byte[16];
            var macreslen = mac.DoFinal(tag, 0);
            Debug.Assert(macreslen == 16);
            return tag;
        }

        public override byte[] Encrypt(byte[] key, byte[] plainText)
        {
            var nonce = GetNonce();
            var cipherText = EncryptOrDecrypt(true, plainText, key, nonce, true);
            var mac = CalcMac(key, nonce, cipherText, new byte[0]);
            var result = new byte[nonce.Length + mac.Length + cipherText.Length];
            nonce.CopyTo(result.AsSpan());
            cipherText.CopyTo(result.AsSpan().Slice(nonce.Length));
            mac.CopyTo(result.AsSpan().Slice(nonce.Length + cipherText.Length));
            
            return result;
        }

        public override byte[] Decrypt(byte[] key, byte[] nonceAndMacAndCipherText)
        {
            var nonce = nonceAndMacAndCipherText.AsSpan().Slice(0, SECRET_BOX_NONCE_BYTES).ToArray();
            var cipherText = nonceAndMacAndCipherText.AsSpan().Slice(nonce.Length, nonceAndMacAndCipherText.Length - 16 - nonce.Length).ToArray();
            var mac = nonceAndMacAndCipherText.AsSpan().Slice(nonceAndMacAndCipherText.Length - 16, 16).ToArray();
            var actualMac = CalcMac(key, nonce, cipherText, new byte[0]);
            if (!actualMac.SequenceEqual(mac))
                throw new CryptographicException("bogus cipherText! unmatched mac");
            var plainText = EncryptOrDecrypt(false, cipherText, key, nonce, true);
            return plainText;
        }
    }
}
#else
namespace Macaroons
{
    public class ChaCha20Poly1305CryptoAlgorithm: CryptoAlgorithm
    {
        private const int SECRET_BOX_NONCE_BYTES = 12;
        private static NSec.Cryptography.ChaCha20Poly1305 _secretBox = new NSec.Cryptography.ChaCha20Poly1305();

        public bool UseRandomNonce {get; set;} = true;
        private byte[] GetNonce() =>
            UseRandomNonce ? RandomUtils.GetBytes(SECRET_BOX_NONCE_BYTES): new byte[SECRET_BOX_NONCE_BYTES];


        public override byte[] Encrypt(byte[] key, byte[] plainText)
        {
            var n = GetNonce();
            var nonce = new NSec.Cryptography.Nonce(n, 0);
            using var encryptionKey = NSec.Cryptography.Key.Import(_secretBox, key,
                NSec.Cryptography.KeyBlobFormat.RawSymmetricKey);
            var macAndCipherText = _secretBox.Encrypt(encryptionKey, nonce, Span<byte>.Empty, plainText);
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
            if (_secretBox.Decrypt(encryptionKey, nonce,Span<byte>.Empty, macAndCipherText, out var plaintext))
                return plaintext;
            
            throw new CryptographicException("Failed to decrypt data");
        }
    }
    
    /// <summary>
    /// For testing reference implementation test vector.
    /// </summary>
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
}
#endif
