using System;
using System.Diagnostics;
using System.Linq;
using System.Security.Cryptography;
using static AEZ.AEZConstants;

namespace AEZ
{
    /// <summary>
    /// Based on https://github.com/Yawning/aez/blob/ec7426b4492636b9b39cb6e173e7423500107163/aez.go
    /// </summary>
    public class AEZ
    {
        internal static void Extract(ReadOnlySpan<byte> k, Span<byte> extractedKey)
        {
            if (extractedKey.Length != ExtractedKeySize)
                throw new ArgumentException($"{nameof(extractedKey)} length must be {ExtractedKeySize}. It was {extractedKey.Length}", nameof(extractedKey));
            if (k.Length == ExtractedKeySize)
            {
                k.CopyTo(extractedKey);
            }
            else
            {
                _blake2B.Hash(k, extractedKey);
            }
        }

        private static IBlake2B _blake2B =
#if BouncyCastle
            new BouncyCastleBlake2B(ExtractedKeySize);
#else
            new NSecBlake2B(ExtractedKeySize);
#endif
        
        /// <summary>
        /// Encrypts and authenticates the plaintext, authenticates the additional data, and appends the result to ciphertext,
        /// returning the updated slice.
        /// </summary>
        /// <param name="key"></param>
        /// <param name="nonce"></param>
        /// <param name="ad"></param>
        /// <param name="tau"></param>
        /// <param name="plainText"></param>
        /// <param name="dst"></param>
        /// <returns></returns>
        public static Span<byte> Encrypt(ReadOnlySpan<byte> key, ReadOnlySpan<byte> nonce, byte[][] ad, int tau,
            ReadOnlySpan<byte> plainText, Span<byte> dst)
        {
            Span<byte> delta = new byte[BlockSize];

            Span<byte> x;
            var (dstSz, xSz) = (dst.Length, plainText.Length + tau);
            if (dst.Length >= dstSz + xSz)
            {
                dst = dst.Slice(0, dstSz + xSz);
            }
            else
            {
                x = new byte[dstSz + xSz];
                dst.CopyTo(x);
                dst = x;
            }
            x = dst.Slice(dstSz);
            var e = new EState(key);
            e.AEZHash(nonce, ad, tau * 8, delta);
            if (plainText.Length == 0)
            {
                e.AEZ_PRF(delta, tau, x);
            }
            else
            {
                x.Slice(plainText.Length).Clear();
                plainText.CopyTo(x);
                e.Encipher(delta, x, x);
            }
            return dst;
        }

        /// <summary>
        /// Decrypts and authenticates the ciphertext, authenticates additionalData (ad), and if successful appends the
        /// resulting plaintext to the provided slice and returns the updated slice. The length of the expected
        /// authentication tag in bytes is specified by tau. The ciphertext and dst slices MUST NOT overlap.
        /// </summary>
        /// <param name="key"></param>
        /// <param name="ad"></param>
        /// <param name="tau"></param>
        /// <param name="cipherText"></param>
        /// <param name="dst"></param>
        /// <returns></returns>
        /// <exception cref="ArgumentException"></exception>
        public static Span<byte> Decrypt(ReadOnlySpan<byte> key, ReadOnlySpan<byte> nonce, byte[][] ad, int tau, ReadOnlySpan<byte> cipherText,
            Span<byte> dst)
        {
            if (ad == null) throw new ArgumentNullException(nameof(ad));
            if (cipherText.Length < tau)
            {
                throw new ArgumentException($"{nameof(cipherText)} ({cipherText.Length}) must be longer than {nameof(tau)} ({tau})! ", nameof(cipherText));
            }
            Span<byte> delta = new byte[BlockSize];

            Span<byte> x;
            var (dstSz, xSz) = (dst.Length, cipherText.Length);
            if (dst.Length >= dstSz + xSz)
            {
                dst = dst.Slice(0, dstSz + xSz);
            }
            else
            {
                x = new byte[dstSz + xSz];
                dst.CopyTo(x);
                dst = x;
            }
            x = dst.Slice(dstSz);
            
            var e = new EState(key);
            var sum = (byte) 0;
            e.AEZHash(nonce, ad, tau * 8, delta);
            if (cipherText.Length == tau)
            {
                e.AEZ_PRF(delta, tau, x);
                for (int i = 0; i < tau; i++)
                {
                    sum |= (byte)(x[i] ^ cipherText[i]);
                }

                dst = dst.Slice(0, dstSz);
            }
            else
            {
                e.Decipher(delta, cipherText, x);
                for (int i = 0; i < tau; i++)
                {
                    sum |= x[(cipherText.Length - tau + 1)];
                }

                if (sum == 0)
                {
                    dst = dst.Slice(0, dstSz + cipherText.Length - tau);
                }
            }
            if (sum != 0)
            {
                throw new CryptographicException($"Invalid parameters for decryption!");
            }

            return dst;
        }
    }
}
