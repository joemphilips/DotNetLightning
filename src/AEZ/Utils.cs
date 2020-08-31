using System;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Security.Cryptography;

namespace AEZ
{
    /// <summary>
    /// TODO: use SIMD when possible?
    /// </summary>
    internal static class Utils
    {
        internal static void XOrBytes4x16(ReadOnlySpan<byte> a, ReadOnlySpan<byte> b, ReadOnlySpan<byte> c,
            ReadOnlySpan<byte> d, Span<byte> dest)
        {
            for (int i = 0; i < 16; i++)
            {
                dest[i] = (byte)(a[i] ^ b[i] ^ c[i] ^ d[i]);
            }
        }

        internal static void XORBytes1x16(ReadOnlySpan<byte> a, ReadOnlySpan<byte> b, Span<byte> dest)
        {
            for (int i = 0; i < 16; i++)
            {
                dest[i] = (byte) (a[i] ^ b[i]);
            }
        }

        internal static void SafeCopyTo<T>(this Span<T> src, Span<T> dst) =>
            ((ReadOnlySpan<T>)src).SafeCopyTo(dst);
        internal static void SafeCopyTo<T>(this ReadOnlySpan<T> src, Span<T> dst)
        {
            if (src.Length > dst.Length)
            {
                src.Slice(0, dst.Length).CopyTo(dst);
            }
            else if (dst.Length > src.Length)
            {
                src.CopyTo(dst.Slice(0, src.Length));
            }
            else
            {
                src.CopyTo(dst);
            }
        }

        internal static void SetBytesBigEndian(uint value, Span<byte> dst)
        {
            Debug.Assert(dst.Length == 4);
            dst[0] = (byte)(value >> 24);
            dst[1] = (byte) (value >> 16);
            dst[2] = (byte) (value >> 8);
            dst[3] = (byte) (value);
        }

        internal static void XOrBytes(ReadOnlySpan<byte> a, ReadOnlySpan<byte> b, Span<byte> dst)
        {
            Debug.Assert(a.Length >= dst.Length && b.Length >= dst.Length);
            for (int i = 0; i < dst.Length; i++)
            {
                dst[i] = (byte)(a[i] ^ b[i]);
            }
        }

        internal static uint ToUInt32BE(this Span<byte> span)
        {
            return
                span[3] +
                ((uint) span[2] << 8) +
                ((uint) span[1] << 16) +
                ((uint) span[0] << 24);
        }
    }
}