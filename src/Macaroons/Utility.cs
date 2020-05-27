using System;
using System.Linq;
using System.Runtime.InteropServices;

namespace Macaroons
{
    public class Utility
    {
        public static string ToBase64UrlSafe(byte[] data) =>
            Convert.ToBase64String(data)
                .Replace("=", String.Empty)
                .Replace('+', '-')
                .Replace('/', '_');

        public static byte[] FromBase64UrlSafe(string s)
        {
            s = s.PadRight(s.Length + (4 - s.Length % 4) % 4, '=');
            s = s.Replace('-', '+').Replace('_', '/');
            return Convert.FromBase64String(s);
        }

        public static byte[] CopyByteArray(byte[] src)
        {
            if (src is null)
                return null;

            byte[] dst = new byte[src.Length];
            Buffer.BlockCopy(src, 0, dst, 0, src.Length);
            return dst;
        }

        internal static bool ByteArrayEquals(byte[] b1, byte[] b2)
        {
            return (b1.Length == b2.Length) && b1.SequenceEqual(b2);
        }
    }
}