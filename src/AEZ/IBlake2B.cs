using System;
namespace AEZ
{
    public interface IBlake2B
    {
        void Hash(ReadOnlySpan<byte> input, Span<byte> output);
    }

#if BouncyCastle
    public class BouncyCastleBlake2B : IBlake2B
    {
        private readonly int _size;

        public BouncyCastleBlake2B(int size)
        {
            _size = size;
        }

        public void Hash(ReadOnlySpan<byte> input, Span<byte> output)
        {
            var n = new Org.BouncyCastle.Crypto.Digests.Blake2bDigest(null, _size, null, null);
            n.BlockUpdate(input.ToArray(), 0, input.Length);
            var o = new byte[_size];
            n.DoFinal(o, 0);
            o.CopyTo(output);
        }
    }
#else

    public class NSecBlake2B : IBlake2B
    {
        private readonly NSec.Cryptography.Blake2b _instance;

        public NSecBlake2B(int size)
        {
            _instance = new NSec.Cryptography.Blake2b(size);
        }
        public void Hash(ReadOnlySpan<byte> input, Span<byte> output)
        {
            _instance.Hash(input, output);
        }
    }
#endif
}