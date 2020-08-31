using System;
using NSec.Cryptography;

namespace AEZ
{
    public interface IBlake2B
    {
        void Hash(ReadOnlySpan<byte> input, Span<byte> output);
    }

    public class NSecBlake2B : IBlake2B
    {
        private readonly int _size;
        private Blake2b _instanse;
        public NSecBlake2B(int size)
        {
            _size = size;
            _instanse = new Blake2b(size);
        }
        public void Hash(ReadOnlySpan<byte> input, Span<byte> output)
        {
            _instanse.Hash(input, output);
        }
    }
}