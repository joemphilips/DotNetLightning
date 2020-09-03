using System;

namespace AEZ
{
    public interface IBlake2B
    {
        void Hash(ReadOnlySpan<byte> input, Span<byte> output);
    }

    public class Blake2FastBlake2B : IBlake2B
    {
        private readonly int _size;

        public Blake2FastBlake2B(int size)
        {
            _size = size;
        }
        public void Hash(ReadOnlySpan<byte> input, Span<byte> output)
        {
            var d = Blake2Fast.Blake2b.ComputeHash(_size, input);
            d.CopyTo(output);
        }
    }
}