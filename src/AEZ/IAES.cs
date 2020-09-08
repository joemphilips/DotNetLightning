using System;

namespace AEZ
{
    public interface IAES
    {
        void Reset();
        void AES4(ReadOnlySpan<byte> j, ReadOnlySpan<byte> i, ReadOnlySpan<byte> l, ReadOnlySpan<byte> src, Span<byte> dest);
        void AES10(ReadOnlySpan<byte> l, ReadOnlySpan<byte> src, Span<byte> dest);
    }
}