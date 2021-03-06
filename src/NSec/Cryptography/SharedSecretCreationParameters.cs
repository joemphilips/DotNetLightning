using System;
using System.Buffers;
using System.Runtime.InteropServices;
using NSec.Cryptography.Buffers;

namespace NSec.Cryptography
{
    [StructLayout(LayoutKind.Auto)]
    public ref struct SharedSecretCreationParameters
    {
        internal MemoryPool<byte> GetMemoryPool()
        {
            return SecureMemoryPool<byte>.Shared;
        }
    }
}
