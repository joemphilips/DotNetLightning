using System;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using static AEZ.AEZConstants;

namespace AEZ
{
    internal unsafe ref struct EState
    {
        private static byte[] zero = new byte[BlockSize];
        
        private fixed byte _i0[16];
        private fixed byte _i1[16];
        
        private fixed byte _j0[16];
        private fixed byte _j1[16];
        private fixed byte _j2[16];
        
        private fixed byte _l0[16];
        private fixed byte _l1[16];
        private fixed byte _l2[16];
        private fixed byte _l3[16];
        private fixed byte _l4[16];
        private fixed byte _l5[16];
        private fixed byte _l6[16];
        private fixed byte _l7[16];
        
        private Span<byte> I0 => new Span<byte>(Unsafe.AsPointer(ref _i0[0]), 16);
        private Span<byte> I1 => new Span<byte>(Unsafe.AsPointer(ref _i1[0]), 16);
        
        private Span<byte> J0 => new Span<byte>(Unsafe.AsPointer(ref _j0[0]), 16);
        private Span<byte> J1 => new Span<byte>(Unsafe.AsPointer(ref _j1[0]), 16);
        private Span<byte> J2 => new Span<byte>(Unsafe.AsPointer(ref _j2[0]), 16);
        
        private Span<byte> L0 => new Span<byte>(Unsafe.AsPointer(ref _l0[0]), 16);
        private Span<byte> L1 => new Span<byte>(Unsafe.AsPointer(ref _l1[0]), 16);
        private Span<byte> L2 => new Span<byte>(Unsafe.AsPointer(ref _l2[0]), 16);
        private Span<byte> L3 => new Span<byte>(Unsafe.AsPointer(ref _l3[0]), 16);
        private Span<byte> L4 => new Span<byte>(Unsafe.AsPointer(ref _l4[0]), 16);
        private Span<byte> L5 => new Span<byte>(Unsafe.AsPointer(ref _l5[0]), 16);
        private Span<byte> L6 => new Span<byte>(Unsafe.AsPointer(ref _l6[0]), 16);
        private Span<byte> L7 => new Span<byte>(Unsafe.AsPointer(ref _l7[0]), 16);

        private Span<byte> GetL(int index) =>
            index == 0 ? L0 :
            index == 1 ? L1 :
            index == 2 ? L2 :
            index == 3 ? L3 :
            index == 4 ? L4 :
            index == 5 ? L5 :
            index == 6 ? L6 :
            index == 7 ? L7 :
            throw new ArgumentOutOfRangeException(nameof(index));
        
        
        IAES aes;
        
        internal static void DoubleBlock(Span<byte> p)
        {
            var tmp = p[0];
            for (int i = 0; i < 15; i++)
            {
                p[i] =  (byte)((byte) (p[i] << 1) | (byte)(p[i + 1] >> 7));
            }

            var s = tmp >> 7 == 1;
            p[15] = (byte) ((byte) (p[15] << 1) ^ (byte) (s ? 135 : 0));
        }

        internal static void MultBlock(uint x, Span<byte> src, Span<byte> dst)
        {
            if (dst.Length != BlockSize)
                throw new ArgumentException($"{nameof(dst)} length must be {ExtractedKeySize}. It was {dst.Length}", nameof(dst));
            if (src.Length != BlockSize)
                throw new ArgumentException($"{nameof(src)} length must be {ExtractedKeySize}. It was {src.Length}", nameof(src));
            
            Span<byte> t = stackalloc byte[BlockSize];
            Span<byte> r = stackalloc byte[BlockSize];
            
            src.CopyTo(t);
            while (x != 0)
            {
                if ((x & 1) != 0)
                {
                    Utils.XORBytes1x16(r, t, r);
                }
                DoubleBlock(t);
                x >>= 1;
            }
            r.CopyTo(dst);
        }

        public EState(ReadOnlySpan<byte> k) : this()
        {
            byte[] extractedKey = new byte[ExtractedKeySize];
            AEZ.Extract(k, extractedKey);
            extractedKey.AsSpan().Slice(0, 16).CopyTo(this.I0); // 1I
            MultBlock(2, this.I0, this.I1); // 2I
            
            extractedKey.AsSpan().Slice(16, 16).CopyTo(this.J0);
            MultBlock(2, this.J0, this.J1); // 2J
            MultBlock(2, this.J1, this.J2); // 4J
            
            // The upstream `aesni` code only stores L1, L2, and L4, but 
            
            // MultBlock(0, e.L, e.L[0])                            // L0 (all `0x00`s)
            extractedKey.AsSpan().Slice(32, 16).CopyTo(this.L1); // L1
            MultBlock(2, this.L1, this.L2);              // L2 = L1*2;
            Utils.XORBytes1x16(this.L2, this.L1, this.L3);      // L3 = L2 + L1;
            MultBlock(2, this.L2, this.L4);
            Utils.XORBytes1x16(this.L4, this.L1, this.L5);
            MultBlock(2, this.L3, this.L6);
            Utils.XORBytes1x16(this.L6, this.L1, this.L7);
            this.aes = new CustomAES(extractedKey);
        }

        private void Reset()
        {
            this.I0.Clear();
            this.I1.Clear();
            
            this.J0.Clear();
            this.J1.Clear();
            this.J2.Clear();
                
            this.L0.Clear();
            this.L1.Clear();
            this.L2.Clear();
            this.L3.Clear();
            this.L4.Clear();
            this.L5.Clear();
            this.L6.Clear();
            this.L7.Clear();
            
            this.aes.Reset();
        }

        internal unsafe void AEZHash(ReadOnlySpan<byte> nonce, byte[][] ad, int tau, Span<byte> result)
        {
            if (result.Length != BlockSize)
            {
                throw new ArgumentException($"{nameof(result)} length must be {ExtractedKeySize}. It was {result.Length}", nameof(result));
            }
            Span<byte> buf = stackalloc byte[BlockSize];
            Span<byte> sum = stackalloc byte[BlockSize];
            Span<byte> I = stackalloc byte[BlockSize];
            Span<byte> J = stackalloc byte[BlockSize];
            
            Utils.SetBytesBigEndian((uint)tau, buf.Slice(12, 4));
            Utils.XORBytes1x16(this.J0, this.J1, J); // J ^ J2
            this.aes.AES4(J, this.I1, this.L1, buf, sum);    // E (3,1)

            // Hash nonce, accumulate into sum
            var isEmpty = nonce.Length == 0;
            var n = nonce;
            var nBytes = nonce.Length;
            this.I1.CopyTo(I);
            for (int i = 1; nBytes >= BlockSize; (i, nBytes) = (i + 1, nBytes - BlockSize))
            {
                this.aes.AES4(this.J2, I, this.GetL(i % 8), n.Slice(0, BlockSize), buf); // E(4,i)
                Utils.XORBytes1x16(sum, buf, sum);
                n = n.Slice(BlockSize);
                if (i % 8 == 0)
                {
                    DoubleBlock(I);
                }
            }

            if (nBytes > 0 || isEmpty)
            {
                buf.Clear();
                n.CopyTo(buf);
                buf[nBytes] = 0x80;
                this.aes.AES4(this.J2, this.I0, this.L0, buf, buf);
                Utils.XORBytes1x16(sum, buf, sum);
            }

            foreach (var (p, k) in ad.Select((x, i) => (x, i)))
            {
                isEmpty = p.Length == 0;
                var bytes = p.Length;
                this.I1.CopyTo(I);
                MultBlock((uint)(5 + k), this.J0, J);
                var pSpan = p.AsSpan();
                for (int i = 1; bytes >= BlockSize; (i, bytes) = (i + 1, bytes - BlockSize))
                {
                    this.aes.AES4(J, I, this.GetL(i%8), pSpan.Slice(0, BlockSize), buf);
                    Utils.XORBytes1x16(sum, buf, sum);
                    pSpan = pSpan.Slice(BlockSize);
                    if (i % 8 == 0)
                    {
                        DoubleBlock(I);
                    }
                }

                if (bytes > 0 || isEmpty)
                {
                    buf.Clear();
                    pSpan.SafeCopyTo(buf);
                    buf[bytes] = 0x80;
                    this.aes.AES4(J, this.I0, this.L0, buf, buf);
                    Utils.XORBytes1x16(sum, buf, sum);
                }
            }
            
            sum.SafeCopyTo(result);
        }

        public void AEZ_PRF(ReadOnlySpan<byte> delta, int tau, Span<byte> result)
        {
            if (delta.Length != BlockSize)
                throw new ArgumentException($"{nameof(delta)} length must be {BlockSize}. It was {delta.Length}", nameof(delta));

            Span<byte> buf = stackalloc byte[BlockSize];
            Span<byte> ctr = stackalloc byte[BlockSize];
            var off = 0;
            while (tau >= BlockSize)
            {
                Utils.XORBytes1x16(delta, ctr, buf);
                this.aes.AES10(this.L3, buf, buf);
                buf.CopyTo(result.Slice(off));
                var i = 15;
                while (true)
                {
                    ctr[i]++;
                    i--;
                    if (ctr[i + 1] != 0)
                    {
                        break;
                    }
                }

                tau -= BlockSize;
                off += BlockSize;
            }

            if (tau > 0)
            {
                Utils.XORBytes1x16(delta, ctr, buf);
                this.aes.AES10(this.L3, buf, buf);
                buf.SafeCopyTo(result.Slice(off));
            }
        }

        private void AEZCorePass1Ref(ReadOnlySpan<byte> input, Span<byte> output, Span<byte> X)
        {
            Debug.Assert(X.Length == BlockSize);
            Span<byte> tmp = stackalloc byte[BlockSize];
            Span<byte> I = stackalloc byte[BlockSize];
            this.I1.CopyTo(I);

            for (var (i, inBytes) = (1, input.Length); inBytes >= 64; i++, inBytes -= 32)
            {
                this.aes.AES4(this.J0, I, this.GetL(i % 8), input.Slice(BlockSize, BlockSize), tmp);
                Utils.XORBytes1x16(input, tmp, output.Slice(0,BlockSize));
                
                aes.AES4(zero, this.I0, this.L0, output.Slice(0, BlockSize), tmp);
                Utils.XORBytes1x16(input.Slice(BlockSize), tmp, output.Slice(BlockSize, BlockSize));
                Utils.XORBytes1x16(output.Slice(BlockSize), X, X);
                input = input.Slice(32);
                output = output.Slice(32);
                if (i % 8 == 0)
                {
                    DoubleBlock(I);
                }
            }
        }

        private void AEZCorePass1Slow(ReadOnlySpan<byte> input, Span<byte> output, Span<byte> X, int Sz)
        {
            // TODO: consider using constant time algorithm for `Vector64` class when possible
            AEZCorePass1Ref(input, output, X);
        }

        private void AEZCorePass1(ReadOnlySpan<byte> input, Span<byte> output, Span<byte> X, int Sz)
        {
            // TODO: use hardware acceleration when possible
            AEZCorePass1Slow(input, output, X, Sz);
            return;
        }

        private void AEZCorePass2Ref(ReadOnlySpan<byte> input, Span<byte> output, Span<byte> Y, Span<byte> S)
        {
            Span<byte> tmp = stackalloc byte[BlockSize];
            Span<byte> I = stackalloc byte[BlockSize];
            this.I1.CopyTo(I);
            for (var (i, inBytes) = (1, input.Length); inBytes >= 64; i++, inBytes -= 32)
            {
                aes.AES4(this.J1, I, this.GetL(i % 8), S, tmp);
                Utils.XORBytes1x16(output, tmp, output.Slice(0, BlockSize));
                Utils.XORBytes1x16(output.Slice(BlockSize), tmp, output.Slice(BlockSize, BlockSize));
                Utils.XORBytes1x16(output, Y, Y);
                
                aes.AES4(zero, this.I0, this.L0, output.Slice(BlockSize, BlockSize), tmp);
                Utils.XORBytes1x16(output, tmp, output.Slice(0, BlockSize));
                
                aes.AES4(this.J0, I, this.GetL(i % 8), output.Slice(0, BlockSize), tmp);
                Utils.XORBytes1x16(output.Slice(BlockSize), tmp, output.Slice(BlockSize, BlockSize));

                SwapBlocks(tmp, output);
                input = input.Slice(32);
                output = output.Slice(32);
                if (i % 8 == 0)
                {
                    DoubleBlock(I);
                }
            }
        }

        private void AEZCorePass2Slow(ReadOnlySpan<byte> input, Span<byte> output, Span<byte> Y, Span<byte> S, int sz)
        {
            // TODO: consider using constant time algorithm using `Vector64` when possible.
            AEZCorePass2Ref(input, output, Y, S);
        }

        private void AEZCorePass2(ReadOnlySpan<byte> input, Span<byte> output, Span<byte> Y, Span<byte> S, int sz)
        {
            // TODO: Consider using Hardware acceleration when possible.
            AEZCorePass2Slow(input, output, Y, S, sz);
        }
        private static void SwapBlocks(Span<byte> tmp, Span<byte> b)
        {
            Debug.Assert(tmp.Length == BlockSize);
            b.SafeCopyTo(tmp);
            b.Slice(BlockSize, BlockSize).CopyTo(b.Slice(0, BlockSize));
            tmp.CopyTo(b.Slice(BlockSize));
        }

        private void OneZeroPad(ReadOnlySpan<byte> src, int sz, Span<byte> dst)
        {
            Debug.Assert(dst.Length == BlockSize);
            dst.Clear();
            src.Slice(0, sz).CopyTo(dst);
            dst[sz] = 0x80;
        }

        private void AezCore(ReadOnlySpan<byte> delta, ReadOnlySpan<byte> input, uint d, Span<byte> output)
        {
            Debug.Assert(delta.Length == BlockSize);
            Span<byte> tmp = new byte[BlockSize];
            Span<byte> X = new byte[BlockSize];
            Span<byte> Y = new byte[BlockSize];
            Span<byte> S = new byte[BlockSize];

            var inputOrigin = input;
            var outputOrigin = output;
            var fragBytes = input.Length % 32;
            var initialBytes = input.Length - fragBytes - 32;

            // Compute X and store intermediate results
            // Pass 1 over input[0, -32], store intermediate values in output[0, -32]
            if (input.Length >= 64)
            {
                AEZCorePass1(input, output, X, initialBytes);
            }

            input = input.Slice(initialBytes);
            if (fragBytes >= BlockSize)
            {
                aes.AES4(zero, this.I1, this.L4, input.Slice(0, BlockSize), tmp);
                Utils.XORBytes1x16(X, tmp, X);
                OneZeroPad(input.Slice(BlockSize), fragBytes - BlockSize, tmp);
                aes.AES4(zero, this.I1, this.L5, tmp, tmp);
                Utils.XORBytes1x16(X, tmp, X);
            }
            else if (fragBytes > 0)
            {
                OneZeroPad(input, fragBytes, tmp);
                aes.AES4(zero, this.I1, this.L4, tmp, tmp);
                Utils.XORBytes1x16(X, tmp, X);
            }
            
            // Calculate S
            output = outputOrigin.Slice(inputOrigin.Length - 32);
            input = inputOrigin.Slice(inputOrigin.Length - 32);
            
            aes.AES4(zero, this.I1, this.GetL((1+(int)d) % 8), input.Slice(BlockSize, BlockSize), tmp);
            Utils.XOrBytes4x16(X, input, delta, tmp, output.Slice(0, BlockSize));
            aes.AES10(this.GetL((1+(int)d) % 8), output.Slice(0, BlockSize), tmp);
            Utils.XORBytes1x16(input.Slice(BlockSize), tmp, output.Slice(BlockSize, BlockSize));
            Utils.XORBytes1x16(output, output.Slice(BlockSize), S);
            
            // Pass 2 over intermediate values in out[32..]. Final values written
            output = outputOrigin;
            input = inputOrigin;
            if (input.Length >= 64)
            {
                AEZCorePass2(input, output, Y, S, initialBytes);
            }
            output = output.Slice(initialBytes);
            input = input.Slice(initialBytes);
            if (fragBytes >= BlockSize)
            {
                aes.AES10(this.L4, S, tmp); // E(-1, 4)
                Utils.XORBytes1x16(input, tmp, output.Slice(0, BlockSize));
                aes.AES4(zero, this.I1, this.L4, output.Slice(0, BlockSize), tmp);
                Utils.XORBytes1x16(Y, tmp, Y);

                output = output.Slice(BlockSize);
                input = input.Slice(BlockSize);
                fragBytes -= BlockSize;
                
                aes.AES10(this.L5, S, tmp);
                Utils.XOrBytes(input, tmp, tmp.Slice(0, fragBytes));
                tmp.Slice(0, fragBytes).CopyTo(output);
                tmp.Slice(fragBytes).Clear();
                tmp[fragBytes] = 0x80;
                aes.AES4(zero, this.I1, this.L5, tmp, tmp);
                Utils.XORBytes1x16(Y, tmp, Y);
            }
            else if (fragBytes > 0)
            {
                aes.AES10(this.L4, S, tmp); // E(-1, 4)
                Utils.XOrBytes(input, tmp, tmp.Slice(0, fragBytes));
                tmp.Slice(0, fragBytes).CopyTo(output);
                tmp.Slice(fragBytes).Clear();
                tmp[fragBytes] = 0x80;
                aes.AES4(zero, this.I1, this.L4, tmp, tmp);
                Utils.XORBytes1x16(Y, tmp, Y);
            }

            output = outputOrigin.Slice(inputOrigin.Length - 32);
            aes.AES10(this.GetL((2 - (int)d)%8), output.Slice(BlockSize), tmp);
            Utils.XORBytes1x16(output, tmp, output.Slice(0, BlockSize));
            aes.AES4(zero, this.I1, this.GetL((2 - (int)d) % 8), output.Slice(0, BlockSize), tmp);
            Utils.XOrBytes4x16(tmp, output.Slice(BlockSize), delta, Y, output.Slice(BlockSize));
            
            output.Slice(0, BlockSize).CopyTo(tmp);
            output.Slice(BlockSize).CopyTo(output.Slice(0, BlockSize));
            tmp.CopyTo(output.Slice(BlockSize));
            
            X.Clear();
            Y.Clear();
            S.Clear();
        }

        private unsafe void AezTiny(ReadOnlySpan<byte> delta, ReadOnlySpan<byte> input, uint d, Span<byte> output)
        {
            Debug.Assert(delta.Length == BlockSize);
            uint rounds, i, j = 0;
            Span<byte> buf = stackalloc byte[2 * BlockSize];
            Span<byte> L = stackalloc byte[BlockSize];
            Span<byte> R = stackalloc byte[BlockSize];
            int step;
            var (mask, pad) = ((byte) 0x00, (byte)0x80);

            Span<byte> tmp = stackalloc byte[16];
            i = 7;
            var inBytes = input.Length;
            if (inBytes == 1)
            {
                rounds = 24;
            }
            else if (inBytes == 2)
            {
                rounds = 16;
            }
            else if (inBytes < 16)
            {
                rounds = 10;
            }
            else
            {
                i = 6;
                rounds = 8;
            }

            // Split (inBytes * 8) / 2 bits into L and R. Beware: May end in nibble.
            input.Slice(0, (inBytes + 1) / 2).CopyTo(L);
            input.Slice(inBytes /2, (inBytes + 1) / 2).CopyTo(R);

            if ((inBytes & 1) != 0) // Must this R left by half a byte
            {
                for (int k = 0; k < inBytes / 2; k++)
                {
                    R[k] = (byte)((R[k] << 4) | (R[k+1] >> 4));
                }

                R[inBytes / 2] = (byte)(R[inBytes / 2] << 4);
                pad = 0x08;
                mask = 0xf0;
            }

            if (d != 0)
            {
                if (inBytes < 16)
                {
                    buf.Slice(0, BlockSize).Clear();
                    input.CopyTo(buf);
                    buf[0] |= 0x80;
                    Utils.XORBytes1x16(delta, buf, buf.Slice(0, BlockSize));
                    aes.AES4(zero, this.I1, this.L3, buf.Slice(0, BlockSize), tmp); // E(0, 3)
                    L[0] ^= (byte)(tmp[0] & 0x80);
                }
                j = rounds - 1;
                step = -1;
            }
            else
            {
                step = 1;
            }

            for (int k = 0; k < rounds / 2; (k, j) = (k + 1, (uint) ((int) j + 2 * step)))
            {
                buf.Slice(0, BlockSize).Clear();
                R.Slice(0, (inBytes+1)/ 2).SafeCopyTo(buf);
                buf[inBytes / 2] = (byte) ((buf[inBytes / 2] & mask) | pad);
                Utils.XORBytes1x16(buf, delta, buf.Slice(0, BlockSize));
                buf[15] ^= (byte) j;
                aes.AES4(zero, this.I1, this.GetL((int)i), buf.Slice(0, BlockSize), tmp); // E(0, i)
                Utils.XORBytes1x16(L, tmp, L.Slice(0, BlockSize));
                
                buf.Slice(0, BlockSize).Clear();
                L.Slice(0, (inBytes + 1) / 2).CopyTo(buf);
                buf[inBytes / 2] = (byte)((buf[inBytes / 2] & mask) | pad);
                Utils.XORBytes1x16(buf, delta, buf.Slice(0, BlockSize));
                buf[15] ^= (byte)(j + step);
                aes.AES4(zero, this.I1, this.GetL((int)i) ,buf.Slice(0, BlockSize), tmp);
                Utils.XORBytes1x16(R, tmp, R.Slice(0, BlockSize));
            }
            R.Slice(0, inBytes / 2).CopyTo(buf);
            L.Slice(0, (inBytes + 1)/ 2).CopyTo(buf.Slice(inBytes / 2));

            if ((inBytes & 1) != 0)
            {
                for (var k = inBytes - 1; k > inBytes / 2; k--)
                {
                    buf[k] = (byte)((buf[k] >> 4) | (buf[k - 1] << 4));
                }
                buf[inBytes / 2] = (byte)((L[0] >> 4) | (R[inBytes / 2] & 0xf0));
            }
            buf.Slice(0, inBytes).CopyTo(output);
            if (inBytes < 16 && d == 0)
            {
                buf.Slice(inBytes,BlockSize - inBytes).Clear();
                buf[0] |= 0x80;
                Utils.XORBytes1x16(delta, buf, buf.Slice(0, BlockSize));
                aes.AES4(zero, this.I1, this.L3, buf.Slice(0, BlockSize), tmp);
                output[0] ^= (byte)(tmp[0] & 0x80);
            }
        }

        public void Encipher(ReadOnlySpan<byte> delta, ReadOnlySpan<byte> input, Span<byte> output)
        {
            Debug.Assert(delta.Length == BlockSize);
            if (input.Length == 0)
                return;

            if (input.Length < 32)
            {
                AezTiny(delta, input, 0 , output);
            }
            else
            {
                AezCore(delta, input, 0, output);
            }
        }

        public void Decipher(ReadOnlySpan<byte> delta, ReadOnlySpan<byte> input, Span<byte> output)
        {
            if (input.Length == 0)
                return;
            
            if (input.Length < 32)
                AezTiny(delta, input, 1, output);
            else
                AezCore(delta, input, 1, output);
        }
    }

}