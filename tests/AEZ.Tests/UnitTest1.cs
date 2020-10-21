using System;
using System.Security.Cryptography;
using System.Text;
using NBitcoin;
using Xunit;
namespace AEZ.Tests
{
    public class UnitTest1
    {

        [Fact]
        public void TestInvalidEncoding()
        {
            // testcase found in aezeed.
            var salt = new byte[] {0, 0, 0, 1, 98};
            var ad = new byte[]{0, 0, 0, 0, 1, 98};
            var cipherText = new byte[]
            {
                159,
                217,
                93,
                187,
                7,
                127,
                101,
                132,
                202,
                38,
                118,
                248,
                146,
                251,
                87,
                3,
                48,
                10,
                44,
                82,
                129,
                249,
                179,
            };
            var pass = Encoding.UTF8.GetBytes("test");
            var wrongPass = Encoding.UTF8.GetBytes("kek");
            var scryptKey = 
                // b301317b8a1f874e2d2ad563ade52a268d0c4b7fec528110158bdcab94704d26
                NBitcoin.Crypto.SCrypt.ComputeDerivedKey(pass, salt, 16, 8, 1, null, 32);
            var wrongScryptKey = 
                // 4d0d138242993176ab9ccc56463773f265168f21f21a6f6146622fa8c06bc034
                NBitcoin.Crypto.SCrypt.ComputeDerivedKey(wrongPass, salt, 16, 8, 1, null, 32);
            Assert.Equal(0, AEZ.Decrypt(scryptKey, ReadOnlySpan<byte>.Empty,new [] {ad}, 4, cipherText, Span<byte>.Empty)[0]);
            Assert.Throws<CryptographicException>(()  => AEZ.Decrypt(wrongScryptKey, ReadOnlySpan<byte>.Empty,new [] {ad}, 4, cipherText, Span<byte>.Empty));
        }
    }
}