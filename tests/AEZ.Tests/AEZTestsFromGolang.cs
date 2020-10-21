using System;
using System.Collections.Generic;
using NBitcoin.DataEncoders;
using System.IO;
using System.Linq;
using Newtonsoft.Json.Linq;
using Xunit;
using static AEZ.AEZConstants;

namespace AEZ.Tests
{
    public class AEZTestsFromGolang
    {
        private HexEncoder _hex = new HexEncoder();
        private JArray ReadJson(string file)
        {
            return JArray.Parse(File.ReadAllText($"data/{file}"));
        }

        [Fact]
        public void TestExtract()
        {
            var extractVectors = (ReadJson("extract.json"));
            Assert.NotEmpty(extractVectors);
            foreach (var token in extractVectors)
            {
                var a = ((JObject) token).GetValue("a").ToString();
                var extractedKey = new byte[ExtractedKeySize];
                var vecA = _hex.DecodeData(a);
                var b = ((JObject) token).GetValue("b").ToString();
                var vecB = _hex.DecodeData(b);
                AEZ.Extract(vecA, extractedKey);
                Assert.Equal(_hex.EncodeData(vecB), _hex.EncodeData(extractedKey));
            }
        }

        [Fact]
        public void TestAEZHash()
        {
            var hashVectors = ReadJson("hash.json");
            foreach (var token in hashVectors)
            {
                var jobj = (JObject) token;
                var vecK = _hex.DecodeData(jobj.GetValue("k").ToString());
                var data = new List<byte[]>();
                var jdata = (JArray)jobj.GetValue("data");
                foreach (var v in jdata)
                {
                    var d = _hex.DecodeData(v.ToString());
                    data.Add(d);
                }

                var vecV = _hex.DecodeData(jobj.GetValue("v").ToString());
                byte[] nonce = data[0];
                byte[][] ad = {};
                if (data.Count > 1)
                {
                    ad = data.Skip(1).ToArray();
                }

                var result = new byte[BlockSize];
                var e = new EState(vecK);
                e.AEZHash(nonce, ad, ((int)jobj.GetValue("tau")), result);
                Assert.Equal(_hex.EncodeData(vecV), _hex.EncodeData(result));
            }
        }

        [Fact]
        public void TestPRF()
        {
            var prfVectors = ReadJson("prf.json");
            foreach (var token in prfVectors)
            {
                var jobj = (JObject) token;
                var vecK = _hex.DecodeData(jobj.GetValue("k").ToString());
                var vecDelta = _hex.DecodeData(jobj.GetValue("delta").ToString());
                var vecR = _hex.DecodeData(jobj.GetValue("r").ToString());

                Span<byte> vDelta = new byte[BlockSize];
                vecDelta.CopyTo(vDelta);

                var result = new byte[vecR.Length];
                var e = new EState(vecK);
                e.AEZ_PRF(vDelta, jobj.Value<int>("tau"), result);
                Assert.Equal(_hex.EncodeData(vecR), _hex.EncodeData(result));
            }
        }

        private void AssertEncrypt(JArray jarray)
        {
            foreach (var token in jarray)
            {
                var jobj = (JObject) token;
                var vecK = _hex.DecodeData(jobj.GetValue("k").ToString());
                var vecNonce = _hex.DecodeData(jobj.GetValue("nonce").ToString());
                
                var data = new List<byte[]>();
                var jdata = (JArray)jobj.GetValue("data");
                foreach (var v in jdata)
                {
                    var d = _hex.DecodeData(v.ToString());
                    data.Add(d);
                }
                var vecM = _hex.DecodeData(jobj.GetValue("m").ToString());
                var vecC = _hex.DecodeData(jobj.GetValue("c").ToString());

                var vecTau = jobj.Value<int>("tau");
                var dst = Span<byte>.Empty;
                var c = AEZ.Encrypt(vecK, vecNonce, data.ToArray(), vecTau, vecM, dst);
                Assert.Equal(_hex.EncodeData(vecC), _hex.EncodeData(c));
                
                var m = AEZ.Decrypt(vecK, vecNonce, data.ToArray(), vecTau, vecC, dst);
                Assert.Equal(_hex.EncodeData(vecM), _hex.EncodeData(m));
            }
        }

        [Fact]
        public void TestEncrypt()
        {
            AssertEncrypt(ReadJson("encrypt.json"));
            AssertEncrypt(ReadJson("encrypt_16_byte_key.json"));
            AssertEncrypt(ReadJson("encrypt_33_byte_ad.json"));
            AssertEncrypt(ReadJson("encrypt_no_ad.json"));
        }
    }
}