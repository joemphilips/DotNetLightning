using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Macaroons.Tests
{
#if !BouncyCastle
    /// <summary>
    /// These Tests go through the examples from the tutorial on 
    /// the original libmacaroons GitHub page at https://github.com/rescrv/libmacaroons
    /// Since we need `SecretBoxCryptoAlgorithm` to follow those test vectors, we run these tests only in the
    /// non-BouncyCastle build.
    /// </summary>
    public class SanityTestsForSecretBox : TestBase
    {
        public SanityTestsForSecretBox()
        {
          Macaroon.Crypto = new SecretBoxCryptoAlgorithm(false);
        }

        const string Secret2 = "this is a different super-secret key; never use the same secret twice";
        const string Identifier2 = "we used our other secret key";
        const string Location2 = "http://mybank/";

        [Fact]
        public void CanPrepareForRequest()
        {
          // Arrange
          Macaroon m = new Macaroon(Location2, Secret2, Identifier2);
          m.AddFirstPartyCaveat("account = 3735928559");

          string caveat_key = "4; guaranteed random by a fair toss of the dice";
          string identifier = "this was how we remind auth of key/pred";
          m.AddThirdPartyCaveat("http://auth.mybank/", caveat_key, identifier);

          Macaroon d = new Macaroon("http://auth.mybank/", caveat_key, identifier);
          d.AddFirstPartyCaveat("time < 2015-01-01T00:00");

          // Act
          Macaroon dp = m.PrepareForRequest(d);

          // Assert
          Assert.Equal("82A80681F9F32D419AF12F6A71787A1BAC3AB199DF934ED950DDF20C25AC8C65", d.Signature.ToString().ToUpperInvariant());
          Assert.Equal("2EB01D0DD2B4475330739140188648CF25DDA0425EA9F661F1574CA0A9EAC54E", dp.Signature.ToString().ToUpperInvariant());
        }
        
        [Fact]
        public void CanAddThirdPartyCaveat()
        {
            // Arrange
            Macaroon m = new Macaroon(Location2, Secret2, Identifier2);
            m.AddFirstPartyCaveat("account = 3735928559");

            // - just checking (this should although be covered in other Tests) ...
            Assert.Equal("1434E674AD84FDFDC9BC1AA00785325C8B6D57341FC7CE200BA4680C80786DDA", m.Signature.ToString().ToUpperInvariant());

            // Act
            string caveat_key = "4; guaranteed random by a fair toss of the dice";
            // string predicate = "user = Alice";
            // # send_to_auth(caveat_key, predicate)
            // # identifier = recv_from_auth()
            string identifier = "this was how we remind auth of key/pred";

            m.AddThirdPartyCaveat("http://auth.mybank/", caveat_key, identifier);

            // Assert
            Assert.Equal("D27DB2FD1F22760E4C3DAE8137E2D8FC1DF6C0741C18AED4B97256BF78D1F55C", m.Signature.ToString().ToUpperInvariant());

            string expectedStringRepresentation = @"Location = http://mybank/
Identifier = we used our other secret key
CId = account = 3735928559
CId = this was how we remind auth of key/pred
  VId = AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA027FAuBYhtHwJ58FX6UlVNFtFsGxQHS7uD_w_dedwv4Jjw7UorCREw5rXbRqIKhr
  Cl = http://auth.mybank/
Signature = d27db2fd1f22760e4c3dae8137e2d8fc1df6c0741c18aed4b97256bf78d1f55c
";

            Assert.Equal(expectedStringRepresentation, m.Inspect());

            List<Caveat> thirdPartyCaveats = m.ThirdPartyCaveats.ToList();
            Assert.Single(thirdPartyCaveats);
            Assert.Equal("http://auth.mybank/", thirdPartyCaveats[0].Cl.ToString());
            Assert.Equal("this was how we remind auth of key/pred", thirdPartyCaveats[0].CId.ToString());
        }

    }
  #endif
}