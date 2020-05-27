using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection.Emit;
using Xunit;

namespace Macaroons.Tests
{
    // These tests go through the examples from the tutorial on 
    // the original libmacaroons GitHub page at https://github.com/rescrv/libmacaroons
    public class SanityTests : TestBase
    {
        const string Secret2 = "this is a different super-secret key; never use the same secret twice";
        const string Identifier2 = "we used our other secret key";
        const string Location2 = "http://mybank/";

        public SanityTests()
        {
            Macaroon.Crypto = new SecretBoxCryptoAlgorithm(false);
        }

        [Fact]
        public void CanCreateEmptyMacaroonWithSignature()
        {
            Macaroon m = new Macaroon(Location, Secret, Identifier);
            
            Assert.Equal(Identifier, m.Identifier.ToString());
            Assert.Equal(Location, m.Location.ToString());
            Assert.Equal("E3D9E02908526C4C0039AE15114115D97FDD68BF2BA379B342AAF0F617D0552F".ToLowerInvariant(), m.Signature.ToString());
            Assert.Equal(0, m.Caveats.Count);
        }

        [Fact]
        public void CanAddOneFirstPartyCaveat()
        {
            var m = new Macaroon(Location, Secret, Identifier);
            
            Assert.Equal(Identifier, m.Identifier.ToString());
            m.AddFirstPartyCaveat("account = 3735928559");

            Assert.Equal(1, m.Caveats.Count);
            Assert.Equal("CId = account = 3735928559", m.Caveats[0].Inspect());
            Assert.Equal("1EFE4763F290DBCE0C1D08477367E11F4EEE456A64933CF662D79772DBB82128".ToLowerInvariant(), m.Signature.ToString());
        }
        
        [Fact]
        public void CanAddMultipleFirstPartyCaveats()
        {
            Macaroon m = new Macaroon(Location, Secret, Identifier);

            m.AddFirstPartyCaveat("account = 3735928559");
            m.AddFirstPartyCaveat("time < 2015-01-01T00:00");
            m.AddFirstPartyCaveat("email = alice@example.org");

            Assert.Equal(3, m.Caveats.Count);
            Assert.Equal("CId = account = 3735928559", m.Caveats[0].Inspect());
            Assert.Equal("CId = time < 2015-01-01T00:00", m.Caveats[1].Inspect());
            Assert.Equal("CId = email = alice@example.org", m.Caveats[2].Inspect());
            Assert.Equal("882E6D59496ED5245EDB7AB5B8839ECD63E5D504E54839804F164070D8EED952".ToLowerInvariant(), m.Signature.ToString());

            string expectedStringRepresentation = @"Location = http://mybank/
Identifier = we used our secret key
CId = account = 3735928559
CId = time < 2015-01-01T00:00
CId = email = alice@example.org
Signature = 882e6d59496ed5245edb7ab5b8839ecd63e5d504e54839804f164070d8eed952
";

            Assert.Equal(expectedStringRepresentation, m.Inspect());
        }
        
        [Fact]
        public void CanAddThirdPartyCaveat()
        {
            // Arrange
            Macaroon m = new Macaroon(Location2, Secret2, Identifier2);
            m.AddFirstPartyCaveat("account = 3735928559");

            // - just checking (this should although be covered in other tests) ...
            Assert.Equal("1434e674ad84fdfdc9bc1aa00785325c8b6d57341fc7ce200ba4680c80786dda", m.Signature.ToString());

            // Act
            string caveat_key = "4; guaranteed random by a fair toss of the dice";
            // string predicate = "user = Alice";
            // # send_to_auth(caveat_key, predicate)
            // # identifier = recv_from_auth()
            string identifier = "this was how we remind auth of key/pred";

            m.AddThirdPartyCaveat("http://auth.mybank/", caveat_key, identifier);

            // Assert
            Assert.Equal("d27db2fd1f22760e4c3dae8137e2d8fc1df6c0741c18aed4b97256bf78d1f55c", m.Signature.ToString());
     
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
            Assert.Equal("82a80681f9f32d419af12f6a71787a1bac3ab199df934ed950ddf20c25ac8c65", d.Signature.ToString());
            Assert.Equal("2eb01d0dd2b4475330739140188648cf25dda0425ea9f661f1574ca0a9eac54e", dp.Signature.ToString());
        }
        
        [Fact]
        public void CanVerifyWithDischargeMacaroon()
        {
            // Arrange
            Macaroon m = new Macaroon(Location2, Secret2, Identifier2);
            m.AddFirstPartyCaveat("account = 3735928559");

            string caveat_key = "4; guaranteed random by a fair toss of the dice";
            string identifier = "this was how we remind auth of key/pred";
            m.AddThirdPartyCaveat("http://auth.mybank/", caveat_key, identifier);

            Macaroon d = new Macaroon("http://auth.mybank/", caveat_key, identifier);
            d.AddFirstPartyCaveat("time < 2115-01-01T00:00");

            Macaroon dp = m.PrepareForRequest(d);

            Verifier v = new Verifier();
            v.SatisfyExact("account = 3735928559");
            v.SatisfyGeneral(TimeVerifier);

            // Act
            VerificationResult result = m.Verify(v, Secret2, new List<Macaroon> { dp });

            // Assert
            Assert.True(result.Success);
        }
    }
}