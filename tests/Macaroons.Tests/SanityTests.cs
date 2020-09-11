using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Macaroons.Tests
{
#if !BouncyCastle
    // These Tests go through the examples from the tutorial on 
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
      // Act
      Macaroon m = new Macaroon(Location, Secret, Identifier);

      // Assert
      Assert.Equal(Identifier, m.Identifier.ToString());
      Assert.Equal(Location, m.Location.ToString());
      Assert.Equal("E3D9E02908526C4C0039AE15114115D97FDD68BF2BA379B342AAF0F617D0552F", m.Signature.ToString().ToUpperInvariant());
      Assert.Equal(0, m.Caveats.Count);
    }


    [Fact]
    public void CanAddOneFirstPartyCaveat()
    {
      // Arrange
      Macaroon m = new Macaroon(Location, Secret, Identifier);

      // Act
      m.AddFirstPartyCaveat("account = 3735928559");

      // Assert
      Assert.Equal(1, m.Caveats.Count);
      Assert.Equal("CId = account = 3735928559", m.Caveats[0].Inspect());
      Assert.Equal("1EFE4763F290DBCE0C1D08477367E11F4EEE456A64933CF662D79772DBB82128", m.Signature.ToString().ToUpperInvariant());
    }


    [Fact]
    public void CanAddMultipleFirstPartyCaveats()
    {
      // Arrange
      Macaroon m = new Macaroon(Location, Secret, Identifier);

      // Act
      m.AddFirstPartyCaveat("account = 3735928559");
      m.AddFirstPartyCaveat("time < 2015-01-01T00:00");
      m.AddFirstPartyCaveat("email = alice@example.org");

      // Assert
      Assert.Equal(3, m.Caveats.Count);
      Assert.Equal("CId = account = 3735928559", m.Caveats[0].Inspect());
      Assert.Equal("CId = time < 2015-01-01T00:00", m.Caveats[1].Inspect());
      Assert.Equal("CId = email = alice@example.org", m.Caveats[2].Inspect());
      Assert.Equal("882E6D59496ED5245EDB7AB5B8839ECD63E5D504E54839804F164070D8EED952", m.Signature.ToString().ToUpperInvariant());

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
      VerificationResult result = m.Verify(v, Secret2, new List<Macaroon> {dp});

      // Assert
      Assert.True(result.Success);
    }
  }
  #endif
}