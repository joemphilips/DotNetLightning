using System.Collections.Generic;
using Xunit;

namespace Macaroons.Tests
{
    public class VerificationTests : TestBase
    {
        [Fact]
        public void CanVerifyEmptyMacaroon()
        {
            var m = new Macaroon(Location, Secret, Identifier);
            var v = new Verifier();

            var r = m.Verify(v, Secret);
            Assert.True(r.Success);
        }


        [Fact]
        public void CanVerifyFirstPartyCaveat()
        {
            // Arrange
            var m = new Macaroon(Location, Secret, Identifier);
            m.AddFirstPartyCaveat("account = 3735928559");

            var v = new Verifier();
            v.SatisfyExact("account = 3735928559");

            // Act
            var verified = m.Verify(v, Secret);

            // Assert
            Assert.True(verified.Success);
        }


        [Fact]
        public void CanVerifyMultipleFirstPartyCaveats()
        {
            // Arrange
            Macaroon m = new Macaroon(Location, Secret, Identifier);
            m.AddFirstPartyCaveat("account = 3735928559");
            m.AddFirstPartyCaveat("time < 2115-01-01T00:00");
            m.AddFirstPartyCaveat("email = alice@example.org");

            Verifier v = new Verifier();
            v.SatisfyExact("account = 3735928559");
            v.SatisfyExact("time < 2115-01-01T00:00");
            v.SatisfyExact("email = alice@example.org");

            // Act
            VerificationResult verified = m.Verify(v, Secret);

            // Assert
            Assert.True(verified.Success);
        }


        [Fact]
        public void CanVerifyFirstPartyGeneralCaveat()
        {
            // Arrange
            Macaroon mSuccess = new Macaroon(Location, Secret, Identifier);
            mSuccess.AddFirstPartyCaveat("time < 2115-01-01T00:00");

            Macaroon mFailure = new Macaroon(Location, Secret, Identifier);
            mFailure.AddFirstPartyCaveat("time < 2000-01-01T00:00");

            Verifier v = new Verifier();
            v.SatisfyGeneral(TimeVerifier);

            // Act
            VerificationResult verified1 = mSuccess.Verify(v, Secret);
            VerificationResult verified2 = mFailure.Verify(v, Secret);

            // Assert
            Assert.True(verified1.Success);
            Assert.False(verified2.Success);
            Assert.Equal(1, verified2.Messages.Count);
            Assert.Contains("Caveat", verified2.Messages[0]);
            Assert.Contains("time < 2000-01-01T00:00", verified2.Messages[0]);
            Assert.Contains("failed", verified2.Messages[0]);
        }


        [Fact]
        public void VerificationFailsWithUnknownCaveat()
        {
            // Arrange
            Macaroon m = new Macaroon(Location, Secret, Identifier);
            m.AddFirstPartyCaveat("account = 3735928559");

            Verifier v = new Verifier();
            v.SatisfyExact("account = 88778");

            // Act
            VerificationResult verified = m.Verify(v, Secret);

            // Assert
            Assert.False(verified.Success);
            Assert.Equal(1, verified.Messages.Count);
            Assert.Contains("Caveat", verified.Messages[0]);
            Assert.Contains("failed", verified.Messages[0]);
        }


        [Fact]
        public void VerificationFailsWithInvalidSecret()
        {
            // Arrange
            Macaroon m = new Macaroon(Location, "Another secret", Identifier);
            m.AddFirstPartyCaveat("account = 3735928559");

            Verifier v = new Verifier();
            v.SatisfyExact("account = 3735928559");

            // Act
            VerificationResult verified = m.Verify(v, Secret);

            // Assert
            Assert.False(verified.Success);
            Assert.Equal(1, verified.Messages.Count);
            Assert.Contains("Signature mismatch", verified.Messages[0]);
        }


        [Fact]
        public void VerificationFailsWithInvalidSignature()
        {
            // Arrange
            Macaroon mValid = new Macaroon(Location, Secret, Identifier);
            mValid.AddFirstPartyCaveat("account = 3735928559");
            mValid.AddFirstPartyCaveat("time < 2115-01-01T00:00");
            mValid.AddFirstPartyCaveat("email = alice@example.org");

            // This is a Macaroon from the tutorial (https://github.com/rescrv/libmacaroons) containing an invalid signature 
            string serialized =
                "MDAxY2xvY2F0aW9uIGh0dHA6Ly9teWJhbmsvCjAwMjZpZGVudGlmaWVyIHdlIHVzZWQgb3VyIHNlY3JldCBrZXkKMDAxZGNpZCBhY2NvdW50ID0gMzczNTkyODU1OQowMDIwY2lkIHRpbWUgPCAyMDE1LTAxLTAxVDAwOjAwCjAwMjJjaWQgZW1haWwgPSBhbGljZUBleGFtcGxlLm9yZwowMDJmc2lnbmF0dXJlID8f19FL+bkC9p/aoMmIecC7GxdOcLVyUnrv6lJMM7NSCg==";
            Macaroon mInvalid = Macaroon.Deserialize(serialized);

            Verifier v = new Verifier();
            v.SatisfyExact("account = 3735928559");
            v.SatisfyExact("time < 2115-01-01T00:00");
            v.SatisfyExact("email = alice@example.org");

            // Act
            VerificationResult verifiedOk = mValid.Verify(v, Secret);
            VerificationResult verifiedFails = mInvalid.Verify(v, Secret);

            // Assert
            Assert.Equal(mValid.Location, mInvalid.Location);
            Assert.Equal(mValid.Identifier, mInvalid.Identifier);
            Assert.Equal(mValid.Caveats.Count, mInvalid.Caveats.Count);
            Assert.NotEqual(mValid.Signature, mInvalid.Signature);
            Assert.True(verifiedOk.Success);
            Assert.False(verifiedFails.Success);
            Assert.Equal(2, verifiedFails.Messages.Count);
            Assert.Contains("Caveat 'time < 2015-01-01T00:00' failed", verifiedFails.Messages[0]);
            Assert.Contains("Signature mismatch", verifiedFails.Messages[1]);
        }


        [Fact]
        public void CanVerifyWithMultipleDischargeMacaroons()
        {
            // Arrange

            // - Create primary macaroon
            Macaroon m = new Macaroon(Location, Secret, Identifier);
            m.AddFirstPartyCaveat("account = 3735928559");

            // - Add third party caveat (1)
            string caveat_key1 = "4; guaranteed random by a fair toss of the dice";
            string identifier1 = "this was how we remind auth of key/pred";
            m.AddThirdPartyCaveat("http://auth.mybank/", caveat_key1, identifier1);

            // - Add third party caveat (2)
            string caveat_key2 = "random key 2";
            string identifier2 = "identifier 2";
            m.AddThirdPartyCaveat("http://auth.government/", caveat_key2, identifier2);

            // - Create discharge macaroon (1)
            Macaroon d1 = new Macaroon("http://auth.mybank/", caveat_key1, identifier1);
            d1.AddFirstPartyCaveat("time < 2115-01-01T00:00");
            Macaroon dp1 = m.PrepareForRequest(d1);

            // - Create discharge macaroon (2)
            Macaroon d2 = new Macaroon("http://auth.mybank/", caveat_key2, identifier2);
            Macaroon dp2 = m.PrepareForRequest(d2);

            // Create verifier with suitable predicates
            Verifier v = new Verifier();
            v.SatisfyExact("account = 3735928559");
            v.SatisfyGeneral(TimeVerifier);

            // Act
            VerificationResult result = m.Verify(v, Secret, new List<Macaroon> {dp1, dp2});

            // Assert
            Assert.True(result.Success);
        }


        [Fact]
        public void VerificationFailsWhenDischargeMacaroonIsMissing()
        {
            // Arrange

            // - Create primary macaroon
            Macaroon m = new Macaroon(Location, Secret, Identifier);
            m.AddFirstPartyCaveat("account = 3735928559");

            // - Add third party caveat (1)
            string caveat_key1 = "4; guaranteed random by a fair toss of the dice";
            string identifier1 = "this was how we remind auth of key/pred";
            m.AddThirdPartyCaveat("http://auth.mybank/", caveat_key1, identifier1);

            // - Add third party caveat (2)
            string caveat_key2 = "random key 2";
            string identifier2 = "identifier 2";
            m.AddThirdPartyCaveat("http://auth.government/", caveat_key2, identifier2);

            // - Create discharge macaroon (1)
            Macaroon d1 = new Macaroon("http://auth.mybank/", caveat_key1, identifier1);
            d1.AddFirstPartyCaveat("time < 2115-01-01T00:00");
            Macaroon dp1 = m.PrepareForRequest(d1);

            // - Create discharge macaroon (2)
            Macaroon d2 = new Macaroon("http://auth.mybank/", caveat_key2, identifier2);
            Macaroon dp2 = m.PrepareForRequest(d2);

            // Create verifier with suitable predicates
            Verifier v = new Verifier();
            v.SatisfyExact("account = 3735928559");
            v.SatisfyGeneral(TimeVerifier);

            // Act
            VerificationResult result1 = m.Verify(v, Secret, new List<Macaroon> {dp1});
            VerificationResult result2 = m.Verify(v, Secret, new List<Macaroon> {dp2});

            // Assert
            Assert.False(result1.Success);
            Assert.False(result2.Success);
        }


        [Fact]
        public void VerificationFailsWhenPredicatesForThirdPartyCaveatIsMissing()
        {
            // Arrange

            // - Create primary macaroon
            Macaroon m = new Macaroon(Location, Secret, Identifier);
            m.AddFirstPartyCaveat("account = 3735928559");

            // - Add third party caveat (1)
            string caveat_key1 = "4; guaranteed random by a fair toss of the dice";
            string identifier1 = "this was how we remind auth of key/pred";
            m.AddThirdPartyCaveat("http://auth.mybank/", caveat_key1, identifier1);

            // - Add third party caveat (2)
            string caveat_key2 = "random key 2";
            string identifier2 = "identifier 2";
            m.AddThirdPartyCaveat("http://auth.government/", caveat_key2, identifier2);

            // - Create discharge macaroon (1)
            Macaroon d1 = new Macaroon("http://auth.mybank/", caveat_key1, identifier1);
            d1.AddFirstPartyCaveat("time < 2115-01-01T00:00");
            Macaroon dp1 = m.PrepareForRequest(d1);

            // - Create discharge macaroon (2)
            Macaroon d2 = new Macaroon("http://auth.mybank/", caveat_key2, identifier2);
            Macaroon dp2 = m.PrepareForRequest(d2);

            // Create verifier with suitable predicates
            Verifier v = new Verifier();
            v.SatisfyExact("account = 3735928559");
            // - exclude time verifier

            // Act
            VerificationResult result = m.Verify(v, Secret, new List<Macaroon> {dp1, dp2});

            // Assert
            Assert.False(result.Success);
        }


        [Fact]
        public void VerificationFailsWhenHavingCircularMacaroonReferences()
        {
            // Arrange

            // - Create primary macaroon
            Macaroon m = new Macaroon(Location, Secret, Identifier);

            // - Add third party caveat (1)
            string caveat_key1 = "4; guaranteed random by a fair toss of the dice";
            string identifier1 = "this was how we remind auth of key/pred";
            m.AddThirdPartyCaveat("http://auth.mybank/", caveat_key1, identifier1);

            // - Add third party caveat (2)
            string caveat_key2 = "random key 2";
            string identifier2 = "identifier 2";
            m.AddThirdPartyCaveat("http://auth.government/", caveat_key2, identifier2);

            // - Create discharge macaroon (1) with reference to (2)
            Macaroon d1 = new Macaroon("http://auth.mybank/", caveat_key1, identifier1);
            d1.AddThirdPartyCaveat("http://auth.government/", caveat_key2, identifier2);
            Macaroon dp1 = m.PrepareForRequest(d1);

            // - Create discharge macaroon (2) with reference to (1)
            Macaroon d2 = new Macaroon("http://auth.mybank/", caveat_key2, identifier2);
            d2.AddThirdPartyCaveat("http://auth.government/", caveat_key1, identifier1);
            Macaroon dp2 = m.PrepareForRequest(d2);

            Verifier v = new Verifier();

            // Act
            VerificationResult result = m.Verify(v, Secret, new List<Macaroon> {dp1, dp2});

            // Assert
            Assert.False(result.Success);
            Assert.Equal(2, result.Messages.Count);
            Assert.Contains("circular", result.Messages[0]);
        }


        [Fact]
        public void FailedVerificationAddsVerifierReason()
        {
            // Arrange
            Macaroon mFailure = new Macaroon(Location, Secret, Identifier);
            mFailure.AddFirstPartyCaveat("expires: 2000-01-01T00:00:00Z");

            Verifier v = new Verifier();
            v.SatisfyGeneral(StandardCaveatVerifiers.ExpiresVerifier);

            // Act
            VerificationResult verified2 = mFailure.Verify(v, Secret);

            // Assert
            Assert.False(verified2.Success);
            Assert.Equal(1, verified2.Messages.Count);
            Assert.Equal("Timestamp '2000-01-01T00:00:00Z' has expired", verified2.Messages[0]);
        }


        [Fact]
        public void StandardExpiresVerifierOnlyAcceptsISOTimeStamp()
        {
            // Arrange
            Macaroon mFailure = new Macaroon(Location, Secret, Identifier);
            mFailure.AddFirstPartyCaveat("expires: 23-12-2000");

            Verifier v = new Verifier();
            v.SatisfyGeneral(StandardCaveatVerifiers.ExpiresVerifier);

            // Act
            VerificationResult verified2 = mFailure.Verify(v, Secret);

            // Assert
            Assert.False(verified2.Success);
            Assert.Equal(1, verified2.Messages.Count);
            Assert.Equal("Invalid timestamp in 'expires: 23-12-2000'", verified2.Messages[0]);
        }

        [Fact]
        public void StandardExpiresVerifierDoesNotAddReasonWhenNoExpiresCaveatArePresent()
        {
            // Arrange
            Macaroon mFailure = new Macaroon(Location, Secret, Identifier);
            mFailure.AddFirstPartyCaveat("other: 1234");

            Verifier v = new Verifier();
            v.SatisfyGeneral(StandardCaveatVerifiers.ExpiresVerifier);

            // Act
            VerificationResult verified2 = mFailure.Verify(v, Secret);

            // Assert
            Assert.False(verified2.Success);
            Assert.Equal(1, verified2.Messages.Count);
            Assert.Equal("Caveat 'other: 1234' failed", verified2.Messages[0]);
        }
    }
}