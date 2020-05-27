using System.IO;
using Xunit;

namespace Macaroons.Tests
{
    public class SerializationTests : TestBase
    {
        [Fact]
        public void CanSerializeEmptyMacaroon()
        {
            var m = new Macaroon(Location, Secret, Identifier);
            var s = m.Serialize();
            Assert.Equal(
                "MDAxY2xvY2F0aW9uIGh0dHA6Ly9teWJhbmsvCjAwMjZpZGVudGlmaWVyIHdlIHVzZWQgb3VyIHNlY3JldCBrZXkKMDAyZnNpZ25hdHVyZSDj2eApCFJsTAA5rhURQRXZf91ovyujebNCqvD2F9BVLwo",
                s);
        }

        [Fact]
        public void CanSerializeMultipleFirstPartyCaveats()
        {
            var m = new Macaroon(Location, Secret, Identifier);
            m.AddFirstPartyCaveat("account = 3735928559");
            m.AddFirstPartyCaveat("time < 2015-01-01T00:00");
            m.AddFirstPartyCaveat("email = alice@example.org");

            // Act
            string s = m.Serialize();

            // Assert (the expected value here is just calculated - I havent seen any correct value on the web)
            Assert.Equal(
                "MDAxY2xvY2F0aW9uIGh0dHA6Ly9teWJhbmsvCjAwMjZpZGVudGlmaWVyIHdlIHVzZWQgb3VyIHNlY3JldCBrZXkKMDAxZGNpZCBhY2NvdW50ID0gMzczNTkyODU1OQowMDIwY2lkIHRpbWUgPCAyMDE1LTAxLTAxVDAwOjAwCjAwMjJjaWQgZW1haWwgPSBhbGljZUBleGFtcGxlLm9yZwowMDJmc2lnbmF0dXJlIIgubVlJbtUkXtt6tbiDns1j5dUE5Ug5gE8WQHDY7tlSCg",
                s);
        }

        [Fact]
        public void CanDeserializeEmptyMacaroon()
        {
            // Arrange (this is a Macaroon from the tutorial (https://github.com/rescrv/libmacaroons) containing an invalid signature - but that should not be checked here)
            string serialized =
                "MDAxY2xvY2F0aW9uIGh0dHA6Ly9teWJhbmsvCjAwMjZpZGVudGlmaWVyIHdlIHVzZWQgb3VyIHNlY3JldCBrZXkKMDAyZnNpZ25hdHVyZSDj2eApCFJsTAA5rhURQRXZf91ovyujebNCqvD2F9BVLwo";

            // Act
            Macaroon m = Macaroon.Deserialize(serialized);

            // Assert
            Assert.Equal(Location, m.Location.ToString());
            Assert.Equal(Identifier, m.Identifier.ToString());
            Assert.Equal(0, m.Caveats.Count);
            Assert.True(m.Verify(new Verifier(), Secret).Success);
        }

        [Fact]
        public void CanDeserializeMultipleFirstPartyCaveats()
        {
            // Arrange
            string serialized =
                "MDAxY2xvY2F0aW9uIGh0dHA6Ly9teWJhbmsvCjAwMjZpZGVudGlmaWVyIHdlIHVzZWQgb3VyIHNlY3JldCBrZXkKMDAxZGNpZCBhY2NvdW50ID0gMzczNTkyODU1OQowMDIwY2lkIHRpbWUgPCAyMDE1LTAxLTAxVDAwOjAwCjAwMjJjaWQgZW1haWwgPSBhbGljZUBleGFtcGxlLm9yZwowMDJmc2lnbmF0dXJlID8f19FL+bkC9p/aoMmIecC7GxdOcLVyUnrv6lJMM7NSCg==";

            // Act
            Macaroon m = Macaroon.Deserialize(serialized);

            // Assert
            Assert.Equal(Location, m.Location.ToString());
            Assert.Equal(Identifier, m.Identifier.ToString());
            Assert.Equal(3, m.Caveats.Count);
            Assert.Equal("account = 3735928559", m.Caveats[0].CId.ToString());
            Assert.Equal("time < 2015-01-01T00:00", m.Caveats[1].CId.ToString());
            Assert.Equal("email = alice@example.org", m.Caveats[2].CId.ToString());
        }


        [Fact]
        public void CanSerializeAndDeserializeThirdPartyCaveats()
        {
            // Arrange
            Macaroon m1 = new Macaroon(Location, Secret, Identifier);
            m1.AddFirstPartyCaveat("account = 3735928559");

            string caveat_key = "4; guaranteed random by a fair toss of the dice";
            string identifier = "this was how we remind auth of key/pred";
            m1.AddThirdPartyCaveat("http://auth.mybank/", caveat_key, identifier);

            // Act
            string s = m1.Serialize();
            Macaroon m2 = Macaroon.Deserialize(s);

            // Assert
            Assert.Equal(m1.Location, m2.Location);
            Assert.Equal(m1.Identifier, m2.Identifier);
            Assert.Equal(m1.Signature, m2.Signature);
            Assert.Equal(m1.Caveats.Count, m2.Caveats.Count);
            Assert.Equal(m1.Caveats[0].Cl, m2.Caveats[0].Cl);
            Assert.Equal(m1.Caveats[0].CId, m2.Caveats[0].CId);
            Assert.Equal(m1.Caveats[0].VId, m2.Caveats[0].VId);
            Assert.Equal(m1.Caveats[1].Cl, m2.Caveats[1].Cl);
            Assert.Equal(m1.Caveats[1].CId, m2.Caveats[1].CId);
            Assert.Equal(m1.Caveats[1].VId, m2.Caveats[1].VId);
        }


        [Fact]
        public void WhenDeserializingBadPacketItThrowsInvalidDataException()
        {
            // Arrange
            // - This data would make the deserializer run around in circles in earlier versions.
            string s =
                "MDAyNWxvY2F0aW9uIGNTZWFyY2g6ZG9jdW1lbnQ6MTQ5MzY0CjAwMjJpZGVudGlmaWVyIGRvY3VtZW50SWQ6IDE0OTM2NAowMDFiY2lkIGRvY3VtZW50SWQ6IDE0OTM2NAowMDIzY2lkIHRpbWUgPCAyMDE2LTAxLTA0VDEyOjQzOjU2CjAwMmZzaWduyXR1cmUgQbpcMXKEUSc4AE1xANE2V4b1BbKAGSbrEO2oAOqZYhkK";
            Assert.Throws<InvalidDataException>(() => Macaroon.Deserialize(s));
        }
    }
}