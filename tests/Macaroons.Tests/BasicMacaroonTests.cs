using Xunit;

namespace Macaroons.Tests
{
    public class BasicMacaroonTests : TestBase
    {
        [Fact]
        public void CanCopyMacaroon()
        {
            var m1 = new Macaroon(Location, Secret, Identifier);
            m1.AddFirstPartyCaveat("account = 3735928559");
            
            var m2 = new Macaroon(m1);
            Assert.Equal(m1.Location, m2.Location);
            Assert.Equal(m1.Identifier, m2.Identifier);
            Assert.Equal(m1.Signature, m2.Signature);

            // - Change m2 and check that m1 stays the same
            m2.AddFirstPartyCaveat("a = 10");
            Assert.Equal(2, m2.Caveats.Count);
            Assert.Equal("account = 3735928559", m2.Caveats[0].CId.ToString());
            Assert.Equal(1, m1.Caveats.Count);
        }

        [Fact]
        public void CanPrintMacaroon()
        {
            var m = new Macaroon(Location, Secret, Identifier);
            var s = m.ToString();
            Assert.Equal(Location, s);
        }
    }
}