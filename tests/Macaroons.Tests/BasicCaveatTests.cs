using Xunit;

namespace Macaroons.Tests
{
    public class BasicCaveatTests
    {
        [Fact]
        public void CanCopyCaveat()
        {
            byte[] cid = { 1,2,3,4 };
            byte[] vid = {4, 3, 2, 1};
            byte[] cl = {1, 1, 2, 2};
            var c1 = new Caveat(new Packet(cid, DataEncoding.Hex), new Packet(vid, DataEncoding.Hex), new Packet(cl, DataEncoding.Hex));
            var c2 = new Caveat(c1);
            Assert.Equal(c1.CId, c2.CId);
            
            // Change original values and verify the new values doesn't change
            c1.CId[0] = 9;
            c1.VId[0] = 8;
            c1.CId[0] = 7;

            Assert.NotEqual(c1.CId, c2.CId);

            Assert.Equal(1, c2.CId[0]);
            Assert.Equal(4, c2.VId[0]);
            Assert.Equal(1, c2.Cl[0]);
        }

        [Fact]
        public void CanPrintCaveat()
        {
            var c = new Caveat("Caveat 1");
            var s = c.ToString();
            Assert.Equal("Caveat 1", s);
        }
    }
}