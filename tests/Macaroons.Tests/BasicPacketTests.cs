using Xunit;

namespace Macaroons.Tests
{
    public class BasicPacketTests
    {
        [Fact]
        public void CanComparePackets()
        {
            var p1a = new Packet(new byte[] {1, 2, 3}, DataEncoding.Hex);
            var p1b = new Packet(new byte[] {1, 2, 3}, DataEncoding.Hex);
            var p2 = new Packet(new byte[] {3, 4, 5}, DataEncoding.Hex);

            Assert.True(p1a == p1b);
            Assert.False(p1a == p2);
            Assert.False(p1a != p1b);
            Assert.True(p1a != p2);
        }

        [Fact]
        public void CanCalculateHashCodes()
        {
            // Arrange
            var p1a = new Packet(new byte[] { 1, 2, 3 }, DataEncoding.Hex);
            var p1b = new Packet(new byte[] { 1, 2, 3 }, DataEncoding.Hex);
            var p2 = new Packet(new byte[] { 3, 4, 5 }, DataEncoding.Hex);

            // Act
            var h1a = p1a.GetHashCode();
            var h1b = p1b.GetHashCode();
            var h2 = p2.GetHashCode();

            // Assert
            Assert.NotEqual(0, h1a);
            Assert.Equal(h1a, h1b);
            Assert.NotEqual(h1a, h2);
        }

        [Fact]
        public void CanCopyPackets()
        {
            var p1 = new Packet(new byte[] {1,2}, DataEncoding.Hex);
            var p2 = new Packet("abc");
            var q1 = new Packet(p1);
            var q2 = new Packet(p2);
            Assert.Equal(p1.Data, q1.Data);
            Assert.Equal(p1.Encoding, p1.Encoding);
            Assert.Equal(p2.Data, q2.Data);
            Assert.Equal(p2.Encoding, p2.Encoding);
        }
    }
}