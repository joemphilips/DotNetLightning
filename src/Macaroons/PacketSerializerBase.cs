using System.Text;

namespace Macaroons
{
    public class PacketSerializerBase
    {
        public const int PACKET_PREFIX = 4;

        public static readonly byte[] LocationID = Encoding.ASCII.GetBytes("location");

        public static readonly byte[] IdentifierID = Encoding.ASCII.GetBytes("identifier");

        public static readonly byte[] SignatureID = Encoding.ASCII.GetBytes("signature");

        public static readonly byte[] CIdID = Encoding.ASCII.GetBytes("cid");
        public static readonly byte[] VIdID = Encoding.ASCII.GetBytes("vid");

        public static readonly byte[] ClID = Encoding.ASCII.GetBytes("cl");

        internal const string Hex = "0123456789abcdef";
    }
}