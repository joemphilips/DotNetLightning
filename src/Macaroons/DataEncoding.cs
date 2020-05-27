using System.Text;

namespace Macaroons
{
    public abstract class DataEncoding
    {
        public abstract string GetString(byte[] d);

        public abstract byte[] GetBytes(string s);
        public readonly static DataEncoding UTF8 = new TextDataEncoding(Encoding.UTF8);
        public readonly static DataEncoding ASCII = new TextDataEncoding(Encoding.ASCII);
        public readonly static DataEncoding Hex = new HexDataEncoding();
        public readonly static DataEncoding Base64UrlSafe = new Base64UrlSafeDataEncoding();
    }
}