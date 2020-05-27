namespace Macaroons
{
    public class Base64UrlSafeDataEncoding : DataEncoding
    {
        public override string GetString(byte[] d)
        {
            return Utility.ToBase64UrlSafe(d);
        }

        public override byte[] GetBytes(string s)
        {
            return Utility.FromBase64UrlSafe(s);
        }
    }
}