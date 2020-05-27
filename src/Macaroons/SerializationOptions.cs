namespace Macaroons
{
    public class SerializationOptions
    {
        public DataEncoding MacaroonIdentifierEncoding { get; set; }
        public DataEncoding CaveatIdentifierEncoding { get; set; }

        public static readonly SerializationOptions Default = new SerializationOptions()
        {
            MacaroonIdentifierEncoding = DataEncoding.UTF8,
            CaveatIdentifierEncoding = DataEncoding.UTF8
        };
    }
}