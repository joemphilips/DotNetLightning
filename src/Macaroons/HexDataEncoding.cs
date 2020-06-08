using NBitcoin.DataEncoders;

namespace Macaroons
{
    public class HexDataEncoding : DataEncoding
    {
        static HexEncoder _encoder = new HexEncoder();
        public override string GetString(byte[] d)
        {
            return _encoder.EncodeData(d);
        }

        public override byte[] GetBytes(string s)
        {
            return _encoder.DecodeData(s);
        }
    }
}