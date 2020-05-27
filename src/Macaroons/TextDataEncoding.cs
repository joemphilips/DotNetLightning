using System;
using System.Text;

namespace Macaroons
{
    public class TextDataEncoding : DataEncoding
    {
        protected Encoding Enc { get; set; }

        public TextDataEncoding(Encoding enc)
        {
            Enc = enc ?? throw new ArgumentNullException(nameof(enc));
        }
        public override string GetString(byte[] d)
        {
            return Enc.GetString(d);
        }

        public override byte[] GetBytes(string s)
        {
            return Enc.GetBytes(s);
        }
    }
}