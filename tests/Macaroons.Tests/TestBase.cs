using System;

namespace Macaroons.Tests
{
    public class TestBase
    {
        public const string Secret = "this is our super secret key; only we should know it";
        public const string Identifier = "we used our secret key";
        public const string Location = "http://mybank/";

        protected bool TimeVerifier(Packet cid)
        {
            var caveat = cid.ToString();
            if (!caveat.StartsWith("time < "))
                return false;

            var t = DateTime.Parse(caveat.Substring(7));
            return DateTime.Now < t;
        }
    }
}