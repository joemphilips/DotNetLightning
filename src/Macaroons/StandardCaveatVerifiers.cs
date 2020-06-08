using System;
using System.Globalization;

namespace Macaroons
{
    public static class StandardCaveatVerifiers
    {
        public static string CreateExpiresCaveat(DateTime expiry)
            => string.Format("expires: {0:yyyy-MM-dd'T'HH:mm:ss'Z'}", expiry);

        public static bool ExpiresVerifier(Packet cid, out string reason)
        {
            reason = null;

            string caveat = cid.ToString();
            if (!caveat.StartsWith("expires:"))
                return false;

            string ts = caveat.Substring(8).Trim();

            DateTime t;
            if (DateTime.TryParseExact(ts, "yyyy-MM-dd'T'HH:mm:ss'Z'", CultureInfo.InvariantCulture,
                DateTimeStyles.AssumeUniversal, out t))
            {
                if (DateTime.Now < t)
                    return true;

                reason = string.Format("Timestamp '{0}' has expired", ts);
                return false;
            }
            else
            {
                reason = string.Format("Invalid timestamp in '{0}'", caveat);
                return false;
            }
        }
    }
}