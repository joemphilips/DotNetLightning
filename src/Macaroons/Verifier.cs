using System;
using System.Collections.Generic;
using System.Security.Claims;

namespace Macaroons
{
    public class Verifier
    {
        protected List<Packet> Predicates { get; set; }
        
        protected List<Func<Packet, KeyValuePair<bool, string>>> VerifierCallbacks { get; set; }

        public Verifier()
        {
            Predicates = new List<Packet>();
            VerifierCallbacks = new List<Func<Packet, KeyValuePair<bool, string>>>();
        }

        public void SatisfyExact(string predicate)
        {
            if (predicate == null) throw new ArgumentNullException(nameof(predicate));
            SatisfyExact(new Packet(predicate));
        }
        
        public void SatisfyExact(Packet predicate)
        {
            if (predicate == null) throw new ArgumentNullException(nameof(predicate));
            Predicates.Add(predicate);
        }

        public void SatisfyGeneral(Func<Packet, bool> verifier)
        {
            if (verifier == null) throw new ArgumentNullException(nameof(verifier));
            VerifierCallbacks.Add(VerifierWrapper(verifier));
        }

        public void SatisfyGeneral(VerifierWithReasonDelegate verifier)
        {
            if (verifier == null) throw new ArgumentNullException(nameof(verifier));
            VerifierCallbacks.Add(VerifierWrapper(verifier));
        }

        protected static Func<Packet, KeyValuePair<bool, string>> VerifierWrapper(Func<Packet, bool> verifier)
        {
            return (cid) => new KeyValuePair<bool, string>(verifier(cid), null);
        }

        public delegate bool VerifierWithReasonDelegate(Packet cid, out string reason);
        
        protected static Func<Packet, KeyValuePair<bool, string>> VerifierWrapper(VerifierWithReasonDelegate verifier)
        {
            return (cid) =>
            {
                string reason;
                bool result = verifier(cid, out reason);
                return new KeyValuePair<bool, string>(result, reason);
            };
        }

        public VerificationResult Verify(Macaroon m, string key, List<Macaroon> ms = null)
        {
            if (m == null) throw new ArgumentNullException(nameof(m));
            if (key == null) throw new ArgumentNullException(nameof(key));
            return m.Verify(this, key, ms);
        }

        public VerificationResult Verify(Macaroon m, Packet key, List<Macaroon> ms = null)
        {
            if (m == null) throw new ArgumentNullException(nameof(m));
            if (key == null) throw new ArgumentNullException(nameof(key));
            return m.Verify(this, key, ms);
        }

        public bool IsValidFirstPartyCaveat(Packet cid) => IsValidFirstPartyCaveat(cid, out _);

        public bool IsValidFirstPartyCaveat(Packet cid, out string reason)
        {
            reason = null;
            foreach (Packet p in Predicates)
                if (p == cid)
                    return true;

            foreach (var verifier in VerifierCallbacks)
            {
                var r = verifier(cid);
                if (r.Key)
                    return true;
                if (r.Value != null)
                    reason = r.Value;
            }

            return false;
        }
    }
}