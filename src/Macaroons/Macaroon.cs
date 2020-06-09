using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text;

namespace Macaroons
{
    [DebuggerDisplay("{" + nameof(Inspect) + "()}")]
    public class Macaroon
    {
        
        public static CryptoAlgorithm Crypto = new DummyCryptoAlgorithm();

        public const int MACAROON_HASH_BYTES = 32;
        public const int MACAROON_MAX_STRLEN = 32768;

        public const int MACAROON_MAX_CAVEATS = 65536;
        public const int MACAROON_SUGGESTED_SECRET_LENGTH = 32;
        
        public Packet Location { get; protected set; }
        public Packet Identifier { get; protected set; }
        public Packet Signature { get; protected set; }

        public IList<Caveat> Caveats => CaveatsList.AsReadOnly();
        public IEnumerable<Caveat> ThirdPartyCaveats => Caveats.Where(c => c.IsTheirPartyCaveat);
        protected List<Caveat> CaveatsList { get; set; }
        
        protected Macaroon(){}
        
        public Macaroon(string location, string key, string identifier)
        {
            if (key == null) throw new ArgumentNullException(nameof(key));
            if (identifier == null) throw new ArgumentNullException(nameof(identifier));
            Initialize(location != null ? new Packet(location) : null, new Packet(key), new Packet(identifier));
        }

        public Macaroon(Packet location, Packet key, Packet identifier)
            => Initialize(location, key, identifier);


        public Macaroon(Macaroon src)
        {
            if (src.Location != null)
                Location = new Packet(src.Location);
            Identifier = new Packet(src.Identifier);
            Signature = new Packet(src.Signature);
            CaveatsList = new List<Caveat>(src.CaveatsList.Count);
            foreach (Caveat c in src.CaveatsList)
            {
                CaveatsList.Add(new Caveat(c));
            }
        }
        protected void Initialize(Packet location, Packet key, Packet identifier)
        {
            if (key == null) throw new ArgumentNullException(nameof(key));
            Packet derivedKey = GenerateDerivedKey(key);
            InitializeRaw(location, derivedKey, identifier);
        }

        protected void InitializeRaw(Packet location, Packet key, Packet identifier)
        {
            if (key == null) throw new ArgumentNullException(nameof(key));
            if (identifier == null) throw new ArgumentNullException(nameof(identifier));
            if (location == null) throw new ArgumentNullException(nameof(location));
            if (!(location.Length <= MACAROON_MAX_STRLEN)) throw new ArgumentOutOfRangeException(nameof(location));
            if (!(identifier.Length <= MACAROON_MAX_STRLEN)) throw new ArgumentOutOfRangeException(nameof(identifier));
            Location = location;
            Identifier = identifier;
            Signature = CalculateHash1(key, Identifier);
            CaveatsList = new List<Caveat>();
        }

        public Macaroon AddFirstPartyCaveat(string predicate)
        {
            if (predicate == null) throw new ArgumentNullException(nameof(predicate));
            AddFirstPartyCaveat(new Packet(predicate));
            return this;
        }

        public Macaroon AddFirstPartyCaveat(Packet predicate)
        {
            if (predicate == null) throw new ArgumentNullException(nameof(predicate));
            if (Signature == null) throw new InvalidOperationException($"{nameof(Signature)} must not be null");
            if (!(Signature.Length > PacketSerializerBase.PACKET_PREFIX)) throw new InvalidOperationException($"Signature was too short {Signature.Length}");

            Packet hash = CalculateHash1(Signature, predicate);
            CaveatsList.Add(new Caveat(predicate, null, null));
            Signature = hash;
            return this;
        }

        public Macaroon AddThirdPartyCaveat(string location, string key, string identifier)
        {
            if (key == null) throw new ArgumentNullException(nameof(key));
            if (identifier == null) throw new ArgumentNullException(nameof(identifier));
            AddThirdPartyCaveat(location != null ? new Packet(location) :  null, new Packet(key), new Packet(identifier) );
            return this;
        }

        public Macaroon AddThirdPartyCaveat(Packet location, Packet key, Packet identifier)
        {
            if (key == null) throw new ArgumentNullException(nameof(key));
            Packet derivedKey = GenerateDerivedKey(key);
            AddThirdPartyCaveatRaw(location, derivedKey, identifier);
            return this;
        }

        public void AddThirdPartyCaveatRaw(Packet location, Packet key, Packet identifier)
        {
            if (location == null) throw new ArgumentNullException(nameof(location));
            if (key == null) throw new ArgumentNullException(nameof(key));
            if (identifier == null) throw new ArgumentNullException(nameof(identifier));
            if (location.Length > MACAROON_MAX_STRLEN) throw new ArgumentException(nameof(location));
            if (CaveatsList.Count + 1 >= MACAROON_MAX_CAVEATS) throw new InvalidOperationException($"Exceed max caveats");

            var vid = Crypto.Encrypt(Signature.Data, key.Data);
            Packet newSig = CalculateHash2(Signature, new Packet(vid, DataEncoding.Base64UrlSafe), identifier);
            CaveatsList.Add(new Caveat(identifier, new Packet(vid, DataEncoding.Base64UrlSafe), location));;
            Signature = newSig;
        }

        /// <summary>
        /// Prepare this macaroon for request by binding it to the authorizing macaroon.
        /// </summary>
        /// <param name="d">Authorizing macaroon</param>
        /// <returns>A new bound discharge macaroon ready for sending along with the authorizing macaroon.</returns>
        public Macaroon PrepareForRequest(Macaroon d)
        {
            Packet boundSignature = Bind(Signature, d.Signature);
            Macaroon bound = new Macaroon(d) {Signature = boundSignature};
            return bound;
        }

        protected Packet Bind(Packet sig1, Packet sig2)
        {
            byte[] zeros= new byte[MACAROON_HASH_BYTES];
            Packet boundSignature = CalculateHash2(new Packet(zeros, DataEncoding.Hex),sig1, sig2);
            return boundSignature;
        }

        /// <summary>
        /// Verify this macaroon with respect to a set of valid predicates and a set of discharge macaroons.
        /// </summary>
        /// <param name="v"></param>
        /// <param name="key"></param>
        /// <param name="ms"></param>
        /// <returns></returns>
        /// <exception cref="ArgumentNullException"></exception>
        public VerificationResult Verify(Verifier v, string key, List<Macaroon> ms = null)
        {
            if (v == null) throw new ArgumentNullException(nameof(v));
            if (key == null) throw new ArgumentNullException(nameof(key));
            return Verify(v, new Packet(key), ms);
        }

        public VerificationResult Verify(Verifier v, Packet key, List<Macaroon> ms = null)
        {
            if (ms is null)
                ms = new List<Macaroon>();

            Packet derivedKey = GenerateDerivedKey(key);
            return VerifyRaw(v, derivedKey, ms);
        }

        protected VerificationResult VerifyRaw(Verifier v, Packet key, List<Macaroon> ms)
        {
            if (v == null) throw new ArgumentNullException(nameof(v));
            if (key == null) throw new ArgumentNullException(nameof(key));
            if (ms == null) throw new ArgumentNullException(nameof(ms));
            if (key.Length != MACAROON_SUGGESTED_SECRET_LENGTH)
                throw new ArgumentException($"key length must be {MACAROON_SUGGESTED_SECRET_LENGTH}, it was {key.Length}");

            return VerifyInner(this, v, key, ms, new Stack<Macaroon>());
        }

        /// <summary>
        /// Recursive verification of both 1st and 3rd party caveats in this macaroon.
        /// </summary>
        /// <param name="TM"></param>
        /// <param name="v"></param>
        /// <param name="key"></param>
        /// <param name="ms"></param>
        /// <param name="treePath"></param>
        /// <returns></returns>
        protected VerificationResult VerifyInner(Macaroon TM, Verifier v, Packet key, List<Macaroon> ms,
            Stack<Macaroon> treePath)
        {
            VerificationResult result = new VerificationResult();
            Packet csig = CalculateHash1(key, Identifier);
            foreach (var c in Caveats)
            {
                if (c.IsFirstPartyCaveat)
                {
                    string reason;
                    if (!VerifyInner1st(c, v, out reason))
                    {
                        if (reason == null)
                            result.AddFailure(string.Format($"Caveat '{c}' failed"));
                        else
                            result.AddFailure(reason);
                    }

                    csig = CalculateHash1(csig, c.CId);
                }
                else
                {
                    VerificationResult res = VerifyInner3rd(TM, c, v, ms, csig, treePath);
                    result.MergeFailures(res);
                    csig = CalculateHash2(csig, c.VId, c.CId);
                }
            }

            // If this a discharge macaroon? The bind signature to primary authorizing macaroon.
            if (treePath.Count > 0)
            {
                csig = Bind(TM.Signature, csig);
            }

            bool isValidSignature = (Signature == csig);
            if (!isValidSignature)
                result.AddFailure($"Signature mismatch for '{Identifier}'");

            return result;
        }

        protected bool VerifyInner1st(Caveat c, Verifier v, out string reason)
        {
            return v.IsValidFirstPartyCaveat(c.CId, out reason);
        }

        protected VerificationResult VerifyInner3rd(Macaroon TM, Caveat c, Verifier v, List<Macaroon> ms, Packet csig,
            Stack<Macaroon> treePath)
        {
            var discharge = ms.FirstOrDefault(m => m.Identifier == c.CId);
            if (discharge is null)
                return new VerificationResult(String.Format($"No discharge macaroon found for caveat '{c}'"));
            
            if (treePath.Contains(discharge))
                return new VerificationResult($"A circular discharge macaroon reference was found for caveat {c}");

            try
            {
                byte[] keyData = Crypto.Decrypt(csig.Data, c.VId.Data);
                Packet key = new Packet(keyData, DataEncoding.Hex);
                treePath.Push(discharge);

                var result = discharge.VerifyInner(TM, v, key, ms, treePath);
                treePath.Pop();
                return result;
            }
            catch (CryptographicException ex)
            {
                return new VerificationResult(ex.Message);
            }
        }
        
        # region IO

        public override string ToString()
        {
            return Location.ToString();
        }

        public string Inspect()
        {
            var sb = new StringBuilder();
            InspectCore(sb);
            return sb.ToString();
        }

        private void InspectCore(StringBuilder sb)
        {
            sb.AppendLine($"Location = {Location}");
            sb.AppendLine($"Identifier = {Identifier}");
            foreach (var c in CaveatsList)
            {
                sb.AppendLine(c.Inspect());
            }

            sb.AppendLine($"Signature = {Signature}");
        }

        /// <summary>
        /// Serialize this macaroon to a string. The string is safe for use in URLs as it is based on BASE64 URL Safe encoding.
        /// </summary>
        /// <returns></returns>
        public string Serialize()
        {
            return Utility.ToBase64UrlSafe(SerializeToBytes());
        }

        public byte[] SerializeToBytes()
        {
            using var s = new MemoryStream();
            Serialize(s);
            return s.ToArray();
        }

        public void Serialize(Stream s)
        {
            using var w = new PacketWriter(s);
            w.WriteLocationPacket(Location);
            w.WriteIdentifierPacket(Identifier);
            foreach (var c in Caveats)
            {
                w.WriteCIdPacket(c.CId);
                if (c.VId != null)
                    w.WriteVIdPacket(c.VId);
                if (c.Cl != null)
                    w.WriteClPacket(c.Cl);
            }
            w.WriteSignaturePacket(Signature);
        }
        /// <summary>
        /// Deserialize a string into a macaroon
        /// </summary>
        /// <param name="s">Input string.</param>
        /// <param name="options">Serialization options.</param>
        /// <returns>Deserialized macaroon.</returns>
        public static Macaroon Deserialize(string s, SerializationOptions options = null)
        {
            if (String.IsNullOrEmpty(s)) throw new ArgumentNullException(nameof(s)); 
            byte[] data = Utility.FromBase64UrlSafe(s); 
            return Deserialize(data);
        }


        /// <summary>
        /// Deserialize a byte array into a macaroon.
        /// </summary>
        /// <param name="data">Input bytes.</param>
        /// <param name="options">Serialization options.</param>
        /// <returns>Deserialized macaroon.</returns>
        public static Macaroon Deserialize(byte[] data, SerializationOptions options = null)
        {
            using var m = new MemoryStream(data);
            return Deserialize(m, options);
        }
        
        /// <summary>
        /// Deserialize macaroon from a stream.
        /// </summary>
        /// <param name="s">Input stream.</param>
        /// <param name="options">Serialization options.</param>
        /// <returns>Deserialized macaroon.</returns>
        public static Macaroon Deserialize(Stream s, SerializationOptions options = null)
        {
            if (options == null)
                options = SerializationOptions.Default; 
            using PacketReader r = new PacketReader(s, options);
            Packet location = r.ReadLocationPacket();
            Packet identifier = r.ReadIdentifierPacket();
            Packet signature = null;

            // Start by reading the first packet
            KeyValuePair<byte[], byte[]> packet = r.ReadKVPacket();

            List<Caveat> caveats = new List<Caveat>();
            while (true)
            {
                bool handled = false;
                if (Utility.ByteArrayEquals(PacketSerializerBase.CIdID, packet.Key))
                {
                    Packet cid = new Packet(packet.Value, options.CaveatIdentifierEncoding);
                    Packet vid = null;
                    Packet cl = null;
                    // Done with this package, now read next one
                    packet = r.ReadKVPacket();

                    if (Utility.ByteArrayEquals(PacketSerializerBase.VIdID, packet.Key))
                    {
                        vid = new Packet(packet.Value, DataEncoding.Base64UrlSafe);
                        // Done with this package, now read next one
                        packet = r.ReadKVPacket();
                    }
                    if (Utility.ByteArrayEquals(PacketSerializerBase.ClID, packet.Key))
                    {
                        cl = new Packet(packet.Value, DataEncoding.UTF8);

                        // Done with this package, now read next one
                        packet = r.ReadKVPacket();
                    }

                    Caveat c = new Caveat(cid, vid, cl);
                    caveats.Add(c);

                    handled = true;
                }

                if (Utility.ByteArrayEquals(PacketSerializerBase.SignatureID, packet.Key))
                {
                    signature = new Packet(packet.Value, DataEncoding.Hex);

                    // Done with this package - don't read more packages since signature should be the last one
                    break;
                }

                if (!handled)
                    throw new InvalidDataException("Unexpected data in input (did not find Cid or signature).");
            }

            if (signature == null)
                throw new InvalidDataException("Missing signature packet");

            return new Macaroon()
            {
                Location = location,
                Identifier = identifier,
                Signature = signature,
                CaveatsList = caveats
            };
        }

        # endregion
        
        #region Internal utility methods

        private static byte[] MacaroonsKeyGeneratorSecret = Encoding.ASCII.GetBytes("macaroons-key-generator");


        protected Packet GenerateDerivedKey(Packet key)
        {
            if (key == null) throw new ArgumentNullException(nameof(key));

            // Generate derived key as a hash of a hard coded "secret" value and the original key.
            byte[] genkey = new byte[MACAROON_HASH_BYTES];
            byte[] mkeygen = MacaroonsKeyGeneratorSecret;
            Buffer.BlockCopy(mkeygen, 0, genkey, 0, mkeygen.Length);

            return CalculateHash1(genkey, key.Data);
        }


        protected Packet CalculateHash1(Packet key, Packet p)
        {
            return CalculateHash1(key.Data, p.Data);
        }


        protected Packet CalculateHash1(byte[] key, byte[] data)
        {
            using (HMACSHA256 alg = new HMACSHA256(key))
            {
                byte[] hash = alg.ComputeHash(data);
                return new Packet(hash, DataEncoding.Hex);
            }
        }


        protected Packet CalculateHash2(Packet key, Packet data1, Packet data2)
        {
            using (HMACSHA256 alg = new HMACSHA256(key.Data))
            {
                byte[] tmp1 = alg.ComputeHash(data1.Data);
                byte[] tmp2 = alg.ComputeHash(data2.Data);
                byte[] tmp = new byte[MACAROON_HASH_BYTES * 2];
                Buffer.BlockCopy(tmp1, 0, tmp, 0, MACAROON_HASH_BYTES);
                Buffer.BlockCopy(tmp2, 0, tmp, MACAROON_HASH_BYTES, MACAROON_HASH_BYTES);

                byte[] hash = alg.ComputeHash(tmp);

                return new Packet(hash, DataEncoding.Hex);
            }
        }

        #endregion
    }
}