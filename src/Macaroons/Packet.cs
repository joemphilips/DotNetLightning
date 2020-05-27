using System;

namespace Macaroons
{
    /// <summary>
    /// Represents a blob of bytes and associated encoding for textual representation of it.
    /// </summary>
    public class Packet
    {
        public byte[] Data { get; protected set; }
        public int Length => Data.Length;
        
        public DataEncoding Encoding { get; protected set; }

        public byte this[int i]
        {
            get => Data[i];
            set => Data[i] = value;
        }
        
        public Packet(byte[] data, DataEncoding enc)
        {
            Data = data ?? throw new ArgumentNullException(nameof(data));
            Encoding = enc ?? throw new ArgumentNullException(nameof(enc));
        }

        public Packet(string s, DataEncoding enc = null)
        {
            if (enc is null)
                enc = DataEncoding.UTF8;

            if (s != null)
                Data = enc.GetBytes(s);

            Encoding = enc;
        }

        public Packet(Packet src)
        {
            if (src == null) throw new ArgumentNullException(nameof(src));
            if (src.Data != null)
                Data = Utility.CopyByteArray(src.Data);
            Encoding = src.Encoding;
        }

        public override string ToString()
        {
            return Data is null ? null : Encoding.GetString(Data);
        }

        public override bool Equals(object obj)
        {
            Packet p = obj as Packet;
            if (p is null)
                return false;

            return Utility.ByteArrayEquals(Data, p.Data);
        }
        
        public override int GetHashCode()
        {
            unchecked
            {
                int hash = 17;

                // Cycle through each element in the array.
                foreach (byte b in Data)
                {
                    // Update the hash.
                    hash = hash * 23 + b.GetHashCode();
                }

                return hash;
            }
        }

        public static bool operator ==(Packet p1, Packet p2)
        {
            if ((object)p1 == null && (object)p2 == null)
                return true;
            if ((object)p1 == null || (object)p2 == null)
                return false;
            return p1.Equals(p2);
        }


        public static bool operator !=(Packet p1, Packet p2)
        {
            return !(p1 == p2);
        }
    }
}