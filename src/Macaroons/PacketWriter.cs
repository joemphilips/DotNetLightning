using System;
using System.IO;

namespace Macaroons
{
    public class PacketWriter : PacketSerializerBase, IDisposable
    {
        protected Stream OutputStream { get; set; }

        protected BinaryWriter Writer { get; set; }

    
        public PacketWriter(Stream s)
        {

            OutputStream = s ?? throw new ArgumentNullException(nameof(s));
            Writer = new BinaryWriter(s);
        }


        public void WriteLocationPacket(Packet location)
        {
            WriteKVPacket(LocationID, location);
        }


        public void WriteIdentifierPacket(Packet identifier)
        {
            WriteKVPacket(IdentifierID, identifier);
        }


        public void WriteSignaturePacket(Packet signature)
        {
            WriteKVPacket(SignatureID, signature);
        }


        public void WriteCIdPacket(Packet cid)
        {
            WriteKVPacket(CIdID, cid);
        }


        public void WriteVIdPacket(Packet vid)
        {
            WriteKVPacket(VIdID, vid);
        }


        public void WriteClPacket(Packet cl)
        {
            WriteKVPacket(ClID, cl);
        }


        public void WriteKVPacket(byte[] key, Packet value)
        {
            int size = PACKET_PREFIX + 2 + key.Length + value.Length;
            WritePacketHeader(size);
            Writer.Write(key);
            Writer.Write(' ');
            Writer.Write(value.Data);
            Writer.Write('\n');
        }


        private void WritePacketHeader(int size)
        {
            Writer.Write(Hex[(size >> 12) & 15]);
            Writer.Write(Hex[(size >> 8) & 15]);
            Writer.Write(Hex[(size >> 4) & 15]);
            Writer.Write(Hex[(size) & 15]);
        }


        #region IDisposable Members

        public void Dispose()
        {
            Writer.Dispose();
        }

        #endregion
        
    }
}