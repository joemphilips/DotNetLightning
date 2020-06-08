using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Macaroons
{
    public class PacketReader : PacketSerializerBase, IDisposable
    {
        protected Stream InputStream { get; set; }

        protected BinaryReader Reader { get; set; }

        protected SerializationOptions Options { get; set; }


        public PacketReader(Stream s, SerializationOptions options)
        {
            InputStream = s;
            Reader = new BinaryReader(s);
            Options = options;
        }
        public void Dispose()
        {
            Reader.Dispose();
        }
        
        public Packet ReadLocationPacket()
        {
            return ReadVerifiedKVPackage(LocationID, DataEncoding.UTF8);
        }


        public Packet ReadIdentifierPacket()
        {
            return ReadVerifiedKVPackage(IdentifierID, Options.MacaroonIdentifierEncoding);
        }


        public Packet ReadSignaturePacket()
        {
            return ReadVerifiedKVPackage(SignatureID, DataEncoding.Hex);
        }


        public Packet ReadVerifiedKVPackage(byte[] expectedKey, DataEncoding enc)
        {
            byte[] data = ReadVerifiedKVBytes(expectedKey);
            return new Packet(data, enc);
        }


        public byte[] ReadVerifiedKVBytes(byte[] expectedKey)
        {
            KeyValuePair<byte[], byte[]> packet = ReadKVPacket();
            if (!Utility.ByteArrayEquals(expectedKey, packet.Key))
                throw new InvalidDataException(string.Format("Missing '{0}' packet (got '{1}')", Encoding.UTF8.GetString(expectedKey), Encoding.UTF8.GetString(packet.Key)));
            return packet.Value;
        }
        public KeyValuePair<byte[], byte[]> ReadKVPacket()
        {
            int size = ReadPacketHeader();
            byte[] data = Reader.ReadBytes(size);
            int delimiterPos = -1;
            while (delimiterPos < size)
                if (data[++delimiterPos] == (byte)' ')
                    break;

            if (delimiterPos == size)
                throw new InvalidDataException("No delimiter found in package");

            if (data[size - 1] != (byte)'\n')
                throw new InvalidDataException("No newline found after package data");

            byte[] key = new byte[delimiterPos];
            Buffer.BlockCopy(data, 0, key, 0, delimiterPos);

            // Skip newline and delimiter => -2
            byte[] value = new byte[size - delimiterPos - 2];
            Buffer.BlockCopy(data, delimiterPos + 1, value, 0, size - delimiterPos - 2);

            return new KeyValuePair<byte[], byte[]>(key, value);
        }


        internal int ReadPacketHeader()
        {
            int size = 0;
            for (int i = 0; i < PACKET_PREFIX; ++i)
            {
                char c = Reader.ReadChar();
                int s = Hex.IndexOf(c);
                if (s < 0 || s > 15)
                    throw new InvalidDataException(string.Format("Unexpected character '{0}' in package header", c));
                size = (size << 4) | s;
            }
            return size - PACKET_PREFIX;
        }
    }
}