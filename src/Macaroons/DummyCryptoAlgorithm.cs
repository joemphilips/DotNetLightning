namespace Macaroons
{
    public class DummyCryptoAlgorithm : CryptoAlgorithm
    {
        public override byte[] Encrypt(byte[] key, byte[] plainText)
        {
            throw new System.NotImplementedException();
        }

        public override byte[] Decrypt(byte[] key, byte[] cipherText)
        {
            throw new System.NotImplementedException();
        }
    }
}