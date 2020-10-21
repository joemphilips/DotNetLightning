namespace Macaroons
{
    /// <summary>
    /// Used for encrypt/decrypt operation for third party caveat.
    /// </summary>
    public abstract class CryptoAlgorithm
    {
        public abstract byte[] Encrypt(byte[] key, byte[] plainText);
        public abstract byte[] Decrypt(byte[] key, byte[] nonceAndMacAndCipherText);
    }
}