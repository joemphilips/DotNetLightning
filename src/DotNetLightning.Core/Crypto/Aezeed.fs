namespace DotNetLightning.Crypto

open System
open System.Diagnostics
open System.IO
open System.Runtime.CompilerServices

open NBitcoin

open System.Security.Cryptography
open System.Text
open DotNetLightning.Core.Utils.Extensions
open DotNetLightning.Serialization
open DotNetLightning.Utils

open ResultUtils
open ResultUtils.Portability

type O = OptionalArgumentAttribute
type D = System.Runtime.InteropServices.DefaultParameterValueAttribute

[<AutoOpen>]
module internal AEZConstants =
    /// CipherSeedVersion is the current version of the aezeed scheme as defined in this package.
    /// This version indicates the following parameters for the deciphered cipher seed: a 1 byte version, 2 bytes
    /// for the Bitcoin Days Genesis timestamp, and 16 bytes for entropy. It also governs how the cipher seed
    [<Literal>]
    let CIPHER_SEED_VERSION = 0uy
    
    [<Literal>]
    let DECIPHERED_CIPHER_SEED_SIZE = 19
    
    [<Literal>]
    let ENCIPHERED_CIPHER_SEED_SIZE = 33
    
    [<Literal>]
    let CIPHER_TEXT_EXPANSION = 4
    
    [<Literal>]
    let ENTROPY_SIZE = 16
    
    [<Literal>]
    let NUM_MNEMONIC_WORDS = 24

    [<Literal>]
    let SALT_SIZE = 5
    
    [<Literal>]
    let AD_SIZE = 6
    
    [<Literal>]
    let CHECKSUM_SIZE = 4
    
    [<Literal>]
    let KEY_LEN = 32
    
    [<Literal>]
    let BITS_PER_WORD = 11
    
    let SALT_OFFSET = ENCIPHERED_CIPHER_SEED_SIZE - CHECKSUM_SIZE - SALT_SIZE
    
    let CHECKSUM_OFFSET = ENCIPHERED_CIPHER_SEED_SIZE - CHECKSUM_SIZE
    
    let ENCIPHERED_SEED_SIZE = DECIPHERED_CIPHER_SEED_SIZE + CIPHER_TEXT_EXPANSION
    
    /// We have to change this value in the test, thus we make it mutable.
    let mutable V0_SCRYPT_N = 32768
    
    [<Literal>]
    let V0_SCRYPT_R = 8
    
    [<Literal>]
    let V0_SCRYPT_P = 1
    
    let DEFAULT_PASPHRASE =
        UTF8Encoding.UTF8.GetBytes("aezeed")
    
    let BITCOIN_GENESIS_DATE = NBitcoin.Network.Main.GetGenesis().Header.BlockTime
    
    let convertBits(data, fromBits, toBits, pad: bool) =
        InternalBech32Encoder.Instance.ConvertBits(data, fromBits, toBits, pad)
        
    let getScryptKey(pass: byte[], salt: byte[]) =
        NBitcoin.Crypto.SCrypt.ComputeDerivedKey(pass, salt, V0_SCRYPT_N, V0_SCRYPT_R, V0_SCRYPT_P, Nullable(), KEY_LEN)
type AezeedError =
    | UnsupportedVersion of uint8
    | InvalidPass of CryptographicException
    /// Returns when the checksum does not match.
    | IncorrectMnemonic of expectedCheckSum: uint32 * actualChecksum: uint32
    
[<AutoOpen>]
module internal AezeedHelpers = 
    let private extractAD(encipheredSeed: byte[]) =
        let ad = Array.zeroCreate AD_SIZE
        ad.[0] <- encipheredSeed.[0]
        let a = encipheredSeed.[SALT_OFFSET..CHECKSUM_OFFSET - 1]
        Array.blit a 0 ad 1 (AD_SIZE - 1)
        ad
        
    let decipherCipherSeed(cipherSeedBytes: byte[], password: byte[]) =
        Debug.Assert(cipherSeedBytes.Length = ENCIPHERED_CIPHER_SEED_SIZE)
        if cipherSeedBytes.[0] <> CIPHER_SEED_VERSION then Error(AezeedError.UnsupportedVersion cipherSeedBytes.[0]) else
        let salt = cipherSeedBytes.[SALT_OFFSET..SALT_OFFSET + SALT_SIZE - 1]
        let cipherSeed = cipherSeedBytes.[1..SALT_OFFSET - 1]
        let checkSum = cipherSeedBytes.[CHECKSUM_OFFSET..] |> UInt32.FromBytesBigEndian
        let freshChecksum =
            AEZ.Crc32.Crc32CAlgorithm.Compute(cipherSeedBytes.[..CHECKSUM_OFFSET - 1])
        if (freshChecksum <> checkSum) then Error(AezeedError.IncorrectMnemonic(freshChecksum, checkSum)) else
        let key = getScryptKey(password, salt)
        let ad = extractAD(cipherSeedBytes)
        try 
            let r = (AEZ.AEZ.Decrypt(ReadOnlySpan(key), ReadOnlySpan.Empty ,[|ad|], CIPHER_TEXT_EXPANSION, ReadOnlySpan(cipherSeed), Span.Empty))
            Ok(r.ToArray())
        with
            | :? CryptographicException as e ->
                Error(AezeedError.InvalidPass e)
        
    let mnemonicToCipherText(mnemonic: string[], lang: Wordlist option) =
        let lang = Option.defaultValue NBitcoin.Wordlist.English lang
        let indices = lang.ToIndices mnemonic
        let cipherBits = BitWriter()
        for i in indices do
            cipherBits.Write(uint32 i, BITS_PER_WORD)
        cipherBits.ToBytes()
        
    let internal cipherTextToMnemonic(cipherText: byte[], lang: Wordlist option) =
        Debug.Assert(cipherText.Length = ENCIPHERED_CIPHER_SEED_SIZE)
        let lang = Option.defaultValue NBitcoin.Wordlist.English lang
        let writer = BitWriter()
        writer.Write(cipherText)
        let wordInt = writer.ToIntegers()
        wordInt |> lang.GetWords
        
/// CipherSeed is a fully decoded instance of the aezeed scheme. At a high level, the encoded cipherseed is the
/// enciphering off
/// 1. 1 byte version byte
/// 2. 2 bytes timestamp
/// 3. 
[<Struct;CustomEquality;NoComparison>]
type CipherSeed = {
    InternalVersion: uint8
    Birthday: uint16
    Entropy: byte[]
    Salt: byte[]
}
    with
    override this.GetHashCode() =
        int (AEZ.Crc32.Crc32CAlgorithm.Compute(this.ToBytes()))
    override this.Equals(o: obj) =
        match o with
        | :? CipherSeed as other -> (this :> IEquatable<CipherSeed>).Equals(other)
        | _ -> false
    interface IEquatable<CipherSeed> with
        member this.Equals(o: CipherSeed) =
            this.InternalVersion = o.InternalVersion &&
            this.Birthday = o.Birthday &&
            this.Entropy.ToHexString() = o.Entropy.ToHexString()
    static member Create() =
        CipherSeed.Create(CIPHER_SEED_VERSION)
    static member Create(internalVersion: uint8) =
        CipherSeed.Create(internalVersion, DateTimeOffset.Now)
    static member Create(internalVersion: uint8, now: DateTimeOffset) =
        CipherSeed.Create(internalVersion, None, now)
    static member Create(internalVersion: uint8, entropy: byte[], now: DateTimeOffset) =
        CipherSeed.Create(internalVersion, Some entropy, now)
    static member Create(internalVersion: uint8, entropy: byte[] option, now: DateTimeOffset) =
        let entropy = Option.defaultWith (fun _ -> RandomUtils.GetBytes(ENTROPY_SIZE)) entropy
        if entropy.Length < ENTROPY_SIZE then raise <| ArgumentException(sprintf "entropy size must be at least %d! it was %d" ENTROPY_SIZE entropy.Length) else
        if (now < BITCOIN_GENESIS_DATE) then raise <| ArgumentOutOfRangeException(sprintf "You shall not create CipherSeed older than BitcoinGenesis!") else
        let seed = Array.zeroCreate ENTROPY_SIZE
        Array.blit entropy 0 seed 0 ENTROPY_SIZE
        
        let birthDate = uint16((now - BITCOIN_GENESIS_DATE).Days)
        {
            CipherSeed.InternalVersion = internalVersion
            Birthday = birthDate
            Entropy = seed
            Salt = RandomUtils.GetBytes SALT_SIZE
        }
        
    member private this.Serialize(ls: LightningWriterStream) =
        ls.Write(this.InternalVersion)
        ls.Write(this.Birthday, false)
        ls.Write(this.Entropy)
    member internal this.ToBytes() =
        use ms = new MemoryStream()
        use ls = new LightningWriterStream(ms)
        this.Serialize ls
        ms.ToArray()
        
    static member private Deserialize(ls: LightningReaderStream) =
        {
            InternalVersion = ls.ReadByte()
            Birthday = ls.ReadUInt16(false)
            Entropy = ls.ReadBytes(ENTROPY_SIZE)
            Salt = Array.zeroCreate SALT_SIZE
        }
        
    member private this.GetADBytes() =
        let res = Array.zeroCreate (SALT_SIZE + 1)
        res.[0] <- byte CIPHER_SEED_VERSION
        Array.blit this.Salt 0 res 1 this.Salt.Length
        res
        
    static member internal FromBytes(b: byte[]) =
        use ms = new MemoryStream(b)
        use ls = new LightningReaderStream(ms)
        CipherSeed.Deserialize ls

    member this.Encipher() = this.Encipher(None)
        
    member this.Encipher([<O;D(null)>]password: byte[]): byte[] =
        let pass = if isNull password then None else Some(password)
        this.Encipher(pass)
    /// Takes a fully populated cipherseed instance, and enciphers the
    /// encoded seed, then appends a randomly generated seed used to stretch th
    /// passphrase out into an appropriate key, then computes a checksum over the
    /// preceding. Returns 33 bytes enciphered cipherseed
    member this.Encipher(password: byte[] option): byte[] =
        let passphrase =
            match password with
            | Some p when p.Length = 0 -> DEFAULT_PASPHRASE
            | Some p -> p
            | None -> DEFAULT_PASPHRASE
        let key = getScryptKey(passphrase, this.Salt)
        
        let seedBytes = this.ToBytes()
        let ad = this.GetADBytes()
        let cipherText = (AEZ.AEZ.Encrypt(ReadOnlySpan(key), ReadOnlySpan.Empty, [| ad |], CIPHER_TEXT_EXPANSION, ReadOnlySpan(seedBytes), Span.Empty)).ToArray()
        
        let result = Array.zeroCreate ENCIPHERED_CIPHER_SEED_SIZE
        result.[0] <- byte CIPHER_SEED_VERSION
        Array.blit cipherText 0 result 1 (SALT_OFFSET - 1)
        Array.blit this.Salt 0 result SALT_OFFSET SALT_SIZE
        
        let checkSum =
            AEZ.Crc32.Crc32CAlgorithm.Compute(result.[0..CHECKSUM_OFFSET - 1])
            |> fun ch -> ch.GetBytesBigEndian()
        Array.blit checkSum 0 result CHECKSUM_OFFSET CHECKSUM_SIZE
        result
        
    member this.ToMnemonicWords(password: byte[] option, lang: Wordlist option) =
        let cipherText = this.Encipher(password)
        cipherTextToMnemonic(cipherText, lang)
        
    member this.ToMnemonicWords([<O; D(null)>] password, [<O; D(null)>] lang) =
        let pass = if isNull password then None else Some(password)
        let lang = if isNull lang then None else Some(lang)
        this.ToMnemonicWords(pass, lang)
        
    member this.ToMnemonic(password: byte[] option, lang: Wordlist option) =
        this.ToMnemonicWords(password, lang) |> Seq.fold(fun x acc -> x + " " + acc) "" |> Mnemonic
        
    member this.ToMnemonic([<O;D(null)>]password, [<O;D(null)>]lang) =
        let pass = if isNull password then None else Some(password)
        let lang = if isNull lang then None else Some(lang)
        this.ToMnemonic(pass, lang)
        
    member this.GetBirthdayTime() =
        let offset = TimeSpan.FromDays(float this.Birthday)
        BITCOIN_GENESIS_DATE + offset
        
[<Extension;Sealed;AbstractClass>]
type MnemonicExtensions =

    /// Attempts to map the mnemonic to the original cipher text byte slice.
    /// Then It will attempt to decrypt the ciphertext using aez with the passed passphrase,
    /// using the last 5 bytes of the ciphertext as a salt for the KDF.
    static member ToCipherSeed(this: Mnemonic, [<O;D(null)>]password: string) =
        password
        |> fun x -> if isNull x then [||] else Encoding.UTF8.GetBytes(password)
        |> fun b -> this.ToCipherSeed(b, null)
        
    /// Attempts to map the mnemonic to the original cipher text byte slice.
    /// Then It will attempt to decrypt the ciphertext using aez with the passed passphrase,
    /// using the last 5 bytes of the ciphertext as a salt for the KDF.
    static member ToCipherSeed(this: Mnemonic, [<O;D(null)>]password: byte[]) =
        this.ToCipherSeed(password, null)
    [<Extension>]
    /// Attempts to map the mnemonic to the original cipher text byte slice.
    /// Then It will attempt to decrypt the ciphertext using aez with the passed passphrase,
    /// using the last 5 bytes of the ciphertext as a salt for the KDF.
    static member ToCipherSeed(this: Mnemonic, [<O;D(null)>]password: byte[], [<O;D(null)>]lang: Wordlist) =
        let pass = if isNull password then None else Some(password)
        let lang = if isNull lang then None else Some(lang)
        MnemonicExtensions.ToCipherSeed(this, pass, lang)
        
    [<Extension>]
    /// Attempts to map the mnemonic to the original cipher text byte slice.
    /// Then It will attempt to decrypt the ciphertext using aez with the passed passphrase,
    /// using the last 5 bytes of the ciphertext as a salt for the KDF.
    static member ToCipherSeed(this: Mnemonic, password: byte[] option, lang: Wordlist option) =
        this.Decipher(password, lang)
        |> Result.map CipherSeed.FromBytes
        
    [<Extension>]
        
    static member internal Decipher(this: Mnemonic, password: byte[] option, lang: Wordlist option) =
        let pass =
            match password with
            | Some p when p.Length = 0 -> DEFAULT_PASPHRASE
            | Some p -> p
            | None -> DEFAULT_PASPHRASE
            
        let cipherText = mnemonicToCipherText (this.Words, lang)
        decipherCipherSeed(cipherText, pass)
        
    [<Extension>]
    static member ChangePass(this: Mnemonic, oldPass: byte[], newPass: byte[], [<O;D(null)>]lang: Wordlist) =
        this.ToCipherSeed(oldPass, lang)
        |> Result.map(fun x -> x.ToMnemonic newPass)
        
