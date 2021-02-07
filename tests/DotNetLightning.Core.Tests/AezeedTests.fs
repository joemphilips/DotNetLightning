module AezeedTests

open System
open NBitcoin

open DotNetLightning.Crypto
open System.Text
open Expecto
open FsCheck
open PrimitiveGenerators

open ResultUtils
open ResultUtils.Portability

type TestVector = {
    Version: byte
    Time: DateTimeOffset
    Entropy: byte[]
    Salt: byte[]
    Password: byte[]
    ExpectedMnemonic: Mnemonic
    ExpectedBirthday: uint16
    MaybeExpectedCipherText: byte[] option
}

type CipherSeedGenerator =
    static member Bytes33() : Arbitrary<byte[]> =
        Gen.arrayOfLength 33 (Arb.generate<byte>) |> Arb.fromGen
        
    static member CipherSeed() : Arbitrary<CipherSeed> =
        cipherSeedGen |> Arb.fromGen
/// Based on: https://github.com/lightningnetwork/lnd/blob/b4bf4b2906c066ec0d0b8d7183c720b1d7d19220/aezeed/cipherseed_test.go
[<Tests>]
let tests =
    AEZConstants.V0_SCRYPT_N <- 16
    let propConfig = {
        FsCheckConfig.defaultConfig
            with
                arbitrary = [typeof<CipherSeedGenerator>;]
                maxTest = 15;
    }
    testList "aezeed unit test ported from lnd" [
        let testEntropy =
            [|
                0x81; 0xb6; 0x37; 0xd8;
                0x63; 0x59; 0xe6; 0x96
                0x0d; 0xe7; 0x95; 0xe4;
                0x1e; 0x0b; 0x4c; 0xfd;
            |] |> Array.map byte
            
        let testSalt = [|0x73uy; 0x61uy; 0x6cuy; 0x74uy; 0x31uy|]
        let version0TestVectors = [|
            {
                Version = 0uy
                Time = Network.Main.GetGenesis().Header.BlockTime
                Entropy = testEntropy
                Salt = testSalt
                Password = [||]
                ExpectedMnemonic =
                    [
                        "ability"; "liquid"; "travel"; "stem"; "barely"; "drastic";
                        "pact"; "cupboard"; "apple"; "thrive"; "morning"; "oak";
                        "feature"; "tissue"; "couch"; "old"; "math"; "inform";
                        "success"; "suggest"; "drink"; "motion"; "know"; "royal";
                    ] |> Seq.fold(fun word acc -> word + " " + acc) "" |> Mnemonic
                ExpectedBirthday = 0us
                MaybeExpectedCipherText =
                    [|0; 48; 75; 158; 106; 161; 40; 132; 167; 169; 174; 10; 188; 38; 63; 203; 229; 67; 197; 140; 60; 208; 137; 14; 115; 97; 108; 116; 49; 32; 158; 253; 229;|]
                    |> Array.map(byte)
                    |> Some
            }
            {
                Version = 0uy
                Time = DateTimeOffset.FromUnixTimeSeconds(1521799345L) // 02/23/2018 @ 10:02am (UTC)
                Entropy = testEntropy
                Salt = testSalt
                Password = Encoding.UTF8.GetBytes("!very_safe_55345_password*")
                ExpectedMnemonic =
                    [
                        "able"; "tree"; "stool"; "crush"; "transfer"; "cloud";
                        "cross"; "three"; "profit"; "outside"; "hen"; "citizen";
                        "plate"; "ride"; "require"; "leg"; "siren"; "drum";
                        "success"; "suggest"; "drink"; "require"; "fiscal"; "upgrade";
                    ] |> Seq.fold(fun word acc -> word + " " + acc) "" |> Mnemonic
                ExpectedBirthday = 3365us
                MaybeExpectedCipherText =
                    [|0; 92; 255; 89; 154; 142; 114; 87; 205; 7; 8; 171; 211; 177; 172; 148; 170; 99; 114; 237; 195; 250; 201; 104; 115; 97; 108; 116; 49; 110; 21; 231; 117|]
                    |> Array.map(byte)
                    |> Some
            }
        |]
        testCase "aed v0 test vectors" <| fun _ ->
            for v in version0TestVectors do
                let cipherSeed = {
                    CipherSeed.Create(v.Version, Some(v.Entropy), v.Time)
                    with Salt = testSalt // salt is usually generated randomly, so we will overwrite it here
                    }
                
                Expect.equal (cipherSeed.Birthday) v.ExpectedBirthday "unmatched birthday"
                v.MaybeExpectedCipherText
                |> Option.iter(fun expectedC ->
                    let actualC = cipherSeed.Encipher(Some v.Password)
                    Expect.sequenceEqual (actualC) expectedC ""
                    )
                let mnemonic = cipherSeed.ToMnemonic(v.Password)
                Expect.equal (mnemonic.ToString()) (v.ExpectedMnemonic.ToString()) "unmatched mnemonic"
                
        testCase "test empty passphrase derivation" <| fun _ ->
            let pass = [||]
            // We'll now create a new cipher seed with an internal version of zero
            // to simulate a wallet that just adopted the scheme.
            let cipherSeed = CipherSeed.Create(0uy, Some testEntropy, DateTimeOffset.Now)
            
            // Now that the seed has been created, we'll attempt to convert it to a
            // valid mnemonic.
            let mnemonic = cipherSeed.ToMnemonic(pass)
            
            // Next, we'll try to decrypt the mnemonic with the passphrase that we used.
            let cipherSeed2 = mnemonic.ToCipherSeed(pass) |> Result.deref
            Expect.equal cipherSeed cipherSeed2 ""
            
        testCase "it should generate entropy when user did not pass it" <| fun _ ->
            let pass = [||]
            let cipherSeed = CipherSeed.Create(0uy, None, DateTimeOffset.Now)
            let mnemonic = cipherSeed.ToMnemonic()
            let cipherSeed2 = mnemonic.ToCipherSeed(pass) |> Result.deref
            Expect.equal cipherSeed cipherSeed2 ""
            
        testCase "should reject invalid passphrase" <| fun _ ->
            let pass = Encoding.UTF8.GetBytes("test")
            let cipherSeed = CipherSeed.Create(0uy, Some testEntropy, DateTimeOffset.Now)
            let mnemonic = cipherSeed.ToMnemonic(pass)
            let wrongPass = Encoding.UTF8.GetBytes "kek"
            let e = mnemonic.ToCipherSeed(wrongPass)
            match e with
            | Error(AezeedError.InvalidPass _ ) -> ()
            | x -> failwithf "it should return invalid pass. it was %A" x
            
        testCase "test raw encipher/decipher" <| fun _ ->
            let pass = Encoding.UTF8.GetBytes("test")
            let cipherSeed = { CipherSeed.Create(0uy, Some testEntropy, BITCOIN_GENESIS_DATE) with Salt = testSalt}
            let cipherText = cipherSeed.Encipher(pass)
            let mnemonic =
                AezeedHelpers.cipherTextToMnemonic(cipherText, None)
                |> (Seq.fold(fun word acc -> word + " " + acc) "")
                |> Mnemonic
            let plainSeedBytes = mnemonic.Decipher(Some pass, None) |> Result.deref
            let newSeed = CipherSeed.FromBytes(plainSeedBytes)
            Expect.equal newSeed cipherSeed ""
            
        testCase "test invalid external version" <| fun _ ->
            let cipherSeed = CipherSeed.Create(0uy, testEntropy, DateTimeOffset.Now)
            let pass = Encoding.UTF8.GetBytes("newpasswhodis")
            let cipherText = cipherSeed.Encipher(pass)
            cipherText.[0] <- 44uy
            
            let wrongPass = Encoding.UTF8.GetBytes("kek")
            let r = AezeedHelpers.decipherCipherSeed(cipherText, wrongPass)
            match r with
            | Error(AezeedError.UnsupportedVersion _) -> ()
            | x -> failwithf "Unexpected %A" x
            
        testCase "test changing passphrase" <| fun _ ->
            let pass = Encoding.UTF8.GetBytes("test")
            let cipherSeed = CipherSeed.Create(0uy, testEntropy, DateTimeOffset.Now)
            let mnemonic = cipherSeed.ToMnemonic(pass)
            let newPass = Encoding.UTF8.GetBytes("strongerpassyeh!")
            let newMnemonic = mnemonic.ChangePass(pass, newPass) |> Result.deref
            let newCipherSeed = newMnemonic.ToCipherSeed(newPass) |> Result.deref
            Expect.equal cipherSeed newCipherSeed "unmatched cipherSeed"
            
        testCase "test change passphrase with wrong pass" <| fun _ ->
            let pass = Encoding.UTF8.GetBytes "test"
            let cipherSeed = CipherSeed.Create(0uy, testEntropy, DateTimeOffset.Now)
            let mnemonic = cipherSeed.ToMnemonic(pass)
            let wrongPass = Encoding.UTF8.GetBytes "kek"
            let newPass = Encoding.UTF8.GetBytes "strongerpassyeh!"
            let r = mnemonic.ChangePass(wrongPass, newPass)
            match r with
            | Error(AezeedError.InvalidPass _) -> ()
            | x -> failwithf "expected invalid pass error. Got: %A" x
            
        testPropertyWithConfig propConfig "test mnemonic encoding" <| fun (bytes33: byte[]) ->
            let mnemonic = AezeedHelpers.cipherTextToMnemonic(bytes33, None)
            let newCipher = AezeedHelpers.mnemonicToCipherText(mnemonic, None)
            Expect.equal newCipher bytes33 ""
            
        testPropertyWithConfig propConfig "test encipher-decipher" <| fun (cipherSeed: CipherSeed, pass: NonNull<byte[]>) ->
            let mnemonic = cipherSeed.ToMnemonic(pass.Get)
            let cipherSeed2 = mnemonic.ToCipherSeed(pass.Get) |> Result.deref
            Expect.equal (cipherSeed) (cipherSeed2) ""
            
        testPropertyWithConfig propConfig "test seed encode-decode" <| fun (cipherSeed: CipherSeed) ->
            let b = cipherSeed.ToBytes()
            let newSeed = CipherSeed.FromBytes b
            
            Expect.equal cipherSeed newSeed ""
            Expect.equal (cipherSeed.GetHashCode()) (newSeed.GetHashCode()) ""
            
        testCase "test Decipher incorrect mnemonic" <| fun _ ->
            let cipherSeed = CipherSeed.Create(0uy, testEntropy, DateTimeOffset.Now)
            let pass = Encoding.UTF8.GetBytes "test"
            let mnemonicW = cipherSeed.ToMnemonicWords(pass)
            
            let i1 = 9
            let i2 = 13
            let tmp = mnemonicW.[i1].Clone()
            mnemonicW.[i1] <- mnemonicW.[i2]
            mnemonicW.[i2] <- (tmp.ToString())
            
            let mnemonic = 
                mnemonicW |> Seq.fold(fun word acc -> word + " " + acc) "" |> Mnemonic
            let r = mnemonic.ToCipherSeed()
            match r with
            | Error(AezeedError.IncorrectMnemonic _ ) -> ()
            | x -> failwithf "Expecting IncorrectMnemonic. got %A" x
    ]
