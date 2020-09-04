module AezeedTests

open System
open NBitcoin

open DotNetLightning.Crypto
open System.Runtime.InteropServices
open Expecto


type TestVector = {
    Version: byte
    Time: DateTimeOffset
    Entropy: byte[]
    Salt: byte[]
    Password: byte[]
    ExpectedMnemonic: Mnemonic
    ExpectedBirthday: uint16
}


/// Based on: https://github.com/lightningnetwork/lnd/blob/b4bf4b2906c066ec0d0b8d7183c720b1d7d19220/aezeed/cipherseed_test.go
[<Tests>]
let tests =
    ftestList "aezeed unit test ported from lnd" [
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
        }
        {
            Version = 0uy
            Time = DateTimeOffset.FromUnixTimeSeconds(1521799345L) // 02/23/2018 @ 10:02am (UTC)
            Entropy = testEntropy
            Salt = testSalt
            Password = MemoryMarshal.AsBytes("!very_safe_55345_password*".AsSpan()).ToArray()
            ExpectedMnemonic =
                [
                    "able"; "tree"; "stool"; "crush"; "transfer"; "cloud";
                    "cross"; "three"; "profit"; "outside"; "hen"; "citizen";
                    "plate"; "ride"; "require"; "leg"; "siren"; "drum";
                    "success"; "suggest"; "drink"; "require"; "fiscal"; "upgrade";
                ] |> Seq.fold(fun word acc -> word + " " + acc) "" |> Mnemonic
            ExpectedBirthday = 3365us
        }
    |]
    testCase "aed v0 test vectors" <| fun _ ->
        for v in version0TestVectors do
            let cipherSeed = {
                CipherSeed.Create(v.Version, Some(v.Entropy), v.Time)
                with Salt = testSalt // salt is usually generated randomly, so we will overwrite it here
                }
            let mnemonic = cipherSeed.ToMnemonic(v.Password)
            Expect.equal (v.ExpectedMnemonic) mnemonic "unmatched mnemonic"
            Expect.equal v.ExpectedBirthday (uint16 cipherSeed.BirthDay.Day) "unmatched birthday"
]