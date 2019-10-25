module EncryptDecryptTest

open System
open DotNetLightning.Crypto
open DotNetLightning.Utils
open Expecto

let hex = NBitcoin.DataEncoders.HexEncoder()

let encryptTest(cryptoImpl: ICryptoImpl) =
    let key = NBitcoin.uint256(ReadOnlySpan (hex.DecodeData "e68f69b7f096d7917245f5e5cf8ae1595febe4d4644333c99f9c4a1282031c9f"))
    let nonce = (hex.DecodeData "000000000000000000000000", 0) |> BitConverter.ToUInt64
    let ad = ReadOnlySpan(hex.DecodeData "9e0e7de8bb75554f21db034633de04be41a2b8a18da7a319a03c803bf02b396c")
    let plaintext = ReadOnlySpan(Array.zeroCreate 0)
    Expect.equal (cryptoImpl.encryptWithAD(nonce, key, ad, plaintext)) (hex.DecodeData "0df6086551151f58b8afe6c195782c6a") "empty plaintext encrypts correctly"

let decryptTest(cryptoImpl: ICryptoImpl) =
    let key = NBitcoin.uint256(ReadOnlySpan(hex.DecodeData "e68f69b7f096d7917245f5e5cf8ae1595febe4d4644333c99f9c4a1282031c9f"))
    let nonce = uint64 0
    let ad = hex.DecodeData "9e0e7de8bb75554f21db034633de04be41a2b8a18da7a319a03c803bf02b396c"
    let ciphertext = ReadOnlySpan(hex.DecodeData "0df6086551151f58b8afe6c195782c6a")
    Expect.equal (cryptoImpl.decryptWithAD(nonce, key, ad, ciphertext)) (RResult.Good Array.empty) "decryption returns empty plaintext"

[<Tests>]
let tests =
    let cryptoImpl = CryptoUtils.impl
    let encrypt = testCase "encrypt" <| fun _ -> encryptTest cryptoImpl
    let decrypt = testCase "decrypt" <| fun _ -> decryptTest cryptoImpl
    testList "BOLT-08 tests" [ encrypt; decrypt ]
