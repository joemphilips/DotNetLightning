module GeneratorsTests

open Expecto
open NBitcoin
open DotNetLightning.Crypto.Generators

let newSecp256k1 = DotNetLightning.Crypto.CryptoUtils.impl.newSecp256k1

let hex = DataEncoders.HexEncoder()
let baseSecret =
    "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
    |> hex.DecodeData
    |> Key

let perCommitmentSecret =
    "1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a09080706050403020100"
    |> hex.DecodeData
    |> Key

let basePoint =
    "036d6caac248af96f6afa7f904f550253a0f3ef3f5aa2fe6838a95b216691468e2"
    |> PubKey

let perCommitmentPoint =
    "025f7117a78150fe2ef97db7cfc83bd57b2e2c0d0dd25eaf467a4a1c2a45ce1486"
    |> PubKey

[<Tests>]
let tests =
    testList "key generator tests" [
        testCase "derivation key from basepoint and per-commitment-point" <| fun _ ->
            use ctx = newSecp256k1()
            let localkey = derivePubKey ctx (basePoint) (perCommitmentPoint) 
            let expected =
                "0235f2dbfaa89b57ec7b055afe29849ef7ddfeb1cefdb9ebdc43f5494984db29e5"
                |> PubKey
            Expect.equal (localkey.ToBytes()) (expected.ToBytes()) ""

        testCase "derivation of secret key from basepoint secret and per-commitment-secret" <| fun _ ->
            use ctx = newSecp256k1()
            let localPrivkey = derivePrivKey ctx (baseSecret) (perCommitmentPoint)
            let expected =
                "cbced912d3b21bf196a766651e436aff192362621ce317704ea2f75d87e7be0f"
                |> hex.DecodeData
            Expect.equal (localPrivkey.ToBytes()) (expected) ""

        testCase "derivation of revocation key from basepoint and per_commitment_point" <| fun _ ->
            use ctx = newSecp256k1()
            let revocationKey = revocationPubKey ctx (basePoint) (perCommitmentPoint)
            let expected =
                "02916e326636d19c33f13e8c0c3a03dd157f332f3e99c317c141dd865eb01f8ff0"
                |> hex.DecodeData
            Expect.equal (revocationKey.ToBytes()) expected ""

        testCase "derivation of revocation secret from basepoint-secret and per-commitment-secret" <| fun _ ->
            use ctx = newSecp256k1()
            let actual = revocationPrivKey ctx (baseSecret) (perCommitmentSecret)
            let expected =
                "d09ffff62ddb2297ab000cc85bcb4283fdeb6aa052affbc9dddcf33b61078110"
                |> hex.DecodeData
            Expect.equal (actual.ToBytes()) expected ""
    ]