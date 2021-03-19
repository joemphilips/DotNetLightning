module CommitmentToLocalExtensionTests

open System
open NBitcoin
open Expecto
open DotNetLightning.Utils
open DotNetLightning.Transactions
open DotNetLightning.Channel

open ResultUtils
open ResultUtils.Portability

[<Tests>]
let commitmentToLocalExtensionTests =
    testList "CommitmentToLocalExtensionTests" [
        testCase "can extract parameters" <| fun _ ->
            let rand = Random()
            let revocationPubKey =
                let key = new Key()
                let pubKey = key.PubKey
                RevocationPubKey pubKey
            let localDelayedPaymentPubKey =
                let key = new Key()
                let pubKey = key.PubKey
                DelayedPaymentPubKey pubKey
            let toSelfDelay =
                BlockHeightOffset16 (rand.Next(1, 1000) |> uint16)
            let scriptPubKey =
                Scripts.toLocalDelayed
                    revocationPubKey
                    toSelfDelay
                    localDelayedPaymentPubKey
            let parametersOpt = CommitmentToLocalParameters.TryExtractParameters scriptPubKey
            Expect.isSome parametersOpt "failed to extract parameters"
            let parameters = parametersOpt.Value
            Expect.equal parameters.ToSelfDelay toSelfDelay "to_self_delay mismatch"
            Expect.equal parameters.LocalDelayedPubKey localDelayedPaymentPubKey "local delayed pubkey mismatch"
    ]
