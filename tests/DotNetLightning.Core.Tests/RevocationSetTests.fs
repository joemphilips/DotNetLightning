module RevocationSetTests

open NBitcoin
open Expecto
open ResultUtils
open DotNetLightning.Utils
open DotNetLightning.Crypto

[<Tests>]
let tests =
    let insert (commitmentNumber: uint64)
               (key: string)
               (revocationSet: RevocationSet)
                   : Result<RevocationSet, InsertRevocationKeyError> =
        let hex = NBitcoin.DataEncoders.HexEncoder()
        let commitmentNumber = CommitmentNumber <| UInt48.FromUInt64 commitmentNumber
        let key = key |> hex.DecodeData |> Key |> RevocationKey
        revocationSet.InsertRevocationKey commitmentNumber key

    let insertUnwrap (commitmentNumber: uint64)
                     (key: string)
                     (revocationSet: RevocationSet)
                         : RevocationSet =
        Result.deref <| insert commitmentNumber key revocationSet

    testList "Revocation tests" [
        testCase "insert secret correct sequence" <| fun _ ->
            let _revocationSet: RevocationSet =
                RevocationSet()
                |> insertUnwrap
                    281474976710655UL
                    "7cc854b54e3e0dcdb010d7a3fee464a9687be6e8db3be6854c475621e007a5dc"
                |> insertUnwrap
                    281474976710654UL
                    "c7518c8ae4660ed02894df8976fa1a3659c1a8b4b5bec0c4b872abeba4cb8964"
                |> insertUnwrap
                    281474976710653UL
                    "2273e227a5b7449b6e70f1fb4652864038b1cbf9cd7c043a7d6456b7fc275ad8"
                |> insertUnwrap
                    281474976710652UL
                    "27cddaa5624534cb6cb9d7da077cf2b22ab21e9b506fd4998a51d54502e99116"
                |> insertUnwrap
                    281474976710651UL
                    "c65716add7aa98ba7acb236352d665cab17345fe45b55fb879ff80e6bd0c41dd"
                |> insertUnwrap
                    281474976710650UL
                    "969660042a28f32d9be17344e09374b379962d03db1574df5a8a5a47e19ce3f2"
                |> insertUnwrap
                    281474976710649UL
                    "a5a64476122ca0925fb344bdc1854c1c0a59fc614298e50a33e331980a220f32"
                |> insertUnwrap
                    281474976710648UL
                    "05cde6323d949933f7f7b78776bcc1ea6d9b31447732e3802e1f7ac44b650e17"
            ()

        testCase "insert secret 1 incorrect" <| fun _ ->
            let res =
                RevocationSet()
                |> insertUnwrap
                    281474976710655UL
                    "02a40c85b6f28da08dfdbe0926c53fab2de6d28c10301f8f7c4073d5e42e3148"
                |> insert
                    281474976710654UL
                    "c7518c8ae4660ed02894df8976fa1a3659c1a8b4b5bec0c4b872abeba4cb8964"
            match res with
            | Error(KeyMismatch(previous, current))
                when previous.Index.UInt64 = 281474976710655UL && current.Index.UInt64 = 281474976710654UL
                -> ()
            | _ -> failwith <| sprintf "unexpected result: %A" res

        testCase "insert secret 2 incorrect" <| fun _ ->
            let res =
                RevocationSet()
                |> insertUnwrap
                    281474976710655UL
                    "02a40c85b6f28da08dfdbe0926c53fab2de6d28c10301f8f7c4073d5e42e3148"
                |> insertUnwrap
                    281474976710654UL
                    "dddc3a8d14fddf2b68fa8c7fbad2748274937479dd0f8930d5ebb4ab6bd866a3"
                |> insertUnwrap
                    281474976710653UL
                    "2273e227a5b7449b6e70f1fb4652864038b1cbf9cd7c043a7d6456b7fc275ad8"
                |> insert
                    281474976710652UL
                    "27cddaa5624534cb6cb9d7da077cf2b22ab21e9b506fd4998a51d54502e99116"
            match res with
            | Error(KeyMismatch(previous, current))
                when previous.Index.UInt64 = 281474976710654UL && current.Index.UInt64 = 281474976710652UL
                -> ()
            | _ -> failwith <| sprintf "unexpected result: %A" res

        testCase "insert secret 3 incorrect" <| fun _ ->
            let res =
                RevocationSet()
                |> insertUnwrap
                    281474976710655UL
                    "7cc854b54e3e0dcdb010d7a3fee464a9687be6e8db3be6854c475621e007a5dc"
                |> insertUnwrap
                    281474976710654UL
                    "c7518c8ae4660ed02894df8976fa1a3659c1a8b4b5bec0c4b872abeba4cb8964"
                |> insertUnwrap
                    281474976710653UL
                    "c51a18b13e8527e579ec56365482c62f180b7d5760b46e9477dae59e87ed423a"
                |> insert
                    281474976710652UL
                    "27cddaa5624534cb6cb9d7da077cf2b22ab21e9b506fd4998a51d54502e99116"
            match res with
            | Error(KeyMismatch(previous, current))
                when previous.Index.UInt64 = 281474976710653UL && current.Index.UInt64 = 281474976710652UL
                -> ()
            | _ -> failwith <| sprintf "unexpected result: %A" res
                
        testCase "insert secret 4 incorrect" <| fun _ ->
            let res =
                RevocationSet()
                |> insertUnwrap
                    281474976710655UL
                    "02a40c85b6f28da08dfdbe0926c53fab2de6d28c10301f8f7c4073d5e42e3148"
                |> insertUnwrap
                    281474976710654UL
                    "dddc3a8d14fddf2b68fa8c7fbad2748274937479dd0f8930d5ebb4ab6bd866a3"
                |> insertUnwrap
                    281474976710653UL
                    "c51a18b13e8527e579ec56365482c62f180b7d5760b46e9477dae59e87ed423a"
                |> insertUnwrap
                    281474976710652UL
                    "ba65d7b0ef55a3ba300d4e87af29868f394f8f138d78a7011669c79b37b936f4"
                |> insertUnwrap
                    281474976710651UL
                    "c65716add7aa98ba7acb236352d665cab17345fe45b55fb879ff80e6bd0c41dd"
                |> insertUnwrap
                    281474976710650UL
                    "969660042a28f32d9be17344e09374b379962d03db1574df5a8a5a47e19ce3f2"
                |> insertUnwrap
                    281474976710649UL
                    "a5a64476122ca0925fb344bdc1854c1c0a59fc614298e50a33e331980a220f32"
                |> insert
                    281474976710648UL
                    "05cde6323d949933f7f7b78776bcc1ea6d9b31447732e3802e1f7ac44b650e17"
            match res with
            | Error(KeyMismatch(previous, current))
                when (
                        previous.Index.UInt64 = 281474976710654UL ||
                        previous.Index.UInt64 = 281474976710653UL ||
                        previous.Index.UInt64 = 281474976710652UL
                    ) && current.Index.UInt64 = 281474976710648UL
                -> ()
            | _ -> failwith <| sprintf "unexpected result: %A" res

        testCase "insert secret 5 incorrect" <| fun _ ->
            let res =
                RevocationSet()
                |> insertUnwrap
                    281474976710655UL
                    "7cc854b54e3e0dcdb010d7a3fee464a9687be6e8db3be6854c475621e007a5dc"
                |> insertUnwrap
                    281474976710654UL
                    "c7518c8ae4660ed02894df8976fa1a3659c1a8b4b5bec0c4b872abeba4cb8964"
                |> insertUnwrap
                    281474976710653UL
                    "2273e227a5b7449b6e70f1fb4652864038b1cbf9cd7c043a7d6456b7fc275ad8"
                |> insertUnwrap
                    281474976710652UL
                    "27cddaa5624534cb6cb9d7da077cf2b22ab21e9b506fd4998a51d54502e99116"
                |> insertUnwrap
                    281474976710651UL
                    "631373ad5f9ef654bb3dade742d09504c567edd24320d2fcd68e3cc47e2ff6a6"
                |> insert
                    281474976710650UL
                    "969660042a28f32d9be17344e09374b379962d03db1574df5a8a5a47e19ce3f2"
            match res with
            | Error(KeyMismatch(previous, current))
                when previous.Index.UInt64 = 281474976710651UL && current.Index.UInt64 = 281474976710650UL
                -> ()
            | _ -> failwith <| sprintf "unexpected result: %A" res

        testCase "insert secret 6 incorrect" <| fun _ ->
            let res =
                RevocationSet()
                |> insertUnwrap
                    281474976710655UL
                    "7cc854b54e3e0dcdb010d7a3fee464a9687be6e8db3be6854c475621e007a5dc"
                |> insertUnwrap
                    281474976710654UL
                    "c7518c8ae4660ed02894df8976fa1a3659c1a8b4b5bec0c4b872abeba4cb8964"
                |> insertUnwrap
                    281474976710653UL
                    "2273e227a5b7449b6e70f1fb4652864038b1cbf9cd7c043a7d6456b7fc275ad8"
                |> insertUnwrap
                    281474976710652UL
                    "27cddaa5624534cb6cb9d7da077cf2b22ab21e9b506fd4998a51d54502e99116"
                |> insertUnwrap
                    281474976710651UL
                    "631373ad5f9ef654bb3dade742d09504c567edd24320d2fcd68e3cc47e2ff6a6"
                |> insertUnwrap
                    281474976710650UL
                    "b7e76a83668bde38b373970155c868a653304308f9896692f904a23731224bb1"
                |> insertUnwrap
                    281474976710649UL
                    "a5a64476122ca0925fb344bdc1854c1c0a59fc614298e50a33e331980a220f32"
                |> insert
                    281474976710648UL
                    "05cde6323d949933f7f7b78776bcc1ea6d9b31447732e3802e1f7ac44b650e17"
            match res with
            | Error(KeyMismatch(previous, current))
                when previous.Index.UInt64 = 281474976710650UL && current.Index.UInt64 = 281474976710648UL
                -> ()
            | _ -> failwith <| sprintf "unexpected result: %A" res

        testCase "insert secret 7 incorrect" <| fun _ ->
            let res =
                RevocationSet()
                |> insertUnwrap
                    281474976710655UL
                    "7cc854b54e3e0dcdb010d7a3fee464a9687be6e8db3be6854c475621e007a5dc"
                |> insertUnwrap
                    281474976710654UL
                    "c7518c8ae4660ed02894df8976fa1a3659c1a8b4b5bec0c4b872abeba4cb8964"
                |> insertUnwrap
                    281474976710653UL
                    "2273e227a5b7449b6e70f1fb4652864038b1cbf9cd7c043a7d6456b7fc275ad8"
                |> insertUnwrap
                    281474976710652UL
                    "27cddaa5624534cb6cb9d7da077cf2b22ab21e9b506fd4998a51d54502e99116"
                |> insertUnwrap
                    281474976710651UL
                    "c65716add7aa98ba7acb236352d665cab17345fe45b55fb879ff80e6bd0c41dd"
                |> insertUnwrap
                    281474976710650UL
                    "969660042a28f32d9be17344e09374b379962d03db1574df5a8a5a47e19ce3f2"
                |> insertUnwrap
                    281474976710649UL
                    "e7971de736e01da8ed58b94c2fc216cb1dca9e326f3a96e7194fe8ea8af6c0a3"
                |> insert
                    281474976710648UL
                    "05cde6323d949933f7f7b78776bcc1ea6d9b31447732e3802e1f7ac44b650e17"
            match res with
            | Error(KeyMismatch(previous, current))
                when previous.Index.UInt64 = 281474976710649UL && current.Index.UInt64 = 281474976710648UL
                -> ()
            | _ -> failwith <| sprintf "unexpected result: %A" res

        testCase "insert secret 8 incorrect" <| fun _ ->
            let res =
                RevocationSet()
                |> insertUnwrap
                    281474976710655UL
                    "7cc854b54e3e0dcdb010d7a3fee464a9687be6e8db3be6854c475621e007a5dc"
                |> insertUnwrap
                    281474976710654UL
                    "c7518c8ae4660ed02894df8976fa1a3659c1a8b4b5bec0c4b872abeba4cb8964"
                |> insertUnwrap
                    281474976710653UL
                    "2273e227a5b7449b6e70f1fb4652864038b1cbf9cd7c043a7d6456b7fc275ad8"
                |> insertUnwrap
                    281474976710652UL
                    "27cddaa5624534cb6cb9d7da077cf2b22ab21e9b506fd4998a51d54502e99116"
                |> insertUnwrap
                    281474976710651UL
                    "c65716add7aa98ba7acb236352d665cab17345fe45b55fb879ff80e6bd0c41dd"
                |> insertUnwrap
                    281474976710650UL
                    "969660042a28f32d9be17344e09374b379962d03db1574df5a8a5a47e19ce3f2"
                |> insertUnwrap
                    281474976710649UL
                    "a5a64476122ca0925fb344bdc1854c1c0a59fc614298e50a33e331980a220f32"
                |> insert
                    281474976710648UL
                    "a7efbc61aac46d34f77778bac22c8a20c6a46ca460addc49009bda875ec88fa4"
            match res with
            | Error(KeyMismatch(previous, current))
                when previous.Index.UInt64 = 281474976710649UL && current.Index.UInt64 = 281474976710648UL
                -> ()
            | _ -> failwith <| sprintf "unexpected result: %A" res
    ]

