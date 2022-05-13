module TransactionBolt3TestVectorTests

open System
open System.Text.Json
open DotNetLightning.Crypto
open DotNetLightning.Serialization.Msgs
open DotNetLightning.Transactions
open DotNetLightning.Transactions.Transactions
open DotNetLightning.Utils
open System.IO
open Expecto
open GeneratorsTests
open NBitcoin

open ResultUtils
open ResultUtils.Portability

// let logger = Log.create "bolt3-transaction tests"
let log =
    // uncomment this if you want to see the debug message for this test
    // logger.LogSimple
    fun _s -> ()


/// data formatted to json
let dataPath1 =
    Path.Join(
        AppDomain.CurrentDomain.BaseDirectory,
        "../../..",
        ("Data/bolt3-tx.json")
    )

let data1 = dataPath1 |> File.ReadAllText |> JsonDocument.Parse

let localPerCommitmentPoint =
    PerCommitmentPoint
    <| PubKey(
        "025f7117a78150fe2ef97db7cfc83bd57b2e2c0d0dd25eaf467a4a1c2a45ce1486"
    )

type LocalConfig =
    {
        CommitTxNumber: CommitmentNumber
        ToSelfDelay: BlockHeightOffset16
        DustLimit: Money
        PaymentBasepointSecret: PaymentBasepointSecret
        PaymentBasepoint: PaymentBasepoint
        RevocationBasepointSecret: RevocationBasepointSecret
        DelayedPaymentBasepointSecret: DelayedPaymentBasepointSecret
        DelayedPaymentBasepoint: DelayedPaymentBasepoint
        PerCommitmentPoint: PerCommitmentPoint
        PaymentPrivKey: PaymentPrivKey
        DelayedPaymentPrivKey: DelayedPaymentPrivKey
        RevocationPubKey: RevocationPubKey
        FeeRatePerKw: FeeRatePerKw
        FundingPrivKey: FundingPrivKey
    }

let getLocal() : LocalConfig =
    let paymentBasepointSecret =
        "1111111111111111111111111111111111111111111111111111111111111111"
        |> hex.DecodeData
        |> fun h -> new Key(h)
        |> PaymentBasepointSecret

    let paymentBasepoint = paymentBasepointSecret.PaymentBasepoint()

    let delayedPaymentBasepointSecret =
        "3333333333333333333333333333333333333333333333333333333333333333"
        |> hex.DecodeData
        |> fun h -> new Key(h)
        |> DelayedPaymentBasepointSecret

    let revocationBasepointSecret =
        "2222222222222222222222222222222222222222222222222222222222222222"
        |> hex.DecodeData
        |> fun h -> new Key(h)
        |> RevocationBasepointSecret

    let fundingPrivKey =
        "30ff4956bbdd3222d44cc5e8a1261dab1e07957bdac5ae88fe3261ef321f3749"
        |> hex.DecodeData
        |> fun h -> new Key(h)
        |> FundingPrivKey

    {
        CommitTxNumber =
            CommitmentNumber(UInt48.MaxValue - (UInt48.FromUInt64 42UL))
        ToSelfDelay = 144us |> BlockHeightOffset16
        DustLimit = Money.Satoshis(546L)
        PaymentBasepointSecret = paymentBasepointSecret
        PaymentBasepoint = paymentBasepoint
        RevocationBasepointSecret = revocationBasepointSecret
        DelayedPaymentBasepointSecret = delayedPaymentBasepointSecret
        DelayedPaymentBasepoint =
            delayedPaymentBasepointSecret.DelayedPaymentBasepoint()
        FundingPrivKey = fundingPrivKey
        PerCommitmentPoint = localPerCommitmentPoint
        PaymentPrivKey =
            localPerCommitmentPoint.DerivePaymentPrivKey paymentBasepointSecret
        DelayedPaymentPrivKey =
            localPerCommitmentPoint.DeriveDelayedPaymentPrivKey
                delayedPaymentBasepointSecret
        RevocationPubKey =
            RevocationPubKey
            <| PubKey(
                "0212a140cd0c6539d07cd08dfe09984dec3251ea808b892efeac3ede9402bf2b19"
            )
        FeeRatePerKw = 15000u |> FeeRatePerKw
    }

type RemoteConfig =
    {
        CommitTxNumber: CommitmentNumber
        ToSelfDelay: BlockHeightOffset16
        DustLimit: Money
        PaymentBasepointSecret: PaymentBasepointSecret
        PaymentBasepoint: PaymentBasepoint
        RevocationBasepointSecret: RevocationBasepointSecret
        RevocationBasepoint: RevocationBasepoint
        FundingPrivKey: FundingPrivKey
        PaymentPrivKey: PaymentPrivKey
        PerCommitmentPoint: PerCommitmentPoint
    }

let getRemote() : RemoteConfig =
    let paymentBasepointSecret =
        "4444444444444444444444444444444444444444444444444444444444444444"
        |> hex.DecodeData
        |> fun h -> new Key(h)
        |> PaymentBasepointSecret

    let paymentBasepoint = paymentBasepointSecret.PaymentBasepoint()

    let revocationBasepointSecret =
        "2222222222222222222222222222222222222222222222222222222222222222"
        |> hex.DecodeData
        |> fun h -> new Key(h)
        |> RevocationBasepointSecret

    let revocationBasepoint = revocationBasepointSecret.RevocationBasepoint()

    let fundingPrivKey =
        "1552dfba4f6cf29a62a0af13c8d6981d36d0ef8d61ba10fb0fe90da7634d7e13"
        |> hex.DecodeData
        |> fun h -> new Key(h)
        |> FundingPrivKey

    let perCommitmentPoint =
        PerCommitmentPoint
        <| PubKey(
            "022c76692fd70814a8d1ed9dedc833318afaaed8188db4d14727e2e99bc619d325"
        )

    {
        CommitTxNumber =
            CommitmentNumber(UInt48.MaxValue - (UInt48.FromUInt64 42UL))
        ToSelfDelay = 144us |> BlockHeightOffset16
        DustLimit = Money.Satoshis(546L)
        PaymentBasepointSecret = paymentBasepointSecret
        PaymentBasepoint = paymentBasepoint
        RevocationBasepointSecret = revocationBasepointSecret
        RevocationBasepoint = revocationBasepoint
        FundingPrivKey = fundingPrivKey
        PaymentPrivKey =
            localPerCommitmentPoint.DerivePaymentPrivKey paymentBasepointSecret
        PerCommitmentPoint = perCommitmentPoint
    }

let n = Network.RegTest

let coinbaseTx =
    Transaction.Parse(
        "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff03510101ffffffff0100f2052a010000001976a9143ca33c2e4446f4a305f23c80df8ad1afdcf652f988ac00000000",
        n
    )

let fundingTx =
    Transaction.Parse(
        "0200000001adbb20ea41a8423ea937e76e8151636bf6093b70eaff942930d20576600521fd000000006b48304502210090587b6201e166ad6af0227d3036a9454223d49a1f11839c1a362184340ef0240220577f7cd5cca78719405cbf1de7414ac027f0239ef6e214c90fcaab0454d84b3b012103535b32d5eb0a6ed0982a0479bbadc9868d9836f6ba94dd5a63be16d875069184ffffffff028096980000000000220020c015c4a6be010e21657068fc2e6a9d02b27ebe4d490a25846f7237f104d1a3cd20256d29010000001600143ca33c2e4446f4a305f23c80df8ad1afdcf652f900000000",
        n
    )

let fundingAmount = fundingTx.Outputs.[0].Value
log(sprintf "# funding-tx: %A" fundingTx)

let local = getLocal()
let remote = getRemote()

let fundingRedeem =
    Scripts.funding
        (local.FundingPrivKey.FundingPubKey())
        (remote.FundingPrivKey.FundingPubKey())

let commitmentInputScriptCoin =
    Coin(
        fundingTx.GetHash(),
        0u,
        fundingAmount,
        fundingRedeem.WitHash.ScriptPubKey
    )
    |> fun c -> ScriptCoin(c, fundingRedeem)

log(sprintf "local payment basepoint is %A" local.PaymentBasepoint)
log(sprintf "remote payment basepoint is %A" remote.PaymentBasepoint)

let obscuredTxNumber =
    let commitmentNumber =
        CommitmentNumber(UInt48.MaxValue - (UInt48.FromUInt64 42UL))

    commitmentNumber.Obscure true local.PaymentBasepoint remote.PaymentBasepoint

Expect.equal
    obscuredTxNumber
    (0x2bb038521914UL ^^^ 42UL |> UInt48.FromUInt64 |> ObscuredCommitmentNumber)
    ""

sprintf "local_payment_basepoint: %A " local.PaymentBasepoint |> log
sprintf "remote_payment_basepoint: %A" remote.PaymentBasepoint |> log
sprintf "local_funding_privkey: %A" local.FundingPrivKey |> log
sprintf "local_funding_pubkey: %A" (local.FundingPrivKey.FundingPubKey()) |> log
sprintf "remote_funding_privkey: %A" remote.FundingPrivKey |> log

sprintf "remote_funding_pubkey: %A" (remote.FundingPrivKey.FundingPubKey())
|> log

sprintf "local_secretkey: %A" local.PaymentPrivKey |> log
sprintf "localkey: %A" (local.PaymentPrivKey.PaymentPubKey()) |> log
sprintf "remotekey: %A" remote.PaymentPrivKey |> log

sprintf
    "local_delayedkey: %A"
    (local.DelayedPaymentPrivKey.DelayedPaymentPubKey())
|> log

sprintf "local_revocation_key: %A" local.RevocationPubKey |> log
sprintf "# funding wscript = %A" fundingRedeem |> log

assert
    (fundingRedeem = Script.FromBytesUnsafe(
        hex.DecodeData
            "5221023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb21030e9f7b623d2ccc7c9bd44d66d5ce21ce504c0acf6385a132cec6d3c39fa711c152ae"
    ))

let paymentPreImages =
    let _s =
        ([
            ("0000000000000000000000000000000000000000000000000000000000000000")
            ("0101010101010101010101010101010101010101010101010101010101010101")
            ("0202020202020202020202020202020202020202020202020202020202020202")
            ("0303030303030303030303030303030303030303030303030303030303030303")
            ("0404040404040404040404040404040404040404040404040404040404040404")
        ])

    _s |> List.map(hex.DecodeData) |> List.map(PaymentPreimage.Create)


log(sprintf "first payment hash is %A" paymentPreImages.[0].Hash)

let incomingHtlcs =
    [
        {
            UpdateAddHTLCMsg.ChannelId = ChannelId.Zero
            HTLCId = HTLCId.Zero
            Amount = LNMoney.MilliSatoshis 1000000L
            PaymentHash = paymentPreImages.[0].Hash
            CLTVExpiry = 500u |> BlockHeight
            OnionRoutingPacket = OnionPacket.LastPacket
        }
        {
            UpdateAddHTLCMsg.ChannelId = ChannelId.Zero
            HTLCId = HTLCId(1UL)
            Amount = LNMoney.MilliSatoshis 2000000L
            PaymentHash = paymentPreImages.[1].Hash
            CLTVExpiry = 501u |> BlockHeight
            OnionRoutingPacket = OnionPacket.LastPacket
        }
        {
            UpdateAddHTLCMsg.ChannelId = ChannelId.Zero
            HTLCId = HTLCId(4UL)
            Amount = LNMoney.MilliSatoshis 4000000L
            PaymentHash = paymentPreImages.[4].Hash
            CLTVExpiry = 504u |> BlockHeight
            OnionRoutingPacket = OnionPacket.LastPacket
        }
    ]

let outgoingHtlcs =
    [
        {
            UpdateAddHTLCMsg.ChannelId = ChannelId.Zero
            HTLCId = HTLCId(2UL)
            Amount = LNMoney.MilliSatoshis 2000000L
            PaymentHash = paymentPreImages.[2].Hash
            CLTVExpiry = 502u |> BlockHeight
            OnionRoutingPacket = OnionPacket.LastPacket
        }
        {
            UpdateAddHTLCMsg.ChannelId = ChannelId.Zero
            HTLCId = HTLCId(3UL)
            Amount = LNMoney.MilliSatoshis 3000000L
            PaymentHash = paymentPreImages.[3].Hash
            CLTVExpiry = 503u |> BlockHeight
            OnionRoutingPacket = OnionPacket.LastPacket
        }
    ]

let incomingHtlcScripts =
    incomingHtlcs
    |> List.map(fun htlc ->
        Scripts.htlcReceived
            // FIXME: payment keys being used as htlc keys??
            (HtlcPubKey <| local.PaymentPrivKey.PaymentPubKey().RawPubKey())
            (HtlcPubKey <| remote.PaymentPrivKey.PaymentPubKey().RawPubKey())
            (local.RevocationPubKey)
            (htlc.PaymentHash)
            (htlc.CLTVExpiry.Value)
    )

let outgoingHtlcScripts =
    outgoingHtlcs
    |> List.map(fun htlc ->
        Scripts.htlcOffered
            // FIXME: payment keys being used as htlc keys??
            (HtlcPubKey <| local.PaymentPrivKey.PaymentPubKey().RawPubKey())
            (HtlcPubKey <| remote.PaymentPrivKey.PaymentPubKey().RawPubKey())
            (local.RevocationPubKey)
            (htlc.PaymentHash)
    )

let run(spec: CommitmentSpec) : (Transaction * _) =
    let local = getLocal()
    log(sprintf "to_local_msat %A" spec.ToLocal)
    log(sprintf "to_remote_msat %A" spec.ToRemote)
    log(sprintf "local_feerate_per_kw %A" spec.FeeRatePerKw)

    let commitTx =
        let commitTx =
            Transactions.makeCommitTx
                (commitmentInputScriptCoin)
                (local.CommitTxNumber)
                (local.PaymentBasepoint)
                (remote.PaymentBasepoint)
                (true)
                (local.DustLimit)
                (local.RevocationPubKey)
                (local.ToSelfDelay)
                (local.DelayedPaymentPrivKey.DelayedPaymentPubKey())
                (remote.PaymentPrivKey.PaymentPubKey())
                // FIXME: payment keys being used as htlc keys??
                (HtlcPubKey <| local.PaymentPrivKey.PaymentPubKey().RawPubKey())
                (HtlcPubKey <| remote.PaymentPrivKey.PaymentPubKey().RawPubKey())
                (spec)
                (n)
        // test vectors requires us to use RFC6974
        let localSig, tx2 =
            Transactions.signCore(
                commitTx,
                local.FundingPrivKey.RawKey(),
                false
            )

        let remoteSig, tx3 =
            Transactions.signCore(tx2, remote.FundingPrivKey.RawKey(), false)

        Transactions.checkSigAndAdd
            (tx3)
            (localSig)
            (local.FundingPrivKey.FundingPubKey().RawPubKey())
        >>= fun tx4 ->
                Transactions.checkSigAndAdd
                    (tx4)
                    (remoteSig)
                    (remote.FundingPrivKey.FundingPubKey().RawPubKey())
        |> function
            | Ok e -> e
            | Error e -> failwithf "%A" e

    let baseFee = Transactions.commitTxFee (local.DustLimit) (spec)
    log(sprintf "base commitment transaction fee is %A" baseFee)

    let actualFee =
        fundingAmount
        - match commitTx.Value.TryGetFee() with
          | true, f -> f
          | false, _ -> failwith "fail: BOLT3 test, couldn't get fee"

    log(sprintf "actual commitment tx fee is %A " actualFee)

    commitTx.Value.GetGlobalTransaction().Outputs
    |> List.ofSeq
    |> List.iter(fun txOut ->
        match txOut.ScriptPubKey.Length with
        | 22 ->
            log(
                sprintf
                    "to-remote amount %A P2WPKH(%A)"
                    (txOut.Value)
                    (remote.PaymentPrivKey.PaymentPubKey())
            )
        | 34 ->
            let htlcScriptOpt =
                Option.orElse
                    (incomingHtlcScripts
                     |> List.tryFind(fun s ->
                         s.WitHash.ScriptPubKey = txOut.ScriptPubKey
                     ))
                    (outgoingHtlcScripts
                     |> List.tryFind(fun s ->
                         s.WitHash.ScriptPubKey = txOut.ScriptPubKey
                     ))

            match htlcScriptOpt with
            | None ->
                (sprintf
                    "to-local amount %A. \n to-local wscript (%A)"
                    txOut.Value
                    (Scripts.toLocalDelayed
                        (local.RevocationPubKey)
                        (local.ToSelfDelay)
                        (local.DelayedPaymentPrivKey.DelayedPaymentPubKey())))
                |> log
            | Some htlcScript ->
                (sprintf
                    "to-local amount %A \n to-local wscript (%A)"
                    txOut.Value
                    htlcScript)
                |> log
        | x -> failwithf "unexpected scriptPubKey length %A" x
    )

    let actualCommitTxNumOpt =
        Transactions.getCommitTxNumber
            (commitTx.Value.GetGlobalTransaction())
            (true)
            (local.PaymentBasepoint)
            (remote.PaymentBasepoint)

    let expectedCommitTxNumber = local.CommitTxNumber
    Expect.equal actualCommitTxNumOpt.Value (expectedCommitTxNumber) ""
    commitTx.Value.Finalize() |> ignore
    Expect.isTrue (commitTx.Value.CanExtractTransaction()) ""
    sprintf "output commit_tx %A" commitTx.Value |> log

    let (unsignedHTLCTimeoutTxs, unsignedHTLCSuccessTxs) =
        Transactions.makeHTLCTxs
            (commitTx.Value.ExtractTransaction())
            local.DustLimit
            local.RevocationPubKey
            local.ToSelfDelay
            (local.DelayedPaymentPrivKey.DelayedPaymentPubKey())
            // FIXME: payment keys being used as htlc keys??
            (HtlcPubKey <| local.PaymentPrivKey.PaymentPubKey().RawPubKey())
            (HtlcPubKey <| remote.PaymentPrivKey.PaymentPubKey().RawPubKey())
            spec
            n
        |> Result.defaultWith(fun _ ->
            failwith "fail(BOLT3 transactions): couldn't make HTLC transactions"
        )

    let htlcTxs =
        Seq.append
            (unsignedHTLCSuccessTxs |> Seq.cast<IHTLCTx>)
            (unsignedHTLCTimeoutTxs |> Seq.cast<IHTLCTx>)

    sprintf "num htlcs: %A" htlcTxs |> log

    let htlcTxs =
        htlcTxs
        |> Seq.toList
        |> List.sortBy(fun x ->
            x.Value.GetGlobalTransaction().Inputs.[0]
                .PrevOut
                .N
        )

    let signedTxs =
        htlcTxs
        |> List.map(fun htlcTx ->
            match htlcTx with
            | :? HTLCSuccessTx as tx ->
                let localSig, tx2 =
                    Transactions.signCore(
                        tx,
                        local.PaymentPrivKey.RawKey(),
                        false
                    )

                let remoteSig, _tx3 =
                    Transactions.signCore(
                        tx2,
                        remote.PaymentPrivKey.RawKey(),
                        false
                    )
                // just checking preimage is in global list
                let paymentPreimage =
                    (paymentPreImages
                     |> List.find(fun p -> p.Hash = tx.PaymentHash))

                log(sprintf "Finalizing %A" tx)

                match tx.Finalize(localSig, remoteSig, paymentPreimage) with
                | Ok tx -> tx
                | Error e -> failwithf "%A" e
            | :? HTLCTimeoutTx as tx ->
                let localSig, _ =
                    Transactions.signCore(
                        tx,
                        local.PaymentPrivKey.RawKey(),
                        false
                    )

                let remoteSig, _ =
                    Transactions.signCore(
                        tx,
                        remote.PaymentPrivKey.RawKey(),
                        false
                    )

                match tx.Finalize(localSig, remoteSig) with
                | Ok tx -> tx
                | Error e -> failwithf "%A" e
            | _ -> failwith "unreachable"
        )

    commitTx.Value.ExtractTransaction(), signedTxs

let testVectors =
    data1
        .RootElement
        .GetProperty("test_vectors")
        .EnumerateArray()
    |> Seq.toArray

let incomingHtlcMap =
    incomingHtlcs |> List.map(fun htlc -> htlc.HTLCId, htlc) |> Map.ofList

let outgoingHtlcMap =
    outgoingHtlcs |> List.map(fun htlc -> htlc.HTLCId, htlc) |> Map.ofList

let runTest (testCase: JsonElement) spec expectedOutputCount =
    log(sprintf "testing %A" (testCase.GetProperty("name").GetString()))
    let commitTx, htlcTxs = run(spec)
    Expect.equal (commitTx.Outputs.Count) (expectedOutputCount) ""

    let expectedTx =
        testCase
            .GetProperty("output_commit_tx")
            .GetString()

    Expect.equal (commitTx.ToHex()) (expectedTx) ""

    let expectedHTLC =
        testCase
            .GetProperty("htlc_output_txs")
            .EnumerateArray()

    expectedHTLC
    |> Seq.iteri(fun i htlc ->
        Expect.equal
            (htlcTxs.[i].ToHex())
            (htlc.GetProperty("value").GetString())
            ""
    )

let specBase =
    {
        CommitmentSpec.IncomingHTLCs = incomingHtlcMap
        CommitmentSpec.OutgoingHTLCs = outgoingHtlcMap
        FeeRatePerKw = 15000u |> FeeRatePerKw
        ToLocal = LNMoney.MilliSatoshis(6988000000L)
        ToRemote = 3000000000L |> LNMoney.MilliSatoshis
    }

[<Tests>]
let tests =
    testList
        "Transaction test vectors"
        [
            testCase "simple commitment tx with no HTLCs"
            <| fun _ ->
                let spec =
                    {
                        CommitmentSpec.OutgoingHTLCs = Map.empty
                        CommitmentSpec.IncomingHTLCs = Map.empty
                        FeeRatePerKw = 15000u |> FeeRatePerKw
                        ToLocal = LNMoney.MilliSatoshis(7000000000L)
                        ToRemote = 3000000000L |> LNMoney.MilliSatoshis
                    }

                let commitTx, _htlcTxs = run(spec)
                let testCase = testVectors.[0]
                Expect.equal (commitTx.Outputs.Count) (2) ""

                let expectedTx =
                    testCase
                        .GetProperty("output_commit_tx")
                        .GetString()

                Expect.equal (commitTx.ToHex()) (expectedTx) ""

            testCase
                "commitment tx with all 5 htlcs untrimmed (minimum feerate)"
            <| fun _ ->
                let testCase = testVectors.[1]

                let spec =
                    {
                        CommitmentSpec.IncomingHTLCs = incomingHtlcMap
                        CommitmentSpec.OutgoingHTLCs = outgoingHtlcMap
                        FeeRatePerKw = 0u |> FeeRatePerKw
                        ToLocal = 6988000000L |> LNMoney.MilliSatoshis
                        ToRemote = 3000000000L |> LNMoney.MilliSatoshis
                    }

                runTest (testCase) (spec) (7)

            testCase
                "commitment tx with seven outputs untrimmed (maximum feerate)"
            <| fun _ ->
                let testCase = testVectors.[2]

                let spec =
                    {
                        CommitmentSpec.IncomingHTLCs = incomingHtlcMap
                        CommitmentSpec.OutgoingHTLCs = outgoingHtlcMap
                        FeeRatePerKw =
                            (454999UL / Constants.HTLC_SUCCESS_WEIGHT)
                            |> uint32
                            |> FeeRatePerKw
                        ToLocal = 6988000000L |> LNMoney.MilliSatoshis
                        ToRemote = 3000000000L |> LNMoney.MilliSatoshis
                    }

                runTest (testCase) (spec) (7)

            testCase
                "commitment tx with six outputs untrimmed (minimum feerate)"
            <| fun _ ->
                let testCase = testVectors.[3]

                let spec =
                    { specBase with
                        FeeRatePerKw =
                            (454999UL / Constants.HTLC_SUCCESS_WEIGHT)
                            |> ((+) 1UL)
                            |> uint32
                            |> FeeRatePerKw
                    }

                runTest (testCase) (spec) (6)

            testCase
                "commitment tx with six outputs untrimmed (maximum feerate)"
            <| fun _ ->
                let testCase = testVectors.[4]

                let spec =
                    { specBase with
                        FeeRatePerKw =
                            (1454999UL / Constants.HTLC_SUCCESS_WEIGHT)
                            |> uint32
                            |> FeeRatePerKw
                    }

                runTest (testCase) (spec) (6)

            testCase
                "commitment tx with five outputs untrimmed (minimum feerate)"
            <| fun _ ->
                let testCase = testVectors.[5]

                let spec =
                    { specBase with
                        FeeRatePerKw =
                            (1454999UL / Constants.HTLC_SUCCESS_WEIGHT)
                            |> ((+) 1UL)
                            |> uint32
                            |> FeeRatePerKw
                    }

                runTest (testCase) (spec) (5)

            testCase
                "commitment tx with five outputs untrimmed (maximum feerate)"
            <| fun _ ->
                let testCase = testVectors.[6]

                let spec =
                    { specBase with
                        FeeRatePerKw =
                            (1454999UL / Constants.HTLC_TIMEOUT_WEIGHT)
                            |> uint32
                            |> FeeRatePerKw
                    }

                runTest (testCase) (spec) (5)

            testCase
                "commitment tx with four outputs untrimmed (minimum feerate)"
            <| fun _ ->
                let testCase = testVectors.[7]

                let spec =
                    { specBase with
                        FeeRatePerKw =
                            (1454999UL / Constants.HTLC_TIMEOUT_WEIGHT)
                            |> ((+) 1UL)
                            |> uint32
                            |> FeeRatePerKw
                    }

                runTest (testCase) (spec) (4)

            testCase
                "commitment tx with four outputs untrimmed (maximum feerate)"
            <| fun _ ->
                let testCase = testVectors.[8]

                let spec =
                    { specBase with
                        FeeRatePerKw =
                            (2454999UL / Constants.HTLC_TIMEOUT_WEIGHT)
                            |> uint32
                            |> FeeRatePerKw
                    }

                runTest (testCase) (spec) (4)

            testCase
                "commitment tx with three outputs untrimmed (minimum feerate)"
            <| fun _ ->
                let feeRate = 2454999UL / Constants.HTLC_TIMEOUT_WEIGHT

                let spec =
                    {
                        CommitmentSpec.IncomingHTLCs = incomingHtlcMap
                        CommitmentSpec.OutgoingHTLCs = outgoingHtlcMap
                        FeeRatePerKw = feeRate + 1UL |> uint32 |> FeeRatePerKw
                        ToLocal = 6988000000L |> LNMoney.MilliSatoshis
                        ToRemote = 3000000000L |> LNMoney.MilliSatoshis
                    }

                let testCase = testVectors.[9]
                runTest (testCase) (spec) (3)

            testCase
                "commitment tx with three outputs untrimmed (maximum feerate)"
            <| fun _ ->
                let feeRate = 3454999UL / Constants.HTLC_SUCCESS_WEIGHT

                let spec =
                    {
                        CommitmentSpec.IncomingHTLCs = incomingHtlcMap
                        CommitmentSpec.OutgoingHTLCs = outgoingHtlcMap
                        FeeRatePerKw = feeRate |> uint32 |> FeeRatePerKw
                        ToLocal = 6988000000L |> LNMoney.MilliSatoshis
                        ToRemote = 3000000000L |> LNMoney.MilliSatoshis
                    }

                let testCase = testVectors.[10]
                runTest (testCase) (spec) (3)

            testCase
                "commitment tx with two outputs untrimmed (minimum feerate)"
            <| fun _ ->
                let feeRate = 3454999UL / Constants.HTLC_SUCCESS_WEIGHT

                let spec =
                    {
                        CommitmentSpec.IncomingHTLCs = incomingHtlcMap
                        CommitmentSpec.OutgoingHTLCs = outgoingHtlcMap
                        FeeRatePerKw = feeRate + 1UL |> uint32 |> FeeRatePerKw
                        ToLocal = 6988000000L |> LNMoney.MilliSatoshis
                        ToRemote = 3000000000L |> LNMoney.MilliSatoshis
                    }

                let testCase = testVectors.[11]
                runTest (testCase) (spec) (2)

            testCase
                "commitment tx with two outputs untrimmed (maximum feerate)"
            <| fun _ ->
                let spec =
                    {
                        CommitmentSpec.IncomingHTLCs = incomingHtlcMap
                        CommitmentSpec.OutgoingHTLCs = outgoingHtlcMap
                        FeeRatePerKw = 9651180u |> FeeRatePerKw
                        ToLocal = 6988000000L |> LNMoney.MilliSatoshis
                        ToRemote = 3000000000L |> LNMoney.MilliSatoshis
                    }

                let testCase = testVectors.[12]
                runTest (testCase) (spec) (2)

            testCase "commitment tx with one output untrimmed (minimum feerate)"
            <| fun _ ->
                let testCase = testVectors.[13]

                let spec =
                    { specBase with
                        FeeRatePerKw = (9651181u |> FeeRatePerKw)
                    }

                runTest (testCase) (spec) (1)

            testCase "commitment tx with fee greater than funder amount"
            <| fun _ ->
                let spec =
                    {
                        CommitmentSpec.IncomingHTLCs = incomingHtlcMap
                        CommitmentSpec.OutgoingHTLCs = outgoingHtlcMap
                        FeeRatePerKw = 9651936u |> FeeRatePerKw
                        ToLocal = 6988000000L |> LNMoney.MilliSatoshis
                        ToRemote = 3000000000L |> LNMoney.MilliSatoshis
                    }

                let testCase = testVectors.[14]
                runTest (testCase) (spec) (1)

        ]
