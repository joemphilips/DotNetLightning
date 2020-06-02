module TransactionBolt3TestVectorTests

open ResultUtils
open System
open System.Text.Json
open DotNetLightning.Crypto
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Tests.Utils
open DotNetLightning.Transactions
open DotNetLightning.Transactions.Transactions
open DotNetLightning.Utils
open System.IO
open Expecto
open GeneratorsTests
open NBitcoin

let newSecp256k1 = DotNetLightning.Crypto.CryptoUtils.impl.newSecp256k1

// let logger = Log.create "bolt3-transaction tests"
let logger = TestLogger.Create("bolt3-transaction tests")
let log =
    // uncomment this if you want to see the debug message for this test
    // logger.LogSimple
    fun s -> ()


/// data formatted to json
let dataPath1 = Path.Join(AppDomain.CurrentDomain.BaseDirectory, "../../..", ("Data/bolt3-tx.json"))
let data1 = dataPath1 |> File.ReadAllText |> JsonDocument.Parse

let localPerCommitmentPoint = PubKey("025f7117a78150fe2ef97db7cfc83bd57b2e2c0d0dd25eaf467a4a1c2a45ce1486")
type LocalConfig = {
    Ctx: ISecp256k1
    CommitTxNumber: CommitmentNumber
    ToSelfDelay: BlockHeightOffset16
    DustLimit: Money
    PaymentBasePointSecret: Key
    PaymentBasePoint: PubKey
    RevocationBasePointSecret: uint256
    DelayedPaymentBasePointSecret: Key
    delayedPaymentBasePoint: PubKey
    PerCommitmentPoint: PubKey
    PaymentPrivKey: Key
    DelayedPaymentPrivKey: Key
    RevocationPubKey: PubKey
    FeeRatePerKw: FeeRatePerKw
    FundingPrivKey: Key
}
let getLocal(): LocalConfig =
    let ctx = newSecp256k1()
    let paymentBasePointSecret = "1111111111111111111111111111111111111111111111111111111111111111" |> hex.DecodeData |> Key
    let paymentBasePoint = paymentBasePointSecret.PubKey
    let delayedPaymentBasePointSecret = "3333333333333333333333333333333333333333333333333333333333333333" |> hex.DecodeData |> Key
    {
      Ctx = ctx
      CommitTxNumber = CommitmentNumber(UInt48.FromUInt64 42UL)
      ToSelfDelay = 144us |> BlockHeightOffset16
      DustLimit = Money.Satoshis(546L)
      PaymentBasePointSecret = paymentBasePointSecret
      PaymentBasePoint = paymentBasePoint
      RevocationBasePointSecret = uint256.Parse("2222222222222222222222222222222222222222222222222222222222222222")
      DelayedPaymentBasePointSecret = delayedPaymentBasePointSecret
      delayedPaymentBasePoint = delayedPaymentBasePointSecret.PubKey
      FundingPrivKey = "30ff4956bbdd3222d44cc5e8a1261dab1e07957bdac5ae88fe3261ef321f3749" |> hex.DecodeData |> Key
      PerCommitmentPoint = localPerCommitmentPoint
      PaymentPrivKey = Generators.derivePrivKey(ctx) (paymentBasePointSecret) (localPerCommitmentPoint)
      DelayedPaymentPrivKey = Generators.derivePrivKey(ctx) (delayedPaymentBasePointSecret) (localPerCommitmentPoint)
      RevocationPubKey = PubKey("0212a140cd0c6539d07cd08dfe09984dec3251ea808b892efeac3ede9402bf2b19")
      FeeRatePerKw = 15000u |> FeeRatePerKw
    }

type RemoteConfig = {
    Ctx: ISecp256k1
    CommitTxNumber: CommitmentNumber
    ToSelfDelay: BlockHeightOffset16
    DustLimit: Money
    PaymentBasePointSecret: Key
    PaymentBasePoint: PubKey
    RevocationBasePointSecret: Key
    RevocationBasePoint: PubKey
    FundingPrivKey: Key
    PaymentPrivKey: Key
    PerCommitmentPoint: PubKey
}
let getRemote(): RemoteConfig =
    let ctx = newSecp256k1()
    let paymentBasePointSecret = "4444444444444444444444444444444444444444444444444444444444444444" |> hex.DecodeData |> Key
    let paymentBasePoint = paymentBasePointSecret.PubKey
    let revocationBasePointSecret = "2222222222222222222222222222222222222222222222222222222222222222" |> hex.DecodeData |> Key
    let revocationBasePoint = revocationBasePointSecret.PubKey
    {
      Ctx = ctx
      CommitTxNumber = CommitmentNumber(UInt48.FromUInt64 42UL)
      ToSelfDelay = 144us |> BlockHeightOffset16
      DustLimit = Money.Satoshis(546L)
      PaymentBasePointSecret = paymentBasePointSecret
      PaymentBasePoint = paymentBasePoint
      RevocationBasePointSecret = revocationBasePointSecret
      RevocationBasePoint = revocationBasePoint
      FundingPrivKey = "1552dfba4f6cf29a62a0af13c8d6981d36d0ef8d61ba10fb0fe90da7634d7e13" |> hex.DecodeData |> Key
      PaymentPrivKey = Generators.derivePrivKey (ctx) (paymentBasePointSecret) localPerCommitmentPoint
      PerCommitmentPoint = "022c76692fd70814a8d1ed9dedc833318afaaed8188db4d14727e2e99bc619d325" |> PubKey
    }
    
let n = Network.RegTest
let coinbaseTx = Transaction.Parse("01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff03510101ffffffff0100f2052a010000001976a9143ca33c2e4446f4a305f23c80df8ad1afdcf652f988ac00000000", n)

let fundingTx = Transaction.Parse("0200000001adbb20ea41a8423ea937e76e8151636bf6093b70eaff942930d20576600521fd000000006b48304502210090587b6201e166ad6af0227d3036a9454223d49a1f11839c1a362184340ef0240220577f7cd5cca78719405cbf1de7414ac027f0239ef6e214c90fcaab0454d84b3b012103535b32d5eb0a6ed0982a0479bbadc9868d9836f6ba94dd5a63be16d875069184ffffffff028096980000000000220020c015c4a6be010e21657068fc2e6a9d02b27ebe4d490a25846f7237f104d1a3cd20256d29010000001600143ca33c2e4446f4a305f23c80df8ad1afdcf652f900000000", n)
let fundingAmount = fundingTx.Outputs.[0].Value
log(sprintf "# funding-tx: %A" fundingTx)

let local = getLocal()
let remote = getRemote()
let fundingRedeem =
    let fundingPks = [| local.FundingPrivKey.PubKey
                        remote.FundingPrivKey.PubKey |]
    Scripts.multiSigOfM_2(true) (fundingPks)

let commitmentInputScriptCoin =
    Coin(fundingTx.GetHash(), 0u, fundingAmount, fundingRedeem.WitHash.ScriptPubKey)
    |> fun c -> ScriptCoin(c, fundingRedeem)

log (sprintf "local payment basepoint is %A" local.PaymentBasePoint)
log (sprintf "remote payment basepoint is %A" remote.PaymentBasePoint)
let obscuredTxNumber =
    let commitmentNumber = CommitmentNumber(UInt48.FromUInt64 42UL)
    commitmentNumber.Obscure true local.PaymentBasePoint remote.PaymentBasePoint
Expect.equal obscuredTxNumber (0x2bb038521914UL ^^^ 42UL |> UInt48.FromUInt64 |> ObscuredCommitmentNumber) ""

sprintf "local_payment_basepoint: %A " local.PaymentBasePoint |> log
sprintf "remote_payment_basepoint: %A" remote.PaymentBasePoint |> log
sprintf "local_funding_privkey: %A" local.FundingPrivKey |> log
sprintf "local_funding_pubkey: %A" local.FundingPrivKey.PubKey |> log
sprintf "remote_funding_privkey: %A" remote.FundingPrivKey |> log
sprintf "remote_funding_pubkey: %A" remote.FundingPrivKey.PubKey |> log
sprintf "local_secretkey: %A" local.PaymentPrivKey |> log
sprintf "localkey: %A" local.PaymentPrivKey.PubKey|> log
sprintf "remotekey: %A" remote.PaymentPrivKey |> log
sprintf "local_delayedkey: %A" local.DelayedPaymentPrivKey.PubKey |> log
sprintf "local_revocation_key: %A" local.RevocationPubKey|> log
sprintf "# funding wscript = %A" fundingRedeem |> log
assert(fundingRedeem = Script.FromBytesUnsafe(hex.DecodeData "5221023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb21030e9f7b623d2ccc7c9bd44d66d5ce21ce504c0acf6385a132cec6d3c39fa711c152ae"))

let paymentPreImages =
    let _s = ([
            ("0000000000000000000000000000000000000000000000000000000000000000")
            ("0101010101010101010101010101010101010101010101010101010101010101")
            ("0202020202020202020202020202020202020202020202020202020202020202")
            ("0303030303030303030303030303030303030303030303030303030303030303")
            ("0404040404040404040404040404040404040404040404040404040404040404")
        ])
    _s |> List.map(hex.DecodeData) |> List.map(PaymentPreimage.Create)
    
type h = DirectedHTLC

log (sprintf "first payment hash is %A" paymentPreImages.[0].Hash)
let htlcs = [
    { DirectedHTLC.Direction = In;
      Add = { UpdateAddHTLCMsg.ChannelId = ChannelId.Zero;
              HTLCId = HTLCId.Zero;
              Amount = LNMoney.MilliSatoshis 1000000L
              PaymentHash = paymentPreImages.[0].Hash
              CLTVExpiry = 500u |> BlockHeight;
              OnionRoutingPacket = OnionPacket.LastPacket } }
    { DirectedHTLC.Direction = In;
      Add = { UpdateAddHTLCMsg.ChannelId = ChannelId.Zero;
              HTLCId = HTLCId(1UL);
              Amount = LNMoney.MilliSatoshis 2000000L
              PaymentHash = paymentPreImages.[1].Hash
              CLTVExpiry = 501u |> BlockHeight;
              OnionRoutingPacket = OnionPacket.LastPacket } }
    { DirectedHTLC.Direction = Out;
      Add = { UpdateAddHTLCMsg.ChannelId = ChannelId.Zero;
              HTLCId = HTLCId(2UL);
              Amount = LNMoney.MilliSatoshis 2000000L
              PaymentHash = paymentPreImages.[2].Hash
              CLTVExpiry = 502u |> BlockHeight;
              OnionRoutingPacket = OnionPacket.LastPacket } }
    { DirectedHTLC.Direction = Out;
      Add = { UpdateAddHTLCMsg.ChannelId = ChannelId.Zero;
              HTLCId = HTLCId(3UL);
              Amount = LNMoney.MilliSatoshis 3000000L
              PaymentHash = paymentPreImages.[3].Hash
              CLTVExpiry = 503u |> BlockHeight;
              OnionRoutingPacket = OnionPacket.LastPacket } }
    { DirectedHTLC.Direction = In;
      Add = { UpdateAddHTLCMsg.ChannelId = ChannelId.Zero;
              HTLCId = HTLCId(4UL);
              Amount = LNMoney.MilliSatoshis 4000000L
              PaymentHash = paymentPreImages.[4].Hash
              CLTVExpiry = 504u |> BlockHeight;
              OnionRoutingPacket = OnionPacket.LastPacket } }
]

let htlcScripts =
    htlcs
    |> List.map(fun htlc -> match htlc.Direction with
                            | Out -> Scripts.htlcOffered
                                        local.PaymentPrivKey.PubKey
                                        (remote.PaymentPrivKey.PubKey)
                                        (local.RevocationPubKey)
                                        (htlc.Add.PaymentHash)
                            | In -> Scripts.htlcReceived
                                        (local.PaymentPrivKey.PubKey)
                                        (remote.PaymentPrivKey.PubKey)
                                        (local.RevocationPubKey)
                                        (htlc.Add.PaymentHash)
                                        (htlc.Add.CLTVExpiry.Value))
let run (spec: CommitmentSpec): (Transaction * _) =
    let local = getLocal()
    log (sprintf "to_local_msat %A" spec.ToLocal)
    log (sprintf "to_remote_msat %A" spec.ToRemote)
    log (sprintf "local_feerate_per_kw %A" spec.FeeRatePerKw)
    
    let commitTx =
        let commitTx = Transactions.makeCommitTx
                         (commitmentInputScriptCoin)
                         (local.CommitTxNumber)
                         (local.PaymentBasePoint)
                         (remote.PaymentBasePoint)
                         (true)
                         (local.DustLimit)
                         (local.RevocationPubKey)
                         (local.ToSelfDelay)
                         (local.DelayedPaymentPrivKey.PubKey)
                         (remote.PaymentPrivKey.PubKey)
                         (local.PaymentPrivKey.PubKey)
                         (remote.PaymentPrivKey.PubKey)
                         (spec)
                         (n)
        // test vectors requires us to use RFC6974
        commitTx.Value.Settings.UseLowR <- false
        let localSig, tx2 = Transactions.sign(commitTx, local.FundingPrivKey)
        let remoteSig, tx3 = Transactions.sign(tx2, remote.FundingPrivKey)
        Transactions.checkSigAndAdd (tx3) (localSig) (local.FundingPrivKey.PubKey)
        >>= fun tx4 ->
            Transactions.checkSigAndAdd (tx4) (remoteSig) (remote.FundingPrivKey.PubKey)
        |> function Ok e -> e | Error e -> failwithf "%A" e
    let baseFee = Transactions.commitTxFee(local.DustLimit)(spec)
    log (sprintf "base commitment transaction fee is %A" baseFee)
    let actualFee = fundingAmount - match commitTx.Value.TryGetFee() with
                                    | true, f -> f | false, _ -> failwith "fail: BOLT3 test, couldn't get fee"
    log (sprintf "actual commitment tx fee is %A " actualFee)
    commitTx.Value.GetGlobalTransaction().Outputs
        |> List.ofSeq
        |> List.iter(fun txOut -> match txOut.ScriptPubKey.Length with
                                  | 22 -> log(sprintf "to-remote amount %A P2WPKH(%A)" (txOut.Value) (remote.PaymentPrivKey.PubKey))
                                  | 34 ->
                                      let maybeIndex = htlcScripts |> List.tryFindIndex(fun s -> s.WitHash.ScriptPubKey = txOut.ScriptPubKey)
                                      match maybeIndex with
                                      | None ->
                                          (sprintf "to-local amount %A. \n to-local wscript (%A)"
                                               txOut.Value
                                               (Scripts.toLocalDelayed(local.RevocationPubKey)
                                                                      (local.ToSelfDelay)
                                                                      (local.DelayedPaymentPrivKey.PubKey)))
                                          |> log
                                      | Some i ->
                                          (sprintf "to-local amount %A \n to-local wscript (%A)" txOut.Value htlcScripts.[i])
                                          |> log
                                  | x -> failwithf "unexpected scriptPubKey length %A" x)
    let actualCommitTxNumOpt =
        Transactions.getCommitTxNumber
            (commitTx.Value.GetGlobalTransaction())
            (true)
            (local.PaymentBasePoint)
            (remote.PaymentBasePoint)
    let expectedCommitTxNumber = local.CommitTxNumber
    Expect.equal actualCommitTxNumOpt.Value (expectedCommitTxNumber) ""
    commitTx.Value.Finalize() |> ignore
    Expect.isTrue (commitTx.Value.CanExtractTransaction()) ""
    sprintf "output commit_tx %A" commitTx.Value |> log
    let (unsignedHTLCTimeoutTxs, unsignedHTLCSuccessTxs) =
        Transactions.makeHTLCTxs(commitTx.Value.ExtractTransaction())
                                 local.DustLimit
                                 local.RevocationPubKey
                                 local.ToSelfDelay
                                 local.DelayedPaymentPrivKey.PubKey
                                 local.PaymentPrivKey.PubKey
                                 remote.PaymentPrivKey.PubKey
                                 spec
                                 n
        |> Result.defaultWith(fun _ -> failwith "fail(BOLT3 transactions): couldn't make HTLC transactions")
    let htlcTxs =
        Seq.append (unsignedHTLCSuccessTxs |> Seq.cast<IHTLCTx>) (unsignedHTLCTimeoutTxs |> Seq.cast<IHTLCTx>)
        // test vectors requires us to use RFC6974
        |> Seq.map(fun htlc -> htlc.Value.Settings.UseLowR <- false; htlc)
    sprintf "num htlcs: %A" htlcTxs |> log
    let htlcTxs = htlcTxs
                  |> Seq.toList
                  |> List.sortBy(fun x -> x.Value.GetGlobalTransaction().Inputs.[0].PrevOut.N)
    let signedTxs =
        htlcTxs
        |> List.map(fun htlcTx -> match htlcTx with
                                  | :? HTLCSuccessTx as tx ->
                                      let localSig, tx2 = Transactions.sign(tx, local.PaymentPrivKey)
                                      let remoteSig, _tx3 = Transactions.sign(tx2, remote.PaymentPrivKey)
                                      // just checking preimage is in global list
                                      let paymentPreimage = (paymentPreImages |> List.find(fun p -> p.Hash = tx.PaymentHash))
                                      log (sprintf "Finalizing %A" tx)
                                      match tx.Finalize(localSig, remoteSig, paymentPreimage) with
                                      | Ok tx -> tx
                                      | Error e -> failwithf "%A" e
                                  | :? HTLCTimeoutTx as tx ->
                                      let localSig, _ = Transactions.sign(tx, local.PaymentPrivKey)
                                      let remoteSig, _ = Transactions.sign(tx, remote.PaymentPrivKey)
                                      match tx.Finalize(localSig, remoteSig) with
                                      | Ok tx -> tx
                                      | Error e -> failwithf "%A" e
                                  | _ -> failwith "unreachable")
    commitTx.Value.ExtractTransaction(), signedTxs

let testVectors = data1.RootElement.GetProperty("test_vectors").EnumerateArray() |> Seq.toArray

let htlcMap = htlcs |> List.map(fun htlc ->  htlc.Add.HTLCId, htlc) |> Map.ofList

let runTest(testCase: JsonElement) (spec) (expectedOutputCount) =
    log (sprintf "testing %A" (testCase.GetProperty("name").GetString()))
    let commitTx, htlcTxs = run (spec)
    Expect.equal(commitTx.Outputs.Count) (expectedOutputCount) ""
    let expectedTx = testCase.GetProperty("output_commit_tx").GetString()
    Expect.equal (commitTx.ToHex()) (expectedTx) ""
    let expectedHTLC = testCase.GetProperty("htlc_output_txs").EnumerateArray()
    expectedHTLC
        |> Seq.iteri( fun i htlc ->
            Expect.equal (htlcTxs.[i].ToHex()) (htlc.GetProperty("value").GetString())""
        )

let specBase = { CommitmentSpec.HTLCs = htlcMap; FeeRatePerKw = 15000u |> FeeRatePerKw;
                 ToLocal = LNMoney.MilliSatoshis(6988000000L); ToRemote =  3000000000L |> LNMoney.MilliSatoshis}
[<Tests>]
let tests =
    testList "Transaction test vectors" [
        testCase "simple commitment tx with no HTLCs" <| fun _ ->
            let spec = { CommitmentSpec.HTLCs = Map.empty; FeeRatePerKw = 15000u |> FeeRatePerKw;
                         ToLocal = LNMoney.MilliSatoshis(7000000000L); ToRemote =  3000000000L |> LNMoney.MilliSatoshis}
            let commitTx, _htlcTxs = run(spec)
            let testCase = testVectors.[0]
            Expect.equal(commitTx.Outputs.Count) (2) ""
            let expectedTx =
                testCase.GetProperty("output_commit_tx").GetString()
            Expect.equal(commitTx.ToHex()) (expectedTx) ""
            
        testCase "commitment tx with all 5 htlcs untrimmed (minimum feerate)"  <| fun _ ->
            let testCase = testVectors.[1]
            let spec = { CommitmentSpec.HTLCs = htlcMap
                         FeeRatePerKw = 0u |> FeeRatePerKw
                         ToLocal = 6988000000L |> LNMoney.MilliSatoshis
                         ToRemote = 3000000000L |> LNMoney.MilliSatoshis }
            runTest (testCase) (spec) (7)
            
        testCase "commitment tx with seven outputs untrimmed (maximum feerate)" <| fun _ ->
            let testCase = testVectors.[2]
            let spec = { CommitmentSpec.HTLCs = htlcMap
                         FeeRatePerKw = (454999UL / Constants.HTLC_SUCCESS_WEIGHT) |> uint32 |> FeeRatePerKw
                         ToLocal = 6988000000L |> LNMoney.MilliSatoshis
                         ToRemote = 3000000000L |> LNMoney.MilliSatoshis }
            runTest (testCase) (spec) (7)
            
        testCase "commitment tx with six outputs untrimmed (minimum feerate)" <| fun _ ->
            let testCase = testVectors.[3]
            let spec = { specBase with
                             FeeRatePerKw = (454999UL / Constants.HTLC_SUCCESS_WEIGHT)
                                            |> ((+)1UL) |> uint32 |> FeeRatePerKw }
            runTest (testCase) (spec) (6)
            
        testCase "commitment tx with six outputs untrimmed (maximum feerate)" <| fun _ ->
            let testCase = testVectors.[4]
            let spec = { specBase with
                            FeeRatePerKw = (1454999UL / Constants.HTLC_SUCCESS_WEIGHT)
                                           |> uint32 |> FeeRatePerKw }
            runTest (testCase) (spec) (6)
            
        testCase "commitment tx with five outputs untrimmed (minimum feerate)" <| fun _ ->
            let testCase = testVectors.[5]
            let spec = { specBase with
                            FeeRatePerKw = (1454999UL / Constants.HTLC_SUCCESS_WEIGHT)
                                           |> ((+)1UL)|> uint32 |> FeeRatePerKw }
            runTest (testCase) (spec) (5)
            
        testCase "commitment tx with five outputs untrimmed (maximum feerate)" <| fun _ ->
            let testCase = testVectors.[6]
            let spec = { specBase with
                            FeeRatePerKw = (1454999UL / Constants.HTLC_TIMEOUT_WEIGHT)
                                           |> uint32 |> FeeRatePerKw }
            runTest (testCase) (spec) (5)
            
        testCase "commitment tx with four outputs untrimmed (minimum feerate)" <| fun _ ->
            let testCase = testVectors.[7]
            let spec = { specBase with
                            FeeRatePerKw = (1454999UL / Constants.HTLC_TIMEOUT_WEIGHT)
                                           |> ((+)1UL) |> uint32 |> FeeRatePerKw }
            runTest (testCase) (spec) (4)
            
        testCase "commitment tx with four outputs untrimmed (maximum feerate)" <| fun _ ->
            let testCase = testVectors.[8]
            let spec = { specBase with
                            FeeRatePerKw = (2454999UL / Constants.HTLC_TIMEOUT_WEIGHT)
                                           |> uint32 |> FeeRatePerKw }
            runTest (testCase) (spec) (4)
            
        testCase "commitment tx with three outputs untrimmed (minimum feerate)" <| fun _ ->
            let feeRate = 2454999UL / Constants.HTLC_TIMEOUT_WEIGHT
            let spec = { CommitmentSpec.HTLCs = htlcMap
                         FeeRatePerKw = feeRate + 1UL |> uint32 |> FeeRatePerKw
                         ToLocal = 6988000000L |> LNMoney.MilliSatoshis
                         ToRemote = 3000000000L |> LNMoney.MilliSatoshis }
            let testCase = testVectors.[9]
            runTest (testCase) (spec) (3)
            
        testCase "commitment tx with three outputs untrimmed (maximum feerate)" <| fun _ ->
            let feeRate = 3454999UL / Constants.HTLC_SUCCESS_WEIGHT
            let spec = { CommitmentSpec.HTLCs = htlcMap
                         FeeRatePerKw = feeRate |> uint32 |> FeeRatePerKw
                         ToLocal = 6988000000L |> LNMoney.MilliSatoshis
                         ToRemote = 3000000000L |> LNMoney.MilliSatoshis }
            let testCase = testVectors.[10]
            runTest (testCase) (spec) (3)
            
        testCase "commitment tx with two outputs untrimmed (minimum feerate)" <| fun _ ->
            let feeRate = 3454999UL / Constants.HTLC_SUCCESS_WEIGHT
            let spec = { CommitmentSpec.HTLCs = htlcMap
                         FeeRatePerKw = feeRate + 1UL |> uint32 |> FeeRatePerKw
                         ToLocal = 6988000000L |> LNMoney.MilliSatoshis
                         ToRemote = 3000000000L |> LNMoney.MilliSatoshis }
            let testCase = testVectors.[11]
            runTest (testCase) (spec) (2)
            
        testCase "commitment tx with two outputs untrimmed (maximum feerate)" <| fun _ ->
            let spec = { CommitmentSpec.HTLCs = htlcMap
                         FeeRatePerKw = 9651180u |> FeeRatePerKw
                         ToLocal = 6988000000L |> LNMoney.MilliSatoshis
                         ToRemote = 3000000000L |> LNMoney.MilliSatoshis }
            let testCase = testVectors.[12]
            runTest (testCase) (spec) (2)
            
        testCase "commitment tx with one output untrimmed (minimum feerate)" <| fun _ ->
            let testCase = testVectors.[13]
            let spec = { specBase with
                            FeeRatePerKw = (9651181u |> FeeRatePerKw) }
            runTest (testCase) (spec) (1)
            
        testCase "commitment tx with fee greater than funder amount" <| fun _ ->
            let spec = { CommitmentSpec.HTLCs = htlcMap;
                         FeeRatePerKw = 9651936u |> FeeRatePerKw;
                         ToLocal = 6988000000L |> LNMoney.MilliSatoshis
                         ToRemote = 3000000000L |> LNMoney.MilliSatoshis }
            let testCase = testVectors.[14]
            runTest(testCase) (spec) (1)
            
    ]
