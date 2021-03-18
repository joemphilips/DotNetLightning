module TransactionTests

open System
open ResultUtils
open ResultUtils.Portability

open DotNetLightning.Transactions
open DotNetLightning.Transactions.Transactions
open DotNetLightning.Utils
open DotNetLightning.Crypto
open DotNetLightning.Channel
open DotNetLightning.Serialization
open Expecto
open NBitcoin

let n = Network.RegTest

[<Tests>]
let testList = testList "transaction tests" [
    testCase "check fund recovery from local/remote commitment txs" <| fun _ ->
        let rand = Random()

        let localNodeMasterPrivKey =
            let extKey = ExtKey()
            NodeMasterPrivKey extKey
        let localChannelPrivKeys = localNodeMasterPrivKey.ChannelPrivKeys (rand.Next(1, 100))
        let localChannelPubKeys = localChannelPrivKeys.ToChannelPubKeys()
        let localDestPrivKey = new Key()
        let localDestPubKey = localDestPrivKey.PubKey

        let remoteNodeMasterPrivKey =
            let extKey = ExtKey()
            NodeMasterPrivKey extKey
        let remoteChannelPrivKeys = remoteNodeMasterPrivKey.ChannelPrivKeys (rand.Next(1, 100))
        let remoteChannelPubKeys = remoteChannelPrivKeys.ToChannelPubKeys()

        let fundingAmount = 10_000_000L |> Money.Satoshis
        let fundingScriptPubKey =
            Scripts.funding
                localChannelPubKeys.FundingPubKey
                remoteChannelPubKeys.FundingPubKey
        let fundingDestination = fundingScriptPubKey.WitHash :> IDestination
        let fundingTxId = NBitcoin.RandomUtils.GetUInt256()
        let fundingOutputIndex = uint32(rand.Next(0, 10))
        let fundingCoin = Coin(fundingTxId, fundingOutputIndex, fundingAmount, fundingDestination.ScriptPubKey)
        let fundingScriptCoin = ScriptCoin(fundingCoin, fundingScriptPubKey)

        let commitmentNumber =
            let uint48 = rand.Next(1, 100) |> uint64 |> UInt48.FromUInt64
            CommitmentNumber (UInt48.MaxValue - uint48)
        let perCommitmentSecret = localChannelPrivKeys.CommitmentSeed.DerivePerCommitmentSecret commitmentNumber
        let perCommitmentPoint = perCommitmentSecret.PerCommitmentPoint()
        let localCommitmentPubKeys = perCommitmentPoint.DeriveCommitmentPubKeys localChannelPubKeys
        let remoteCommitmentPubKeys = perCommitmentPoint.DeriveCommitmentPubKeys remoteChannelPubKeys

        let localParams : LocalParams = {
            DustLimitSatoshis = 546L |> Money.Satoshis
            MaxHTLCValueInFlightMSat = 10_000_000L |> LNMoney
            ChannelReserveSatoshis = 1000L |> Money.Satoshis
            HTLCMinimumMSat = 1000L |> LNMoney
            ToSelfDelay = 144us |> BlockHeightOffset16
            MaxAcceptedHTLCs = 1000us
            Features = FeatureBits.Zero
        }
        
        let remoteLocalParam : LocalParams = {
            DustLimitSatoshis = 546L |> Money.Satoshis
            MaxHTLCValueInFlightMSat = 10_000_000L |> LNMoney
            ChannelReserveSatoshis = 1000L |> Money.Satoshis
            HTLCMinimumMSat = 1000L |> LNMoney
            ToSelfDelay = 144us |> BlockHeightOffset16
            MaxAcceptedHTLCs = 1000us
            Features = FeatureBits.Zero
        }

        let remoteParam : RemoteParams = {
            DustLimitSatoshis = 546L |> Money.Satoshis
            MaxHTLCValueInFlightMSat = 10_000_000L |> LNMoney
            ChannelReserveSatoshis = 1000L |> Money.Satoshis
            HTLCMinimumMSat = 1000L |> LNMoney
            ToSelfDelay = 144us |> BlockHeightOffset16
            MaxAcceptedHTLCs = 1000us
            Features = FeatureBits.Zero
        }

        let feeRate = FeeRatePerKw (rand.Next(0, 300) |> uint32)
        let localAmount = 2_000_000_000L |> LNMoney
        let remoteAmount = LNMoney.Satoshis(fundingAmount.Satoshi) - localAmount
        let commitmentSpec = {
            IncomingHTLCs = Map.empty
            OutgoingHTLCs = Map.empty
            FeeRatePerKw = feeRate
            ToLocal = localAmount
            ToRemote = remoteAmount
        }

        let staticLocalChannelConfig : StaticChannelConfig = 
            {
                FundingScriptCoin = fundingScriptCoin
                AnnounceChannel = false
                RemoteNodeId = remoteNodeMasterPrivKey.NodeId()
                Network = Network.RegTest
                IsFunder = true
                FundingTxMinimumDepth = BlockHeightOffset32 6u
                LocalStaticShutdownScriptPubKey = None
                RemoteStaticShutdownScriptPubKey = None
                LocalParams = localParams
                RemoteParams = remoteParam
                RemoteChannelPubKeys = remoteChannelPubKeys
            }

        let unsignedCommitmentTx =
            makeCommitTx
                fundingScriptCoin
                commitmentNumber
                localChannelPubKeys.PaymentBasepoint
                remoteChannelPubKeys.PaymentBasepoint
                true
                localParams.DustLimitSatoshis
                remoteCommitmentPubKeys.RevocationPubKey
                localParams.ToSelfDelay
                localCommitmentPubKeys.DelayedPaymentPubKey
                remoteCommitmentPubKeys.PaymentPubKey
                localCommitmentPubKeys.HtlcPubKey
                remoteCommitmentPubKeys.HtlcPubKey
                commitmentSpec
                Network.RegTest
        let commitmentTx =
            unsignedCommitmentTx.Value
                .SignWithKeys(localChannelPrivKeys.FundingPrivKey.RawKey(), remoteChannelPrivKeys.FundingPrivKey.RawKey())
                .Finalize()
                .ExtractTransaction()

        let transactionBuilder =
            ForceCloseFundsRecovery.tryGetFundsFromLocalCommitmentTx
                localChannelPrivKeys
                staticLocalChannelConfig
                commitmentTx
            |> Result.deref

        let recoveryTransaction =
            transactionBuilder
                .SendAll(localDestPubKey)
                .BuildTransaction(true)
        let inputs = recoveryTransaction.Inputs
        Expect.equal inputs.Count 1 "wrong number of inputs"
        let input = inputs.[0]
        Expect.equal input.Sequence.Value (uint32 localParams.ToSelfDelay.Value) "wrong sequence nuber"
        Expect.equal input.PrevOut.Hash (commitmentTx.GetHash()) "wrong prevout hash"
        let expectedAmount =
            let fullAmount = commitmentSpec.ToLocal.ToMoney()
            let fee = commitmentTx.GetFee [| fundingScriptCoin |]
            fullAmount - fee
        let actualAmount =
            commitmentTx.Outputs.[input.PrevOut.N].Value
        Expect.equal actualAmount expectedAmount "wrong prevout amount"

        let remoteDestPrivKey = new Key()
        let remoteDestPubKey = remoteDestPrivKey.PubKey
        let remoteRemotePerCommitmentSecrets =
            let rec addKeys (remoteRemotePerCommitmentSecrets: PerCommitmentSecretStore)
                            (currentCommitmentNumber: CommitmentNumber)
                                : PerCommitmentSecretStore =
                if currentCommitmentNumber = commitmentNumber then
                    remoteRemotePerCommitmentSecrets
                else
                    let currentPerCommitmentSecret =
                        localChannelPrivKeys.CommitmentSeed.DerivePerCommitmentSecret
                            currentCommitmentNumber
                    let nextLocalPerCommitmentSecretsRes =
                        remoteRemotePerCommitmentSecrets.InsertPerCommitmentSecret
                            currentCommitmentNumber
                            currentPerCommitmentSecret
                    addKeys
                        (Result.deref nextLocalPerCommitmentSecretsRes)
                        (currentCommitmentNumber.NextCommitment())
            addKeys (PerCommitmentSecretStore()) CommitmentNumber.FirstCommitment

        let remoteRemoteCommit = {
            Index = commitmentNumber
            Spec = commitmentSpec
            TxId = TxId <| commitmentTx.GetHash()
            RemotePerCommitmentPoint = perCommitmentPoint
        }
        let remoteRemoteParams = {
            DustLimitSatoshis = localParams.DustLimitSatoshis
            MaxHTLCValueInFlightMSat = localParams.MaxHTLCValueInFlightMSat
            ChannelReserveSatoshis = localParams.ChannelReserveSatoshis
            HTLCMinimumMSat = localParams.HTLCMinimumMSat
            ToSelfDelay = localParams.ToSelfDelay
            MaxAcceptedHTLCs = localParams.MaxAcceptedHTLCs
            Features = localParams.Features
        }

        let staticRemoteChannelConfig : StaticChannelConfig = 
            {
                FundingScriptCoin = fundingScriptCoin
                AnnounceChannel = false
                RemoteNodeId = localNodeMasterPrivKey.NodeId()
                Network = Network.RegTest
                IsFunder = false
                FundingTxMinimumDepth = BlockHeightOffset32 6u
                LocalStaticShutdownScriptPubKey = None
                RemoteStaticShutdownScriptPubKey = None
                LocalParams = remoteLocalParam
                RemoteParams = remoteRemoteParams
                RemoteChannelPubKeys = localChannelPubKeys
            }

        let remoteCommitmentSpec = {
            IncomingHTLCs = Map.empty
            OutgoingHTLCs = Map.empty
            FeeRatePerKw = feeRate
            ToLocal = remoteAmount
            ToRemote = localAmount
        }
        let remoteLocalCommit : LocalCommit =
            {
                Index = commitmentNumber
                Spec = remoteCommitmentSpec
                PublishableTxs = {
                    CommitTx = FinalizedTx commitmentTx
                    HTLCTxs = List.Empty
                }
                PendingHTLCSuccessTxs = List.Empty
            }

        let remoteSavedChannelState : SavedChannelState = {
            StaticChannelConfig = staticRemoteChannelConfig
            RemotePerCommitmentSecrets = remoteRemotePerCommitmentSecrets
            ShortChannelId = None
            LocalCommit = remoteLocalCommit
            RemoteCommit = remoteRemoteCommit 
        }

        let transactionBuilder =
            ForceCloseFundsRecovery.tryGetFundsFromRemoteCommitmentTx
                remoteChannelPrivKeys
                remoteSavedChannelState
                commitmentTx
            |> Result.deref

        let recoveryTransaction =
            transactionBuilder
                .SendAll(remoteDestPubKey)
                .BuildTransaction(true)
        let inputs = recoveryTransaction.Inputs
        Expect.equal inputs.Count 1 "wrong number of inputs"
        let input = inputs.[0]
        Expect.equal input.PrevOut.Hash (commitmentTx.GetHash()) "wrong prevout hash"
        let expectedAmount = commitmentSpec.ToRemote.ToMoney()
        let actualAmount =
            commitmentTx.Outputs.[input.PrevOut.N].Value
        Expect.equal actualAmount expectedAmount "wrong prevout amount"

        let transactionBuilder =
            ForceCloseFundsRecovery.createPenaltyTx
                perCommitmentSecret
                remoteSavedChannelState
                remoteChannelPrivKeys
        let penaltyTransaction =
            transactionBuilder
                .SendAll(remoteDestPubKey)
                .BuildTransaction(true)
        let inputs = penaltyTransaction.Inputs
        Expect.equal inputs.Count 2 "wrong number of inputs"
        Expect.equal inputs.[0].PrevOut.Hash (commitmentTx.GetHash()) "wrong prevout hash on input 0"
        Expect.equal inputs.[1].PrevOut.Hash (commitmentTx.GetHash()) "wrong prevout hash on input 1"

        let expectedAmountFromToLocal =
            let localAmount = commitmentSpec.ToLocal.ToMoney()
            let fee = commitmentTx.GetFee [| fundingScriptCoin |]
            localAmount - fee
        let expectedAmountFromToRemote =
            commitmentSpec.ToRemote.ToMoney()

        let actualAmount0 =
            commitmentTx.Outputs.[inputs.[0].PrevOut.N].Value
        let actualAmount1 =
            commitmentTx.Outputs.[inputs.[1].PrevOut.N].Value

        if actualAmount0 = expectedAmountFromToLocal then
            Expect.equal actualAmount1 expectedAmountFromToRemote "wrong prevout amount for to_remote"
        elif actualAmount0 = expectedAmountFromToRemote then
            Expect.equal actualAmount1 expectedAmountFromToLocal "wrong prevout amount for to_local"
        else
            failwith "amount of input 0 does not match either expected amount"

    (*
    testCase "check pre-computed transaction weights" <| fun _ ->
        let localPaymentPriv = [| for _ in 0..31 -> 0xdduy |] |> fun b -> new Key(b)
        let finalSpk =
            let s = [| for _ in 0..31 -> 0xfeuy |] |> fun b -> new Key(b)
            s.PubKey.WitHash
        let localDustLimit = 546L |> Money.Satoshis
        let feeRatePerKw = 1000u |> FeeRatePerKw
        
        let _ =
            let pubkeyScript = localPaymentPriv.PubKey.WitHash.ScriptPubKey
            let commitTx =
                let t = n.CreateTransaction()
                t.Version <- 0u
                t.Outputs.Add(TxOut(Money.Satoshis(20000L), pubkeyScript)) |> ignore
                t.LockTime <- LockTime.Zero
                t
            let claimP2WPKHOutputTx =
                Transactions.makeClaimP2WPKHOutputTx(commitTx)
                                                    (localDustLimit)
                                                    (PaymentPubKey localPaymentPriv.PubKey)
                                                    (finalSpk)
                                                    (feeRatePerKw)
                                                    n |> Result.defaultWith (fun _  -> failwith "fail: precomputed tx weights")
            let weight =
                let tx = claimP2WPKHOutputTx.Value.GetGlobalTransaction()
                let witScript =
                    let dummySig = [| for _ in 0..70 -> 0xbbuy |]
                    let dummyPk = (new Key()).PubKey.ToBytes()
                    let dummy = seq[ Op.GetPushOp(dummySig); Op.GetPushOp(dummyPk)]
                    Script(dummy).ToWitScript()
                tx.Inputs.[0].WitScript <- witScript
                tx.GetVirtualSize() |> uint64
            Expect.equal(Constants.CLAIM_P2WPKH_OUTPUT_WEIGHT) (weight) ""
            ()
            
        ()
    *)
]
