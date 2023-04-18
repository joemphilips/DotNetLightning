module ChannelTests

open System

open Expecto
open NBitcoin

open DotNetLightning.Transactions
open DotNetLightning.Utils
open DotNetLightning.Crypto
open DotNetLightning.Channel
open DotNetLightning.Serialization

[<Tests>]
let tests =
    testList
        "test from channel type"
        [
            testCase "check spendable balance of a channel"
            <| fun _ ->
                let rand = Random()

                let dummyNodeMasterPrivKey =
                    let extKey = ExtKey()
                    NodeMasterPrivKey extKey

                let dummyChannelPrivKeys =
                    dummyNodeMasterPrivKey.ChannelPrivKeys(rand.Next(1, 100))

                let dummyChannelPubKeys =
                    dummyChannelPrivKeys.ToChannelPubKeys()

                let feeRate = FeeRatePerKw(rand.Next(0, 300) |> uint32)
                let fundingAmount = 10_000_000L |> Money.Satoshis
                let localAmount = 2_000_000_000L |> LNMoney

                let remoteAmount =
                    LNMoney.Satoshis fundingAmount.Satoshi - localAmount

                let localCommitmentSpec =
                    {
                        IncomingHTLCs = Map.empty
                        OutgoingHTLCs = Map.empty
                        FeeRatePerKw = feeRate
                        ToLocal = localAmount
                        ToRemote = remoteAmount
                    }

                let remoteCommitmentSpec =
                    {
                        IncomingHTLCs = Map.empty
                        OutgoingHTLCs = Map.empty
                        FeeRatePerKw = feeRate
                        ToLocal = remoteAmount
                        ToRemote = localAmount
                    }

                // Not used in SpendableBalance
                let dummyFundingScriptCoin =
                    let fundingScriptPubKey: Script =
                        Scripts.funding
                            dummyChannelPubKeys.FundingPubKey
                            dummyChannelPubKeys.FundingPubKey

                    let fundingDestination =
                        fundingScriptPubKey.WitHash :> IDestination

                    let fundingTxId = NBitcoin.RandomUtils.GetUInt256()
                    let fundingOutputIndex = uint32(rand.Next(0, 10))

                    let fundingCoin =
                        Coin(
                            fundingTxId,
                            fundingOutputIndex,
                            fundingAmount,
                            fundingDestination.ScriptPubKey
                        )

                    ScriptCoin(fundingCoin, fundingScriptPubKey)

                // Not used in SpendableBalance
                let dummyCommitmentNumber =
                    let uint48 =
                        rand.Next(2, 100) |> uint64 |> UInt48.FromUInt64

                    CommitmentNumber(UInt48.MaxValue - uint48)

                // Not used in SpendableBalance
                let dummyPerCommitmentPoint =
                    let perCommitmentSecret =
                        dummyChannelPrivKeys.CommitmentSeed.DerivePerCommitmentSecret
                            dummyCommitmentNumber

                    perCommitmentSecret.PerCommitmentPoint()

                // Not used in SpendableBalance
                let dummyLocalParams: LocalParams =
                    {
                        DustLimitSatoshis = 546L |> Money.Satoshis
                        MaxHTLCValueInFlightMSat = 10_000_000L |> LNMoney
                        ChannelReserveSatoshis = 1000L |> Money.Satoshis
                        HTLCMinimumMSat = 1000L |> LNMoney
                        ToSelfDelay = 144us |> BlockHeightOffset16
                        MaxAcceptedHTLCs = 1000us
                        Features = FeatureBits.Zero
                        MutualCloseMaxFeeMultiplier = 3
                    }

                let remoteParams: RemoteParams =
                    {
                        DustLimitSatoshis = 546L |> Money.Satoshis
                        MaxHTLCValueInFlightMSat = 10_000_000L |> LNMoney
                        ChannelReserveSatoshis = 1000L |> Money.Satoshis
                        HTLCMinimumMSat = 1000L |> LNMoney
                        ToSelfDelay = 144us |> BlockHeightOffset16
                        MaxAcceptedHTLCs = 1000us
                        Features = FeatureBits.Zero
                    }

                let remoteCommit =
                    {
                        Index = dummyCommitmentNumber
                        Spec = remoteCommitmentSpec
                        TxId = TxId.Zero
                        RemotePerCommitmentPoint = dummyPerCommitmentPoint
                    }

                let localCommit: LocalCommit =
                    let dummyTransaction = Transaction.Create Network.RegTest

                    {
                        Index = dummyCommitmentNumber
                        Spec = localCommitmentSpec
                        PublishableTxs =
                            {
                                CommitTx = FinalizedTx dummyTransaction
                                HTLCTxs = List.Empty
                            }
                        PendingHTLCSuccessTxs = List.Empty
                    }

                let savedChannelState(weAreFunder: bool) =
                    let staticChannelConfig: StaticChannelConfig =
                        {
                            FundingScriptCoin = dummyFundingScriptCoin
                            AnnounceChannel = false
                            RemoteNodeId = dummyNodeMasterPrivKey.NodeId()
                            Network = Network.RegTest
                            IsFunder = weAreFunder
                            FundingTxMinimumDepth = BlockHeightOffset32 6u
                            LocalStaticShutdownScriptPubKey = None
                            RemoteStaticShutdownScriptPubKey = None
                            LocalParams = dummyLocalParams
                            RemoteParams = remoteParams
                            RemoteChannelPubKeys = dummyChannelPubKeys
                        }

                    {
                        StaticChannelConfig = staticChannelConfig
                        RemotePerCommitmentSecrets =
                            PerCommitmentSecretStore.FromSecrets(List.empty)
                        ShortChannelId = None
                        LocalCommit = localCommit
                        RemoteCommit = remoteCommit
                        LocalChanges = LocalChanges.Zero
                        RemoteChanges = RemoteChanges.Zero
                    }

                let commitments =
                    {
                        OriginChannels = Map.empty
                        ProposedLocalChanges = List.empty
                        ProposedRemoteChanges = List.empty
                        LocalNextHTLCId = HTLCId.Zero
                        RemoteNextHTLCId = HTLCId.Zero
                    }

                let spendableBalanceIfWeAreFunder =
                    Channel.SpendableBalanceFromParts
                        (savedChannelState true)
                        None
                        commitments

                let spendableBalanceIfWeAreFundee =
                    Channel.SpendableBalanceFromParts
                        (savedChannelState false)
                        None
                        commitments

                let shouldBeEqualToFees =
                    spendableBalanceIfWeAreFundee
                    - spendableBalanceIfWeAreFunder

                let commitmentFees =
                    let feeRate = localCommit.Spec.FeeRatePerKw
                    let weight = COMMITMENT_TX_BASE_WEIGHT
                    LNMoney.FromMoney <| feeRate.CalculateFeeFromWeight weight

                Expect.equal
                    shouldBeEqualToFees.Satoshi
                    commitmentFees.Satoshi
                    ""

                Expect.equal
                    spendableBalanceIfWeAreFundee.Satoshi
                    (localAmount.Satoshi
                     - remoteParams.ChannelReserveSatoshis.Satoshi)
                    ""
        ]
