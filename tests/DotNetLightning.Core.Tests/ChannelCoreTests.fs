module ChannelCoreTests

open DotNetLightning.Chain
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Channel
open DotNetLightning.Utils
open Expecto
open NBitcoin

let n = Network.RegTest

[<Tests>]
let tests =
    ptestList "Channel Domain Tests" [
        ptestCase "Should accept accept_channel" <| fun _ ->
            let seed = [| for _ in 0..31 -> 0uy |] |> uint256
            let repo = DefaultKeyRepository(seed) :> IKeysRepository
            let tempChannelId = [| for _ in 0..31 -> 0uy |] |> uint256 |> ChannelId
            let keys = repo.GetChannelKeys(false)
            let openChannel = { OpenChannel.Chainhash = n.GenesisHash
                                TemporaryChannelId = tempChannelId
                                FundingSatoshis = 700000L |> Money.Satoshis
                                PushMSat = 300000L |> LNMoney.MilliSatoshis
                                DustLimitSatoshis = Money.Satoshis (546L)
                                MaxHTLCValueInFlightMsat = 40000000L |> LNMoney.MilliSatoshis
                                ChannelReserveSatoshis = 300000L |> Money.Satoshis
                                HTLCMinimumMsat = 1000L |> LNMoney.MilliSatoshis
                                FeeRatePerKw = 1000u |> FeeRatePerKw
                                ToSelfDelay = 720us |> BlockHeightOffset
                                MaxAcceptedHTLCs = 400us
                                FundingPubKey = keys.FundingKey.PubKey
                                RevocationBasepoint = keys.RevocationBaseKey.PubKey
                                PaymentBasepoint = keys.PaymentBaseKey.PubKey
                                DelayedPaymentBasepoint = keys.DelayedPaymentBaseKey.PubKey
                                HTLCBasepoint = keys.HTLCBaseKey.PubKey
                                FirstPerCommitmentPoint =
                                    ChannelUtils.buildCommitmentPoint (keys.CommitmentSeed, 0UL)
                                ChannelFlags = 0uy
                                ShutdownScriptPubKey = None }
            let localParams = { LocalParams.GlobalFeatures = GlobalFeatures.Flags[||]
                                NodeId = failwith "todo"
                                ChannelPubKeys = failwith "todo"
                                DustLimitSatoshis = failwith "todo"
                                MaxHTLCValueInFlightMSat = failwith "todo"
                                ChannelReserveSatoshis = failwith "todo"
                                HTLCMinimumMSat = failwith "todo"
                                ToSelfDelay = failwith "todo"
                                MaxAcceptedHTLCs = failwith "todo"
                                IsFunder = failwith "todo"
                                DefaultFinalScriptPubKey = failwith "todo"
                                LocalFeatures = failwith "todo" }
            let remoteInit = { Init.GlobalFeatures = GlobalFeatures.Flags[||]
                               LocalFeatures = failwith "todo" }
            let state = ChannelState.WaitForAcceptChannel { WaitForAcceptChannelData.LastSent = openChannel
                                                            InputInitFunder =
                                                                InputInitFunder.FromOpenChannel
                                                                    (localParams)
                                                                    (remoteInit)
                                                                    (keys)
                                                                    (openChannel) }
            let c = { Channel.State = state
                      Config = failwith "todo"
                      ChainListener = failwith "todo"
                      KeysRepository = failwith "todo"
                      FeeEstimator = failwith "todo"
                      FundingTxProvider = failwith "todo"
                      Logger = failwith "todo"
                      RemoteNodeId = failwith "todo"
                      LocalNodeSecret = failwith "todo"
                      Network = failwith "todo"
                      Secp256k1Context = failwith "todo" }
            ()
    ]
