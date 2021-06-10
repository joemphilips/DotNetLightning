namespace DotNetLightning.Channel

open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Utils.Aether
open DotNetLightning.Chain
open DotNetLightning.Crypto
open DotNetLightning.Transactions
open DotNetLightning.Serialization
open DotNetLightning.Serialization.Msgs
open NBitcoin
open System

open ResultUtils
open ResultUtils.Portability

type ChannelWaitingForFundingSigned = {
    ChannelOptions: ChannelOptions
    ChannelPrivKeys: ChannelPrivKeys
    FeeEstimator: IFeeEstimator
    RemoteNodeId: NodeId
    NodeSecret: NodeSecret
    Network: Network
    FundingTxMinimumDepth: BlockHeightOffset32
    WaitForFundingSignedData: WaitForFundingSignedData
    LocalShutdownScriptPubKey: Option<ShutdownScriptPubKey>
} with
    member self.ApplyFundingSigned (msg: FundingSignedMsg)
                                       : Result<FinalizedTx * Channel, ChannelError> = result {
        let state = self.WaitForFundingSignedData
        let remoteChannelKeys = state.RemoteParams.ChannelPubKeys
        let! finalizedLocalCommitTx =
            let theirFundingPk = remoteChannelKeys.FundingPubKey.RawPubKey()
            let _, signedLocalCommitTx =
                self.ChannelPrivKeys.SignWithFundingPrivKey state.LocalCommitTx.Value
            let remoteSigPairOfLocalTx = (theirFundingPk,  TransactionSignature(msg.Signature.Value, SigHash.All))
            let sigPairs = seq [ remoteSigPairOfLocalTx; ]
            Transactions.checkTxFinalized signedLocalCommitTx state.LocalCommitTx.WhichInput sigPairs |> expectTransactionError
        let commitments = {
            Commitments.LocalParams = state.LocalParams
            RemoteParams = state.RemoteParams
            ChannelFlags = state.ChannelFlags
            FundingScriptCoin =
                let amount =
                    let index = int state.LastSent.FundingOutputIndex.Value
                    state.FundingTx.Value.Outputs.[index].Value
                ChannelHelpers.getFundingScriptCoin
                    state.LocalParams.ChannelPubKeys.FundingPubKey
                    remoteChannelKeys.FundingPubKey
                    state.LastSent.FundingTxId
                    state.LastSent.FundingOutputIndex
                    amount
            LocalCommit = {
                Index = CommitmentNumber.FirstCommitment
                Spec = state.LocalSpec
                PublishableTxs = {
                    PublishableTxs.CommitTx = finalizedLocalCommitTx
                    HTLCTxs = []
                }
                PendingHTLCSuccessTxs = []
            }
            RemoteCommit = state.RemoteCommit
            LocalChanges = LocalChanges.Zero
            RemoteChanges = RemoteChanges.Zero
            LocalNextHTLCId = HTLCId.Zero
            RemoteNextHTLCId = HTLCId.Zero
            OriginChannels = Map.empty
            // we will receive their next per-commitment point in the next msg, so we temporarily put a random byte array
            RemoteNextCommitInfo =
                let dummyBytes = DataEncoders.HexEncoder().DecodeData("0101010101010101010101010101010101010101010101010101010101010101")
                let dummyKey = new Key(dummyBytes)
                dummyKey.PubKey
                |> PerCommitmentPoint
                |> RemoteNextCommitInfo.Revoked
            RemotePerCommitmentSecrets = PerCommitmentSecretStore()
        }
        let nextState = WaitForFundingConfirmed {
            Deferred = None
            LastSent = Choice1Of2 state.LastSent
            InitialFeeRatePerKw = state.InitialFeeRatePerKw
        }
        let channel = {
            ChannelOptions = self.ChannelOptions
            ChannelPrivKeys = self.ChannelPrivKeys
            FeeEstimator = self.FeeEstimator
            RemoteNodeId = self.RemoteNodeId
            NodeSecret = self.NodeSecret
            State = nextState
            Network = self.Network
            FundingTxMinimumDepth = self.FundingTxMinimumDepth
            LocalShutdownScriptPubKey = self.LocalShutdownScriptPubKey
            Commitments = commitments
        }
        return state.FundingTx, channel
    }

and ChannelWaitingForFundingCreated = {
    ChannelOptions: ChannelOptions
    ChannelPrivKeys: ChannelPrivKeys
    FeeEstimator: IFeeEstimator
    RemoteNodeId: NodeId
    NodeSecret: NodeSecret
    Network: Network
    FundingTxMinimumDepth: BlockHeightOffset32
    LocalShutdownScriptPubKey: Option<ShutdownScriptPubKey>
    WaitForFundingCreatedData: WaitForFundingCreatedData
} with
    member self.ApplyFundingCreated (msg: FundingCreatedMsg)
                                        : Result<FundingSignedMsg * Channel, ChannelError> = result {
        let state = self.WaitForFundingCreatedData
        let remoteChannelKeys = state.RemoteParams.ChannelPubKeys
        let! (localSpec, localCommitTx, remoteSpec, remoteCommitTx) =
            ChannelHelpers.makeFirstCommitTxs
                state.LocalParams
                state.RemoteParams
                state.FundingSatoshis
                state.PushMSat
                state.InitialFeeRatePerKw
                msg.FundingOutputIndex
                msg.FundingTxId
                state.LastSent.FirstPerCommitmentPoint
                state.RemoteFirstPerCommitmentPoint
                self.Network
        assert (localCommitTx.Value.IsReadyToSign())
        let _s, signedLocalCommitTx =
            self.ChannelPrivKeys.SignWithFundingPrivKey localCommitTx.Value
        let remoteTxSig = TransactionSignature(msg.Signature.Value, SigHash.All)
        let theirSigPair = (remoteChannelKeys.FundingPubKey.RawPubKey(), remoteTxSig)
        let sigPairs = seq [ theirSigPair ]
        let! finalizedCommitTx =
            Transactions.checkTxFinalized (signedLocalCommitTx) (localCommitTx.WhichInput) sigPairs
            |> expectTransactionError
        let localSigOfRemoteCommit, _ =
            self.ChannelPrivKeys.SignWithFundingPrivKey remoteCommitTx.Value
        let commitments = {
            LocalParams = state.LocalParams
            RemoteParams = state.RemoteParams
            ChannelFlags = state.ChannelFlags
            FundingScriptCoin =
                ChannelHelpers.getFundingScriptCoin
                    state.LocalParams.ChannelPubKeys.FundingPubKey
                    remoteChannelKeys.FundingPubKey
                    msg.FundingTxId
                    msg.FundingOutputIndex
                    state.FundingSatoshis
            LocalCommit = {
                Index = CommitmentNumber.FirstCommitment
                Spec = localSpec
                PublishableTxs = {
                    PublishableTxs.CommitTx = finalizedCommitTx
                    HTLCTxs = []
                }
                PendingHTLCSuccessTxs = []
            }
            RemoteCommit = {
                Index = CommitmentNumber.FirstCommitment
                Spec = remoteSpec
                TxId = remoteCommitTx.Value.GetGlobalTransaction().GetTxId()
                RemotePerCommitmentPoint = state.RemoteFirstPerCommitmentPoint
            }
            LocalChanges = LocalChanges.Zero
            RemoteChanges = RemoteChanges.Zero
            LocalNextHTLCId = HTLCId.Zero
            RemoteNextHTLCId = HTLCId.Zero
            OriginChannels = Map.empty
            RemoteNextCommitInfo =
                let dummyBytes = DataEncoders.HexEncoder().DecodeData("0101010101010101010101010101010101010101010101010101010101010101")
                let dummyKey = new Key(dummyBytes)
                dummyKey.PubKey
                |> PerCommitmentPoint
                |> RemoteNextCommitInfo.Revoked
            RemotePerCommitmentSecrets = PerCommitmentSecretStore()
        }
        let channelId = commitments.ChannelId()
        let msgToSend: FundingSignedMsg = {
            ChannelId = channelId
            Signature = !>localSigOfRemoteCommit.Signature
        }
        let nextState = WaitForFundingConfirmed {
            Deferred = None
            LastSent = msgToSend |> Choice2Of2
            InitialFeeRatePerKw = state.InitialFeeRatePerKw
        }
        let channel = {
            ChannelOptions = self.ChannelOptions
            ChannelPrivKeys = self.ChannelPrivKeys
            FeeEstimator = self.FeeEstimator
            RemoteNodeId = self.RemoteNodeId
            NodeSecret = self.NodeSecret
            Network = self.Network
            State = nextState
            LocalShutdownScriptPubKey = self.LocalShutdownScriptPubKey
            FundingTxMinimumDepth = self.FundingTxMinimumDepth
            Commitments = commitments
        }
        return msgToSend, channel
    }

and ChannelWaitingForFundingTx = {
    ChannelOptions: ChannelOptions
    ChannelPrivKeys: ChannelPrivKeys
    FeeEstimator: IFeeEstimator
    RemoteNodeId: NodeId
    NodeSecret: NodeSecret
    Network: Network
    LocalShutdownScriptPubKey: Option<ShutdownScriptPubKey>
    WaitForFundingTxData: WaitForFundingTxData
} with
    member self.CreateFundingTx (fundingTx: FinalizedTx)
                                (outIndex: TxOutIndex)
                                    : Result<FundingCreatedMsg * ChannelWaitingForFundingSigned, ChannelError> = result {
        let state = self.WaitForFundingTxData
        let remoteParams = RemoteParams.FromAcceptChannel self.RemoteNodeId (state.InputInitFunder.RemoteInit) state.LastReceived
        let localParams = state.InputInitFunder.LocalParams
        assert (state.LastSent.FundingPubKey = localParams.ChannelPubKeys.FundingPubKey)
        let commitmentSpec = state.InputInitFunder.DeriveCommitmentSpec()
        let commitmentSeed = state.InputInitFunder.ChannelPrivKeys.CommitmentSeed
        let fundingTxId = fundingTx.Value.GetTxId()
        let! (_localSpec, localCommitTx, remoteSpec, remoteCommitTx) =
            ChannelHelpers.makeFirstCommitTxs localParams
                                       remoteParams
                                       state.LastSent.FundingSatoshis
                                       state.LastSent.PushMSat
                                       state.LastSent.FeeRatePerKw
                                       outIndex
                                       fundingTxId
                                       (commitmentSeed.DerivePerCommitmentPoint CommitmentNumber.FirstCommitment)
                                       state.LastReceived.FirstPerCommitmentPoint
                                       self.Network
        let localSigOfRemoteCommit, _ =
            self.ChannelPrivKeys.SignWithFundingPrivKey remoteCommitTx.Value
        let nextMsg: FundingCreatedMsg = {
            TemporaryChannelId = state.LastReceived.TemporaryChannelId
            FundingTxId = fundingTxId
            FundingOutputIndex = outIndex
            Signature = !>localSigOfRemoteCommit.Signature
        }
        let channelId = OutPoint(fundingTxId.Value, uint32 outIndex.Value).ToChannelId()
        let waitForFundingSignedData = {
            Data.WaitForFundingSignedData.ChannelId = channelId
            LocalParams = localParams
            RemoteParams = remoteParams
            Data.WaitForFundingSignedData.FundingTx = fundingTx
            Data.WaitForFundingSignedData.LocalSpec = commitmentSpec
            LocalCommitTx = localCommitTx
            RemoteCommit = {
                RemoteCommit.Index = CommitmentNumber.FirstCommitment
                Spec = remoteSpec
                TxId = remoteCommitTx.Value.GetGlobalTransaction().GetTxId()
                RemotePerCommitmentPoint = state.LastReceived.FirstPerCommitmentPoint
            }
            ChannelFlags = state.InputInitFunder.ChannelFlags
            LastSent = nextMsg
            InitialFeeRatePerKw = state.InputInitFunder.InitFeeRatePerKw
        }
        let channelWaitingForFundingSigned = {
            ChannelOptions = self.ChannelOptions
            ChannelPrivKeys = self.ChannelPrivKeys
            FeeEstimator = self.FeeEstimator
            RemoteNodeId = self.RemoteNodeId
            NodeSecret = self.NodeSecret
            Network = self.Network
            FundingTxMinimumDepth = state.LastReceived.MinimumDepth
            WaitForFundingSignedData = waitForFundingSignedData
            LocalShutdownScriptPubKey = self.LocalShutdownScriptPubKey
        }
        return nextMsg, channelWaitingForFundingSigned
    }


and ChannelWaitingForAcceptChannel = {
    ChannelOptions: ChannelOptions
    ChannelHandshakeLimits: ChannelHandshakeLimits
    ChannelPrivKeys: ChannelPrivKeys
    FeeEstimator: IFeeEstimator
    RemoteNodeId: NodeId
    NodeSecret: NodeSecret
    Network: Network
    LocalShutdownScriptPubKey: Option<ShutdownScriptPubKey>
    WaitForAcceptChannelData: WaitForAcceptChannelData
} with
    member self.ApplyAcceptChannel (msg: AcceptChannelMsg)
                                       : Result<IDestination * Money * ChannelWaitingForFundingTx, ChannelError> = result {
        let state = self.WaitForAcceptChannelData
        do! Validation.checkAcceptChannelMsgAcceptable self.ChannelHandshakeLimits state msg
        let redeem =
            Scripts.funding
                (state.InputInitFunder.ChannelPrivKeys.ToChannelPubKeys().FundingPubKey)
                msg.FundingPubKey
        let destination = redeem.WitHash :> IDestination
        let amount = state.InputInitFunder.FundingSatoshis
        let waitForFundingTxData = {
            InputInitFunder = state.InputInitFunder
            LastSent = state.LastSent
            LastReceived = msg
        }
        let channelWaitingForFundingTx = {
            ChannelOptions = self.ChannelOptions
            ChannelPrivKeys = self.ChannelPrivKeys
            FeeEstimator = self.FeeEstimator
            RemoteNodeId = self.RemoteNodeId
            NodeSecret = self.NodeSecret
            Network = self.Network
            LocalShutdownScriptPubKey = self.LocalShutdownScriptPubKey
            WaitForFundingTxData = waitForFundingTxData
        }
        return destination, amount, channelWaitingForFundingTx
    }

and Channel = {
    ChannelOptions: ChannelOptions
    ChannelPrivKeys: ChannelPrivKeys
    FeeEstimator: IFeeEstimator
    RemoteNodeId: NodeId
    NodeSecret: NodeSecret
    State: ChannelState
    Network: Network
    LocalShutdownScriptPubKey: Option<ShutdownScriptPubKey>
    FundingTxMinimumDepth: BlockHeightOffset32
    Commitments: Commitments
 }
        with
        static member NewOutbound(channelHandshakeLimits: ChannelHandshakeLimits,
                                  channelOptions: ChannelOptions,
                                  nodeMasterPrivKey: NodeMasterPrivKey,
                                  channelIndex: int,
                                  feeEstimator: IFeeEstimator,
                                  network: Network,
                                  remoteNodeId: NodeId,
                                  inputInitFunder: InputInitFunder,
                                  shutdownScriptPubKey: Option<ShutdownScriptPubKey>
                                 ): Result<OpenChannelMsg * ChannelWaitingForAcceptChannel, ChannelError> =
            let openChannelMsgToSend: OpenChannelMsg = {
                Chainhash = network.Consensus.HashGenesisBlock
                TemporaryChannelId = inputInitFunder.TemporaryChannelId
                FundingSatoshis = inputInitFunder.FundingSatoshis
                PushMSat = inputInitFunder.PushMSat
                DustLimitSatoshis = inputInitFunder.LocalParams.DustLimitSatoshis
                MaxHTLCValueInFlightMsat = inputInitFunder.LocalParams.MaxHTLCValueInFlightMSat
                ChannelReserveSatoshis = inputInitFunder.LocalParams.ChannelReserveSatoshis
                HTLCMinimumMsat = inputInitFunder.LocalParams.HTLCMinimumMSat
                FeeRatePerKw = inputInitFunder.InitFeeRatePerKw
                ToSelfDelay = inputInitFunder.LocalParams.ToSelfDelay
                MaxAcceptedHTLCs = inputInitFunder.LocalParams.MaxAcceptedHTLCs
                FundingPubKey = inputInitFunder.ChannelPrivKeys.FundingPrivKey.FundingPubKey()
                RevocationBasepoint = inputInitFunder.ChannelPrivKeys.RevocationBasepointSecret.RevocationBasepoint()
                PaymentBasepoint = inputInitFunder.ChannelPrivKeys.PaymentBasepointSecret.PaymentBasepoint()
                DelayedPaymentBasepoint = inputInitFunder.ChannelPrivKeys.DelayedPaymentBasepointSecret.DelayedPaymentBasepoint()
                HTLCBasepoint = inputInitFunder.ChannelPrivKeys.HtlcBasepointSecret.HtlcBasepoint()
                FirstPerCommitmentPoint = inputInitFunder.ChannelPrivKeys.CommitmentSeed.DerivePerCommitmentPoint CommitmentNumber.FirstCommitment
                ChannelFlags = inputInitFunder.ChannelFlags
                TLVs = [| OpenChannelTLV.UpfrontShutdownScript shutdownScriptPubKey |]
            }
            result {
                do! Validation.checkOurOpenChannelMsgAcceptable openChannelMsgToSend
                let channelPrivKeys = nodeMasterPrivKey.ChannelPrivKeys channelIndex
                let nodeSecret = nodeMasterPrivKey.NodeSecret()
                let waitForAcceptChannelData = {
                    InputInitFunder = inputInitFunder
                    LastSent = openChannelMsgToSend
                }
                let channelWaitingForAcceptChannel = {
                    ChannelHandshakeLimits = channelHandshakeLimits
                    ChannelOptions = channelOptions
                    ChannelPrivKeys = channelPrivKeys
                    FeeEstimator = feeEstimator
                    RemoteNodeId = remoteNodeId
                    NodeSecret = nodeSecret
                    Network = network
                    WaitForAcceptChannelData = waitForAcceptChannelData
                    LocalShutdownScriptPubKey = shutdownScriptPubKey
                }
                return (openChannelMsgToSend, channelWaitingForAcceptChannel)
            }

        static member NewInbound (channelHandshakeLimits: ChannelHandshakeLimits,
                                  channelOptions: ChannelOptions,
                                  nodeMasterPrivKey: NodeMasterPrivKey,
                                  channelIndex: int,
                                  feeEstimator: IFeeEstimator,
                                  network: Network,
                                  remoteNodeId: NodeId,
                                  inputInitFundee: InputInitFundee,
                                  minimumDepth: BlockHeightOffset32,
                                  shutdownScriptPubKey: Option<ShutdownScriptPubKey>,
                                  openChannelMsg: OpenChannelMsg
                                 ): Result<AcceptChannelMsg * ChannelWaitingForFundingCreated, ChannelError> =
            result {
                do! Validation.checkOpenChannelMsgAcceptable feeEstimator channelHandshakeLimits channelOptions openChannelMsg
                let localParams = inputInitFundee.LocalParams
                let channelKeys = inputInitFundee.ChannelPrivKeys
                let localCommitmentPubKey = channelKeys.CommitmentSeed.DerivePerCommitmentPoint CommitmentNumber.FirstCommitment
                let acceptChannelMsg: AcceptChannelMsg = {
                    TemporaryChannelId = openChannelMsg.TemporaryChannelId
                    DustLimitSatoshis = localParams.DustLimitSatoshis
                    MaxHTLCValueInFlightMsat = localParams.MaxHTLCValueInFlightMSat
                    ChannelReserveSatoshis = localParams.ChannelReserveSatoshis
                    HTLCMinimumMSat = localParams.HTLCMinimumMSat
                    MinimumDepth = minimumDepth
                    ToSelfDelay = localParams.ToSelfDelay
                    MaxAcceptedHTLCs = localParams.MaxAcceptedHTLCs
                    FundingPubKey = channelKeys.FundingPrivKey.FundingPubKey()
                    RevocationBasepoint = channelKeys.RevocationBasepointSecret.RevocationBasepoint()
                    PaymentBasepoint = channelKeys.PaymentBasepointSecret.PaymentBasepoint()
                    DelayedPaymentBasepoint = channelKeys.DelayedPaymentBasepointSecret.DelayedPaymentBasepoint()
                    HTLCBasepoint = channelKeys.HtlcBasepointSecret.HtlcBasepoint()
                    FirstPerCommitmentPoint = localCommitmentPubKey
                    TLVs = [| AcceptChannelTLV.UpfrontShutdownScript shutdownScriptPubKey |]
                }
                let remoteParams = RemoteParams.FromOpenChannel remoteNodeId inputInitFundee.RemoteInit openChannelMsg
                let waitForFundingCreatedData = Data.WaitForFundingCreatedData.Create localParams remoteParams openChannelMsg acceptChannelMsg
                let channelPrivKeys = nodeMasterPrivKey.ChannelPrivKeys channelIndex
                let nodeSecret = nodeMasterPrivKey.NodeSecret()
                let channelWaitingForFundingCreated = {
                    ChannelOptions = channelOptions
                    ChannelPrivKeys = channelPrivKeys
                    FeeEstimator = feeEstimator
                    RemoteNodeId = remoteNodeId
                    NodeSecret = nodeSecret
                    Network = network
                    FundingTxMinimumDepth = minimumDepth
                    LocalShutdownScriptPubKey = shutdownScriptPubKey
                    WaitForFundingCreatedData = waitForFundingCreatedData
                }
                return (acceptChannelMsg, channelWaitingForFundingCreated)
            }

module Channel =

    let private hex = NBitcoin.DataEncoders.HexEncoder()
    let private ascii = System.Text.ASCIIEncoding.ASCII
    let private dummyPrivKey = new Key(hex.DecodeData("0101010101010101010101010101010101010101010101010101010101010101"))
    let private dummyPubKey = dummyPrivKey.PubKey
    let private dummySig =
        "01010101010101010101010101010101" |> ascii.GetBytes
        |> uint256
        |> fun m -> dummyPrivKey.SignCompact(m)
        |> fun d -> LNECDSASignature.FromBytesCompact(d, true)
        |> fun ecdsaSig -> TransactionSignature(ecdsaSig.Value, SigHash.All)

    module Closing =
        let makeClosingTx (channelPrivKeys: ChannelPrivKeys,
                           cm: Commitments,
                           localSpk: ShutdownScriptPubKey,
                           remoteSpk: ShutdownScriptPubKey,
                           closingFee: Money,
                           network: Network
                          ) =
            let dustLimitSatoshis = Money.Max(cm.LocalParams.DustLimitSatoshis, cm.RemoteParams.DustLimitSatoshis)
            result {
                let! closingTx = Transactions.makeClosingTx (cm.FundingScriptCoin) (localSpk) (remoteSpk) (cm.LocalParams.IsFunder) (dustLimitSatoshis) (closingFee) (cm.LocalCommit.Spec) network
                let localSignature, psbtUpdated = channelPrivKeys.SignWithFundingPrivKey closingTx.Value
                let msg: ClosingSignedMsg = {
                    ChannelId = cm.ChannelId()
                    FeeSatoshis = closingFee
                    Signature = localSignature.Signature |> LNECDSASignature
                }
                return (ClosingTx psbtUpdated, msg)
            }

        let firstClosingFee (cm: Commitments)
                            (localSpk: ShutdownScriptPubKey)
                            (remoteSpk: ShutdownScriptPubKey)
                            (feeEst: IFeeEstimator)
                            (network: Network) =
            result {
                let! dummyClosingTx = Transactions.makeClosingTx cm.FundingScriptCoin localSpk remoteSpk cm.LocalParams.IsFunder Money.Zero Money.Zero cm.LocalCommit.Spec network
                let tx = dummyClosingTx.Value.GetGlobalTransaction()
                tx.Inputs.[0].WitScript <-
                    let witness = seq [ dummySig.ToBytes(); dummySig.ToBytes(); dummyClosingTx.Value.Inputs.[0].WitnessScript.ToBytes() ]
                    WitScript(witness)
                let feeRatePerKw = FeeRatePerKw.Max (feeEst.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority), cm.LocalCommit.Spec.FeeRatePerKw)
                return feeRatePerKw.CalculateFeeFromVirtualSize(tx)
            }

        let makeFirstClosingTx (channelPrivKeys: ChannelPrivKeys,
                                commitments: Commitments,
                                localSpk: ShutdownScriptPubKey,
                                remoteSpk: ShutdownScriptPubKey,
                                feeEst: IFeeEstimator,
                                network: Network
                               ) =
            result {
                let! closingFee = firstClosingFee commitments localSpk remoteSpk feeEst network
                return! makeClosingTx (channelPrivKeys, commitments, localSpk, remoteSpk, closingFee, network)
            } |> expectTransactionError

        let nextClosingFee (localClosingFee: Money, remoteClosingFee: Money) =
            ((localClosingFee.Satoshi + remoteClosingFee.Satoshi) / 4L) * 2L
            |> Money.Satoshis

        let handleMutualClose (closingTx: FinalizedTx, d: NegotiatingData, nextMessage: Option<ClosingSignedMsg>) =
            let nextData =
                ClosingData.Create
                    None
                    DateTime.Now
                    (d.ClosingTxProposed |> List.collect id |> List.map (fun tx -> tx.UnsignedTx))
                    closingTx
            [ MutualClosePerformed (closingTx, nextData, nextMessage) ]
            |> Ok

        let claimCurrentLocalCommitTxOutputs (channelPrivKeys: ChannelPrivKeys,
                                              commitments: Commitments,
                                              commitTx: CommitTx
                                             ) =
            result {
                let commitmentSeed = channelPrivKeys.CommitmentSeed
                do! check (commitments.LocalCommit.PublishableTxs.CommitTx.Value.GetTxId()) (=) (commitTx.Value.GetTxId()) "txid mismatch. provided txid (%A) does not match current local commit tx (%A)"
                let _localPerCommitmentPoint =
                    commitmentSeed.DerivePerCommitmentPoint commitments.LocalCommit.Index
                failwith "TODO"
            }

    let makeChannelReestablish (channelPrivKeys: ChannelPrivKeys)
                               (commitments: Commitments)
                                   : Result<ChannelEvent list, ChannelError> =
        let commitmentSeed = channelPrivKeys.CommitmentSeed
        let ourChannelReestablish =
            {
                ChannelId = commitments.ChannelId()
                NextCommitmentNumber =
                    (commitments.RemotePerCommitmentSecrets.NextCommitmentNumber().NextCommitment())
                NextRevocationNumber =
                    commitments.RemotePerCommitmentSecrets.NextCommitmentNumber()
                DataLossProtect = OptionalField.Some <| {
                    YourLastPerCommitmentSecret =
                        commitments.RemotePerCommitmentSecrets.MostRecentPerCommitmentSecret()
                    MyCurrentPerCommitmentPoint =
                        commitmentSeed.DerivePerCommitmentPoint commitments.RemoteCommit.Index
                }
            }
        [ WeSentChannelReestablish ourChannelReestablish ] |> Ok

    let executeCommand (cs: Channel) (command: ChannelCommand): Result<ChannelEvent list, ChannelError> =
        match cs.State, command with

        // --------------- open channel procedure: case we are funder -------------
        | WaitForFundingConfirmed _state, CreateChannelReestablish ->
            makeChannelReestablish cs.ChannelPrivKeys cs.Commitments
        | ChannelState.Normal _state, CreateChannelReestablish ->
            makeChannelReestablish cs.ChannelPrivKeys cs.Commitments
        | WaitForFundingConfirmed _state, ApplyFundingLocked msg ->
            [ TheySentFundingLocked msg ] |> Ok
        | WaitForFundingConfirmed state, ApplyFundingConfirmedOnBC(height, txindex, depth) ->
            if cs.FundingTxMinimumDepth > depth then
                [] |> Ok
            else
                let nextPerCommitmentPoint =
                    cs.ChannelPrivKeys.CommitmentSeed.DerivePerCommitmentPoint
                        (CommitmentNumber.FirstCommitment.NextCommitment())
                let msgToSend: FundingLockedMsg = {
                    ChannelId = cs.Commitments.ChannelId()
                    NextPerCommitmentPoint = nextPerCommitmentPoint
                }

                // This is temporary channel id that we will use in our channel_update message, the goal is to be able to use our channel
                // as soon as it reaches NORMAL state, and before it is announced on the network
                // (this id might be updated when the funding tx gets deeply buried, if there was a reorg in the meantime)
                // this is not specified in BOLT.
                let shortChannelId = {
                    ShortChannelId.BlockHeight = height;
                    BlockIndex = txindex
                    TxOutIndex =
                        cs.Commitments.FundingScriptCoin.Outpoint.N
                        |> uint16
                        |> TxOutIndex
                }
                let nextState = {
                    ShortChannelId = shortChannelId
                    OurMessage = msgToSend
                    TheirMessage = None
                    HaveWeSentFundingLocked = false
                    InitialFeeRatePerKw = state.InitialFeeRatePerKw
                }
                
                match (state.Deferred) with
                | None ->
                    [ FundingConfirmed nextState; WeSentFundingLocked msgToSend ] |> Ok
                | Some msg ->
                    [ FundingConfirmed nextState; WeSentFundingLocked msgToSend; WeResumedDelayedFundingLocked msg ] |> Ok
        | WaitForFundingLocked _state, ApplyFundingConfirmedOnBC(height, _txindex, depth) ->
            if (cs.FundingTxMinimumDepth <= depth) then
                [] |> Ok
            else
                onceConfirmedFundingTxHasBecomeUnconfirmed(height, depth)
        | WaitForFundingLocked state, ApplyFundingLocked msg ->
            if (state.HaveWeSentFundingLocked) then
                let initialChannelUpdate =
                    let feeBase = ChannelHelpers.getOurFeeBaseMSat cs.FeeEstimator state.InitialFeeRatePerKw cs.Commitments.LocalParams.IsFunder
                    ChannelHelpers.makeChannelUpdate (cs.Network.Consensus.HashGenesisBlock,
                                               cs.NodeSecret,
                                               cs.RemoteNodeId,
                                               state.ShortChannelId,
                                               cs.Commitments.LocalParams.ToSelfDelay,
                                               cs.Commitments.RemoteParams.HTLCMinimumMSat,
                                               feeBase,
                                               cs.ChannelOptions.FeeProportionalMillionths,
                                               true,
                                               None)
                let nextCommitments = {
                    cs.Commitments with
                        RemoteNextCommitInfo =
                            RemoteNextCommitInfo.Revoked(msg.NextPerCommitmentPoint)
                }
                let nextState = {
                    NormalData.Buried = true
                    ShortChannelId = state.ShortChannelId
                    ChannelAnnouncement = None
                    ChannelUpdate = initialChannelUpdate
                    LocalShutdown = None
                    RemoteShutdown = None
                }
                [ BothFundingLocked(nextState, nextCommitments) ] |> Ok
            else
                [] |> Ok

        // ---------- normal operation ---------
        | ChannelState.Normal state, AddHTLC op when state.LocalShutdown.IsSome || state.RemoteShutdown.IsSome ->
            sprintf "Could not add new HTLC %A since shutdown is already in progress." op
            |> apiMisuse
        | ChannelState.Normal _state, AddHTLC op ->
            result {
                do! Validation.checkOperationAddHTLC cs.Commitments op
                let add: UpdateAddHTLCMsg = {
                    ChannelId = cs.Commitments.ChannelId()
                    HTLCId = cs.Commitments.LocalNextHTLCId
                    Amount = op.Amount
                    PaymentHash = op.PaymentHash
                    CLTVExpiry = op.Expiry
                    OnionRoutingPacket = op.Onion
                }
                let commitments1 =
                    let commitments = {
                        cs.Commitments.AddLocalProposal(add) with
                            LocalNextHTLCId = cs.Commitments.LocalNextHTLCId + 1UL
                    }
                    match op.Origin with
                    | None -> commitments
                    | Some origin -> {
                        commitments with
                            OriginChannels =
                                cs.Commitments.OriginChannels
                                |> Map.add add.HTLCId origin
                    }

                // we need to base the next current commitment on the last sig we sent, even if we didn't yet receive their revocation
                let remoteCommit1 =
                    match commitments1.RemoteNextCommitInfo with
                    | RemoteNextCommitInfo.Waiting info -> info.NextRemoteCommit
                    | RemoteNextCommitInfo.Revoked _info -> commitments1.RemoteCommit
                let! reduced = remoteCommit1.Spec.Reduce(commitments1.RemoteChanges.ACKed, commitments1.LocalChanges.Proposed) |> expectTransactionError
                do! Validation.checkOurUpdateAddHTLCIsAcceptableWithCurrentSpec reduced commitments1 add
                return [ WeAcceptedOperationAddHTLC(add, commitments1) ]
            }
        | ChannelState.Normal _state, ApplyUpdateAddHTLC (msg, height) ->
            result {
                do! Validation.checkTheirUpdateAddHTLCIsAcceptable cs.Commitments msg height
                let commitments1 = {
                    cs.Commitments.AddRemoteProposal(msg) with
                        RemoteNextHTLCId = cs.Commitments.LocalNextHTLCId + 1UL
                }
                let! reduced =
                    commitments1.LocalCommit.Spec.Reduce (
                        commitments1.LocalChanges.ACKed,
                        commitments1.RemoteChanges.Proposed
                    ) |> expectTransactionError
                do! Validation.checkTheirUpdateAddHTLCIsAcceptableWithCurrentSpec reduced commitments1 msg
                return [ WeAcceptedUpdateAddHTLC commitments1 ]
            }

        | ChannelState.Normal _state, FulfillHTLC cmd ->
            result {
                let! t = cs.Commitments |> Commitments.sendFulfill (cmd)
                return [ WeAcceptedOperationFulfillHTLC t ]
            }

        | ChannelState.Normal _state, ChannelCommand.ApplyUpdateFulfillHTLC msg ->
            cs.Commitments |> Commitments.receiveFulfill msg

        | ChannelState.Normal _state, FailHTLC op ->
            cs.Commitments |> Commitments.sendFail cs.NodeSecret op

        | ChannelState.Normal _state, FailMalformedHTLC op ->
            cs.Commitments |> Commitments.sendFailMalformed op

        | ChannelState.Normal _state, ApplyUpdateFailHTLC msg ->
            cs.Commitments |> Commitments.receiveFail msg

        | ChannelState.Normal _state, ApplyUpdateFailMalformedHTLC msg ->
            cs.Commitments |> Commitments.receiveFailMalformed msg

        | ChannelState.Normal _state, UpdateFee op ->
            cs.Commitments |> Commitments.sendFee op
        | ChannelState.Normal _state, ApplyUpdateFee msg ->
            let localFeerate = cs.FeeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority)
            cs.Commitments |> Commitments.receiveFee cs.ChannelOptions localFeerate msg

        | ChannelState.Normal _state, SignCommitment ->
            let cm = cs.Commitments
            result {
                match cm.RemoteNextCommitInfo with
                | _ when (cm.LocalHasChanges() |> not) ->
                    // Ignore SignCommitment Command (nothing to sign)
                    return []
                | RemoteNextCommitInfo.Revoked _ ->
                    return! cm |> Commitments.sendCommit cs.ChannelPrivKeys (cs.Network)
                | RemoteNextCommitInfo.Waiting _ ->
                    // Already in the process of signing
                    return []
            }

        | ChannelState.Normal _state, ApplyCommitmentSigned msg ->
            cs.Commitments |> Commitments.receiveCommit cs.ChannelPrivKeys msg cs.Network

        | ChannelState.Normal _state, ApplyRevokeAndACK msg ->
            let cm = cs.Commitments
            match cm.RemoteNextCommitInfo with
            | RemoteNextCommitInfo.Waiting _ when (msg.PerCommitmentSecret.PerCommitmentPoint() <> cm.RemoteCommit.RemotePerCommitmentPoint) ->
                let errorMsg = sprintf "Invalid revoke_and_ack %A; must be %A" msg.PerCommitmentSecret cm.RemoteCommit.RemotePerCommitmentPoint
                invalidRevokeAndACK msg errorMsg
            | RemoteNextCommitInfo.Revoked _ ->
                let errorMsg = sprintf "Unexpected revocation"
                invalidRevokeAndACK msg errorMsg
            | RemoteNextCommitInfo.Waiting({ NextRemoteCommit = theirNextCommit }) ->
                let remotePerCommitmentSecretsOpt =
                    cm.RemotePerCommitmentSecrets.InsertPerCommitmentSecret
                        cm.RemoteCommit.Index
                        msg.PerCommitmentSecret
                match remotePerCommitmentSecretsOpt with
                | Error err -> invalidRevokeAndACK msg err.Message
                | Ok remotePerCommitmentSecrets ->
                    let commitments1 = { cm with LocalChanges = { cm.LocalChanges with Signed = []; ACKed = cm.LocalChanges.ACKed @ cm.LocalChanges.Signed }
                                                 RemoteChanges = { cm.RemoteChanges with Signed = [] }
                                                 RemoteCommit = theirNextCommit
                                                 RemoteNextCommitInfo = RemoteNextCommitInfo.Revoked msg.NextPerCommitmentPoint
                                                 RemotePerCommitmentSecrets = remotePerCommitmentSecrets }
                    let _result = Ok [ WeAcceptedRevokeAndACK commitments1 ]
                    failwith "needs update"

        | ChannelState.Normal state, ChannelCommand.Close localShutdownScriptPubKey ->
            result {
                match cs.LocalShutdownScriptPubKey with
                | Some commitedShutdownScriptPubKey ->
                    if commitedShutdownScriptPubKey <> localShutdownScriptPubKey then
                        do! cannotCloseChannel "Shutdown script does not match the shutdown script we orginally gave the peer"
                | None -> ()
                if (state.LocalShutdown.IsSome) then
                    do! cannotCloseChannel "shutdown is already in progress"
                if (cs.Commitments.LocalHasUnsignedOutgoingHTLCs()) then
                    do! cannotCloseChannel "Cannot close with unsigned outgoing htlcs"
                let shutdownMsg: ShutdownMsg = {
                    ChannelId = cs.Commitments.ChannelId()
                    ScriptPubKey = localShutdownScriptPubKey
                }
                return [ AcceptedOperationShutdown shutdownMsg ]
            }
        | ChannelState.Normal state, RemoteShutdown(msg, localShutdownScriptPubKey) ->
            result {
                match cs.LocalShutdownScriptPubKey with
                | Some commitedShutdownScriptPubKey ->
                    if commitedShutdownScriptPubKey <> localShutdownScriptPubKey then
                        do! cannotCloseChannel "Shutdown script does not match the shutdown script we orginally gave the peer"
                | None -> ()
                let cm = cs.Commitments
                // They have pending unsigned htlcs => they violated the spec, close the channel
                // they don't have pending unsigned htlcs
                //      We have pending unsigned htlcs
                //          We already sent a shutdown msg => spec violation (we can't send htlcs after having sent shutdown)
                //          We did not send a shutdown msg
                //              We are ready to sign => we stop sending further htlcs, we initiate a signature
                //              We are waiting for a rev => we stop sending further htlcs, we wait for their revocation, will resign immediately after, and then we will send our shutdown msg
                //      We have no pending unsigned htlcs
                //          we already sent a shutdown msg
                //              There are pending signed htlcs => send our shutdown msg, go to SHUTDOWN state
                //              there are no htlcs => send our shutdown msg, goto NEGOTIATING state
                //          We did not send a shutdown msg
                //              There are pending signed htlcs => go to SHUTDOWN state
                //              there are no HTLCs => go to NEGOTIATING state
                
                if (cm.RemoteHasUnsignedOutgoingHTLCs()) then
                    return! receivedShutdownWhenRemoteHasUnsignedOutgoingHTLCs msg
                // Do we have Unsigned Outgoing HTLCs?
                else if (cm.LocalHasUnsignedOutgoingHTLCs()) then
                    if (state.LocalShutdown.IsSome) then
                        return failwith "can't have pending unsigned outgoing htlcs after having sent Shutdown. this should never happen"
                    else
                        // Are we in the middle of a signature?
                        match cm.RemoteNextCommitInfo with
                        // yes.
                        | RemoteNextCommitInfo.Waiting waitingForRevocation ->
                            let nextCommitments = {
                                cs.Commitments with
                                    RemoteNextCommitInfo =
                                        RemoteNextCommitInfo.Waiting {
                                            waitingForRevocation with
                                                ReSignASAP = true
                                        }
                            }
                            return [
                                AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs(
                                    msg,
                                    nextCommitments
                                )
                            ]
                        // No. let's sign right away.
                        | RemoteNextCommitInfo.Revoked _ ->
                            return [
                                ChannelStateRequestedSignCommitment;
                                AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs(msg, cm)
                            ]
                else
                    let (localShutdown, _sendList) =
                        match state.LocalShutdown with
                        | Some localShutdown -> (localShutdown, [])
                        | None ->
                            let localShutdown: ShutdownMsg = {
                                ChannelId = cs.Commitments.ChannelId()
                                ScriptPubKey = localShutdownScriptPubKey
                            }
                            (localShutdown, [ localShutdown ])
                    if (cm.HasNoPendingHTLCs()) then
                        // we have to send first closing_signed msg iif we are the funder
                        if (cm.LocalParams.IsFunder) then
                            let! (closingTx, closingSignedMsg) =
                                Closing.makeFirstClosingTx (cs.ChannelPrivKeys,
                                                            cm,
                                                            localShutdown.ScriptPubKey,
                                                            msg.ScriptPubKey,
                                                            cs.FeeEstimator,
                                                            cs.Network)
                            let nextState = {
                                LocalShutdown = localShutdown
                                RemoteShutdown = msg
                                ClosingTxProposed = [[{
                                    ClosingTxProposed.UnsignedTx = closingTx
                                    LocalClosingSigned = closingSignedMsg
                                }]]
                                MaybeBestUnpublishedTx = None
                            }
                            return [
                                AcceptedShutdownWhenNoPendingHTLCs(
                                    closingSignedMsg |> Some,
                                    nextState
                                )
                            ]
                        else
                            let nextState = {
                                LocalShutdown = localShutdown
                                RemoteShutdown = msg
                                ClosingTxProposed = [ [] ]
                                MaybeBestUnpublishedTx = None
                            }
                            return [ AcceptedShutdownWhenNoPendingHTLCs(None, nextState) ]
                    else
                        let nextState = {
                            LocalShutdown = localShutdown
                            RemoteShutdown = msg
                        }
                        return [ AcceptedShutdownWhenWeHavePendingHTLCs(nextState) ]
            }
        // ----------- closing ---------
        | Shutdown _state, FulfillHTLC op ->
            result {
                let! t = cs.Commitments |> Commitments.sendFulfill op
                return [ WeAcceptedOperationFulfillHTLC t ]
            }
        | Shutdown _state, ApplyUpdateFulfillHTLC msg ->
            cs.Commitments |> Commitments.receiveFulfill msg
        | Shutdown _state, FailHTLC op ->
            cs.Commitments |> Commitments.sendFail cs.NodeSecret op
        | Shutdown _state, FailMalformedHTLC op ->
            cs.Commitments |> Commitments.sendFailMalformed op
        | Shutdown _state, ApplyUpdateFailMalformedHTLC msg ->
            cs.Commitments |> Commitments.receiveFailMalformed msg
        | Shutdown _state, UpdateFee op ->
            cs.Commitments |> Commitments.sendFee op
        | Shutdown _state, ApplyUpdateFee msg ->
            let localFeerate = cs.FeeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority)
            cs.Commitments |> Commitments.receiveFee cs.ChannelOptions localFeerate msg
        | Shutdown _state, SignCommitment ->
            let cm = cs.Commitments
            match cm.RemoteNextCommitInfo with
            | _ when (not <| cm.LocalHasChanges()) ->
                // nothing to sign
                [] |> Ok
            | RemoteNextCommitInfo.Revoked _ ->
                cm |> Commitments.sendCommit cs.ChannelPrivKeys (cs.Network)
            | RemoteNextCommitInfo.Waiting _waitForRevocation ->
                // Already in the process of signing.
                [] |> Ok
        | Shutdown _state, ApplyCommitmentSigned msg ->
            cs.Commitments |> Commitments.receiveCommit cs.ChannelPrivKeys msg cs.Network
        | Shutdown _state, ApplyRevokeAndACK _msg ->
            failwith "not implemented"

        | Negotiating state, ApplyClosingSigned msg ->
            result {
                let cm = cs.Commitments
                let remoteChannelKeys = cm.RemoteParams.ChannelPubKeys
                let lastCommitFeeSatoshi =
                    cm.FundingScriptCoin.TxOut.Value - (cm.LocalCommit.PublishableTxs.CommitTx.Value.TotalOut)
                do! checkRemoteProposedHigherFeeThanBefore lastCommitFeeSatoshi msg.FeeSatoshis
                let! closingTx, closingSignedMsg =
                    Closing.makeClosingTx (
                        cs.ChannelPrivKeys,
                        cm,
                        state.LocalShutdown.ScriptPubKey,
                        state.RemoteShutdown.ScriptPubKey,
                        msg.FeeSatoshis,
                        cs.Network
                    )
                    |> expectTransactionError
                let! finalizedTx =
                    Transactions.checkTxFinalized
                        closingTx.Value
                        closingTx.WhichInput
                        (seq [
                            remoteChannelKeys.FundingPubKey.RawPubKey(),
                            TransactionSignature(msg.Signature.Value, SigHash.All)
                        ])
                    |> expectTransactionError
                let maybeLocalFee =
                    state.ClosingTxProposed
                    |> List.tryHead
                    |> Option.bind (List.tryHead)
                    |> Option.map (fun v -> v.LocalClosingSigned.FeeSatoshis)
                let areWeInDeal = Some(msg.FeeSatoshis) = maybeLocalFee
                let hasTooManyNegotiationDone =
                    (state.ClosingTxProposed |> List.collect (id) |> List.length) >= cs.ChannelOptions.MaxClosingNegotiationIterations
                if (areWeInDeal || hasTooManyNegotiationDone) then
                    return! Closing.handleMutualClose (finalizedTx, { state with MaybeBestUnpublishedTx = Some(finalizedTx) }, None)
                else
                    let lastLocalClosingFee = state.ClosingTxProposed |> List.tryHead |> Option.bind (List.tryHead) |> Option.map (fun txp -> txp.LocalClosingSigned.FeeSatoshis)
                    let! localF = 
                        match lastLocalClosingFee with
                        | Some v -> Ok v
                        | None ->
                            Closing.firstClosingFee
                                cs.Commitments
                                state.LocalShutdown.ScriptPubKey
                                state.RemoteShutdown.ScriptPubKey
                                cs.FeeEstimator
                                cs.Network
                            |> expectTransactionError
                    let nextClosingFee =
                        Closing.nextClosingFee (localF, msg.FeeSatoshis)
                    if (Some nextClosingFee = lastLocalClosingFee) then
                        return! Closing.handleMutualClose (finalizedTx, { state with MaybeBestUnpublishedTx = Some(finalizedTx) }, None)
                    else if (nextClosingFee = msg.FeeSatoshis) then
                        // we have reached on agreement!
                        let closingTxProposed1 =
                            let newProposed = [ { ClosingTxProposed.UnsignedTx = closingTx
                                                  LocalClosingSigned = closingSignedMsg } ]
                            newProposed :: state.ClosingTxProposed
                        let negoData = { state with ClosingTxProposed = closingTxProposed1
                                                    MaybeBestUnpublishedTx = Some(finalizedTx) }
                        return! Closing.handleMutualClose (finalizedTx, negoData, Some closingSignedMsg)
                    else
                        let! closingTx, closingSignedMsg =
                            Closing.makeClosingTx (
                                cs.ChannelPrivKeys,
                                cm,
                                state.LocalShutdown.ScriptPubKey,
                                state.RemoteShutdown.ScriptPubKey,
                                nextClosingFee,
                                cs.Network
                            )
                            |> expectTransactionError
                        let closingTxProposed1 =
                            let newProposed = [ { ClosingTxProposed.UnsignedTx = closingTx
                                                  LocalClosingSigned = closingSignedMsg } ]
                            newProposed :: state.ClosingTxProposed
                        let nextState = { state with ClosingTxProposed = closingTxProposed1; MaybeBestUnpublishedTx = Some(finalizedTx) }
                        return [ WeProposedNewClosingSigned(closingSignedMsg, nextState) ]
            }
        | Closing state, FulfillHTLC op ->
            // got valid payment preimage, recalculating txs to redeem the corresponding htlc on-chain
            result {
                let cm = cs.Commitments
                let! (_msgToSend, newCommitments) = cm |> Commitments.sendFulfill op
                let _localCommitPublished =
                    state.LocalCommitPublished
                    |> Option.map (fun localCommitPublished ->
                        Closing.claimCurrentLocalCommitTxOutputs (
                            cs.ChannelPrivKeys,
                            newCommitments,
                            localCommitPublished.CommitTx
                        )
                    )
                return failwith "Not Implemented yet"
            }
        | state, cmd ->
            undefinedStateAndCmdPair state cmd

    let applyEvent c (e: ChannelEvent): Channel =
        match e, c.State with
        // --------- init both ------
        | FundingConfirmed data, WaitForFundingConfirmed _ ->
            { c with State = WaitForFundingLocked data }
        | TheySentFundingLocked msg, WaitForFundingConfirmed s ->
            { c with State = WaitForFundingConfirmed({ s with Deferred = Some(msg) }) }
        | TheySentFundingLocked _msg, WaitForFundingLocked s ->
            let feeBase = ChannelHelpers.getOurFeeBaseMSat c.FeeEstimator s.InitialFeeRatePerKw c.Commitments.LocalParams.IsFunder
            let channelUpdate = ChannelHelpers.makeChannelUpdate (c.Network.Consensus.HashGenesisBlock,
                                                           c.NodeSecret,
                                                           c.RemoteNodeId,
                                                           s.ShortChannelId,
                                                           c.Commitments.LocalParams.ToSelfDelay,
                                                           c.Commitments.RemoteParams.HTLCMinimumMSat,
                                                           feeBase,
                                                           c.ChannelOptions.FeeProportionalMillionths,
                                                           true,
                                                           None)
            let nextState = {
                NormalData.Buried = false;
                ShortChannelId = s.ShortChannelId
                ChannelAnnouncement = None
                ChannelUpdate = channelUpdate
                LocalShutdown = None
                RemoteShutdown = None
            }
            { c with State = ChannelState.Normal nextState }
        | WeSentFundingLocked msg, WaitForFundingLocked prevState ->
            { c with State = WaitForFundingLocked { prevState with OurMessage = msg; HaveWeSentFundingLocked = true } }
        | BothFundingLocked (data, newCommitments), WaitForFundingLocked _s ->
            { c with
                Commitments = newCommitments
                State = ChannelState.Normal data
            }

        // ----- normal operation --------
        | WeAcceptedOperationAddHTLC(_, newCommitments), ChannelState.Normal _normalData ->
            { c with Commitments = newCommitments }
        | WeAcceptedUpdateAddHTLC(newCommitments), ChannelState.Normal _normalData ->
            { c with Commitments = newCommitments }

        | WeAcceptedOperationFulfillHTLC(_, newCommitments), ChannelState.Normal _normalData ->
            { c with Commitments = newCommitments }
        | WeAcceptedFulfillHTLC(_msg, _origin, _htlc, newCommitments), ChannelState.Normal _normalData ->
            { c with Commitments = newCommitments }

        | WeAcceptedOperationFailHTLC(_msg, newCommitments), ChannelState.Normal _normalData ->
            { c with Commitments = newCommitments }
        | WeAcceptedFailHTLC(_origin, _msg, newCommitments), ChannelState.Normal _normalData ->
            { c with Commitments = newCommitments }

        | WeAcceptedOperationFailMalformedHTLC(_msg, newCommitments), ChannelState.Normal _normalData ->
            { c with Commitments = newCommitments }
        | WeAcceptedFailMalformedHTLC(_origin, _msg, newCommitments), ChannelState.Normal _normalData ->
            { c with Commitments = newCommitments }

        | WeAcceptedOperationUpdateFee(_msg, newCommitments), ChannelState.Normal _normalData ->
            { c with Commitments = newCommitments }
        | WeAcceptedUpdateFee(_msg, newCommitments), ChannelState.Normal _normalData ->
            { c with Commitments = newCommitments }

        | WeAcceptedOperationSign(_msg, newCommitments), ChannelState.Normal _normalData ->
            { c with Commitments = newCommitments }
        | WeAcceptedCommitmentSigned(_msg, newCommitments), ChannelState.Normal _normalData ->
            { c with Commitments = newCommitments }

        | WeAcceptedRevokeAndACK(newCommitments), ChannelState.Normal _normalData ->
            { c with Commitments = newCommitments }

        // -----  closing ------
        | AcceptedOperationShutdown msg, ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with LocalShutdown = Some msg }) }
        | AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs(remoteShutdown, nextCommitments), ChannelState.Normal normalData ->
            { 
                c with
                    Commitments = nextCommitments
                    State = ChannelState.Normal {
                        normalData with
                            RemoteShutdown = Some remoteShutdown
                    }
            }
        | AcceptedShutdownWhenNoPendingHTLCs(_maybeMsg, nextState), ChannelState.Normal _d ->
            { c with State = Negotiating nextState }
        | AcceptedShutdownWhenWeHavePendingHTLCs(nextState), ChannelState.Normal _d ->
            { c with State = Shutdown nextState }
        | MutualClosePerformed (_txToPublish, nextState, _), ChannelState.Negotiating _d ->
            { c with State = Closing nextState }
        | WeProposedNewClosingSigned(_msg, nextState), ChannelState.Negotiating _d ->
            { c with State = Negotiating(nextState) }
        // ----- else -----
        | _otherEvent -> c
