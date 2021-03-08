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
    StaticChannelConfig: StaticChannelConfig
    ChannelOptions: ChannelOptions
    ChannelPrivKeys: ChannelPrivKeys
    NodeSecret: NodeSecret
    ChannelId: ChannelId
    FundingTx: FinalizedTx
    LocalSpec: CommitmentSpec
    LocalCommitTx: CommitTx
    RemoteCommit: RemoteCommit
    LastSent: FundingCreatedMsg
} with
    member self.ApplyFundingSigned (msg: FundingSignedMsg)
                                       : Result<FinalizedTx * Channel, ChannelError> = result {
        let! finalizedLocalCommitTx =
            let theirFundingPk = self.StaticChannelConfig.RemoteChannelPubKeys.FundingPubKey.RawPubKey()
            let _, signedLocalCommitTx =
                self.ChannelPrivKeys.SignWithFundingPrivKey self.LocalCommitTx.Value
            let remoteSigPairOfLocalTx = (theirFundingPk,  TransactionSignature(msg.Signature.Value, SigHash.All))
            let sigPairs = seq [ remoteSigPairOfLocalTx; ]
            Transactions.checkTxFinalized signedLocalCommitTx self.LocalCommitTx.WhichInput sigPairs |> expectTransactionError
        let commitments = {
            LocalCommit = {
                Index = CommitmentNumber.FirstCommitment
                Spec = self.LocalSpec
                PublishableTxs = {
                    PublishableTxs.CommitTx = finalizedLocalCommitTx
                    HTLCTxs = []
                }
                PendingHTLCSuccessTxs = []
            }
            RemoteCommit = self.RemoteCommit
            LocalChanges = LocalChanges.Zero
            RemoteChanges = RemoteChanges.Zero
            LocalNextHTLCId = HTLCId.Zero
            RemoteNextHTLCId = HTLCId.Zero
            OriginChannels = Map.empty
            RemotePerCommitmentSecrets = PerCommitmentSecretStore()
        }
        let channel = {
            StaticChannelConfig = self.StaticChannelConfig
            ChannelOptions = self.ChannelOptions
            ChannelPrivKeys = self.ChannelPrivKeys
            NodeSecret = self.NodeSecret
            ShortChannelId = None
            RemoteNextCommitInfo = None
            NegotiatingState = NegotiatingState.New()
            Commitments = commitments
        }
        return self.FundingTx, channel
    }

and ChannelWaitingForFundingCreated = {
    AnnounceChannel: bool
    RemoteNodeId: NodeId
    Network: Network
    FundingTxMinimumDepth: BlockHeightOffset32
    LocalStaticShutdownScriptPubKey: Option<ShutdownScriptPubKey>
    RemoteStaticShutdownScriptPubKey: Option<ShutdownScriptPubKey>
    IsFunder: bool
    ChannelOptions: ChannelOptions
    ChannelPrivKeys: ChannelPrivKeys
    NodeSecret: NodeSecret
    TemporaryFailure: ChannelId
    LocalParams: LocalParams
    RemoteParams: RemoteParams
    RemoteChannelPubKeys: ChannelPubKeys
    FundingSatoshis: Money
    PushMSat: LNMoney
    InitialFeeRatePerKw: FeeRatePerKw
    RemoteFirstPerCommitmentPoint: PerCommitmentPoint
} with
    member self.ApplyFundingCreated (msg: FundingCreatedMsg)
                                        : Result<FundingSignedMsg * Channel, ChannelError> = result {
        let! (localSpec, localCommitTx, remoteSpec, remoteCommitTx) =
            let firstPerCommitmentPoint =
                self.ChannelPrivKeys.CommitmentSeed.DerivePerCommitmentPoint
                    CommitmentNumber.FirstCommitment
            ChannelHelpers.makeFirstCommitTxs
                false
                (self.ChannelPrivKeys.ToChannelPubKeys())
                self.RemoteChannelPubKeys
                self.LocalParams
                self.RemoteParams
                self.FundingSatoshis
                self.PushMSat
                self.InitialFeeRatePerKw
                msg.FundingOutputIndex
                msg.FundingTxId
                firstPerCommitmentPoint
                self.RemoteFirstPerCommitmentPoint
                self.Network
        assert (localCommitTx.Value.IsReadyToSign())
        let _s, signedLocalCommitTx =
            self.ChannelPrivKeys.SignWithFundingPrivKey localCommitTx.Value
        let remoteTxSig = TransactionSignature(msg.Signature.Value, SigHash.All)
        let theirSigPair = (self.RemoteChannelPubKeys.FundingPubKey.RawPubKey(), remoteTxSig)
        let sigPairs = seq [ theirSigPair ]
        let! finalizedCommitTx =
            Transactions.checkTxFinalized (signedLocalCommitTx) (localCommitTx.WhichInput) sigPairs
            |> expectTransactionError
        let localSigOfRemoteCommit, _ =
            self.ChannelPrivKeys.SignWithFundingPrivKey remoteCommitTx.Value
        let fundingScriptCoin =
            ChannelHelpers.getFundingScriptCoin
                (self.ChannelPrivKeys.FundingPrivKey.FundingPubKey())
                self.RemoteChannelPubKeys.FundingPubKey
                msg.FundingTxId
                msg.FundingOutputIndex
                self.FundingSatoshis
        let commitments = {
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
                RemotePerCommitmentPoint = self.RemoteFirstPerCommitmentPoint
            }
            LocalChanges = LocalChanges.Zero
            RemoteChanges = RemoteChanges.Zero
            LocalNextHTLCId = HTLCId.Zero
            RemoteNextHTLCId = HTLCId.Zero
            OriginChannels = Map.empty
            RemotePerCommitmentSecrets = PerCommitmentSecretStore()
        }
        let staticChannelConfig = {
            AnnounceChannel = self.AnnounceChannel
            RemoteNodeId = self.RemoteNodeId
            Network = self.Network
            FundingTxMinimumDepth = self.FundingTxMinimumDepth
            LocalStaticShutdownScriptPubKey = self.LocalStaticShutdownScriptPubKey
            RemoteStaticShutdownScriptPubKey = self.RemoteStaticShutdownScriptPubKey
            IsFunder = self.IsFunder
            FundingScriptCoin = fundingScriptCoin
            LocalParams = self.LocalParams
            RemoteParams = self.RemoteParams
            RemoteChannelPubKeys = self.RemoteChannelPubKeys
        }
        let channelId = staticChannelConfig.ChannelId()
        let msgToSend: FundingSignedMsg = {
            ChannelId = channelId
            Signature = !>localSigOfRemoteCommit.Signature
        }
        let channel = {
            StaticChannelConfig = staticChannelConfig
            ChannelOptions = self.ChannelOptions
            ChannelPrivKeys = self.ChannelPrivKeys
            NodeSecret = self.NodeSecret
            RemoteNextCommitInfo = None
            ShortChannelId = None
            NegotiatingState = NegotiatingState.New()
            Commitments = commitments
        }
        return msgToSend, channel
    }

and ChannelWaitingForFundingTx = {
    AnnounceChannel: bool
    ChannelOptions: ChannelOptions
    ChannelPrivKeys: ChannelPrivKeys
    RemoteNodeId: NodeId
    NodeSecret: NodeSecret
    Network: Network
    LocalStaticShutdownScriptPubKey: Option<ShutdownScriptPubKey>
    RemoteStaticShutdownScriptPubKey: Option<ShutdownScriptPubKey>
    LastReceived: AcceptChannelMsg
    TemporaryChannelId: ChannelId
    RemoteChannelPubKeys: ChannelPubKeys
    FundingSatoshis: Money
    PushMSat: LNMoney
    InitFeeRatePerKw: FeeRatePerKw
    LocalParams: LocalParams
    RemoteInit: InitMsg
} with
    member self.CreateFundingTx (fundingTx: FinalizedTx)
                                (outIndex: TxOutIndex)
                                    : Result<FundingCreatedMsg * ChannelWaitingForFundingSigned, ChannelError> = result {
        let remoteParams = RemoteParams.FromAcceptChannel self.RemoteInit self.LastReceived
        let localParams = self.LocalParams
        let commitmentSpec = CommitmentSpec.Create (self.FundingSatoshis.ToLNMoney() - self.PushMSat) self.PushMSat self.InitFeeRatePerKw
        let commitmentSeed = self.ChannelPrivKeys.CommitmentSeed
        let fundingTxId = fundingTx.Value.GetTxId()
        let! (_localSpec, localCommitTx, remoteSpec, remoteCommitTx) =
            ChannelHelpers.makeFirstCommitTxs
                true
                (self.ChannelPrivKeys.ToChannelPubKeys())
                self.RemoteChannelPubKeys
                localParams
                remoteParams
                self.FundingSatoshis
                self.PushMSat
                self.InitFeeRatePerKw
                outIndex
                fundingTxId
                (commitmentSeed.DerivePerCommitmentPoint CommitmentNumber.FirstCommitment)
                self.LastReceived.FirstPerCommitmentPoint
                self.Network
        let localSigOfRemoteCommit, _ =
            self.ChannelPrivKeys.SignWithFundingPrivKey remoteCommitTx.Value
        let nextMsg: FundingCreatedMsg = {
            TemporaryChannelId = self.LastReceived.TemporaryChannelId
            FundingTxId = fundingTxId
            FundingOutputIndex = outIndex
            Signature = !>localSigOfRemoteCommit.Signature
        }
        let fundingScriptCoin =
            ChannelHelpers.getFundingScriptCoin
                (self.ChannelPrivKeys.FundingPrivKey.FundingPubKey())
                self.RemoteChannelPubKeys.FundingPubKey
                fundingTxId
                outIndex
                self.FundingSatoshis
        let channelId = OutPoint(fundingTxId.Value, uint32 outIndex.Value).ToChannelId()
        let channelWaitingForFundingSigned = {
            StaticChannelConfig = {
                AnnounceChannel = self.AnnounceChannel
                RemoteNodeId = self.RemoteNodeId
                Network = self.Network
                FundingTxMinimumDepth = self.LastReceived.MinimumDepth
                LocalStaticShutdownScriptPubKey = self.LocalStaticShutdownScriptPubKey
                RemoteStaticShutdownScriptPubKey = self.RemoteStaticShutdownScriptPubKey
                IsFunder = true
                FundingScriptCoin = fundingScriptCoin
                LocalParams = localParams
                RemoteParams = remoteParams
                RemoteChannelPubKeys = self.RemoteChannelPubKeys
            }
            ChannelOptions = self.ChannelOptions
            ChannelPrivKeys = self.ChannelPrivKeys
            NodeSecret = self.NodeSecret
            ChannelId = channelId
            FundingTx = fundingTx
            LocalSpec = commitmentSpec
            LocalCommitTx = localCommitTx
            RemoteCommit = {
                RemoteCommit.Index = CommitmentNumber.FirstCommitment
                Spec = remoteSpec
                TxId = remoteCommitTx.Value.GetGlobalTransaction().GetTxId()
                RemotePerCommitmentPoint = self.LastReceived.FirstPerCommitmentPoint
            }
            LastSent = nextMsg
        }
        return nextMsg, channelWaitingForFundingSigned
    }


and ChannelWaitingForAcceptChannel = {
    AnnounceChannel: bool
    ChannelOptions: ChannelOptions
    ChannelHandshakeLimits: ChannelHandshakeLimits
    ChannelPrivKeys: ChannelPrivKeys
    RemoteNodeId: NodeId
    NodeSecret: NodeSecret
    Network: Network
    LocalStaticShutdownScriptPubKey: Option<ShutdownScriptPubKey>
    TemporaryChannelId: ChannelId
    FundingSatoshis: Money
    PushMSat: LNMoney
    InitFeeRatePerKw: FeeRatePerKw
    LocalParams: LocalParams
    RemoteInit: InitMsg
} with
    member self.ApplyAcceptChannel (msg: AcceptChannelMsg)
                                       : Result<IDestination * Money * ChannelWaitingForFundingTx, ChannelError> = result {
        do! Validation.checkAcceptChannelMsgAcceptable self.ChannelHandshakeLimits self.FundingSatoshis self.LocalParams.ChannelReserveSatoshis self.LocalParams.DustLimitSatoshis msg
        let redeem =
            Scripts.funding
                (self.ChannelPrivKeys.ToChannelPubKeys().FundingPubKey)
                msg.FundingPubKey
        let remoteChannelPubKeys = {
            FundingPubKey = msg.FundingPubKey
            RevocationBasepoint = msg.RevocationBasepoint
            PaymentBasepoint = msg.PaymentBasepoint
            DelayedPaymentBasepoint = msg.DelayedPaymentBasepoint
            HtlcBasepoint = msg.HTLCBasepoint
        }
        let destination = redeem.WitHash :> IDestination
        let amount = self.FundingSatoshis
        let channelWaitingForFundingTx = {
            AnnounceChannel = self.AnnounceChannel
            ChannelOptions = self.ChannelOptions
            ChannelPrivKeys = self.ChannelPrivKeys
            RemoteNodeId = self.RemoteNodeId
            NodeSecret = self.NodeSecret
            Network = self.Network
            LocalStaticShutdownScriptPubKey = self.LocalStaticShutdownScriptPubKey
            RemoteStaticShutdownScriptPubKey = msg.ShutdownScriptPubKey()
            LastReceived = msg
            TemporaryChannelId = self.TemporaryChannelId
            RemoteChannelPubKeys = remoteChannelPubKeys
            FundingSatoshis = self.FundingSatoshis
            PushMSat = self.PushMSat
            InitFeeRatePerKw = self.InitFeeRatePerKw
            LocalParams = self.LocalParams
            RemoteInit = self.RemoteInit
        }
        return destination, amount, channelWaitingForFundingTx
    }

and Channel = {
    StaticChannelConfig: StaticChannelConfig
    ChannelOptions: ChannelOptions
    ChannelPrivKeys: ChannelPrivKeys
    NodeSecret: NodeSecret
    ShortChannelId: Option<ShortChannelId>
    RemoteNextCommitInfo: Option<RemoteNextCommitInfo>
    NegotiatingState: NegotiatingState
    Commitments: Commitments
 }
    with

    member internal self.RemoteNextCommitInfoIfFundingLocked (operation: string)
                                                                 : Result<RemoteNextCommitInfo, ChannelError> =
        match self.RemoteNextCommitInfo with
        | None ->
            sprintf
                "cannot perform operation %s because peer has not sent funding_locked"
                operation
            |> apiMisuse
        | Some remoteNextCommitInfo -> Ok remoteNextCommitInfo

    member internal self.RemoteNextCommitInfoIfFundingLockedNormal (operation: string)
                                                                       : Result<RemoteNextCommitInfo, ChannelError> =
        match self.ShortChannelId with
        | None ->
            sprintf
                "cannot perform operation %s because funding is not confirmed"
                operation
            |> apiMisuse
        | Some _ ->
            self.RemoteNextCommitInfoIfFundingLocked operation

    static member NewOutbound(channelHandshakeLimits: ChannelHandshakeLimits,
                                channelOptions: ChannelOptions,
                                announceChannel: bool,
                                nodeMasterPrivKey: NodeMasterPrivKey,
                                channelIndex: int,
                                network: Network,
                                remoteNodeId: NodeId,
                                shutdownScriptPubKey: Option<ShutdownScriptPubKey>,
                                temporaryChannelId: ChannelId,
                                fundingSatoshis: Money,
                                pushMSat: LNMoney,
                                initFeeRatePerKw: FeeRatePerKw,
                                localParams: LocalParams,
                                remoteInit: InitMsg
                                ): Result<OpenChannelMsg * ChannelWaitingForAcceptChannel, ChannelError> =
        let channelPrivKeys = nodeMasterPrivKey.ChannelPrivKeys channelIndex
        let openChannelMsgToSend: OpenChannelMsg = {
            Chainhash = network.Consensus.HashGenesisBlock
            TemporaryChannelId = temporaryChannelId
            FundingSatoshis = fundingSatoshis
            PushMSat = pushMSat
            DustLimitSatoshis = localParams.DustLimitSatoshis
            MaxHTLCValueInFlightMsat = localParams.MaxHTLCValueInFlightMSat
            ChannelReserveSatoshis = localParams.ChannelReserveSatoshis
            HTLCMinimumMsat = localParams.HTLCMinimumMSat
            FeeRatePerKw = initFeeRatePerKw
            ToSelfDelay = localParams.ToSelfDelay
            MaxAcceptedHTLCs = localParams.MaxAcceptedHTLCs
            FundingPubKey = channelPrivKeys.FundingPrivKey.FundingPubKey()
            RevocationBasepoint = channelPrivKeys.RevocationBasepointSecret.RevocationBasepoint()
            PaymentBasepoint = channelPrivKeys.PaymentBasepointSecret.PaymentBasepoint()
            DelayedPaymentBasepoint = channelPrivKeys.DelayedPaymentBasepointSecret.DelayedPaymentBasepoint()
            HTLCBasepoint = channelPrivKeys.HtlcBasepointSecret.HtlcBasepoint()
            FirstPerCommitmentPoint = channelPrivKeys.CommitmentSeed.DerivePerCommitmentPoint CommitmentNumber.FirstCommitment
            ChannelFlags = {
                AnnounceChannel = announceChannel
            }
            TLVs = [| OpenChannelTLV.UpfrontShutdownScript shutdownScriptPubKey |]
        }
        result {
            do! Validation.checkOurOpenChannelMsgAcceptable openChannelMsgToSend
            let channelPrivKeys = nodeMasterPrivKey.ChannelPrivKeys channelIndex
            let nodeSecret = nodeMasterPrivKey.NodeSecret()
            let channelWaitingForAcceptChannel = {
                AnnounceChannel = announceChannel
                ChannelHandshakeLimits = channelHandshakeLimits
                ChannelOptions = channelOptions
                ChannelPrivKeys = channelPrivKeys
                RemoteNodeId = remoteNodeId
                NodeSecret = nodeSecret
                Network = network
                LocalStaticShutdownScriptPubKey = shutdownScriptPubKey
                TemporaryChannelId = temporaryChannelId
                FundingSatoshis = fundingSatoshis
                PushMSat = pushMSat
                InitFeeRatePerKw = initFeeRatePerKw
                LocalParams = localParams
                RemoteInit = remoteInit
            }
            return (openChannelMsgToSend, channelWaitingForAcceptChannel)
        }

    static member NewInbound (channelHandshakeLimits: ChannelHandshakeLimits,
                                channelOptions: ChannelOptions,
                                announceChannel: bool,
                                nodeMasterPrivKey: NodeMasterPrivKey,
                                channelIndex: int,
                                network: Network,
                                remoteNodeId: NodeId,
                                minimumDepth: BlockHeightOffset32,
                                shutdownScriptPubKey: Option<ShutdownScriptPubKey>,
                                openChannelMsg: OpenChannelMsg,
                                localParams: LocalParams,
                                remoteInit: InitMsg): Result<AcceptChannelMsg * ChannelWaitingForFundingCreated, ChannelError> =
        result {
            let channelPrivKeys = nodeMasterPrivKey.ChannelPrivKeys channelIndex
            do!
                Validation.checkOpenChannelMsgAcceptable
                    channelHandshakeLimits
                    channelOptions
                    announceChannel
                    openChannelMsg
            let firstPerCommitmentPoint = channelPrivKeys.CommitmentSeed.DerivePerCommitmentPoint CommitmentNumber.FirstCommitment
            let acceptChannelMsg: AcceptChannelMsg = {
                TemporaryChannelId = openChannelMsg.TemporaryChannelId
                DustLimitSatoshis = localParams.DustLimitSatoshis
                MaxHTLCValueInFlightMsat = localParams.MaxHTLCValueInFlightMSat
                ChannelReserveSatoshis = localParams.ChannelReserveSatoshis
                HTLCMinimumMSat = localParams.HTLCMinimumMSat
                MinimumDepth = minimumDepth
                ToSelfDelay = localParams.ToSelfDelay
                MaxAcceptedHTLCs = localParams.MaxAcceptedHTLCs
                FundingPubKey = channelPrivKeys.FundingPrivKey.FundingPubKey()
                RevocationBasepoint = channelPrivKeys.RevocationBasepointSecret.RevocationBasepoint()
                PaymentBasepoint = channelPrivKeys.PaymentBasepointSecret.PaymentBasepoint()
                DelayedPaymentBasepoint = channelPrivKeys.DelayedPaymentBasepointSecret.DelayedPaymentBasepoint()
                HTLCBasepoint = channelPrivKeys.HtlcBasepointSecret.HtlcBasepoint()
                FirstPerCommitmentPoint = firstPerCommitmentPoint
                TLVs = [| AcceptChannelTLV.UpfrontShutdownScript shutdownScriptPubKey |]
            }
            let remoteChannelPubKeys = {
                FundingPubKey = openChannelMsg.FundingPubKey
                RevocationBasepoint = openChannelMsg.RevocationBasepoint
                PaymentBasepoint = openChannelMsg.PaymentBasepoint
                DelayedPaymentBasepoint = openChannelMsg.DelayedPaymentBasepoint
                HtlcBasepoint = openChannelMsg.HTLCBasepoint
            }
            let remoteParams = RemoteParams.FromOpenChannel remoteInit openChannelMsg
            let nodeSecret = nodeMasterPrivKey.NodeSecret()
            let channelWaitingForFundingCreated = {
                AnnounceChannel = openChannelMsg.ChannelFlags.AnnounceChannel
                RemoteNodeId = remoteNodeId
                Network = network
                FundingTxMinimumDepth = minimumDepth
                LocalStaticShutdownScriptPubKey = shutdownScriptPubKey
                RemoteStaticShutdownScriptPubKey = openChannelMsg.ShutdownScriptPubKey()
                IsFunder = false
                ChannelOptions = channelOptions
                ChannelPrivKeys = channelPrivKeys
                NodeSecret = nodeSecret
                TemporaryFailure = openChannelMsg.TemporaryChannelId
                LocalParams = localParams
                RemoteParams = remoteParams
                RemoteChannelPubKeys = remoteChannelPubKeys
                FundingSatoshis = openChannelMsg.FundingSatoshis
                PushMSat = openChannelMsg.PushMSat
                InitialFeeRatePerKw = openChannelMsg.FeeRatePerKw
                RemoteFirstPerCommitmentPoint = openChannelMsg.FirstPerCommitmentPoint
            }
            return (acceptChannelMsg, channelWaitingForFundingCreated)
        }

    member self.CreateChannelReestablish (): ChannelReestablishMsg =
        let commitmentSeed = self.ChannelPrivKeys.CommitmentSeed
        let ourChannelReestablish = {
            ChannelId = self.StaticChannelConfig.ChannelId()
            NextCommitmentNumber =
                (self.Commitments.RemotePerCommitmentSecrets.NextCommitmentNumber().NextCommitment())
            NextRevocationNumber =
                self.Commitments.RemotePerCommitmentSecrets.NextCommitmentNumber()
            DataLossProtect = OptionalField.Some <| {
                YourLastPerCommitmentSecret =
                    self.Commitments.RemotePerCommitmentSecrets.MostRecentPerCommitmentSecret()
                MyCurrentPerCommitmentPoint =
                    commitmentSeed.DerivePerCommitmentPoint self.Commitments.RemoteCommit.Index
            }
        }
        ourChannelReestablish

    member self.ApplyFundingLocked (fundingLockedMsg: FundingLockedMsg)
                                       : Result<Channel, ChannelError> = result {
        do!
            match self.RemoteNextCommitInfo with
            | None -> Ok ()
            | Some remoteNextCommitInfo ->
                if remoteNextCommitInfo.PerCommitmentPoint()
                    <> fundingLockedMsg.NextPerCommitmentPoint then
                    Error <| InvalidFundingLocked { NetworkMsg = fundingLockedMsg }
                else
                    Ok ()
        match self.ShortChannelId with
        | None ->
            return {
                self with
                    RemoteNextCommitInfo =
                        RemoteNextCommitInfo.Revoked fundingLockedMsg.NextPerCommitmentPoint
                        |> Some
            }
        | Some _shortChannelId ->
            return self
    }

    member self.ApplyFundingConfirmedOnBC (height: BlockHeight)
                                          (txindex: TxIndexInBlock)
                                          (depth: BlockHeightOffset32)
                                              : Result<Channel * Option<FundingLockedMsg>, ChannelError> = result {
        match self.ShortChannelId with
        | None ->
            if self.StaticChannelConfig.FundingTxMinimumDepth > depth then
                // TODO: this should probably be an error (?)
                return self, None
            else
                let nextPerCommitmentPoint =
                    self.ChannelPrivKeys.CommitmentSeed.DerivePerCommitmentPoint
                        (CommitmentNumber.FirstCommitment.NextCommitment())
                let msgToSend: FundingLockedMsg = {
                    ChannelId = self.StaticChannelConfig.ChannelId()
                    NextPerCommitmentPoint = nextPerCommitmentPoint
                }

                // This is temporary channel id that we will use in our
                // channel_update message, the goal is to be able to use our
                // channel as soon as it reaches NORMAL state, and before it is
                // announced on the network (this id might be updated when the
                // funding tx gets deeply buried, if there was a reorg in the
                // meantime) this is not specified in BOLT.
                let shortChannelId = {
                    ShortChannelId.BlockHeight = height;
                    BlockIndex = txindex
                    TxOutIndex =
                        self.StaticChannelConfig.FundingScriptCoin.Outpoint.N
                        |> uint16
                        |> TxOutIndex
                }
                let channel = {
                    self with
                        ShortChannelId = Some shortChannelId
                }
                return channel, Some msgToSend
        | Some _shortChannelId ->
            if (self.StaticChannelConfig.FundingTxMinimumDepth <= depth) then
                return self, None
            else
                return! Error <| OnceConfirmedFundingTxHasBecomeUnconfirmed(height, depth)
    }

    member self.AddHTLC (op: OperationAddHTLC)
                            : Result<Channel * UpdateAddHTLCMsg, ChannelError> = result {
        if self.NegotiatingState.HasEnteredShutdown() then
            return!
                sprintf "Could not add new HTLC %A since shutdown is already in progress." op
                |> apiMisuse
        else
            do! Validation.checkOperationAddHTLC self.StaticChannelConfig.RemoteParams op
            let add: UpdateAddHTLCMsg = {
                ChannelId = self.StaticChannelConfig.ChannelId()
                HTLCId = self.Commitments.LocalNextHTLCId
                Amount = op.Amount
                PaymentHash = op.PaymentHash
                CLTVExpiry = op.Expiry
                OnionRoutingPacket = op.Onion
            }
            let commitments1 =
                let commitments = {
                    self.Commitments.AddLocalProposal(add) with
                        LocalNextHTLCId = self.Commitments.LocalNextHTLCId + 1UL
                }
                match op.Origin with
                | None -> commitments
                | Some origin -> {
                    commitments with
                        OriginChannels =
                            self.Commitments.OriginChannels
                            |> Map.add add.HTLCId origin
                }

            let! remoteNextCommitInfo =
                self.RemoteNextCommitInfoIfFundingLockedNormal "AddHTLC"
            // we need to base the next current commitment on the last sig we sent, even if we didn't yet receive their revocation
            let remoteCommit1 =
                match remoteNextCommitInfo with
                | Waiting nextRemoteCommit -> nextRemoteCommit
                | Revoked _info -> commitments1.RemoteCommit
            let! reduced = remoteCommit1.Spec.Reduce(commitments1.RemoteChanges.ACKed, commitments1.LocalChanges.Proposed) |> expectTransactionError
            do!
                Validation.checkOurUpdateAddHTLCIsAcceptableWithCurrentSpec
                    reduced
                    self.StaticChannelConfig
                    add
            let channel = {
                self with
                    Commitments = commitments1
            }
            return channel, add
    }

    member self.ApplyUpdateAddHTLC (msg: UpdateAddHTLCMsg)
                                   (height: BlockHeight)
                                       : Result<Channel, ChannelError> = result {
        do!
            Validation.checkTheirUpdateAddHTLCIsAcceptable
                self.Commitments
                self.StaticChannelConfig.LocalParams
                msg
                height
        let commitments1 = {
            self.Commitments.AddRemoteProposal(msg) with
                RemoteNextHTLCId = self.Commitments.LocalNextHTLCId + 1UL
        }
        let! reduced =
            commitments1.LocalCommit.Spec.Reduce (
                commitments1.LocalChanges.ACKed,
                commitments1.RemoteChanges.Proposed
            ) |> expectTransactionError
        do!
            Validation.checkTheirUpdateAddHTLCIsAcceptableWithCurrentSpec
                reduced
                self.StaticChannelConfig
                msg
        return {
            self with
                Commitments = commitments1
        }
    }

    member self.FulFillHTLC (cmd: OperationFulfillHTLC)
                                : Result<Channel * UpdateFulfillHTLCMsg, ChannelError> = result {
        let! remoteNextCommitInfo =
            self.RemoteNextCommitInfoIfFundingLockedNormal "FulfillHTLC"
        let! updateFulfillHTLCMsg, newCommitments =
            Commitments.sendFulfill
                cmd
                self.Commitments
                self.StaticChannelConfig
                remoteNextCommitInfo

        let channel = {
            self with
                Commitments = newCommitments
        }
        return channel, updateFulfillHTLCMsg
    }

    member self.ApplyUpdateFulfillHTLC (msg: UpdateFulfillHTLCMsg)
                                           : Result<Channel, ChannelError> = result {
        let! remoteNextCommitInfo =
            self.RemoteNextCommitInfoIfFundingLockedNormal "ApplyUpdateFulfullHTLC"
        let! newCommitments = Commitments.receiveFulfill msg self.Commitments remoteNextCommitInfo
        return {
            self with
                Commitments = newCommitments
        }
    }

    member self.FailHTLC (op: OperationFailHTLC)
                             : Result<Channel * UpdateFailHTLCMsg, ChannelError> = result {
        let! remoteNextCommitInfo =
            self.RemoteNextCommitInfoIfFundingLockedNormal "FailHTLC"
        let! updateFailHTLCMsg, newCommitments =
            Commitments.sendFail
                self.NodeSecret
                op
                self.Commitments
                self.StaticChannelConfig
                remoteNextCommitInfo
        let channel = {
            self with
                Commitments = newCommitments
        }
        return channel, updateFailHTLCMsg
    }

    member self.FailMalformedHTLC (op: OperationFailMalformedHTLC)
                                      : Result<Channel * UpdateFailMalformedHTLCMsg, ChannelError> = result {
        let! remoteNextCommitInfo =
            self.RemoteNextCommitInfoIfFundingLockedNormal "FailMalformedHTLC"
        let! updateFailMalformedHTLCMsg, newCommitments =
            Commitments.sendFailMalformed
                op
                self.Commitments
                self.StaticChannelConfig
                remoteNextCommitInfo
        let channel = {
            self with
                Commitments = newCommitments
        }
        return channel, updateFailMalformedHTLCMsg
    }

    member self.ApplyUpdateFailHTLC (msg: UpdateFailHTLCMsg)
                                        : Result<Channel, ChannelError> = result {
        let! remoteNextCommitInfo =
            self.RemoteNextCommitInfoIfFundingLockedNormal "ApplyUpdateFailHTLC"
        let! newCommitments = Commitments.receiveFail msg self.Commitments remoteNextCommitInfo
        return {
            self with
                Commitments = newCommitments
        }
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
        let makeClosingTx (channelPrivKeys: ChannelPrivKeys)
                          (cm: Commitments)
                          (localSpk: ShutdownScriptPubKey)
                          (remoteSpk: ShutdownScriptPubKey)
                          (closingFee: Money)
                          (staticChannelConfig: StaticChannelConfig) =
            let dustLimitSatoshis = Money.Max(staticChannelConfig.LocalParams.DustLimitSatoshis, staticChannelConfig.RemoteParams.DustLimitSatoshis)
            result {
                let! closingTx = Transactions.makeClosingTx staticChannelConfig.FundingScriptCoin (localSpk) (remoteSpk) staticChannelConfig.IsFunder (dustLimitSatoshis) (closingFee) (cm.LocalCommit.Spec) staticChannelConfig.Network
                let localSignature, psbtUpdated = channelPrivKeys.SignWithFundingPrivKey closingTx.Value
                let msg: ClosingSignedMsg = {
                    ChannelId = staticChannelConfig.ChannelId()
                    FeeSatoshis = closingFee
                    Signature = localSignature.Signature |> LNECDSASignature
                }
                return (ClosingTx psbtUpdated, msg)
            }

        let firstClosingFee (cm: Commitments)
                            (localSpk: ShutdownScriptPubKey)
                            (remoteSpk: ShutdownScriptPubKey)
                            (feeEst: IFeeEstimator)
                            (staticChannelConfig: StaticChannelConfig) =
            result {
                let! dummyClosingTx = Transactions.makeClosingTx staticChannelConfig.FundingScriptCoin localSpk remoteSpk staticChannelConfig.IsFunder Money.Zero Money.Zero cm.LocalCommit.Spec staticChannelConfig.Network
                let tx = dummyClosingTx.Value.GetGlobalTransaction()
                tx.Inputs.[0].WitScript <-
                    let witness = seq [ dummySig.ToBytes(); dummySig.ToBytes(); dummyClosingTx.Value.Inputs.[0].WitnessScript.ToBytes() ]
                    WitScript(witness)
                let feeRatePerKw = FeeRatePerKw.Max (feeEst.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority), cm.LocalCommit.Spec.FeeRatePerKw)
                return feeRatePerKw.CalculateFeeFromVirtualSize(tx)
            }

        let nextClosingFee (localClosingFee: Money, remoteClosingFee: Money) =
            ((localClosingFee.Satoshi + remoteClosingFee.Satoshi) / 4L) * 2L
            |> Money.Satoshis

    let executeCommand (cs: Channel) (command: ChannelCommand): Result<ChannelEvent list, ChannelError> =
        match command with
        | ApplyUpdateFailMalformedHTLC msg ->
            result {
                let! remoteNextCommitInfo =
                    cs.RemoteNextCommitInfoIfFundingLockedNormal "ApplyUpdateFailMalformedHTLC"
                return! Commitments.receiveFailMalformed msg cs.Commitments remoteNextCommitInfo
            }
        | UpdateFee op ->
            result {
                let! _remoteNextCommitInfo =
                    cs.RemoteNextCommitInfoIfFundingLockedNormal "UpdateFee"
                return! Commitments.sendFee op cs.StaticChannelConfig cs.Commitments
            }
        | ApplyUpdateFee msg ->
            result {
                let! _remoteNextCommitInfo =
                    cs.RemoteNextCommitInfoIfFundingLockedNormal "ApplyUpdateFee"
                let localFeerate = cs.ChannelOptions.FeeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority)
                return!
                    Commitments.receiveFee
                        cs.ChannelOptions
                        localFeerate
                        msg
                        cs.StaticChannelConfig
                        cs.Commitments
            }
        | SignCommitment ->
            let cm = cs.Commitments
            result {
                let! remoteNextCommitInfo =
                    cs.RemoteNextCommitInfoIfFundingLockedNormal "SignCommit"
                match remoteNextCommitInfo with
                | _ when (cm.LocalHasChanges() |> not) ->
                    // Ignore SignCommitment Command (nothing to sign)
                    return []
                | RemoteNextCommitInfo.Revoked _ ->
                    return!
                        Commitments.sendCommit
                            cs.ChannelPrivKeys
                            cm
                            cs.StaticChannelConfig
                            remoteNextCommitInfo
                | RemoteNextCommitInfo.Waiting _ ->
                    // Already in the process of signing
                    return []
            }
        | ApplyCommitmentSigned msg ->
            result {
                let! _remoteNextCommitInfo =
                    cs.RemoteNextCommitInfoIfFundingLockedNormal "ApplyCommitmentSigned"
                return!
                    Commitments.receiveCommit
                        cs.ChannelPrivKeys
                        msg
                        cs.StaticChannelConfig
                        cs.Commitments
            }
        | ApplyRevokeAndACK msg ->
            result {
                let! remoteNextCommitInfo =
                    cs.RemoteNextCommitInfoIfFundingLockedNormal "ApplyRevokeAndACK"
                let cm = cs.Commitments
                match remoteNextCommitInfo with
                | RemoteNextCommitInfo.Waiting _ when (msg.PerCommitmentSecret.PerCommitmentPoint() <> cm.RemoteCommit.RemotePerCommitmentPoint) ->
                    let errorMsg = sprintf "Invalid revoke_and_ack %A; must be %A" msg.PerCommitmentSecret cm.RemoteCommit.RemotePerCommitmentPoint
                    return! Error <| invalidRevokeAndACK msg errorMsg
                | RemoteNextCommitInfo.Revoked _ ->
                    let errorMsg = sprintf "Unexpected revocation"
                    return! Error <| invalidRevokeAndACK msg errorMsg
                | RemoteNextCommitInfo.Waiting theirNextCommit ->
                    let remotePerCommitmentSecretsOpt =
                        cm.RemotePerCommitmentSecrets.InsertPerCommitmentSecret
                            cm.RemoteCommit.Index
                            msg.PerCommitmentSecret
                    match remotePerCommitmentSecretsOpt with
                    | Error err -> return! Error <| invalidRevokeAndACK msg err.Message
                    | Ok remotePerCommitmentSecrets ->
                        let commitments1 = {
                            cm with
                                LocalChanges = {
                                    cm.LocalChanges with
                                        Signed = [];
                                        ACKed = cm.LocalChanges.ACKed @ cm.LocalChanges.Signed
                                }
                                RemoteChanges = {
                                    cm.RemoteChanges with
                                        Signed = []
                                }
                                RemoteCommit = theirNextCommit
                                RemotePerCommitmentSecrets = remotePerCommitmentSecrets
                        }
                        return [ WeAcceptedRevokeAndACK(commitments1, msg.NextPerCommitmentPoint) ]
            }
        | ChannelCommand.Close localShutdownScriptPubKey ->
            result {
                if cs.NegotiatingState.LocalRequestedShutdown.IsSome then
                    do! Error <| cannotCloseChannel "shutdown is already in progress"
                do!
                    Validation.checkShutdownScriptPubKeyAcceptable
                        cs.StaticChannelConfig.LocalStaticShutdownScriptPubKey
                        localShutdownScriptPubKey
                if (cs.Commitments.LocalHasUnsignedOutgoingHTLCs()) then
                    do! Error <| cannotCloseChannel "Cannot close with unsigned outgoing htlcs"
                let shutdownMsg: ShutdownMsg = {
                    ChannelId = cs.StaticChannelConfig.ChannelId()
                    ScriptPubKey = localShutdownScriptPubKey
                }
                return [ AcceptedOperationShutdown(shutdownMsg, localShutdownScriptPubKey) ]
            }
        | RemoteShutdown(msg, localShutdownScriptPubKey) ->
            result {
                let remoteShutdownScriptPubKey = msg.ScriptPubKey
                do!
                    Validation.checkShutdownScriptPubKeyAcceptable
                        cs.StaticChannelConfig.LocalStaticShutdownScriptPubKey
                        localShutdownScriptPubKey
                do!
                    Validation.checkShutdownScriptPubKeyAcceptable
                        cs.StaticChannelConfig.RemoteStaticShutdownScriptPubKey
                        remoteShutdownScriptPubKey
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
                    let remoteNextCommitInfo =
                        match cs.RemoteNextCommitInfo with
                        | None -> failwith "can't have unsigned outgoing htlcs if we haven't recieved funding_locked. this should never happen"
                        | Some remoteNextCommitInfo -> remoteNextCommitInfo
                    // Are we in the middle of a signature?
                    match remoteNextCommitInfo with
                    // yes.
                    | RemoteNextCommitInfo.Waiting _waitingForRevocation ->
                        return [
                            AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs remoteShutdownScriptPubKey
                        ]
                    // No. let's sign right away.
                    | RemoteNextCommitInfo.Revoked _ ->
                        return [
                            ChannelStateRequestedSignCommitment;
                            AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs remoteShutdownScriptPubKey
                        ]
                else
                    let hasNoPendingHTLCs =
                        match cs.RemoteNextCommitInfo with
                        | None -> true
                        | Some remoteNextCommitInfo -> cm.HasNoPendingHTLCs remoteNextCommitInfo
                    if hasNoPendingHTLCs then
                        // we have to send first closing_signed msg iif we are the funder
                        if cs.StaticChannelConfig.IsFunder then
                            let! closingFee =
                                Closing.firstClosingFee
                                    cm
                                    localShutdownScriptPubKey
                                    remoteShutdownScriptPubKey
                                    cs.ChannelOptions.FeeEstimator
                                    cs.StaticChannelConfig
                                |> expectTransactionError
                            let! (_closingTx, closingSignedMsg) =
                                Closing.makeClosingTx
                                    cs.ChannelPrivKeys
                                    cm
                                    localShutdownScriptPubKey
                                    remoteShutdownScriptPubKey
                                    closingFee
                                    cs.StaticChannelConfig
                                |> expectTransactionError
                            let nextState = {
                                LocalRequestedShutdown = Some localShutdownScriptPubKey
                                RemoteRequestedShutdown = Some remoteShutdownScriptPubKey
                                LocalClosingFeesProposed = [ closingFee ]
                                RemoteClosingFeeProposed = None
                            }
                            return [
                                AcceptedShutdownWhenNoPendingHTLCs(
                                    closingSignedMsg |> Some,
                                    nextState
                                )
                            ]
                        else
                            let nextState = {
                                LocalRequestedShutdown = Some localShutdownScriptPubKey
                                RemoteRequestedShutdown = Some remoteShutdownScriptPubKey
                                LocalClosingFeesProposed = []
                                RemoteClosingFeeProposed = None
                            }
                            return [ AcceptedShutdownWhenNoPendingHTLCs(None, nextState) ]
                    else
                        let localShutdownMsg: ShutdownMsg = {
                            ChannelId = cs.StaticChannelConfig.ChannelId()
                            ScriptPubKey = localShutdownScriptPubKey
                        }
                        return [
                            AcceptedShutdownWhenWeHavePendingHTLCs(
                                localShutdownMsg,
                                localShutdownScriptPubKey,
                                remoteShutdownScriptPubKey
                            )
                        ]
            }
        | ApplyClosingSigned msg ->
            result {
                let! localShutdownScriptPubKey, remoteShutdownScriptPubKey =
                    match (cs.NegotiatingState.LocalRequestedShutdown, cs.NegotiatingState.RemoteRequestedShutdown) with
                    | (Some localShutdownScriptPubKey, Some remoteShutdownScriptPubKey) ->
                        Ok (localShutdownScriptPubKey, remoteShutdownScriptPubKey)
                    // FIXME: these should be new channel errors
                    | (Some _, None) ->
                        Error ReceivedClosingSignedBeforeReceivingShutdown
                    | (None, Some _) ->
                        Error ReceivedClosingSignedBeforeSendingShutdown
                    | (None, None) ->
                        Error ReceivedClosingSignedBeforeSendingOrReceivingShutdown
                let cm = cs.Commitments
                let remoteChannelKeys = cs.StaticChannelConfig.RemoteChannelPubKeys
                let lastCommitFeeSatoshi =
                    cs.StaticChannelConfig.FundingScriptCoin.TxOut.Value - (cm.LocalCommit.PublishableTxs.CommitTx.Value.TotalOut)
                do! checkRemoteProposedHigherFeeThanBaseFee lastCommitFeeSatoshi msg.FeeSatoshis
                do!
                    checkRemoteProposedFeeWithinNegotiatedRange
                        (List.tryHead cs.NegotiatingState.LocalClosingFeesProposed)
                        (Option.map (fun (fee, _sig) -> fee) cs.NegotiatingState.RemoteClosingFeeProposed)
                        msg.FeeSatoshis

                let! closingTx, closingSignedMsg =
                    Closing.makeClosingTx
                        cs.ChannelPrivKeys
                        cm
                        localShutdownScriptPubKey
                        remoteShutdownScriptPubKey
                        msg.FeeSatoshis
                        cs.StaticChannelConfig
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
                    cs.NegotiatingState.LocalClosingFeesProposed
                    |> List.tryHead
                let areWeInDeal = Some(msg.FeeSatoshis) = maybeLocalFee
                let hasTooManyNegotiationDone =
                    (cs.NegotiatingState.LocalClosingFeesProposed |> List.length) >= cs.ChannelOptions.MaxClosingNegotiationIterations
                if (areWeInDeal || hasTooManyNegotiationDone) then
                    return [ MutualClosePerformed (finalizedTx, None) ]
                else
                    let lastLocalClosingFee = cs.NegotiatingState.LocalClosingFeesProposed |> List.tryHead
                    let! localF = 
                        match lastLocalClosingFee with
                        | Some v -> Ok v
                        | None ->
                            Closing.firstClosingFee
                                cs.Commitments
                                localShutdownScriptPubKey
                                remoteShutdownScriptPubKey
                                cs.ChannelOptions.FeeEstimator
                                cs.StaticChannelConfig
                            |> expectTransactionError
                    let nextClosingFee =
                        Closing.nextClosingFee (localF, msg.FeeSatoshis)
                    if (Some nextClosingFee = lastLocalClosingFee) then
                        return [ MutualClosePerformed (finalizedTx, None) ]
                    else if (nextClosingFee = msg.FeeSatoshis) then
                        // we have reached on agreement!
                        return [ MutualClosePerformed (finalizedTx, Some closingSignedMsg) ]
                    else
                        let! _closingTx, closingSignedMsg =
                            Closing.makeClosingTx
                                cs.ChannelPrivKeys
                                cm
                                localShutdownScriptPubKey
                                remoteShutdownScriptPubKey
                                nextClosingFee
                                cs.StaticChannelConfig
                            |> expectTransactionError
                        let nextState = {
                            cs.NegotiatingState with
                                LocalClosingFeesProposed =
                                    nextClosingFee :: cs.NegotiatingState.LocalClosingFeesProposed
                                RemoteClosingFeeProposed = Some (msg.FeeSatoshis, msg.Signature)
                        }
                        return [ WeProposedNewClosingSigned(closingSignedMsg, nextState) ]
            }
        | _cmd ->
            failwith "not implemented"

    let applyEvent c (e: ChannelEvent): Channel =
        match e with
        // ----- normal operation --------
        | WeAcceptedFailMalformedHTLC(_origin, _msg, newCommitments) ->
            { c with Commitments = newCommitments }

        | WeAcceptedOperationUpdateFee(_msg, newCommitments) ->
            { c with Commitments = newCommitments }
        | WeAcceptedUpdateFee(_msg, newCommitments) ->
            { c with Commitments = newCommitments }

        | WeAcceptedOperationSign(_msg, newCommitments, waitingForRevocation) ->
            {
                c with
                    Commitments = newCommitments
                    RemoteNextCommitInfo =
                        Some <| RemoteNextCommitInfo.Waiting waitingForRevocation
            }
        | WeAcceptedCommitmentSigned(_msg, newCommitments) ->
            { c with Commitments = newCommitments }

        | WeAcceptedRevokeAndACK(newCommitments, remoteNextPerCommitmentPoint) ->
            {
                c with
                    Commitments = newCommitments
                    RemoteNextCommitInfo =
                        Some <| RemoteNextCommitInfo.Revoked remoteNextPerCommitmentPoint
            }

        // -----  closing ------
        | AcceptedOperationShutdown(_msg, shutdownScriptPubKey) ->
            {
                c with
                    NegotiatingState = {
                        c.NegotiatingState with
                            LocalRequestedShutdown = Some shutdownScriptPubKey
                    }
            }
        | AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs remoteShutdownScriptPubKey ->
            { 
                c with
                    NegotiatingState = {
                        c.NegotiatingState with
                            RemoteRequestedShutdown = Some remoteShutdownScriptPubKey
                    }
            }
        | AcceptedShutdownWhenNoPendingHTLCs(_maybeMsg, nextNegotiatingState) ->
            {
                c with
                    NegotiatingState = nextNegotiatingState
            }
        | AcceptedShutdownWhenWeHavePendingHTLCs(_localShutdownMsg, localShutdownScriptPubKey, remoteShutdownScriptPubKey) ->
            {
                c with
                    NegotiatingState = {
                        c.NegotiatingState with
                            LocalRequestedShutdown = Some localShutdownScriptPubKey
                            RemoteRequestedShutdown = Some remoteShutdownScriptPubKey
                    }
            }
        | MutualClosePerformed (_txToPublish, _) -> c
        | WeProposedNewClosingSigned(_msg, nextNegotiatingState) ->
            {
                c with
                    NegotiatingState = nextNegotiatingState
            }
        // ----- else -----
        | _otherEvent -> c
