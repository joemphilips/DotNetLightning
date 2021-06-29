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
} with
    member this.ApplyFundingSigned (msg: FundingSignedMsg)
                                       : Result<FinalizedTx * Channel, ChannelError> = result {
        let! finalizedLocalCommitTx =
            let theirFundingPk = this.StaticChannelConfig.RemoteChannelPubKeys.FundingPubKey.RawPubKey()
            let _, signedLocalCommitTx =
                this.ChannelPrivKeys.SignWithFundingPrivKey this.LocalCommitTx.Value
            let remoteSigPairOfLocalTx = (theirFundingPk,  TransactionSignature(msg.Signature.Value, SigHash.All))
            let sigPairs = seq [ remoteSigPairOfLocalTx; ]
            Transactions.checkTxFinalized signedLocalCommitTx CommitTx.WhichInput sigPairs |> expectTransactionError
        let commitments = {
            ProposedLocalChanges = List.empty
            ProposedRemoteChanges = List.empty
            LocalNextHTLCId = HTLCId.Zero
            RemoteNextHTLCId = HTLCId.Zero
            OriginChannels = Map.empty
        }
        let channel = {
            SavedChannelState = {
                StaticChannelConfig = this.StaticChannelConfig
                RemotePerCommitmentSecrets = PerCommitmentSecretStore()
                ShortChannelId = None
                LocalCommit = {
                    Index = CommitmentNumber.FirstCommitment
                    Spec = this.LocalSpec
                    PublishableTxs = {
                        PublishableTxs.CommitTx = finalizedLocalCommitTx
                        HTLCTxs = []
                    }
                    PendingHTLCSuccessTxs = []
                }
                RemoteCommit = this.RemoteCommit
                LocalChanges = LocalChanges.Zero
                RemoteChanges = RemoteChanges.Zero
            }
            ChannelOptions = this.ChannelOptions
            ChannelPrivKeys = this.ChannelPrivKeys
            NodeSecret = this.NodeSecret
            RemoteNextCommitInfo = None
            NegotiatingState = NegotiatingState.New()
            Commitments = commitments
        }
        return this.FundingTx, channel
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
    LocalParams: LocalParams
    RemoteParams: RemoteParams
    RemoteChannelPubKeys: ChannelPubKeys
    FundingSatoshis: Money
    PushMSat: LNMoney
    InitialFeeRatePerKw: FeeRatePerKw
    RemoteFirstPerCommitmentPoint: PerCommitmentPoint
} with
    member this.ApplyFundingCreated (msg: FundingCreatedMsg)
                                        : Result<FundingSignedMsg * Channel, ChannelError> = result {
        let! (localSpec, localCommitTx, remoteSpec, remoteCommitTx) =
            let firstPerCommitmentPoint =
                this.ChannelPrivKeys.CommitmentSeed.DerivePerCommitmentPoint
                    CommitmentNumber.FirstCommitment
            ChannelHelpers.makeFirstCommitTxs
                false
                (this.ChannelPrivKeys.ToChannelPubKeys())
                this.RemoteChannelPubKeys
                this.LocalParams
                this.RemoteParams
                this.FundingSatoshis
                this.PushMSat
                this.InitialFeeRatePerKw
                msg.FundingOutputIndex
                msg.FundingTxId
                firstPerCommitmentPoint
                this.RemoteFirstPerCommitmentPoint
                this.Network
        assert (localCommitTx.Value.IsReadyToSign())
        let _s, signedLocalCommitTx =
            this.ChannelPrivKeys.SignWithFundingPrivKey localCommitTx.Value
        let remoteTxSig = TransactionSignature(msg.Signature.Value, SigHash.All)
        let theirSigPair = (this.RemoteChannelPubKeys.FundingPubKey.RawPubKey(), remoteTxSig)
        let sigPairs = seq [ theirSigPair ]
        let! finalizedCommitTx =
            Transactions.checkTxFinalized (signedLocalCommitTx) CommitTx.WhichInput sigPairs
            |> expectTransactionError
        let localSigOfRemoteCommit, _ =
            this.ChannelPrivKeys.SignWithFundingPrivKey remoteCommitTx.Value
        let fundingScriptCoin =
            ChannelHelpers.getFundingScriptCoin
                (this.ChannelPrivKeys.FundingPrivKey.FundingPubKey())
                this.RemoteChannelPubKeys.FundingPubKey
                msg.FundingTxId
                msg.FundingOutputIndex
                this.FundingSatoshis
        let commitments = {
            ProposedLocalChanges = List.empty
            ProposedRemoteChanges = List.empty
            LocalNextHTLCId = HTLCId.Zero
            RemoteNextHTLCId = HTLCId.Zero
            OriginChannels = Map.empty
        }
        let staticChannelConfig = {
            AnnounceChannel = this.AnnounceChannel
            RemoteNodeId = this.RemoteNodeId
            Network = this.Network
            FundingTxMinimumDepth = this.FundingTxMinimumDepth
            LocalStaticShutdownScriptPubKey = this.LocalStaticShutdownScriptPubKey
            RemoteStaticShutdownScriptPubKey = this.RemoteStaticShutdownScriptPubKey
            IsFunder = this.IsFunder
            FundingScriptCoin = fundingScriptCoin
            LocalParams = this.LocalParams
            RemoteParams = this.RemoteParams
            RemoteChannelPubKeys = this.RemoteChannelPubKeys
        }
        let channelId = staticChannelConfig.ChannelId()
        let msgToSend: FundingSignedMsg = {
            ChannelId = channelId
            Signature = !>localSigOfRemoteCommit.Signature
        }
        let channel = {
            SavedChannelState = {
                StaticChannelConfig = staticChannelConfig
                RemotePerCommitmentSecrets = PerCommitmentSecretStore()
                ShortChannelId = None
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
                    RemotePerCommitmentPoint = this.RemoteFirstPerCommitmentPoint
                }
                LocalChanges = LocalChanges.Zero
                RemoteChanges = RemoteChanges.Zero
            }
            ChannelOptions = this.ChannelOptions
            ChannelPrivKeys = this.ChannelPrivKeys
            NodeSecret = this.NodeSecret
            RemoteNextCommitInfo = None
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
    TemporaryChannelId: ChannelId
    RemoteChannelPubKeys: ChannelPubKeys
    FundingSatoshis: Money
    PushMSat: LNMoney
    InitFeeRatePerKw: FeeRatePerKw
    LocalParams: LocalParams
    RemoteFirstPerCommitmentPoint: PerCommitmentPoint
    RemoteParams: RemoteParams
    RemoteInit: InitMsg
    FundingTxMinimumDepth: BlockHeightOffset32
} with
    member this.CreateFundingTx (fundingTx: FinalizedTx)
                                (outIndex: TxOutIndex)
                                    : Result<FundingCreatedMsg * ChannelWaitingForFundingSigned, ChannelError> = result {
        let localParams = this.LocalParams
        let remoteParams = this.RemoteParams
        let commitmentSpec = CommitmentSpec.Create (this.FundingSatoshis.ToLNMoney() - this.PushMSat) this.PushMSat this.InitFeeRatePerKw
        let commitmentSeed = this.ChannelPrivKeys.CommitmentSeed
        let fundingTxId = fundingTx.Value.GetTxId()
        let! (_localSpec, localCommitTx, remoteSpec, remoteCommitTx) =
            ChannelHelpers.makeFirstCommitTxs
                true
                (this.ChannelPrivKeys.ToChannelPubKeys())
                this.RemoteChannelPubKeys
                localParams
                remoteParams
                this.FundingSatoshis
                this.PushMSat
                this.InitFeeRatePerKw
                outIndex
                fundingTxId
                (commitmentSeed.DerivePerCommitmentPoint CommitmentNumber.FirstCommitment)
                this.RemoteFirstPerCommitmentPoint
                this.Network
        let localSigOfRemoteCommit, _ =
            this.ChannelPrivKeys.SignWithFundingPrivKey remoteCommitTx.Value
        let nextMsg: FundingCreatedMsg = {
            TemporaryChannelId = this.TemporaryChannelId
            FundingTxId = fundingTxId
            FundingOutputIndex = outIndex
            Signature = !>localSigOfRemoteCommit.Signature
        }
        let fundingScriptCoin =
            ChannelHelpers.getFundingScriptCoin
                (this.ChannelPrivKeys.FundingPrivKey.FundingPubKey())
                this.RemoteChannelPubKeys.FundingPubKey
                fundingTxId
                outIndex
                this.FundingSatoshis
        let channelId = OutPoint(fundingTxId.Value, uint32 outIndex.Value).ToChannelId()
        let channelWaitingForFundingSigned = {
            StaticChannelConfig = {
                AnnounceChannel = this.AnnounceChannel
                RemoteNodeId = this.RemoteNodeId
                Network = this.Network
                FundingTxMinimumDepth = this.FundingTxMinimumDepth
                LocalStaticShutdownScriptPubKey = this.LocalStaticShutdownScriptPubKey
                RemoteStaticShutdownScriptPubKey = this.RemoteStaticShutdownScriptPubKey
                IsFunder = true
                FundingScriptCoin = fundingScriptCoin
                LocalParams = localParams
                RemoteParams = remoteParams
                RemoteChannelPubKeys = this.RemoteChannelPubKeys
            }
            ChannelOptions = this.ChannelOptions
            ChannelPrivKeys = this.ChannelPrivKeys
            NodeSecret = this.NodeSecret
            ChannelId = channelId
            FundingTx = fundingTx
            LocalSpec = commitmentSpec
            LocalCommitTx = localCommitTx
            RemoteCommit = {
                RemoteCommit.Index = CommitmentNumber.FirstCommitment
                Spec = remoteSpec
                TxId = remoteCommitTx.Value.GetGlobalTransaction().GetTxId()
                RemotePerCommitmentPoint = this.RemoteFirstPerCommitmentPoint
            }
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
    member this.ApplyAcceptChannel (msg: AcceptChannelMsg)
                                       : Result<IDestination * Money * ChannelWaitingForFundingTx, ChannelError> = result {
        do!
            Validation.checkAcceptChannelMsgAcceptable
                this.ChannelHandshakeLimits
                this.FundingSatoshis
                this.LocalParams.ChannelReserveSatoshis
                this.LocalParams.DustLimitSatoshis msg
        let redeem =
            Scripts.funding
                (this.ChannelPrivKeys.ToChannelPubKeys().FundingPubKey)
                msg.FundingPubKey
        let remoteChannelPubKeys = {
            FundingPubKey = msg.FundingPubKey
            RevocationBasepoint = msg.RevocationBasepoint
            PaymentBasepoint = msg.PaymentBasepoint
            DelayedPaymentBasepoint = msg.DelayedPaymentBasepoint
            HtlcBasepoint = msg.HTLCBasepoint
        }
        let destination = redeem.WitHash :> IDestination
        let amount = this.FundingSatoshis
        let remoteParams = RemoteParams.FromAcceptChannel this.RemoteInit msg
        let channelWaitingForFundingTx = {
            AnnounceChannel = this.AnnounceChannel
            ChannelOptions = this.ChannelOptions
            ChannelPrivKeys = this.ChannelPrivKeys
            RemoteNodeId = this.RemoteNodeId
            NodeSecret = this.NodeSecret
            Network = this.Network
            LocalStaticShutdownScriptPubKey = this.LocalStaticShutdownScriptPubKey
            RemoteStaticShutdownScriptPubKey = msg.ShutdownScriptPubKey()
            TemporaryChannelId = msg.TemporaryChannelId
            RemoteChannelPubKeys = remoteChannelPubKeys
            FundingSatoshis = this.FundingSatoshis
            PushMSat = this.PushMSat
            InitFeeRatePerKw = this.InitFeeRatePerKw
            LocalParams = this.LocalParams
            RemoteInit = this.RemoteInit
            RemoteFirstPerCommitmentPoint = msg.FirstPerCommitmentPoint
            FundingTxMinimumDepth = msg.MinimumDepth
            RemoteParams = remoteParams
        }
        return destination, amount, channelWaitingForFundingTx
    }

and Channel = {
    SavedChannelState: SavedChannelState
    ChannelOptions: ChannelOptions
    ChannelPrivKeys: ChannelPrivKeys
    NodeSecret: NodeSecret
    RemoteNextCommitInfo: Option<RemoteNextCommitInfo>
    NegotiatingState: NegotiatingState
    Commitments: Commitments
 }
    with

    member internal this.RemoteNextCommitInfoIfFundingLocked (operation: string)
                                                                 : Result<RemoteNextCommitInfo, ChannelError> =
        match this.RemoteNextCommitInfo with
        | None ->
            sprintf
                "cannot perform operation %s because peer has not sent funding_locked"
                operation
            |> apiMisuse
        | Some remoteNextCommitInfo -> Ok remoteNextCommitInfo

    member internal this.RemoteNextCommitInfoIfFundingLockedNormal (operation: string)
                                                                       : Result<RemoteNextCommitInfo, ChannelError> =
        match this.SavedChannelState.ShortChannelId with
        | None ->
            sprintf
                "cannot perform operation %s because funding is not confirmed"
                operation
            |> apiMisuse
        | Some _ ->
            this.RemoteNextCommitInfoIfFundingLocked operation

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
            do! Validation.checkOurOpenChannelMsgAcceptable openChannelMsgToSend localParams
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
                    localParams
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

    member this.CreateChannelReestablish (): ChannelReestablishMsg =
        let commitmentSeed = this.ChannelPrivKeys.CommitmentSeed
        let ourChannelReestablish = {
            ChannelId = this.SavedChannelState.StaticChannelConfig.ChannelId()
            NextCommitmentNumber =
                (this.SavedChannelState.RemotePerCommitmentSecrets.NextCommitmentNumber().NextCommitment())
            NextRevocationNumber =
                this.SavedChannelState.RemotePerCommitmentSecrets.NextCommitmentNumber()
            DataLossProtect = OptionalField.Some <| {
                YourLastPerCommitmentSecret =
                    this.SavedChannelState.RemotePerCommitmentSecrets.MostRecentPerCommitmentSecret()
                MyCurrentPerCommitmentPoint =
                    commitmentSeed.DerivePerCommitmentPoint this.SavedChannelState.RemoteCommit.Index
            }
        }
        ourChannelReestablish

    member this.ApplyFundingLocked (fundingLockedMsg: FundingLockedMsg)
                                       : Result<Channel, ChannelError> = result {
        do!
            match this.RemoteNextCommitInfo with
            | None -> Ok ()
            | Some remoteNextCommitInfo ->
                if remoteNextCommitInfo.PerCommitmentPoint()
                    <> fundingLockedMsg.NextPerCommitmentPoint then
                    Error <| InvalidFundingLocked { NetworkMsg = fundingLockedMsg }
                else
                    Ok ()
        return {
            this with
                RemoteNextCommitInfo =
                    RemoteNextCommitInfo.Revoked fundingLockedMsg.NextPerCommitmentPoint
                    |> Some
        }
    }

    member this.ApplyFundingConfirmedOnBC (height: BlockHeight)
                                          (txindex: TxIndexInBlock)
                                          (depth: BlockHeightOffset32)
                                              : Result<Channel * FundingLockedMsg, ChannelError> = result {
        let requiredDepth = this.SavedChannelState.StaticChannelConfig.FundingTxMinimumDepth
        if depth < requiredDepth then
            return! Error <| InsufficientConfirmations (requiredDepth, depth)
        else
            let nextPerCommitmentPoint =
                this.ChannelPrivKeys.CommitmentSeed.DerivePerCommitmentPoint
                    (CommitmentNumber.FirstCommitment.NextCommitment())
            let msgToSend: FundingLockedMsg = {
                ChannelId = this.SavedChannelState.StaticChannelConfig.ChannelId()
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
                    this.SavedChannelState.StaticChannelConfig.FundingScriptCoin.Outpoint.N
                    |> uint16
                    |> TxOutIndex
            }
            let savedChannelState = {
                this.SavedChannelState with
                    ShortChannelId = Some shortChannelId
            }
            let channel = {
                this with
                    SavedChannelState = savedChannelState
            }
            return channel, msgToSend
    }

    member this.AddHTLC (op: OperationAddHTLC)
                            : Result<Channel * UpdateAddHTLCMsg, ChannelError> = result {
        if this.NegotiatingState.HasEnteredShutdown() then
            return!
                sprintf "Could not add new HTLC %A since shutdown is already in progress." op
                |> apiMisuse
        else
            do! Validation.checkOperationAddHTLC this.SavedChannelState.StaticChannelConfig.RemoteParams op
            let add: UpdateAddHTLCMsg = {
                ChannelId = this.SavedChannelState.StaticChannelConfig.ChannelId()
                HTLCId = this.Commitments.LocalNextHTLCId
                Amount = op.Amount
                PaymentHash = op.PaymentHash
                CLTVExpiry = op.Expiry
                OnionRoutingPacket = op.Onion
            }
            let commitments1 =
                let commitments = {
                    this.Commitments.AddLocalProposal(add) with
                        LocalNextHTLCId = this.Commitments.LocalNextHTLCId + 1UL
                }
                match op.Origin with
                | None -> commitments
                | Some origin -> {
                    commitments with
                        OriginChannels =
                            this.Commitments.OriginChannels
                            |> Map.add add.HTLCId origin
                }

            let! remoteNextCommitInfo =
                this.RemoteNextCommitInfoIfFundingLockedNormal "AddHTLC"
            // we need to base the next current commitment on the last sig we sent, even if we didn't yet receive their revocation
            let remoteCommit1 =
                match remoteNextCommitInfo with
                | Waiting nextRemoteCommit -> nextRemoteCommit
                | Revoked _info -> this.SavedChannelState.RemoteCommit
            let! reduced = remoteCommit1.Spec.Reduce(this.SavedChannelState.RemoteChanges.ACKed, commitments1.ProposedLocalChanges) |> expectTransactionError
            do!
                Validation.checkOurUpdateAddHTLCIsAcceptableWithCurrentSpec
                    reduced
                    this.SavedChannelState.StaticChannelConfig
                    add
            let channel = {
                this with
                    Commitments = commitments1
            }
            return channel, add
    }

    member this.ApplyUpdateAddHTLC (msg: UpdateAddHTLCMsg)
                                   (height: BlockHeight)
                                       : Result<Channel, ChannelError> = result {
        do!
            Validation.checkTheirUpdateAddHTLCIsAcceptable
                this.Commitments
                this.SavedChannelState.StaticChannelConfig.LocalParams
                msg
                height
        let commitments1 = {
            this.Commitments.AddRemoteProposal(msg) with
                RemoteNextHTLCId = this.Commitments.LocalNextHTLCId + 1UL
        }
        let! reduced =
            this.SavedChannelState.LocalCommit.Spec.Reduce (
                this.SavedChannelState.LocalChanges.ACKed,
                commitments1.ProposedRemoteChanges
            ) |> expectTransactionError
        do!
            Validation.checkTheirUpdateAddHTLCIsAcceptableWithCurrentSpec
                reduced
                this.SavedChannelState.StaticChannelConfig
                msg
        return {
            this with
                Commitments = commitments1
        }
    }

    member this.FulFillHTLC (cmd: OperationFulfillHTLC)
                                : Result<Channel * UpdateFulfillHTLCMsg, ChannelError> = result {
        let! remoteNextCommitInfo =
            this.RemoteNextCommitInfoIfFundingLockedNormal "FulfillHTLC"
        let! updateFulfillHTLCMsg, newCommitments =
            Commitments.sendFulfill
                cmd
                this.Commitments
                this.SavedChannelState
                remoteNextCommitInfo

        let channel = {
            this with
                Commitments = newCommitments
        }
        return channel, updateFulfillHTLCMsg
    }

    member this.ApplyUpdateFulfillHTLC (msg: UpdateFulfillHTLCMsg)
                                           : Result<Channel, ChannelError> = result {
        let! remoteNextCommitInfo =
            this.RemoteNextCommitInfoIfFundingLockedNormal "ApplyUpdateFulfullHTLC"
        let! newCommitments =
            Commitments.receiveFulfill
                msg
                this.Commitments
                this.SavedChannelState
                remoteNextCommitInfo
        return {
            this with
                Commitments = newCommitments
        }
    }

    member this.FailHTLC (op: OperationFailHTLC)
                             : Result<Channel * UpdateFailHTLCMsg, ChannelError> = result {
        let! remoteNextCommitInfo =
            this.RemoteNextCommitInfoIfFundingLockedNormal "FailHTLC"
        let! updateFailHTLCMsg, newCommitments =
            Commitments.sendFail
                this.NodeSecret
                op
                this.Commitments
                this.SavedChannelState
                remoteNextCommitInfo
        let channel = {
            this with
                Commitments = newCommitments
        }
        return channel, updateFailHTLCMsg
    }

    member this.FailMalformedHTLC (op: OperationFailMalformedHTLC)
                                      : Result<Channel * UpdateFailMalformedHTLCMsg, ChannelError> = result {
        let! remoteNextCommitInfo =
            this.RemoteNextCommitInfoIfFundingLockedNormal "FailMalformedHTLC"
        let! updateFailMalformedHTLCMsg, newCommitments =
            Commitments.sendFailMalformed
                op
                this.Commitments
                this.SavedChannelState
                remoteNextCommitInfo
        let channel = {
            this with
                Commitments = newCommitments
        }
        return channel, updateFailMalformedHTLCMsg
    }

    member this.ApplyUpdateFailHTLC (msg: UpdateFailHTLCMsg)
                                        : Result<Channel, ChannelError> = result {
        let! remoteNextCommitInfo =
            this.RemoteNextCommitInfoIfFundingLockedNormal "ApplyUpdateFailHTLC"
        let! newCommitments =
            Commitments.receiveFail
                msg
                this.Commitments
                this.SavedChannelState
                remoteNextCommitInfo
        return {
            this with
                Commitments = newCommitments
        }
    }

    member this.ApplyUpdateFailMalformedHTLC (msg: UpdateFailMalformedHTLCMsg)
                                                 : Result<Channel, ChannelError> = result {
        let! remoteNextCommitInfo =
            this.RemoteNextCommitInfoIfFundingLockedNormal "ApplyUpdateFailMalformedHTLC"
        let! newCommitments =
            Commitments.receiveFailMalformed
                msg
                this.Commitments
                this.SavedChannelState
                remoteNextCommitInfo
        return {
            this with
                Commitments = newCommitments
        }
    }

    member this.UpdateFee (op: OperationUpdateFee)
                              : Result<Channel * UpdateFeeMsg, ChannelError> = result {
        let! _remoteNextCommitInfo =
            this.RemoteNextCommitInfoIfFundingLockedNormal "UpdateFee"
        let! updateFeeMsg, newCommitments =
            Commitments.sendFee op this.SavedChannelState this.Commitments
        let channel = {
            this with
                Commitments = newCommitments
        }
        return channel, updateFeeMsg
    }

    member this.ApplyUpdateFee (msg: UpdateFeeMsg)
                                   : Result<Channel, ChannelError> = result {
        let! _remoteNextCommitInfo =
            this.RemoteNextCommitInfoIfFundingLockedNormal "ApplyUpdateFee"
        let localFeerate = this.ChannelOptions.FeeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority)
        let! newCommitments =
            Commitments.receiveFee
                this.ChannelOptions
                localFeerate
                msg
                this.SavedChannelState
                this.Commitments
        return {
            this with
                Commitments = newCommitments
        }
    }

    member this.SignCommitment(): Result<Channel * CommitmentSignedMsg, ChannelError> = result {
        let! remoteNextCommitInfo =
            this.RemoteNextCommitInfoIfFundingLockedNormal "SignCommit"
        match remoteNextCommitInfo with
        | _ when (this.LocalHasChanges() |> not) ->
            return! Error NoUpdatesToSign
        | RemoteNextCommitInfo.Revoked _ ->
            let! commitmentSignedMsg, channel =
                this.sendCommit remoteNextCommitInfo
            return channel, commitmentSignedMsg
        | RemoteNextCommitInfo.Waiting _ ->
            return! Error CannotSignCommitmentBeforeRevocation
    }

    member this.ApplyCommitmentSigned (msg: CommitmentSignedMsg)
                                          : Result<Channel * RevokeAndACKMsg, ChannelError> = result {
        let! _remoteNextCommitInfo =
            this.RemoteNextCommitInfoIfFundingLockedNormal "ApplyCommitmentSigned"
        let! revokeAndACKMsg, channel = this.receiveCommit msg
        return channel, revokeAndACKMsg
    }

    member this.ApplyRevokeAndACK (msg: RevokeAndACKMsg)
                                      : Result<Channel, ChannelError> = result {
        let! remoteNextCommitInfo =
            this.RemoteNextCommitInfoIfFundingLockedNormal "ApplyRevokeAndACK"
        match remoteNextCommitInfo with
        | RemoteNextCommitInfo.Waiting _ when (msg.PerCommitmentSecret.PerCommitmentPoint() <> this.SavedChannelState.RemoteCommit.RemotePerCommitmentPoint) ->
            let errorMsg = sprintf "Invalid revoke_and_ack %A; must be %A" msg.PerCommitmentSecret this.SavedChannelState.RemoteCommit.RemotePerCommitmentPoint
            return! Error <| invalidRevokeAndACK msg errorMsg
        | RemoteNextCommitInfo.Revoked _ ->
            let errorMsg = sprintf "Unexpected revocation"
            return! Error <| invalidRevokeAndACK msg errorMsg
        | RemoteNextCommitInfo.Waiting theirNextCommit ->
            let remotePerCommitmentSecretsOpt =
                this.SavedChannelState.RemotePerCommitmentSecrets.InsertPerCommitmentSecret
                    this.SavedChannelState.RemoteCommit.Index
                    msg.PerCommitmentSecret
            match remotePerCommitmentSecretsOpt with
            | Error err -> return! Error <| invalidRevokeAndACK msg err.Message
            | Ok remotePerCommitmentSecrets ->
                let savedChannelState = {
                    this.SavedChannelState with
                        RemotePerCommitmentSecrets = remotePerCommitmentSecrets
                        RemoteCommit = theirNextCommit
                        LocalChanges = {
                            this.SavedChannelState.LocalChanges with
                                Signed = [];
                                ACKed = this.SavedChannelState.LocalChanges.ACKed @ this.SavedChannelState.LocalChanges.Signed
                        }
                        RemoteChanges = {
                            this.SavedChannelState.RemoteChanges with
                                Signed = []
                        }
                }
                return {
                    this with
                        SavedChannelState = savedChannelState
                        RemoteNextCommitInfo =
                            Some <| RemoteNextCommitInfo.Revoked msg.NextPerCommitmentPoint
                }
    }

    member this.Close (localShutdownScriptPubKey: ShutdownScriptPubKey)
                          : Result<Channel * ShutdownMsg, ChannelError> = result {
        if this.NegotiatingState.LocalRequestedShutdown.IsSome then
            do! Error <| cannotCloseChannel "shutdown is already in progress"
        do!
            Validation.checkShutdownScriptPubKeyAcceptable
                this.SavedChannelState.StaticChannelConfig.LocalStaticShutdownScriptPubKey
                localShutdownScriptPubKey
        if (this.Commitments.LocalHasUnsignedOutgoingHTLCs()) then
            do! Error <| cannotCloseChannel "Cannot close with unsigned outgoing htlcs"
        let shutdownMsg: ShutdownMsg = {
            ChannelId = this.SavedChannelState.StaticChannelConfig.ChannelId()
            ScriptPubKey = localShutdownScriptPubKey
        }
        let channel = {
            this with
                NegotiatingState = {
                    this.NegotiatingState with
                        LocalRequestedShutdown = Some localShutdownScriptPubKey
                }
        }
        return channel, shutdownMsg
    }

    static member private Hex = NBitcoin.DataEncoders.HexEncoder()
    static member private Ascii = System.Text.ASCIIEncoding.ASCII
    static member private DummyPrivKey = new Key(Channel.Hex.DecodeData("0101010101010101010101010101010101010101010101010101010101010101"))
    static member private DummySig =
        "01010101010101010101010101010101" |> Channel.Ascii.GetBytes
        |> uint256
        |> fun m -> Channel.DummyPrivKey.SignCompact(m)
        |> fun d -> LNECDSASignature.FromBytesCompact(d, true)
        |> fun ecdsaSig -> TransactionSignature(ecdsaSig.Value, SigHash.All)

    member internal this.MakeClosingTx (localSpk: ShutdownScriptPubKey)
                                       (remoteSpk: ShutdownScriptPubKey)
                                       (closingFee: Money) = result {
        let channelPrivKeys = this.ChannelPrivKeys
        let staticChannelConfig = this.SavedChannelState.StaticChannelConfig
        let dustLimit = Money.Max(staticChannelConfig.LocalParams.DustLimitSatoshis, staticChannelConfig.RemoteParams.DustLimitSatoshis)
        let! closingTx = Transactions.makeClosingTx staticChannelConfig.FundingScriptCoin localSpk remoteSpk staticChannelConfig.IsFunder dustLimit closingFee this.SavedChannelState.LocalCommit.Spec staticChannelConfig.Network
        let localSignature, psbtUpdated = channelPrivKeys.SignWithFundingPrivKey closingTx.Value
        let msg: ClosingSignedMsg = {
            ChannelId = staticChannelConfig.ChannelId()
            FeeSatoshis = closingFee
            Signature = localSignature.Signature |> LNECDSASignature
        }
        return (ClosingTx psbtUpdated, msg)
    }

    member internal this.FirstClosingFee (localSpk: ShutdownScriptPubKey)
                                         (remoteSpk: ShutdownScriptPubKey) = result {
        let feeEst = this.ChannelOptions.FeeEstimator
        let staticChannelConfig = this.SavedChannelState.StaticChannelConfig
        let! dummyClosingTx =
            Transactions.makeClosingTx
                staticChannelConfig.FundingScriptCoin
                localSpk
                remoteSpk
                staticChannelConfig.IsFunder
                Money.Zero
                Money.Zero
                this.SavedChannelState.LocalCommit.Spec
                staticChannelConfig.Network
        let tx = dummyClosingTx.Value.GetGlobalTransaction()
        tx.Inputs.[0].WitScript <-
            let witness = seq [ Channel.DummySig.ToBytes(); Channel.DummySig.ToBytes(); dummyClosingTx.Value.Inputs.[0].WitnessScript.ToBytes() ]
            WitScript(witness)
        let feeRatePerKw =
            FeeRatePerKw.Max (
                feeEst.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority),
                this.SavedChannelState.LocalCommit.Spec.FeeRatePerKw
            )
        return feeRatePerKw.CalculateFeeFromVirtualSize(tx)
    }

    static member internal NextClosingFee (localClosingFee: Money, remoteClosingFee: Money) =
        ((localClosingFee.Satoshi + remoteClosingFee.Satoshi) / 4L) * 2L
        |> Money.Satoshis

    member this.RemoteShutdown (msg: ShutdownMsg)
                               (localShutdownScriptPubKey: ShutdownScriptPubKey)
                                   : Result<Channel * Option<ShutdownMsg> * Option<ClosingSignedMsg>, ChannelError> = result {
        let remoteShutdownScriptPubKey = msg.ScriptPubKey
        do!
            Validation.checkShutdownScriptPubKeyAcceptable
                this.SavedChannelState.StaticChannelConfig.LocalStaticShutdownScriptPubKey
                localShutdownScriptPubKey
        do!
            Validation.checkShutdownScriptPubKeyAcceptable
                this.SavedChannelState.StaticChannelConfig.RemoteStaticShutdownScriptPubKey
                remoteShutdownScriptPubKey
        let cm = this.Commitments
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
            let channel = {
                this with
                    NegotiatingState = {
                        this.NegotiatingState with
                            RemoteRequestedShutdown = Some remoteShutdownScriptPubKey
                    }
            }
            return channel, None, None
        else
            let localShutdownMsgOpt: Option<ShutdownMsg> =
                match this.NegotiatingState.LocalRequestedShutdown with
                | None ->
                    Some {
                        ChannelId = this.SavedChannelState.StaticChannelConfig.ChannelId()
                        ScriptPubKey = localShutdownScriptPubKey
                    }
                | Some _ -> None
            let hasNoPendingHTLCs =
                match this.RemoteNextCommitInfo with
                | None -> true
                | Some remoteNextCommitInfo ->
                    this.SavedChannelState.HasNoPendingHTLCs remoteNextCommitInfo
            if hasNoPendingHTLCs then
                // we have to send first closing_signed msg iif we are the funder
                if this.SavedChannelState.StaticChannelConfig.IsFunder then
                    let! closingFee =
                        this.FirstClosingFee
                            localShutdownScriptPubKey
                            remoteShutdownScriptPubKey
                        |> expectTransactionError
                    let! (_closingTx, closingSignedMsg) =
                        this.MakeClosingTx
                            localShutdownScriptPubKey
                            remoteShutdownScriptPubKey
                            closingFee
                        |> expectTransactionError
                    let nextState = {
                        LocalRequestedShutdown = Some localShutdownScriptPubKey
                        RemoteRequestedShutdown = Some remoteShutdownScriptPubKey
                        LocalClosingFeesProposed = [ closingFee ]
                        RemoteClosingFeeProposed = None
                    }
                    let channel = {
                        this with
                            NegotiatingState = nextState
                    }
                    return channel, localShutdownMsgOpt, Some closingSignedMsg
                else
                    let nextState = {
                        LocalRequestedShutdown = Some localShutdownScriptPubKey
                        RemoteRequestedShutdown = Some remoteShutdownScriptPubKey
                        LocalClosingFeesProposed = []
                        RemoteClosingFeeProposed = None
                    }
                    let channel = {
                        this with
                            NegotiatingState = nextState
                    }
                    return channel, localShutdownMsgOpt, None
            else
                let channel = {
                    this with
                        NegotiatingState = {
                            this.NegotiatingState with
                                LocalRequestedShutdown = Some localShutdownScriptPubKey
                                RemoteRequestedShutdown = Some remoteShutdownScriptPubKey
                        }
                }
                return channel, localShutdownMsgOpt, None
    }

    member this.ApplyClosingSigned (msg: ClosingSignedMsg)
                                       : Result<Channel * ClosingSignedResponse, ChannelError> = result {
        let! localShutdownScriptPubKey, remoteShutdownScriptPubKey =
            match (this.NegotiatingState.LocalRequestedShutdown, this.NegotiatingState.RemoteRequestedShutdown) with
            | (Some localShutdownScriptPubKey, Some remoteShutdownScriptPubKey) ->
                Ok (localShutdownScriptPubKey, remoteShutdownScriptPubKey)
            // FIXME: these should be new channel errors
            | (Some _, None) ->
                Error ReceivedClosingSignedBeforeReceivingShutdown
            | (None, Some _) ->
                Error ReceivedClosingSignedBeforeSendingShutdown
            | (None, None) ->
                Error ReceivedClosingSignedBeforeSendingOrReceivingShutdown
        let remoteChannelKeys = this.SavedChannelState.StaticChannelConfig.RemoteChannelPubKeys
        let lastCommitFeeSatoshi =
            this.SavedChannelState.StaticChannelConfig.FundingScriptCoin.TxOut.Value - (this.SavedChannelState.LocalCommit.PublishableTxs.CommitTx.Value.TotalOut)
        do! checkRemoteProposedHigherFeeThanBaseFee lastCommitFeeSatoshi msg.FeeSatoshis
        do!
            checkRemoteProposedFeeWithinNegotiatedRange
                (List.tryHead this.NegotiatingState.LocalClosingFeesProposed)
                (Option.map (fun (fee, _sig) -> fee) this.NegotiatingState.RemoteClosingFeeProposed)
                msg.FeeSatoshis

        let! closingTx, closingSignedMsg =
            this.MakeClosingTx
                localShutdownScriptPubKey
                remoteShutdownScriptPubKey
                msg.FeeSatoshis
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
            this.NegotiatingState.LocalClosingFeesProposed
            |> List.tryHead
        let areWeInDeal = Some(msg.FeeSatoshis) = maybeLocalFee
        let hasTooManyNegotiationDone =
            (this.NegotiatingState.LocalClosingFeesProposed |> List.length) >= this.ChannelOptions.MaxClosingNegotiationIterations
        if (areWeInDeal || hasTooManyNegotiationDone) then
            return this, MutualClose (finalizedTx, None)
        else
            let lastLocalClosingFee = this.NegotiatingState.LocalClosingFeesProposed |> List.tryHead
            let! localF = 
                match lastLocalClosingFee with
                | Some v -> Ok v
                | None ->
                    this.FirstClosingFee
                        localShutdownScriptPubKey
                        remoteShutdownScriptPubKey
                    |> expectTransactionError
            let nextClosingFee =
                Channel.NextClosingFee (localF, msg.FeeSatoshis)
            if (Some nextClosingFee = lastLocalClosingFee) then
                return this, MutualClose (finalizedTx, None)
            else if (nextClosingFee = msg.FeeSatoshis) then
                // we have reached on agreement!
                return this, MutualClose (finalizedTx, Some closingSignedMsg)
            else
                let! _closingTx, closingSignedMsg =
                    this.MakeClosingTx
                        localShutdownScriptPubKey
                        remoteShutdownScriptPubKey
                        nextClosingFee
                    |> expectTransactionError
                let nextState = {
                    this.NegotiatingState with
                        LocalClosingFeesProposed =
                            nextClosingFee :: this.NegotiatingState.LocalClosingFeesProposed
                        RemoteClosingFeeProposed = Some (msg.FeeSatoshis, msg.Signature)
                }
                let channel = {
                    this with
                        NegotiatingState = nextState
                }
                return channel, NewClosingSigned closingSignedMsg
    }

    member this.LocalHasChanges() =
        (not this.SavedChannelState.RemoteChanges.ACKed.IsEmpty)
        || (not this.Commitments.ProposedLocalChanges.IsEmpty)

    member this.RemoteHasChanges() =
        (not this.SavedChannelState.LocalChanges.ACKed.IsEmpty)
        || (not this.Commitments.ProposedRemoteChanges.IsEmpty)

    member private this.sendCommit (remoteNextCommitInfo: RemoteNextCommitInfo)
                                       : Result<CommitmentSignedMsg * Channel, ChannelError> =
        let channelPrivKeys = this.ChannelPrivKeys
        let cm = this.Commitments
        let savedChannelState = this.SavedChannelState
        match remoteNextCommitInfo with
        | RemoteNextCommitInfo.Revoked remoteNextPerCommitmentPoint ->
            result {
                // remote commitment will include all local changes + remote acked changes
                let! spec = savedChannelState.RemoteCommit.Spec.Reduce(savedChannelState.RemoteChanges.ACKed, cm.ProposedLocalChanges) |> expectTransactionError
                let! (remoteCommitTx, htlcTimeoutTxs, htlcSuccessTxs) =
                    Commitments.Helpers.makeRemoteTxs savedChannelState.StaticChannelConfig
                                          (savedChannelState.RemoteCommit.Index.NextCommitment())
                                          (channelPrivKeys.ToChannelPubKeys())
                                          (remoteNextPerCommitmentPoint)
                                          (spec)
                    |> expectTransactionErrors
                let signature,_ = channelPrivKeys.SignWithFundingPrivKey remoteCommitTx.Value
                let sortedHTLCTXs = Commitments.Helpers.sortBothHTLCs htlcTimeoutTxs htlcSuccessTxs
                let htlcSigs =
                    sortedHTLCTXs
                    |> List.map(
                            (fun htlc -> channelPrivKeys.SignHtlcTx htlc.Value remoteNextPerCommitmentPoint)
                            >> fst
                            >> (fun txSig -> txSig.Signature)
                            )
                let msg = {
                    CommitmentSignedMsg.ChannelId = savedChannelState.StaticChannelConfig.ChannelId()
                    Signature = !> signature.Signature
                    HTLCSignatures = htlcSigs |> List.map (!>)
                }
                let nextRemoteCommitInfo = {
                    savedChannelState.RemoteCommit
                    with
                        Index = savedChannelState.RemoteCommit.Index.NextCommitment()
                        TxId = remoteCommitTx.GetTxId()
                        Spec = spec
                        RemotePerCommitmentPoint = remoteNextPerCommitmentPoint
                }
                let nextCommitments = {
                    cm with
                        ProposedLocalChanges = []
                }
                let nextSavedChannelState = {
                    this.SavedChannelState with
                        LocalChanges = {
                            this.SavedChannelState.LocalChanges with
                                Signed = cm.ProposedLocalChanges
                        }
                        RemoteChanges = {
                            this.SavedChannelState.RemoteChanges with
                                ACKed = []
                                Signed = this.SavedChannelState.RemoteChanges.ACKed
                        }
                }
                let channel = {
                    this with
                        Commitments = nextCommitments
                        SavedChannelState = nextSavedChannelState
                        RemoteNextCommitInfo =
                            Some <| RemoteNextCommitInfo.Waiting nextRemoteCommitInfo
                }
                return msg, channel
            }
        | RemoteNextCommitInfo.Waiting _ ->
            CanNotSignBeforeRevocation |> Error

    member private this.receiveCommit (msg: CommitmentSignedMsg)
                                          : Result<RevokeAndACKMsg * Channel, ChannelError> =
        let channelPrivKeys = this.ChannelPrivKeys
        let cm = this.Commitments
        let savedChannelState = this.SavedChannelState
        if this.RemoteHasChanges() |> not then
            ReceivedCommitmentSignedWhenWeHaveNoPendingChanges |> Error
        else
            let commitmentSeed = channelPrivKeys.CommitmentSeed
            let localChannelKeys = channelPrivKeys.ToChannelPubKeys()
            let remoteChannelKeys = savedChannelState.StaticChannelConfig.RemoteChannelPubKeys
            let nextI = savedChannelState.LocalCommit.Index.NextCommitment()
            result {
                let! spec = savedChannelState.LocalCommit.Spec.Reduce(savedChannelState.LocalChanges.ACKed, cm.ProposedRemoteChanges) |> expectTransactionError
                let localPerCommitmentPoint = commitmentSeed.DerivePerCommitmentPoint nextI
                let! (localCommitTx, htlcTimeoutTxs, htlcSuccessTxs) =
                    Commitments.Helpers.makeLocalTXs
                        savedChannelState.StaticChannelConfig
                        nextI
                        (channelPrivKeys.ToChannelPubKeys())
                        localPerCommitmentPoint
                        spec
                    |> expectTransactionErrors
                let signature, signedCommitTx = channelPrivKeys.SignWithFundingPrivKey localCommitTx.Value

                let sigPair =
                    let localSigPair = seq [(localChannelKeys.FundingPubKey.RawPubKey(), signature)]
                    let remoteSigPair = seq[ (remoteChannelKeys.FundingPubKey.RawPubKey(), TransactionSignature(msg.Signature.Value, SigHash.All)) ]
                    Seq.append localSigPair remoteSigPair
                let tmp =
                    Transactions.checkTxFinalized signedCommitTx CommitTx.WhichInput sigPair
                    |> expectTransactionError
                let! finalizedCommitTx = tmp
                let sortedHTLCTXs = Commitments.Helpers.sortBothHTLCs htlcTimeoutTxs htlcSuccessTxs
                do! Commitments.checkSignatureCountMismatch sortedHTLCTXs msg
                
                let _localHTLCSigs, sortedHTLCTXs =
                    let localHtlcSigsAndHTLCTxs =
                        sortedHTLCTXs |> List.map(fun htlc ->
                            channelPrivKeys.SignHtlcTx htlc.Value localPerCommitmentPoint
                        )
                    localHtlcSigsAndHTLCTxs |> List.map(fst), localHtlcSigsAndHTLCTxs |> List.map(snd) |> Seq.cast<IHTLCTx> |> List.ofSeq

                let remoteHTLCPubKey = localPerCommitmentPoint.DeriveHtlcPubKey remoteChannelKeys.HtlcBasepoint

                let checkHTLCSig (htlc: IHTLCTx, remoteECDSASig: LNECDSASignature): Result<_, _> =
                    let remoteS = TransactionSignature(remoteECDSASig.Value, SigHash.All)
                    match htlc with
                    | :? HTLCTimeoutTx ->
                        (Transactions.checkTxFinalized (htlc.Value) (0) (seq [(remoteHTLCPubKey.RawPubKey(), remoteS)]))
                        |> Result.map(box)
                    // we cannot check that htlc-success tx are spendable because we need the payment preimage; thus we only check the remote sig
                    | :? HTLCSuccessTx ->
                        (Transactions.checkSigAndAdd (htlc) (remoteS) (remoteHTLCPubKey.RawPubKey()))
                        |> Result.map(box)
                    | _ -> failwith "Unreachable!"

                let! txList =
                    List.zip sortedHTLCTXs msg.HTLCSignatures
                    |> List.map(checkHTLCSig)
                    |> List.sequenceResultA
                    |> expectTransactionErrors
                let successTxs =
                    txList |> List.choose(fun o ->
                        match o with
                        | :? HTLCSuccessTx as tx -> Some tx
                        | _ -> None
                    )
                let finalizedTxs =
                    txList |> List.choose(fun o ->
                        match o with
                        | :? FinalizedTx as tx -> Some tx
                        | _ -> None
                    )
                let localPerCommitmentSecret =
                    channelPrivKeys.CommitmentSeed.DerivePerCommitmentSecret savedChannelState.LocalCommit.Index
                let localNextPerCommitmentPoint =
                    let perCommitmentSecret =
                        channelPrivKeys.CommitmentSeed.DerivePerCommitmentSecret
                            (savedChannelState.LocalCommit.Index.NextCommitment().NextCommitment())
                    perCommitmentSecret.PerCommitmentPoint()

                let nextMsg = {
                    RevokeAndACKMsg.ChannelId = savedChannelState.StaticChannelConfig.ChannelId()
                    PerCommitmentSecret = localPerCommitmentSecret
                    NextPerCommitmentPoint = localNextPerCommitmentPoint
                }
                
                let localCommit1 = { LocalCommit.Index = savedChannelState.LocalCommit.Index.NextCommitment()
                                     Spec = spec
                                     PublishableTxs = { PublishableTxs.CommitTx = finalizedCommitTx
                                                        HTLCTxs = finalizedTxs }
                                     PendingHTLCSuccessTxs = successTxs }
                let nextSavedChannelState = {
                    savedChannelState with
                        LocalCommit = localCommit1
                        LocalChanges = {
                            savedChannelState.LocalChanges with
                                ACKed = []
                        }
                        RemoteChanges = {
                            savedChannelState.RemoteChanges with
                                ACKed = (savedChannelState.RemoteChanges.ACKed @ cm.ProposedRemoteChanges)
                        }
                }
                let nextCommitments =
                    let completedOutgoingHTLCs =
                        let t1 = savedChannelState.LocalCommit.Spec.OutgoingHTLCs
                                 |> Map.toSeq |> Seq.map (fun (k, _) -> k) |> Set.ofSeq
                        let t2 = localCommit1.Spec.OutgoingHTLCs
                                 |> Map.toSeq |> Seq.map (fun (k, _) -> k) |> Set.ofSeq
                        Set.difference t1 t2
                    let originChannels1 = cm.OriginChannels |> Map.filter(fun k _ -> Set.contains k completedOutgoingHTLCs)
                    {
                        cm with
                            ProposedRemoteChanges = []
                            OriginChannels = originChannels1
                    }
                let nextChannel = {
                    this with
                        SavedChannelState = nextSavedChannelState
                        Commitments = nextCommitments
                }
                return nextMsg, nextChannel
            }
