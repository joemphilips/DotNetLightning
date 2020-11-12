namespace DotNetLightning.Channel

open ResultUtils

open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Utils.Aether
open DotNetLightning.Chain
open DotNetLightning.Crypto
open DotNetLightning.Transactions
open DotNetLightning.Serialization.Msgs
open NBitcoin
open System


type ProvideFundingTx = IDestination * Money * FeeRatePerKw -> Result<FinalizedTx * TxOutIndex, string> 
type Channel = {
    Config: ChannelConfig
    ChannelPrivKeys: ChannelPrivKeys
    FeeEstimator: IFeeEstimator
    FundingTxProvider:ProvideFundingTx
    RemoteNodeId: NodeId
    NodeSecret: NodeSecret
    State: ChannelState
    Network: Network
 }
        with
        static member Create (config: ChannelConfig,
                              nodeMasterPrivKey: NodeMasterPrivKey,
                              channelIndex: int,
                              feeEstimator: IFeeEstimator,
                              fundingTxProvider: ProvideFundingTx,
                              n: Network,
                              remoteNodeId: NodeId
                             ) =
            let channelPrivKeys = nodeMasterPrivKey.ChannelPrivKeys channelIndex
            let nodeSecret = nodeMasterPrivKey.NodeSecret()
            {
                Config = config
                ChannelPrivKeys = channelPrivKeys
                FeeEstimator = feeEstimator
                FundingTxProvider = fundingTxProvider
                RemoteNodeId = remoteNodeId
                NodeSecret = nodeSecret
                State = WaitForInitInternal
                Network = n
            }
        static member CreateCurried = curry7 (Channel.Create)

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
                           localSpk: Script,
                           remoteSpk: Script,
                           closingFee: Money,
                           localFundingPk: FundingPubKey,
                           network: Network
                          ) =
            assert (Scripts.isValidFinalScriptPubKey (remoteSpk) && Scripts.isValidFinalScriptPubKey (localSpk))
            let dustLimitSatoshis = Money.Max(cm.LocalParams.DustLimitSatoshis, cm.RemoteParams.DustLimitSatoshis)
            result {
                let! closingTx = Transactions.makeClosingTx (cm.FundingScriptCoin) (localSpk) (remoteSpk) (cm.LocalParams.IsFunder) (dustLimitSatoshis) (closingFee) (cm.LocalCommit.Spec) network
                let localSignature, psbtUpdated = channelPrivKeys.SignWithFundingPrivKey closingTx.Value
                let msg: ClosingSignedMsg = {
                    ChannelId = cm.ChannelId
                    FeeSatoshis = closingFee
                    Signature = localSignature.Signature |> LNECDSASignature
                }
                return (ClosingTx psbtUpdated, msg)
            }

        let firstClosingFee (cm: Commitments, localSpk: Script, remoteSpk: Script, feeEst: IFeeEstimator, network) =
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
                                localSpk: Script,
                                remoteSpk: Script,
                                feeEst: IFeeEstimator,
                                localFundingPk: FundingPubKey,
                                network: Network
                               ) =
            result {
                let! closingFee = firstClosingFee (commitments, localSpk, remoteSpk, feeEst, network)
                return! makeClosingTx (channelPrivKeys, commitments, localSpk, remoteSpk, closingFee, localFundingPk, network)
            } |> expectTransactionError

        let nextClosingFee (localClosingFee: Money, remoteClosingFee: Money) =
            ((localClosingFee.Satoshi + remoteClosingFee.Satoshi) / 4L) * 2L
            |> Money.Satoshis

        let handleMutualClose (closingTx: FinalizedTx, d: NegotiatingData) =
            let nextData =
                ClosingData.Create (d.ChannelId, d.Commitments, None, DateTime.Now, (d.ClosingTxProposed |> List.collect id |> List.map (fun tx -> tx.UnsignedTx)), closingTx)
            [ MutualClosePerformed (closingTx, nextData) ]
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
                               (data: Data.IHasCommitments)
                                   : Result<ChannelEvent list, ChannelError> =
        let commitments = data.Commitments
        let commitmentSeed = channelPrivKeys.CommitmentSeed
        let ourChannelReestablish =
            {
                ChannelId = data.ChannelId
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
        | WaitForInitInternal, CreateOutbound inputInitFunder ->
            let openChannelMsgToSend: OpenChannelMsg = {
                Chainhash = cs.Network.Consensus.HashGenesisBlock
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
                ShutdownScriptPubKey = cs.Config.ChannelOptions.ShutdownScriptPubKey
            }
            result {
                do! Validation.checkOurOpenChannelMsgAcceptable (cs.Config) openChannelMsgToSend
                return [
                    NewOutboundChannelStarted(
                        openChannelMsgToSend, {
                            InputInitFunder = inputInitFunder
                            LastSent = openChannelMsgToSend
                    })
                ]
            }
        | WaitForAcceptChannel state, ApplyAcceptChannel msg ->
            result {
                do! Validation.checkAcceptChannelMsgAcceptable (cs.Config) state msg
                let redeem =
                    Scripts.funding
                        (state.InputInitFunder.ChannelPrivKeys.ToChannelPubKeys().FundingPubKey)
                        msg.FundingPubKey
                let! fundingTx, outIndex =
                    cs.FundingTxProvider (redeem.WitHash :> IDestination, state.InputInitFunder.FundingSatoshis, state.InputInitFunder.FundingTxFeeRatePerKw)
                    |> expectFundingTxError
                let remoteParams = RemoteParams.FromAcceptChannel cs.RemoteNodeId (state.InputInitFunder.RemoteInit) msg
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
                                               msg.FirstPerCommitmentPoint
                                               cs.Network
                let localSigOfRemoteCommit, _ =
                    cs.ChannelPrivKeys.SignWithFundingPrivKey remoteCommitTx.Value
                let nextMsg: FundingCreatedMsg = {
                    TemporaryChannelId = msg.TemporaryChannelId
                    FundingTxId = fundingTxId
                    FundingOutputIndex = outIndex
                    Signature = !>localSigOfRemoteCommit.Signature
                }
                let channelId = OutPoint(fundingTxId.Value, uint32 outIndex.Value).ToChannelId()
                let data = { Data.WaitForFundingSignedData.ChannelId = channelId
                             LocalParams = localParams
                             RemoteParams = remoteParams
                             Data.WaitForFundingSignedData.FundingTx = fundingTx
                             Data.WaitForFundingSignedData.LocalSpec = commitmentSpec
                             LocalCommitTx = localCommitTx
                             RemoteCommit = { RemoteCommit.Index = CommitmentNumber.FirstCommitment
                                              Spec = remoteSpec
                                              TxId = remoteCommitTx.Value.GetGlobalTransaction().GetTxId()
                                              RemotePerCommitmentPoint = msg.FirstPerCommitmentPoint }
                             ChannelFlags = state.InputInitFunder.ChannelFlags
                             LastSent = nextMsg
                             InitialFeeRatePerKw = state.InputInitFunder.InitFeeRatePerKw }
                return [ WeAcceptedAcceptChannel(nextMsg, data) ]
            }
        | WaitForFundingSigned state, ApplyFundingSigned msg ->
            result {
                let remoteChannelKeys = state.RemoteParams.ChannelPubKeys
                let! finalizedLocalCommitTx =
                    let theirFundingPk = remoteChannelKeys.FundingPubKey.RawPubKey()
                    let _, signedLocalCommitTx =
                        cs.ChannelPrivKeys.SignWithFundingPrivKey state.LocalCommitTx.Value
                    let remoteSigPairOfLocalTx = (theirFundingPk,  TransactionSignature(msg.Signature.Value, SigHash.All))
                    let sigPairs = seq [ remoteSigPairOfLocalTx; ]
                    Transactions.checkTxFinalized signedLocalCommitTx state.LocalCommitTx.WhichInput sigPairs |> expectTransactionError
                let commitments = { Commitments.LocalParams = state.LocalParams
                                    RemoteParams = state.RemoteParams
                                    ChannelFlags = state.ChannelFlags
                                    FundingScriptCoin =
                                        let amount = state.FundingTx.Value.Outputs.[int state.LastSent.FundingOutputIndex.Value].Value
                                        ChannelHelpers.getFundingScriptCoin
                                            state.LocalParams.ChannelPubKeys.FundingPubKey
                                            remoteChannelKeys.FundingPubKey
                                            state.LastSent.FundingTxId
                                            state.LastSent.FundingOutputIndex
                                            amount
                                    LocalCommit = { Index = CommitmentNumber.FirstCommitment;
                                                    Spec = state.LocalSpec;
                                                    PublishableTxs = { PublishableTxs.CommitTx = finalizedLocalCommitTx
                                                                       HTLCTxs = [] }
                                                    PendingHTLCSuccessTxs = [] }
                                    RemoteCommit = state.RemoteCommit
                                    LocalChanges = LocalChanges.Zero
                                    RemoteChanges = RemoteChanges.Zero
                                    LocalNextHTLCId = HTLCId.Zero
                                    RemoteNextHTLCId = HTLCId.Zero
                                    OriginChannels = Map.empty
                                    // we will receive their next per-commitment point in the next msg, so we temporarily put a random byte array
                                    RemoteNextCommitInfo = DataEncoders.HexEncoder().DecodeData("0101010101010101010101010101010101010101010101010101010101010101") |> fun h -> new Key(h) |> fun k -> k.PubKey |> PerCommitmentPoint |> RemoteNextCommitInfo.Revoked
                                    RemotePerCommitmentSecrets = PerCommitmentSecretStore()
                                    ChannelId =
                                        msg.ChannelId }
                let nextState = { WaitForFundingConfirmedData.Commitments = commitments
                                  Deferred = None
                                  LastSent = Choice1Of2 state.LastSent
                                  InitialFeeRatePerKw = state.InitialFeeRatePerKw
                                  ChannelId = msg.ChannelId }
                return [ WeAcceptedFundingSigned(state.FundingTx, nextState) ]
            }
        // --------------- open channel procedure: case we are fundee -------------
        | WaitForInitInternal, CreateInbound inputInitFundee ->
            [ NewInboundChannelStarted({ InitFundee = inputInitFundee }) ] |> Ok

        | WaitForFundingConfirmed state, CreateChannelReestablish ->
            makeChannelReestablish cs.ChannelPrivKeys state
        | ChannelState.Normal state, CreateChannelReestablish ->
            makeChannelReestablish cs.ChannelPrivKeys state
        | WaitForOpenChannel state, ApplyOpenChannel msg ->
            result {
                do! Validation.checkOpenChannelMsgAcceptable (cs.FeeEstimator) (cs.Config) msg
                let localParams = state.InitFundee.LocalParams
                let channelKeys = state.InitFundee.ChannelPrivKeys
                let localCommitmentPubKey = channelKeys.CommitmentSeed.DerivePerCommitmentPoint CommitmentNumber.FirstCommitment
                let acceptChannelMsg: AcceptChannelMsg = {
                    TemporaryChannelId = msg.TemporaryChannelId
                    DustLimitSatoshis = localParams.DustLimitSatoshis
                    MaxHTLCValueInFlightMsat = localParams.MaxHTLCValueInFlightMSat
                    ChannelReserveSatoshis = localParams.ChannelReserveSatoshis
                    HTLCMinimumMSat = localParams.HTLCMinimumMSat
                    MinimumDepth = cs.Config.ChannelHandshakeConfig.MinimumDepth
                    ToSelfDelay = localParams.ToSelfDelay
                    MaxAcceptedHTLCs = localParams.MaxAcceptedHTLCs
                    FundingPubKey = channelKeys.FundingPrivKey.FundingPubKey()
                    RevocationBasepoint = channelKeys.RevocationBasepointSecret.RevocationBasepoint()
                    PaymentBasepoint = channelKeys.PaymentBasepointSecret.PaymentBasepoint()
                    DelayedPaymentBasepoint = channelKeys.DelayedPaymentBasepointSecret.DelayedPaymentBasepoint()
                    HTLCBasepoint = channelKeys.HtlcBasepointSecret.HtlcBasepoint()
                    FirstPerCommitmentPoint = localCommitmentPubKey
                    ShutdownScriptPubKey = cs.Config.ChannelOptions.ShutdownScriptPubKey
                }
                let remoteParams = RemoteParams.FromOpenChannel cs.RemoteNodeId state.InitFundee.RemoteInit msg cs.Config.ChannelHandshakeConfig
                let data = Data.WaitForFundingCreatedData.Create localParams remoteParams msg acceptChannelMsg
                return [ WeAcceptedOpenChannel(acceptChannelMsg, data) ]
            }
        | WaitForOpenChannel _state, ChannelCommand.Close _spk ->
            [ ChannelEvent.Closed ] |> Ok

        | WaitForFundingCreated state, ApplyFundingCreated msg ->
            result {
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
                        cs.Network
                assert (localCommitTx.Value.IsReadyToSign())
                let _s, signedLocalCommitTx =
                    cs.ChannelPrivKeys.SignWithFundingPrivKey localCommitTx.Value
                let remoteTxSig = TransactionSignature(msg.Signature.Value, SigHash.All)
                let theirSigPair = (remoteChannelKeys.FundingPubKey.RawPubKey(), remoteTxSig)
                let sigPairs = seq [ theirSigPair ]
                let! finalizedCommitTx =
                    Transactions.checkTxFinalized (signedLocalCommitTx) (localCommitTx.WhichInput) sigPairs
                    |> expectTransactionError
                let localSigOfRemoteCommit, _ =
                    cs.ChannelPrivKeys.SignWithFundingPrivKey remoteCommitTx.Value
                let channelId = OutPoint(msg.FundingTxId.Value, uint32 msg.FundingOutputIndex.Value).ToChannelId()
                let msgToSend: FundingSignedMsg = { ChannelId = channelId; Signature = !>localSigOfRemoteCommit.Signature }
                let commitments = { Commitments.LocalParams = state.LocalParams
                                    RemoteParams = state.RemoteParams
                                    ChannelFlags = state.ChannelFlags
                                    FundingScriptCoin =
                                        ChannelHelpers.getFundingScriptCoin
                                            state.LocalParams.ChannelPubKeys.FundingPubKey
                                            remoteChannelKeys.FundingPubKey
                                            msg.FundingTxId
                                            msg.FundingOutputIndex
                                            state.FundingSatoshis
                                    LocalCommit = { LocalCommit.Index = CommitmentNumber.FirstCommitment
                                                    Spec = localSpec
                                                    PublishableTxs = { PublishableTxs.CommitTx = finalizedCommitTx;
                                                                       HTLCTxs = [] }
                                                    PendingHTLCSuccessTxs = [] }
                                    RemoteCommit = { RemoteCommit.Index = CommitmentNumber.FirstCommitment
                                                     Spec = remoteSpec
                                                     TxId = remoteCommitTx.Value.GetGlobalTransaction().GetTxId()
                                                     RemotePerCommitmentPoint = state.RemoteFirstPerCommitmentPoint }
                                    LocalChanges = LocalChanges.Zero
                                    RemoteChanges = RemoteChanges.Zero
                                    LocalNextHTLCId = HTLCId.Zero
                                    RemoteNextHTLCId = HTLCId.Zero
                                    OriginChannels = Map.empty
                                    RemoteNextCommitInfo = DataEncoders.HexEncoder().DecodeData("0101010101010101010101010101010101010101010101010101010101010101") |> fun h -> new Key(h) |> fun k -> k.PubKey |> PerCommitmentPoint |> RemoteNextCommitInfo.Revoked
                                    RemotePerCommitmentSecrets = PerCommitmentSecretStore()
                                    ChannelId = channelId }
                let nextState = { WaitForFundingConfirmedData.Commitments = commitments
                                  Deferred = None
                                  LastSent = msgToSend |> Choice2Of2
                                  InitialFeeRatePerKw = state.InitialFeeRatePerKw
                                  ChannelId = channelId }
                return [ WeAcceptedFundingCreated(msgToSend, nextState) ]
            }
        | WaitForFundingConfirmed _state, ApplyFundingLocked msg ->
            [ TheySentFundingLocked msg ] |> Ok
        | WaitForFundingConfirmed state, ApplyFundingConfirmedOnBC(height, txindex, depth) ->
            if state.Commitments.RemoteParams.MinimumDepth > depth then
                [] |> Ok
            else
                let nextPerCommitmentPoint =
                    cs.ChannelPrivKeys.CommitmentSeed.DerivePerCommitmentPoint
                        (CommitmentNumber.FirstCommitment.NextCommitment())
                let msgToSend: FundingLockedMsg = { ChannelId = state.Commitments.ChannelId; NextPerCommitmentPoint = nextPerCommitmentPoint }

                // This is temporary channel id that we will use in our channel_update message, the goal is to be able to use our channel
                // as soon as it reaches NORMAL state, and before it is announced on the network
                // (this id might be updated when the funding tx gets deeply buried, if there was a reorg in the meantime)
                // this is not specified in BOLT.
                let shortChannelId = { ShortChannelId.BlockHeight = height;
                                       BlockIndex = txindex
                                       TxOutIndex = state.Commitments.FundingScriptCoin.Outpoint.N |> uint16 |> TxOutIndex }
                let nextState = { Data.WaitForFundingLockedData.Commitments = state.Commitments
                                  ShortChannelId = shortChannelId
                                  OurMessage = msgToSend
                                  TheirMessage = None
                                  HaveWeSentFundingLocked = false
                                  InitialFeeRatePerKw = state.InitialFeeRatePerKw
                                  ChannelId = state.Commitments.ChannelId }
                
                match (state.Deferred) with
                | None ->
                    [ FundingConfirmed nextState; WeSentFundingLocked msgToSend ] |> Ok
                | Some msg ->
                    [ FundingConfirmed nextState; WeSentFundingLocked msgToSend; WeResumedDelayedFundingLocked msg ] |> Ok
        | WaitForFundingLocked _state, ApplyFundingConfirmedOnBC(height, _txindex, depth) ->
            if (cs.Config.ChannelHandshakeConfig.MinimumDepth <= depth) then
                [] |> Ok
            else
                onceConfirmedFundingTxHasBecomeUnconfirmed(height, depth)
        | WaitForFundingLocked state, ApplyFundingLocked msg ->
            if (state.HaveWeSentFundingLocked) then
                let initialChannelUpdate =
                    let feeBase = ChannelHelpers.getOurFeeBaseMSat cs.FeeEstimator state.InitialFeeRatePerKw state.Commitments.LocalParams.IsFunder
                    ChannelHelpers.makeChannelUpdate (cs.Network.Consensus.HashGenesisBlock,
                                               cs.NodeSecret,
                                               cs.RemoteNodeId,
                                               state.ShortChannelId,
                                               state.Commitments.LocalParams.ToSelfDelay,
                                               state.Commitments.RemoteParams.HTLCMinimumMSat,
                                               feeBase,
                                               cs.Config.ChannelOptions.FeeProportionalMillionths,
                                               true,
                                               None)
                let nextState = { NormalData.Buried = true
                                  Commitments = { state.Commitments with RemoteNextCommitInfo = RemoteNextCommitInfo.Revoked(msg.NextPerCommitmentPoint) }
                                  ShortChannelId = state.ShortChannelId
                                  ChannelAnnouncement = None
                                  ChannelUpdate = initialChannelUpdate
                                  LocalShutdown = None
                                  RemoteShutdown = None
                                  ChannelId = state.ChannelId }
                [ BothFundingLocked nextState ] |> Ok
            else
                [] |> Ok

        // ---------- normal operation ---------
        | ChannelState.Normal state, MonoHopUnidirectionalPayment op when state.LocalShutdown.IsSome || state.RemoteShutdown.IsSome ->
            sprintf "Could not send mono-hop unidirectional payment %A since shutdown is already in progress." op
            |> apiMisuse
        | ChannelState.Normal state, MonoHopUnidirectionalPayment op ->
            result {
                let payment: MonoHopUnidirectionalPaymentMsg = {
                    ChannelId = state.Commitments.ChannelId
                    Amount = op.Amount
                }
                let commitments1 = state.Commitments.AddLocalProposal(payment)

                let remoteCommit1 =
                    match commitments1.RemoteNextCommitInfo with
                    | RemoteNextCommitInfo.Waiting info -> info.NextRemoteCommit
                    | RemoteNextCommitInfo.Revoked _info -> commitments1.RemoteCommit
                let! reduced = remoteCommit1.Spec.Reduce(commitments1.RemoteChanges.ACKed, commitments1.LocalChanges.Proposed) |> expectTransactionError
                do! Validation.checkOurMonoHopUnidirectionalPaymentIsAcceptableWithCurrentSpec reduced commitments1 payment
                return [ WeAcceptedOperationMonoHopUnidirectionalPayment(payment, commitments1) ]
            }
        | ChannelState.Normal state, ApplyMonoHopUnidirectionalPayment msg ->
            result {
                let commitments1 = state.Commitments.AddRemoteProposal(msg)
                let! reduced = commitments1.LocalCommit.Spec.Reduce (commitments1.LocalChanges.ACKed, commitments1.RemoteChanges.Proposed) |> expectTransactionError
                do! Validation.checkTheirMonoHopUnidirectionalPaymentIsAcceptableWithCurrentSpec reduced commitments1 msg
                return [ WeAcceptedMonoHopUnidirectionalPayment commitments1 ]
            }
        | ChannelState.Normal state, AddHTLC op when state.LocalShutdown.IsSome || state.RemoteShutdown.IsSome ->
            sprintf "Could not add new HTLC %A since shutdown is already in progress." op
            |> apiMisuse
        | ChannelState.Normal state, AddHTLC op ->
            result {
                do! Validation.checkOperationAddHTLC state op
                let add: UpdateAddHTLCMsg = {
                    ChannelId = state.Commitments.ChannelId
                    HTLCId = state.Commitments.LocalNextHTLCId
                    Amount = op.Amount
                    PaymentHash = op.PaymentHash
                    CLTVExpiry = op.Expiry
                    OnionRoutingPacket = op.Onion
                }
                let commitments1 = { state.Commitments.AddLocalProposal(add)
                                        with LocalNextHTLCId = state.Commitments.LocalNextHTLCId + 1UL }
                                        |> fun commitments ->
                                            match op.Origin with
                                            | None -> commitments
                                            | Some o -> { commitments with OriginChannels = state.Commitments.OriginChannels |> Map.add add.HTLCId o }
                // we need to base the next current commitment on the last sig we sent, even if we didn't yet receive their revocation
                let remoteCommit1 =
                    match commitments1.RemoteNextCommitInfo with
                    | RemoteNextCommitInfo.Waiting info -> info.NextRemoteCommit
                    | RemoteNextCommitInfo.Revoked _info -> commitments1.RemoteCommit
                let! reduced = remoteCommit1.Spec.Reduce(commitments1.RemoteChanges.ACKed, commitments1.LocalChanges.Proposed) |> expectTransactionError
                do! Validation.checkOurUpdateAddHTLCIsAcceptableWithCurrentSpec reduced commitments1 add
                return [ WeAcceptedOperationAddHTLC(add, commitments1) ]
            }
        | ChannelState.Normal state, ApplyUpdateAddHTLC (msg, height) ->
            result {
                do! Validation.checkTheirUpdateAddHTLCIsAcceptable state.Commitments msg height
                let commitments1 = { state.Commitments.AddRemoteProposal(msg)
                                        with RemoteNextHTLCId = state.Commitments.LocalNextHTLCId + 1UL }
                let! reduced = commitments1.LocalCommit.Spec.Reduce (commitments1.LocalChanges.ACKed, commitments1.RemoteChanges.Proposed) |> expectTransactionError
                do! Validation.checkTheirUpdateAddHTLCIsAcceptableWithCurrentSpec reduced commitments1 msg
                return [ WeAcceptedUpdateAddHTLC commitments1 ]
            }

        | ChannelState.Normal state, FulfillHTLC cmd ->
            result {
                let! t = state.Commitments |> Commitments.sendFulfill (cmd)
                return [ WeAcceptedOperationFulfillHTLC t ]
            }

        | ChannelState.Normal state, ChannelCommand.ApplyUpdateFulfillHTLC msg ->
            state.Commitments |> Commitments.receiveFulfill msg

        | ChannelState.Normal state, FailHTLC op ->
            state.Commitments |> Commitments.sendFail cs.NodeSecret op

        | ChannelState.Normal state, FailMalformedHTLC op ->
            state.Commitments |> Commitments.sendFailMalformed op

        | ChannelState.Normal state, ApplyUpdateFailHTLC msg ->
            state.Commitments |> Commitments.receiveFail msg

        | ChannelState.Normal state, ApplyUpdateFailMalformedHTLC msg ->
            state.Commitments |> Commitments.receiveFailMalformed msg

        | ChannelState.Normal state, UpdateFee op ->
            state.Commitments |> Commitments.sendFee op
        | ChannelState.Normal state, ApplyUpdateFee msg ->
            let localFeerate = cs.FeeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority)
            state.Commitments |> Commitments.receiveFee cs.Config localFeerate msg

        | ChannelState.Normal state, SignCommitment ->
            let cm = state.Commitments
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

        | ChannelState.Normal state, ApplyCommitmentSigned msg ->
            state.Commitments |> Commitments.receiveCommit cs.ChannelPrivKeys msg cs.Network

        | ChannelState.Normal state, ApplyRevokeAndACK msg ->
            let cm = state.Commitments
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
                    Console.WriteLine("WARNING: revocation is not implemented yet")
                    Ok [ WeAcceptedRevokeAndACK(commitments1) ]

        | ChannelState.Normal state, ChannelCommand.Close cmd ->
            let localSPK = cmd.ScriptPubKey |> Option.defaultValue (state.Commitments.LocalParams.DefaultFinalScriptPubKey)
            if (state.LocalShutdown.IsSome) then
                cannotCloseChannel "shutdown is already in progress"
            else if (state.Commitments.LocalHasUnsignedOutgoingHTLCs()) then
                cannotCloseChannel "Cannot close with unsigned outgoing htlcs"
            else
                let shutDown: ShutdownMsg = {
                    ChannelId = state.ChannelId
                    ScriptPubKey = localSPK
                }
                [ AcceptedOperationShutdown shutDown ]
                |> Ok
        | ChannelState.Normal state, RemoteShutdown msg ->
            result {
                let cm = state.Commitments
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
                            let nextCommitments = { state.Commitments with
                                                        RemoteNextCommitInfo = RemoteNextCommitInfo.Waiting({ waitingForRevocation with ReSignASAP = true }) }
                            return [ AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs(msg, nextCommitments) ]
                        // No. let's sign right away.
                        | RemoteNextCommitInfo.Revoked _ ->
                            return [ ChannelStateRequestedSignCommitment; AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs(msg, cm) ]
                else
                    let (localShutdown, _sendList) = match state.LocalShutdown with
                                                     | Some localShutdown -> (localShutdown, [])
                                                     | None ->
                                                         let localShutdown: ShutdownMsg = {
                                                             ChannelId = state.ChannelId
                                                             ScriptPubKey = cm.LocalParams.DefaultFinalScriptPubKey
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
                                                            cm.LocalParams.ChannelPubKeys.FundingPubKey,
                                                            cs.Network)
                            let nextState = { NegotiatingData.ChannelId = cm.ChannelId
                                              Commitments = cm
                                              LocalShutdown = localShutdown
                                              RemoteShutdown = msg
                                              ClosingTxProposed = [ [ { ClosingTxProposed.UnsignedTx = closingTx; LocalClosingSigned = closingSignedMsg } ] ]
                                              MaybeBestUnpublishedTx = None }
                            return [ AcceptedShutdownWhenNoPendingHTLCs(closingSignedMsg |> Some, nextState) ]
                        else
                            let nextState = { NegotiatingData.ChannelId = cm.ChannelId
                                              Commitments = cm
                                              LocalShutdown = localShutdown
                                              RemoteShutdown = msg
                                              ClosingTxProposed = [ [] ]
                                              MaybeBestUnpublishedTx = None }
                            return [ AcceptedShutdownWhenNoPendingHTLCs(None, nextState) ]
                    else
                        let nextState = { ShutdownData.Commitments = cm
                                          LocalShutdown = localShutdown
                                          RemoteShutdown = msg
                                          ChannelId = cm.ChannelId }
                        return [ AcceptedShutdownWhenWeHavePendingHTLCs(nextState) ]
            }
        // ----------- closing ---------
        | Shutdown state, FulfillHTLC op ->
            result {
                let! t = state.Commitments |> Commitments.sendFulfill op
                return [ WeAcceptedOperationFulfillHTLC t ]
            }
        | Shutdown state, ApplyUpdateFulfillHTLC msg ->
            state.Commitments |> Commitments.receiveFulfill msg
        | Shutdown state, FailHTLC op ->
            state.Commitments |> Commitments.sendFail cs.NodeSecret op
        | Shutdown state, FailMalformedHTLC op ->
            state.Commitments |> Commitments.sendFailMalformed op
        | Shutdown state, ApplyUpdateFailMalformedHTLC msg ->
            state.Commitments |> Commitments.receiveFailMalformed msg
        | Shutdown state, UpdateFee op ->
            state.Commitments |> Commitments.sendFee op
        | Shutdown state, ApplyUpdateFee msg ->
            let localFeerate = cs.FeeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority)
            state.Commitments |> Commitments.receiveFee cs.Config localFeerate msg
        | Shutdown state, SignCommitment ->
            let cm = state.Commitments
            match cm.RemoteNextCommitInfo with
            | _ when (not <| cm.LocalHasChanges()) ->
                // nothing to sign
                [] |> Ok
            | RemoteNextCommitInfo.Revoked _ ->
                cm |> Commitments.sendCommit cs.ChannelPrivKeys (cs.Network)
            | RemoteNextCommitInfo.Waiting _waitForRevocation ->
                // Already in the process of signing.
                [] |> Ok
        | Shutdown state, ApplyCommitmentSigned msg ->
            state.Commitments |> Commitments.receiveCommit cs.ChannelPrivKeys msg cs.Network
        | Shutdown _state, ApplyRevokeAndACK _msg ->
            failwith "not implemented"

        | Negotiating state, ApplyClosingSigned msg ->
            result {
                let cm = state.Commitments
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
                        cm.LocalParams.ChannelPubKeys.FundingPubKey,
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
                    (state.ClosingTxProposed |> List.collect (id) |> List.length) >= cs.Config.PeerChannelConfigLimits.MaxClosingNegotiationIterations
                if (areWeInDeal || hasTooManyNegotiationDone) then
                    return! Closing.handleMutualClose (finalizedTx, { state with MaybeBestUnpublishedTx = Some(finalizedTx) })
                else
                    let lastLocalClosingFee = state.ClosingTxProposed |> List.tryHead |> Option.bind (List.tryHead) |> Option.map (fun txp -> txp.LocalClosingSigned.FeeSatoshis)
                    let! localF = 
                        match lastLocalClosingFee with
                        | Some v -> Ok v
                        | None ->
                            Closing.firstClosingFee (state.Commitments,
                                                      state.LocalShutdown.ScriptPubKey,
                                                      state.RemoteShutdown.ScriptPubKey,
                                                      cs.FeeEstimator,
                                                      cs.Network)
                            |> expectTransactionError
                    let nextClosingFee =
                        Closing.nextClosingFee (localF, msg.FeeSatoshis)
                    if (Some nextClosingFee = lastLocalClosingFee) then
                        return! Closing.handleMutualClose (finalizedTx, { state with MaybeBestUnpublishedTx = Some(finalizedTx) })
                    else if (nextClosingFee = msg.FeeSatoshis) then
                        // we have reached on agreement!
                        let closingTxProposed1 =
                            let newProposed = [ { ClosingTxProposed.UnsignedTx = closingTx
                                                  LocalClosingSigned = closingSignedMsg } ]
                            newProposed :: state.ClosingTxProposed
                        let negoData = { state with ClosingTxProposed = closingTxProposed1
                                                    MaybeBestUnpublishedTx = Some(finalizedTx) }
                        return! Closing.handleMutualClose (finalizedTx, negoData)
                    else
                        let! closingTx, closingSignedMsg =
                            Closing.makeClosingTx (
                                cs.ChannelPrivKeys,
                                cm,
                                state.LocalShutdown.ScriptPubKey,
                                state.RemoteShutdown.ScriptPubKey,
                                nextClosingFee,
                                cm.LocalParams.ChannelPubKeys.FundingPubKey,
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
                let cm = state.Commitments
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
        | NewOutboundChannelStarted(_, data), WaitForInitInternal ->
            { c with State = (WaitForAcceptChannel data) }
        | NewInboundChannelStarted(data), WaitForInitInternal ->
            { c with State = (WaitForOpenChannel data) }
        // --------- init fundee -----
        | WeAcceptedOpenChannel(_, data), WaitForOpenChannel _ ->
            let state = WaitForFundingCreated data
            { c with State = state }
        | WeAcceptedFundingCreated(_, data), WaitForFundingCreated _ ->
            let state = WaitForFundingConfirmed data
            { c with State = state }

        // --------- init funder -----
        | WeAcceptedAcceptChannel(_, data), WaitForAcceptChannel _ ->
            { c with State = WaitForFundingSigned data }
        | WeAcceptedFundingSigned(_, data), WaitForFundingSigned _ ->
            { c with State = WaitForFundingConfirmed data }

        // --------- init both ------
        | FundingConfirmed data, WaitForFundingConfirmed _ ->
            { c with State = WaitForFundingLocked data }
        | TheySentFundingLocked msg, WaitForFundingConfirmed s ->
            { c with State = WaitForFundingConfirmed({ s with Deferred = Some(msg) }) }
        | TheySentFundingLocked msg, WaitForFundingLocked s ->
            let feeBase = ChannelHelpers.getOurFeeBaseMSat c.FeeEstimator s.InitialFeeRatePerKw s.Commitments.LocalParams.IsFunder
            let channelUpdate = ChannelHelpers.makeChannelUpdate (c.Network.Consensus.HashGenesisBlock,
                                                           c.NodeSecret,
                                                           c.RemoteNodeId,
                                                           s.ShortChannelId,
                                                           s.Commitments.LocalParams.ToSelfDelay,
                                                           s.Commitments.RemoteParams.HTLCMinimumMSat,
                                                           feeBase,
                                                           c.Config.ChannelOptions.FeeProportionalMillionths,
                                                           true,
                                                           None)
            let nextState = { NormalData.Buried = false;
                              Commitments = s.Commitments
                              ShortChannelId = s.ShortChannelId
                              ChannelAnnouncement = None
                              ChannelUpdate = channelUpdate
                              LocalShutdown = None
                              RemoteShutdown = None
                              ChannelId = msg.ChannelId }
            { c with State = ChannelState.Normal nextState }
        | WeSentFundingLocked msg, WaitForFundingLocked prevState ->
            { c with State = WaitForFundingLocked { prevState with OurMessage = msg; HaveWeSentFundingLocked = true } }
        | BothFundingLocked data, WaitForFundingSigned _s ->
            { c with State = ChannelState.Normal data }
        | BothFundingLocked data, WaitForFundingLocked _s ->
            { c with State = ChannelState.Normal data }

        // ----- normal operation --------
        | WeAcceptedOperationMonoHopUnidirectionalPayment(_, newCommitments), ChannelState.Normal normalData ->
            { c with State = ChannelState.Normal({ normalData with Commitments = newCommitments }) }
        | WeAcceptedOperationAddHTLC(_, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }
        | WeAcceptedMonoHopUnidirectionalPayment(newCommitments), ChannelState.Normal normalData ->
            { c with State = ChannelState.Normal({ normalData with Commitments = newCommitments }) }
        | WeAcceptedUpdateAddHTLC(newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }

        | WeAcceptedOperationFulfillHTLC(_, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }
        | WeAcceptedFulfillHTLC(_msg, _origin, _htlc, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }

        | WeAcceptedOperationFailHTLC(_msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }
        | WeAcceptedFailHTLC(_origin, _msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }

        | WeAcceptedOperationFailMalformedHTLC(_msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }
        | WeAcceptedFailMalformedHTLC(_origin, _msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }

        | WeAcceptedOperationUpdateFee(_msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }
        | WeAcceptedUpdateFee(_msg), ChannelState.Normal _d -> c

        | WeAcceptedOperationSign(_msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }
        | WeAcceptedCommitmentSigned(_msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }

        | WeAcceptedRevokeAndACK(newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }

        // -----  closing ------
        | AcceptedOperationShutdown msg, ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with LocalShutdown = Some msg }) }
        | AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs(remoteShutdown, nextCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal ({ d with RemoteShutdown = Some remoteShutdown; Commitments = nextCommitments }) }
        | AcceptedShutdownWhenNoPendingHTLCs(_maybeMsg, nextState), ChannelState.Normal _d ->
            { c with State = Negotiating nextState }
        | AcceptedShutdownWhenWeHavePendingHTLCs(nextState), ChannelState.Normal _d ->
            { c with State = Shutdown nextState }
        | MutualClosePerformed (_txToPublish, nextState), ChannelState.Negotiating _d ->
            { c with State = Closing nextState }
        | WeProposedNewClosingSigned(_msg, nextState), ChannelState.Negotiating _d ->
            { c with State = Negotiating(nextState) }
        // ----- else -----
        | _otherEvent -> c
