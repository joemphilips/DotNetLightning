namespace DotNetLightning.Channel
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Utils.Aether
open DotNetLightning.Chain
open DotNetLightning.Crypto
open DotNetLightning.Transactions
open DotNetLightning.Serialize.Msgs
open NBitcoin
open System


type ProvideFundingTx = IDestination * Money * FeeRatePerKw -> RResult<FinalizedTx * TxOutIndex> 
type Channel = {
    Config: ChannelConfig
    ChainListener: IChainListener
    KeysRepository: IKeysRepository
    FeeEstimator: IFeeEstimator
    FundingTxProvider:ProvideFundingTx
    Logger: Logger
    RemoteNodeId: NodeId
    LocalNodeSecret: Key
    State: ChannelState
    Network: Network
    Secp256k1Context: ISecp256k1
 }
        with
        static member Create(config, logger, chainListener, keysRepo, feeEstimator, localNodeSecret, fundingTxProvider, n, remoteNodeId) =
            {
                Secp256k1Context = CryptoUtils.impl.newSecp256k1()
                Config = config
                ChainListener = chainListener
                KeysRepository = keysRepo
                FeeEstimator = feeEstimator
                Logger = logger
                FundingTxProvider = fundingTxProvider
                RemoteNodeId = remoteNodeId
                LocalNodeSecret = localNodeSecret
                State = WaitForInitInternal
                Network = n
            }
        static member CreateCurried = curry9 (Channel.Create)

module Channel =
    /// represents the user has than something wrong with this library

    let private hex = NBitcoin.DataEncoders.HexEncoder()
    let private ascii = System.Text.ASCIIEncoding.ASCII
    let private dummyPrivKey = Key(hex.DecodeData("0101010101010101010101010101010101010101010101010101010101010101"))
    let private dummyPubKey = dummyPrivKey.PubKey
    let private dummySig =
        "01010101010101010101010101010101" |> ascii.GetBytes
        |> uint256
        |> fun m -> dummyPrivKey.SignCompact(m)
        |> fun d -> LNECDSASignature.FromBytesCompact(d, true)
        |> fun ecdsaSig -> TransactionSignature(ecdsaSig.Value, SigHash.All)

        // facades

    module Closing =
        let makeClosingTx (keyRepo: IKeysRepository, cm: Commitments, localSpk: Script, remoteSpk: Script, closingFee: Money, localFundingPk, n) =
            assert (Scripts.isValidFinalScriptPubKey (remoteSpk) && Scripts.isValidFinalScriptPubKey (localSpk))
            let dustLimitSatoshis = Money.Max(cm.LocalParams.DustLimitSatoshis, cm.RemoteParams.DustLimitSatoshis)
            Transactions.makeClosingTx (cm.FundingSCoin) (localSpk) (remoteSpk) (cm.LocalParams.IsFunder) (dustLimitSatoshis) (closingFee) (cm.LocalCommit.Spec) n
            |>> fun closingTx ->
                let localSignature, psbtUpdated = keyRepo.GetSignatureFor(closingTx.Value, localFundingPk)
                let msg = { ClosingSigned.ChannelId = cm.ChannelId
                            FeeSatoshis = closingFee
                            Signature = localSignature.Signature |> LNECDSASignature }
                (ClosingTx psbtUpdated, msg)

        let firstClosingFee (cm: Commitments, localSpk: Script, remoteSpk: Script, feeEst: IFeeEstimator, n) =
            Transactions.makeClosingTx cm.FundingSCoin localSpk remoteSpk cm.LocalParams.IsFunder Money.Zero Money.Zero cm.LocalCommit.Spec n
            |>> fun dummyClosingTx ->
                let tx = dummyClosingTx.Value.GetGlobalTransaction()
                tx.Inputs.[0].WitScript <-
                    let witness = seq [ dummySig.ToBytes(); dummySig.ToBytes(); dummyClosingTx.Value.Inputs.[0].WitnessScript.ToBytes() ] |> Array.concat
                    Script(witness).ToWitScript()
                let feeRatePerKw = FeeRatePerKw.Max (feeEst.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority), cm.LocalCommit.Spec.FeeRatePerKw)
                let vsize = tx.GetVirtualSize()
                feeRatePerKw.ToFee(uint64 vsize)

        let makeFirstClosingTx (keyRepo, commitments, localSpk, remoteSpk, feeEst, localFundingPk, n) =
            firstClosingFee (commitments, localSpk, remoteSpk, feeEst, n)
            >>= fun closingFee ->
                makeClosingTx (keyRepo, commitments, localSpk, remoteSpk, closingFee, localFundingPk, n)

        let nextClosingFee (localClosingFee: Money, remoteClosingFee: Money) =
            ((localClosingFee.Satoshi + remoteClosingFee.Satoshi) / 4L) * 2L
            |> Money.Satoshis

        let handleMutualClose (closingTx: FinalizedTx, d: Choice<NegotiatingData, ClosingData>) =
            let nextData =
                match d with
                | Choice1Of2 negotiating ->
                    ClosingData.Create (negotiating.ChannelId, negotiating.Commitments, None, DateTime.Now, (negotiating.ClosingTxProposed |> List.collect id |> List.map (fun tx -> tx.UnsignedTx)), closingTx :: [])
                | Choice2Of2 closing -> { closing with MutualClosePublished = closingTx :: closing.MutualClosePublished }
            [ MutualClosePerformed nextData ]
            |> Good

        let claimCurrentLocalCommitTxOutputs (_keyRepo: IKeysRepository, channelPubKeys: ChannelPubKeys, commitments: Commitments, commitTx: CommitTx) =
            Validation.checkOrClose (commitments.LocalCommit.PublishableTxs.CommitTx.Value.GetTxId()) (=) (commitTx.Value.GetTxId()) "txid mismatch. provided txid (%A) does not match current local commit tx (%A)"
            >>= fun _ ->
                let _localPerCommitmentPoint = ChannelUtils.buildCommitmentPoint (channelPubKeys.CommitmentSeed, commitments.LocalCommit.Index)
                let _localRevocationPubKey = Generators.revocationPubKey
                failwith ""


    open Validation

    let executeCommand (cs: Channel) (command: ChannelCommand): RResult<ChannelEvent list> =
        match cs.State, command with

        // --------------- open channel procedure: case we are funder -------------
        | WaitForInitInternal, CreateOutbound inputInitFunder ->
            let openChannelMsgToSend = {
                OpenChannel.Chainhash = cs.Network.GenesisHashRev
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
                FundingPubKey = inputInitFunder.ChannelKeys.FundingKey.PubKey
                RevocationBasepoint = inputInitFunder.ChannelKeys.RevocationBaseKey.PubKey
                PaymentBasepoint = inputInitFunder.ChannelKeys.PaymentBaseKey.PubKey
                DelayedPaymentBasepoint = inputInitFunder.ChannelKeys.DelayedPaymentBaseKey.PubKey
                HTLCBasepoint = inputInitFunder.ChannelKeys.HTLCBaseKey.PubKey
                FirstPerCommitmentPoint =  ChannelUtils.buildCommitmentPoint(inputInitFunder.ChannelKeys.CommitmentSeed, 0UL)
                ChannelFlags = inputInitFunder.ChannelFlags
                ShutdownScriptPubKey = cs.Config.ChannelOptions.ShutdownScriptPubKey
            }
            [ NewOutboundChannelStarted(openChannelMsgToSend, { InputInitFunder = inputInitFunder;
                                                                LastSent = openChannelMsgToSend }) ]
            |> Good
        | WaitForAcceptChannel state, ApplyAcceptChannel msg ->
            Validation.checkAcceptChannelMsgAcceptable (cs.Config) state msg
            >>= fun _ ->
                let redeem = state.InputInitFunder.ChannelKeys.ToChannelPubKeys() |> ChannelHelpers.getFundingRedeemScript <| (msg.FundingPubKey)
                cs.FundingTxProvider (redeem.WitHash :> IDestination, state.InputInitFunder.FundingSatoshis, state.InputInitFunder.FundingTxFeeRatePerKw)
            >>= fun (fundingTx, outIndex) ->
                let remoteParams = RemoteParams.FromAcceptChannel cs.RemoteNodeId (state.InputInitFunder.RemoteInit) msg
                let localParams = state.InputInitFunder.LocalParams
                assert (state.LastSent.FundingPubKey = localParams.ChannelPubKeys.FundingPubKey)
                let commitmentSpec = state.InputInitFunder.DeriveCommitmentSpec()
                let commitmentSeed = state.InputInitFunder.ChannelKeys.CommitmentSeed
                ChannelHelpers.makeFirstCommitTxs localParams
                                           remoteParams
                                           state.LastSent.FundingSatoshis
                                           state.LastSent.PushMSat
                                           state.LastSent.FeeRatePerKw
                                           outIndex
                                           (fundingTx.Value.GetHash() |> TxId)
                                           (ChannelUtils.buildCommitmentPoint(commitmentSeed, 0UL))
                                           msg.FirstPerCommitmentPoint
                                           cs.Secp256k1Context
                                           cs.Network
                |>> fun (_localSpec, localCommitTx, remoteSpec, remoteCommitTx) ->
                    let localSigOfRemoteCommit, _ = (cs.KeysRepository.GetSignatureFor(remoteCommitTx.Value, state.LastSent.FundingPubKey))
                    let nextMsg = { FundingCreated.TemporaryChannelId = msg.TemporaryChannelId
                                    FundingTxId = fundingTx.Value.GetTxId()
                                    FundingOutputIndex = outIndex
                                    Signature = !>localSigOfRemoteCommit.Signature }

                    let data = { Data.WaitForFundingSignedData.ChannelId = msg.TemporaryChannelId
                                 LocalParams = localParams
                                 RemoteParams = remoteParams
                                 Data.WaitForFundingSignedData.FundingTx = fundingTx
                                 Data.WaitForFundingSignedData.LocalSpec = commitmentSpec
                                 LocalCommitTx = localCommitTx
                                 RemoteCommit = { RemoteCommit.Index = 0UL;
                                                  Spec = remoteSpec
                                                  TxId = remoteCommitTx.Value.GetGlobalTransaction().GetTxId()
                                                  RemotePerCommitmentPoint = msg.FirstPerCommitmentPoint }
                                 ChannelFlags = state.InputInitFunder.ChannelFlags
                                 LastSent = nextMsg
                                 InitialFeeRatePerKw = state.InputInitFunder.InitFeeRatePerKw }
                    [ WeAcceptedAcceptChannel(nextMsg, data) ]
        | WaitForFundingSigned state, ApplyFundingSigned msg ->
            let finalLocalCommitTxRR =
                let theirFundingPk = state.RemoteParams.FundingPubKey
                let _, signedLocalCommitTx = cs.KeysRepository.GetSignatureFor(state.LocalCommitTx.Value, state.LocalParams.ChannelPubKeys.FundingPubKey)
                let remoteSigPairOfLocalTx = (theirFundingPk,  TransactionSignature(msg.Signature.Value, SigHash.All))
                let sigPairs = seq [ remoteSigPairOfLocalTx; ]
                Transactions.checkTxFinalized signedLocalCommitTx state.LocalCommitTx.WhichInput sigPairs
            finalLocalCommitTxRR
            |>> fun finalizedLocalCommitTx ->
                let commitments = { Commitments.LocalParams = state.LocalParams
                                    RemoteParams = state.RemoteParams
                                    ChannelFlags = state.ChannelFlags
                                    FundingSCoin =
                                        let amount = state.FundingTx.Value.Outputs.[int state.LastSent.FundingOutputIndex.Value].Value
                                        ChannelHelpers.getFundingSCoin state.LocalParams.ChannelPubKeys
                                                                state.RemoteParams.FundingPubKey
                                                                state.LastSent.FundingTxId
                                                                state.LastSent.FundingOutputIndex
                                                                amount
                                    LocalCommit = { Index = 0UL;
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
                                    RemoteNextCommitInfo = DataEncoders.HexEncoder() .DecodeData("0101010101010101010101010101010101010101010101010101010101010101") |> Key |> fun k -> k.PubKey |> Choice2Of2
                                    RemotePerCommitmentSecrets = ShaChain.Zero
                                    ChannelId =
                                        cs.Logger (LogLevel.Debug) (sprintf "Channel id has been set to %A" msg.ChannelId)
                                        msg.ChannelId }
                let nextState = { WaitForFundingConfirmedData.Commitments = commitments
                                  Deferred = None
                                  LastSent = Choice1Of2 state.LastSent
                                  InitialFeeRatePerKw = state.InitialFeeRatePerKw
                                  ChannelId = msg.ChannelId }
                [ WeAcceptedFundingSigned(state.FundingTx, nextState) ]
        // --------------- open channel procedure: case we are fundee -------------
        | WaitForInitInternal, CreateInbound inputInitFundee ->
            [ NewInboundChannelStarted({ InitFundee = inputInitFundee }) ] |> Good
        | WaitForFundingConfirmed state, ApplyChannelReestablish theirChannelReestablish ->
            // TODO validate msg
            let commitmentSeed = state.Commitments.LocalParams.ChannelPubKeys.CommitmentSeed
            let ourChannelReestablish =
                {
                    ChannelId = state.ChannelId
                    NextLocalCommitmentNumber = 1UL
                    NextRemoteCommitmentNumber = 0UL
                    DataLossProtect = OptionalField.Some({
                                          YourLastPerCommitmentSecret = PaymentPreimage([|for _ in 0..31 -> 0uy|])
                                          MyCurrentPerCommitmentPoint = ChannelUtils.buildCommitmentPoint(commitmentSeed, 0UL)
                                      })
                }
            [ WeReplyToChannelReestablish ourChannelReestablish ] |> Good
        | WaitForOpenChannel state, ApplyOpenChannel msg ->
            Validation.checkOpenChannelMsgAcceptable (cs.FeeEstimator) (cs.Config) msg
            |>> fun _ ->
                let localParams = state.InitFundee.LocalParams
                let channelKeys = state.InitFundee.ChannelKeys
                let localCommitmentSecret = ChannelUtils.buildCommitmentSecret (channelKeys.CommitmentSeed, 0UL)
                let acceptChannel = { AcceptChannel.TemporaryChannelId = msg.TemporaryChannelId
                                      DustLimitSatoshis = localParams.DustLimitSatoshis
                                      MaxHTLCValueInFlightMsat = localParams.MaxHTLCValueInFlightMSat
                                      ChannelReserveSatoshis = localParams.ChannelReserveSatoshis
                                      HTLCMinimumMSat = localParams.HTLCMinimumMSat
                                      MinimumDepth = cs.Config.ChannelHandshakeConfig.MinimumDepth.Value |> uint32 |> BlockHeight
                                      ToSelfDelay = localParams.ToSelfDelay
                                      MaxAcceptedHTLCs = localParams.MaxAcceptedHTLCs
                                      FundingPubKey = channelKeys.FundingKey.PubKey
                                      RevocationBasepoint = channelKeys.RevocationBaseKey.PubKey
                                      PaymentBasepoint = channelKeys.PaymentBaseKey.PubKey
                                      DelayedPaymentBasepoint = channelKeys.DelayedPaymentBaseKey.PubKey
                                      HTLCBasepoint = channelKeys.HTLCBaseKey.PubKey
                                      FirstPerCommitmentPoint = localCommitmentSecret.PubKey
                                      ShutdownScriptPubKey = cs.Config.ChannelOptions.ShutdownScriptPubKey }

                let remoteParams = RemoteParams.FromOpenChannel cs.RemoteNodeId state.InitFundee.RemoteInit msg cs.Config.ChannelHandshakeConfig
                let data = Data.WaitForFundingCreatedData.Create localParams remoteParams msg acceptChannel
                [ WeAcceptedOpenChannel(acceptChannel, data) ]
        | WaitForOpenChannel _state, ChannelCommand.Close _spk -> [ ChannelEvent.Closed ] |> Good

        | WaitForFundingCreated state, ApplyFundingCreated msg ->
            ChannelHelpers.makeFirstCommitTxs state.LocalParams
                                       state.RemoteParams
                                       state.FundingSatoshis
                                       state.PushMSat
                                       state.InitialFeeRatePerKw
                                       msg.FundingOutputIndex
                                       msg.FundingTxId
                                       state.LastSent.FirstPerCommitmentPoint
                                       state.RemoteFirstPerCommitmentPoint
                                       cs.Secp256k1Context
                                       cs.Network
            >>= fun (localSpec, localCommitTx, remoteSpec, remoteCommitTx) ->
                match (localCommitTx.Value.IsReadyToSign()) with
                | false -> failwith "unreachable"
                | true ->
                    let _s, signedLocalCommitTx =
                        cs.KeysRepository.GetSignatureFor (localCommitTx.Value, state.LocalParams.ChannelPubKeys.FundingPubKey)
                    let remoteTxSig = TransactionSignature(msg.Signature.Value, SigHash.All)
                    let theirSigPair = (state.RemoteParams.FundingPubKey, remoteTxSig)
                    let sigPairs = seq [ theirSigPair ]
                    Transactions.checkTxFinalized (signedLocalCommitTx) (localCommitTx.WhichInput) sigPairs
                    >>= fun finalizedCommitTx ->
                        let localSigOfRemoteCommit, _ = cs.KeysRepository.GetSignatureFor (remoteCommitTx.Value, state.LocalParams.ChannelPubKeys.FundingPubKey)
                        let channelId = OutPoint(msg.FundingTxId.Value, uint32 msg.FundingOutputIndex.Value).ToChannelId()
                        let msgToSend: FundingSigned = { ChannelId = channelId; Signature = !>localSigOfRemoteCommit.Signature }
                        let commitments = { Commitments.LocalParams = state.LocalParams
                                            RemoteParams = state.RemoteParams
                                            ChannelFlags = state.ChannelFlags
                                            FundingSCoin = ChannelHelpers.getFundingSCoin state.LocalParams.ChannelPubKeys state.RemoteParams.FundingPubKey msg.FundingTxId msg.FundingOutputIndex state.FundingSatoshis
                                            LocalCommit = { LocalCommit.Index = 0UL;
                                                            Spec = localSpec
                                                            PublishableTxs = { PublishableTxs.CommitTx = finalizedCommitTx;
                                                                               HTLCTxs = [] }
                                                            PendingHTLCSuccessTxs = [] }
                                            RemoteCommit = { RemoteCommit.Index = 0UL;
                                                             Spec = remoteSpec
                                                             TxId = remoteCommitTx.Value.GetGlobalTransaction().GetTxId()
                                                             RemotePerCommitmentPoint = state.RemoteFirstPerCommitmentPoint }
                                            LocalChanges = LocalChanges.Zero
                                            RemoteChanges = RemoteChanges.Zero
                                            LocalNextHTLCId = HTLCId.Zero
                                            RemoteNextHTLCId = HTLCId.Zero
                                            OriginChannels = Map.empty
                                            RemoteNextCommitInfo = DataEncoders.HexEncoder() .DecodeData("0101010101010101010101010101010101010101010101010101010101010101") |> Key |> fun k -> k.PubKey |> Choice2Of2
                                            RemotePerCommitmentSecrets = ShaChain.Zero
                                            ChannelId = channelId }
                        let nextState = { WaitForFundingConfirmedData.Commitments = commitments
                                          Deferred = None
                                          LastSent = msgToSend |> Choice2Of2
                                          InitialFeeRatePerKw = state.InitialFeeRatePerKw
                                          ChannelId = channelId }
                        [ WeAcceptedFundingCreated(msgToSend, nextState) ]
                        |> Good
        | WaitForFundingConfirmed _state, ApplyFundingLocked msg ->
            [ TheySentFundingLocked msg ] |> Good
        | WaitForFundingConfirmed state, ApplyFundingConfirmedOnBC(height, txindex, depth) ->
            cs.Logger (LogLevel.Info) (sprintf "Funding tx for ChannelId (%A) was confirmed at block height %A; depth: %A" state.Commitments.ChannelId height.Value depth)
            if state.Commitments.RemoteParams.MinimumDepth > depth then
                [] |> Good
            else
                let nextPerCommitmentPoint =
                    ChannelUtils.buildCommitmentPoint (state.Commitments.LocalParams.ChannelPubKeys.CommitmentSeed, 1UL)
                let msgToSend: FundingLocked = { ChannelId = state.Commitments.ChannelId; NextPerCommitmentPoint = nextPerCommitmentPoint }

                // This is temporary channel id that we will use in our channel_update message, the goal is to be able to use our channel
                // as soon as it reaches NORMAL state, and before it is announced on the network
                // (this id might be updated when the funding tx gets deeply buried, if there was a reorg in the meantime)
                // this is not specified in BOLT.
                let shortChannelId = { ShortChannelId.BlockHeight = height;
                                       BlockIndex = txindex
                                       TxOutIndex = state.Commitments.FundingSCoin.Outpoint.N |> uint16 |> TxOutIndex }
                let nextState = { Data.WaitForFundingLockedData.Commitments = state.Commitments
                                  ShortChannelId = shortChannelId
                                  OurMessage = msgToSend
                                  TheirMessage = None
                                  HaveWeSentFundingLocked = false
                                  InitialFeeRatePerKw = state.InitialFeeRatePerKw
                                  ChannelId = state.Commitments.ChannelId }
                
                match (state.Deferred) with
                | None ->
                    [ FundingConfirmed nextState; WeSentFundingLocked msgToSend ] |> Good
                | Some msg ->
                    [ FundingConfirmed nextState; WeSentFundingLocked msgToSend; WeResumedDelayedFundingLocked msg ] |> Good
        | WaitForFundingLocked _state, ApplyFundingConfirmedOnBC(height, _txindex, depth) ->
            if (cs.Config.ChannelHandshakeConfig.MinimumDepth <= depth) then
                [] |> Good
            else
                let msg = sprintf "once confirmed funding tx has become less confirmed than threshold %A! This is probably caused by reorg. current depth is: %A " height depth
                cs.Logger (LogLevel.Error) (msg)
                RRClose(msg)
        | WaitForFundingLocked state, ApplyFundingLocked msg ->
            if (state.HaveWeSentFundingLocked) then
                let initialChannelUpdate =
                    let feeBase = ChannelHelpers.getOurFeeBaseMSat cs.FeeEstimator state.InitialFeeRatePerKw state.Commitments.LocalParams.IsFunder
                    ChannelHelpers.makeChannelUpdate (cs.Network.GenesisHashRev,
                                               cs.LocalNodeSecret,
                                               cs.RemoteNodeId,
                                               state.ShortChannelId,
                                               state.Commitments.LocalParams.ToSelfDelay,
                                               state.Commitments.RemoteParams.HTLCMinimumMSat,
                                               feeBase,
                                               cs.Config.ChannelOptions.FeeProportionalMillionths,
                                               true,
                                               None)
                let nextState = { NormalData.Buried = true
                                  Commitments = { state.Commitments with RemoteNextCommitInfo = Choice2Of2(msg.NextPerCommitmentPoint) }
                                  ShortChannelId = state.ShortChannelId
                                  ChannelAnnouncement = None
                                  ChannelUpdate = initialChannelUpdate
                                  LocalShutdown = None
                                  RemoteShutdown = None
                                  ChannelId = state.ChannelId }
                [ BothFundingLocked nextState ] |> Good
            else
                [] |> Good

        // ---------- normal operation ---------
        | ChannelState.Normal state, AddHTLC cmd when state.LocalShutdown.IsSome || state.RemoteShutdown.IsSome ->
            sprintf "Could not add new HTLC %A since shutdown is already in progress." cmd
            |> RRIgnore
        | ChannelState.Normal state, AddHTLC cmd ->
            Validation.checkCMDAddHTLC state cmd
            >>= fun _ ->
                let add: UpdateAddHTLC = { UpdateAddHTLC.ChannelId = state.Commitments.ChannelId
                                           HTLCId = state.Commitments.LocalNextHTLCId
                                           AmountMSat = cmd.AmountMSat
                                           PaymentHash = cmd.PaymentHash
                                           CLTVExpiry = cmd.Expiry
                                           OnionRoutingPacket = cmd.Onion }
                let commitments1 = { state.Commitments.AddLocalProposal(add)
                                        with LocalNextHTLCId = state.Commitments.LocalNextHTLCId + 1UL }
                                        |> fun commitments ->
                                            match cmd.Origin with
                                            | None -> commitments
                                            | Some o -> { commitments with OriginChannels = state.Commitments.OriginChannels |> Map.add add.HTLCId o }
                // we need to base the next current commitment on the last sig we sent, even if we didn't yet receive their revocation
                let remoteCommit1 =
                    match commitments1.RemoteNextCommitInfo with
                    | Choice1Of2 info -> info.NextRemoteCommit
                    | Choice2Of2 _info -> commitments1.RemoteCommit
                remoteCommit1.Spec.Reduce(commitments1.RemoteChanges.ACKed, commitments1.LocalChanges.Proposed)
                >>= fun reduced ->
                    Validation.checkOurUpdateAddHTLCIsAcceptableWithCurrentSpec reduced commitments1 add
                    *> Good([ WeAcceptedCMDAddHTLC(add, commitments1) ])
        | ChannelState.Normal state, ApplyUpdateAddHTLC (msg, height) ->
            Validation.checkTheirUpdateAddHTLCIsAcceptable state.Commitments msg height
            >>= fun _ ->
                let commitments1 = { state.Commitments.AddRemoteProposal(msg)
                                        with RemoteNextHTLCId = state.Commitments.LocalNextHTLCId + 1UL }
                commitments1.LocalCommit.Spec.Reduce (commitments1.LocalChanges.ACKed, commitments1.RemoteChanges.Proposed)
                >>= fun reduced ->
                    Validation.checkTheirUpdateAddHTLCIsAcceptableWithCurrentSpec reduced commitments1 msg
                    *> Good [ WeAcceptedUpdateAddHTLC commitments1 ]

        | ChannelState.Normal state, FulfillHTLC cmd ->
            state.Commitments |> Commitments.sendFulfill (cmd)
            |>> fun t -> [ WeAcceptedCMDFulfillHTLC(t) ]
            >>>= fun e -> RRApiMisuse(e.Describe())

        | ChannelState.Normal state, ChannelCommand.ApplyUpdateFulfillHTLC msg ->
            state.Commitments |> Commitments.receiveFulfill msg
            >>>= fun e -> RRClose(e.Describe())

        | ChannelState.Normal state, FailHTLC cmd ->
            state.Commitments |> Commitments.sendFail cs.LocalNodeSecret cmd
            >>>= fun e -> RRApiMisuse(e.Describe())

        | ChannelState.Normal state, FailMalformedHTLC cmd ->
            state.Commitments |> Commitments.sendFailMalformed cmd
            >>>= fun e -> RRApiMisuse(e.Describe())

        | ChannelState.Normal state, ApplyUpdateFailHTLC msg ->
            state.Commitments |> Commitments.receiveFail msg
            >>>= fun e -> RRClose(e.Describe())

        | ChannelState.Normal state, ApplyUpdateFailMalformedHTLC msg ->
            state.Commitments |> Commitments.receiveFailMalformed msg
            >>>= fun e -> RRClose(e.Describe())

        | ChannelState.Normal state, UpdateFee cmd ->
            state.Commitments |> Commitments.sendFee cmd
            >>>= fun e -> e.Describe() |> RRApiMisuse
        | ChannelState.Normal state, ApplyUpdateFee msg ->
            let localFeerate = cs.FeeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority)
            state.Commitments |> Commitments.receiveFee cs.Config localFeerate msg
            >>>= fun e -> e.Describe() |> RRClose

        | ChannelState.Normal state, SignCommitment ->
            let cm = state.Commitments
            match cm.RemoteNextCommitInfo with
            | _ when (cm.LocalHasChanges() |> not) ->
                sprintf "Ignoring SignCommitment Command (nothing to sign)" |> RRIgnore
            | Choice2Of2 _ ->
                cm |> Commitments.sendCommit (cs.Secp256k1Context) (cs.KeysRepository) (cs.Network)
                >>>= fun e -> e.Describe() |> RRApiMisuse
            | Choice1Of2 _ ->
                sprintf "Already in the process of signing."
                |> RRIgnore

        | ChannelState.Normal state, ApplyCommitmentSigned msg ->
            state.Commitments |> Commitments.receiveCommit (cs.Secp256k1Context) cs.KeysRepository msg cs.Network
            >>>= fun e -> RRClose("Something unexpected happened while handling commitment_signed from remote" + e.Describe())

        | ChannelState.Normal state, ApplyRevokeAndACK msg ->
            let cm = state.Commitments
            match cm.RemoteNextCommitInfo with
            | Choice1Of2 _ when (msg.PerCommitmentSecret.ToPubKey() <> cm.RemoteCommit.RemotePerCommitmentPoint) ->
                sprintf "Invalid revoke_and_ack %A; must be %A" msg.PerCommitmentSecret cm.RemoteCommit.RemotePerCommitmentPoint
                |> RRClose
            | Choice2Of2 _ ->
                sprintf "Unexpected revocation"
                |> RRClose
            | Choice1Of2({ NextRemoteCommit = theirNextCommit }) ->
                let commitments1 = { cm with LocalChanges = { cm.LocalChanges with Signed = []; ACKed = cm.LocalChanges.ACKed @ cm.LocalChanges.Signed }
                                             RemoteChanges = { cm.RemoteChanges with Signed = [] }
                                             RemoteCommit = theirNextCommit
                                             RemoteNextCommitInfo = Choice2Of2(msg.NextPerCommitmentPoint)
                                             RemotePerCommitmentSecrets = cm.RemotePerCommitmentSecrets.AddHash (msg.PerCommitmentSecret.ToBytes(), 0xffffffffffffUL - cm.RemoteCommit.Index) }
                let result = [ WeAcceptedRevokeAndACK(commitments1) ]
                result |> Good
                failwith "needs update"


        | ChannelState.Normal state, ChannelCommand.Close cmd ->
            let localSPK = cmd.ScriptPubKey |> Option.defaultValue (state.Commitments.LocalParams.DefaultFinalScriptPubKey)
            if (not <| Scripts.isValidFinalScriptPubKey (localSPK)) then
                sprintf "Invalid local final ScriptPubKey %O" (localSPK)
                |> RRIgnore
            else if (state.LocalShutdown.IsSome) then
                RRIgnore "shutdown is already in progress"
            else if (state.Commitments.LocalHasUnsignedOutgoingHTLCs()) then
                RRIgnore "Cannot close with unsigned outgoing htlcs"
            else
                let shutDown = { Shutdown.ChannelId = state.ChannelId
                                 ScriptPubKey = localSPK }
                [ AcceptedShutdownCMD shutDown ]
                |> Good
        | ChannelState.Normal state, RemoteShutdown msg ->
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
            if (not <| Scripts.isValidFinalScriptPubKey (msg.ScriptPubKey)) then
                sprintf "Invalid remote final ScriptPubKey %O" (msg.ScriptPubKey.ToString())
                |> RRClose
            else if (cm.RemoteHasUnsignedOutgoingHTLCs()) then
                sprintf "They sent shutdown msg (%A) while they have pending unsigned HTLCs, this is protocol violation" msg
                |> RRClose
            // Do we have Unsigned Outgoing HTLCs?
            else if (cm.LocalHasUnsignedOutgoingHTLCs()) then
                if (state.LocalShutdown.IsSome) then
                    "can't have pending unsigned outgoing htlcs after having sent Shutdown" |> RRClose
                else
                    // Are we in the middle of a signature?
                    match cm.RemoteNextCommitInfo with
                    // yes.
                    | Choice1Of2 waitingForRevocation ->
                        let nextCommitments = { state.Commitments with
                                                    RemoteNextCommitInfo = Choice1Of2({ waitingForRevocation with ReSignASAP = true }) }
                        [ AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs(msg, nextCommitments) ]
                        |> Good
                    // No. let's sign right away.
                    | Choice2Of2 _ ->
                        [ ChannelStateRequestedSignCommitment; AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs(msg, cm) ] |> Good
            else
                let (localShutdown, sendList) = match state.LocalShutdown with
                                                | Some localShutdown -> (localShutdown, [])
                                                | None ->
                                                    let localShutdown = { Shutdown.ChannelId = state.ChannelId
                                                                          ScriptPubKey = cm.LocalParams.DefaultFinalScriptPubKey }
                                                    (localShutdown, [ localShutdown ])
                if (cm.HasNoPendingHTLCs()) then
                    // we have to send first closing_signed msg iif we are the funder
                    if (cm.LocalParams.IsFunder) then
                        Closing.makeFirstClosingTx (cs.KeysRepository, cm, localShutdown.ScriptPubKey, msg.ScriptPubKey, cs.FeeEstimator, cm.LocalParams.ChannelPubKeys.FundingPubKey, cs.Network)
                        |>> fun (closingTx, closingSignedMsg) ->
                            let nextState = { NegotiatingData.ChannelId = cm.ChannelId
                                              Commitments = cm
                                              LocalShutdown = localShutdown
                                              RemoteShutdown = msg
                                              ClosingTxProposed = [ [ { ClosingTxProposed.UnsignedTx = closingTx; LocalClosingSigned = closingSignedMsg } ] ]
                                              MaybeBestUnpublishedTx = None }
                            [ AcceptedShutdownWhenNoPendingHTLCs(closingSignedMsg |> Some, nextState) ]
                    else
                        let nextState = { NegotiatingData.ChannelId = cm.ChannelId
                                          Commitments = cm
                                          LocalShutdown = localShutdown
                                          RemoteShutdown = msg
                                          ClosingTxProposed = [ [] ]
                                          MaybeBestUnpublishedTx = None }
                        [ AcceptedShutdownWhenNoPendingHTLCs(None, nextState) ] |> Good
                else
                    let nextState = { ShutdownData.Commitments = cm
                                      LocalShutdown = localShutdown
                                      RemoteShutdown = msg
                                      ChannelId = cm.ChannelId }
                    [ AcceptedShutdownWhenWeHavePendingHTLCs(nextState) ]
                    |> Good

        // ----------- closing ---------
        | Shutdown state, FulfillHTLC cmd ->
            state.Commitments |> Commitments.sendFulfill cmd
            |>> fun t -> [ WeAcceptedCMDFulfillHTLC(t) ]
            >>>= fun e -> e.Describe() |> RRApiMisuse
        | Shutdown state, ApplyUpdateFulfillHTLC msg ->
            state.Commitments |> Commitments.receiveFulfill msg
            >>>= fun e -> e.Describe() |> RRClose
        | Shutdown state, FailHTLC cmd ->
            state.Commitments |> Commitments.sendFail (cs.LocalNodeSecret) cmd
            >>>= fun e -> e.Describe() |> RRApiMisuse
        | Shutdown state, FailMalformedHTLC cmd ->
            state.Commitments |> Commitments.sendFailMalformed cmd
            >>>= fun e -> e.Describe() |> RRApiMisuse
        | Shutdown state, ApplyUpdateFailMalformedHTLC msg ->
            state.Commitments |> Commitments.receiveFailMalformed msg
            >>>= fun e -> e.Describe() |> RRClose
        | Shutdown state, UpdateFee cmd ->
            state.Commitments |> Commitments.sendFee cmd
            >>>= fun e -> e.Describe() |> RRApiMisuse
        | Shutdown state, ApplyUpdateFee msg ->
            let localFeerate = cs.FeeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority)
            state.Commitments |> Commitments.receiveFee cs.Config localFeerate msg
            >>>= fun e -> e.Describe() |> RRClose
        | Shutdown state, SignCommitment ->
            let cm = state.Commitments
            match cm.RemoteNextCommitInfo with
            | _ when (not <| cm.LocalHasChanges()) ->
                sprintf "nothing to sign" |> RRIgnore
            | Choice2Of2 _ ->
                cm |> Commitments.sendCommit (cs.Secp256k1Context) (cs.KeysRepository) (cs.Network)
                >>>= fun e -> e.Describe() |> RRClose
            | Choice1Of2 _waitForRevocation ->
                sprintf "Already in the process of signing."
                |> RRIgnore
        | Shutdown state, ApplyCommitmentSigned msg ->
            state.Commitments |> Commitments.receiveCommit (cs.Secp256k1Context) (cs.KeysRepository) msg cs.Network
            >>>= fun e -> e.Describe() |> RRClose
        | Shutdown state, ApplyRevokeAndACK msg ->
            failwith "not implemented"

        | Negotiating state, ApplyClosingSigned msg ->
            let cm = state.Commitments
            let lastCommitFeeSatoshi =
                cm.FundingSCoin.TxOut.Value - (cm.LocalCommit.PublishableTxs.CommitTx.Value.TotalOut)
            checkOrClose msg.FeeSatoshis (>) lastCommitFeeSatoshi "remote proposed a commit fee higher than the last commitment fee. remoteClosingFee=%A; localCommitTxFee=%A;"
            >>= fun _ ->
                Closing.makeClosingTx (cs.KeysRepository, cm, state.LocalShutdown.ScriptPubKey, state.RemoteShutdown.ScriptPubKey, msg.FeeSatoshis, cm.LocalParams.ChannelPubKeys.FundingPubKey, cs.Network)
                >>= fun (closingTx, closingSignedMsg) ->
                    Transactions.checkTxFinalized closingTx.Value (closingTx.WhichInput) (seq [ cm.RemoteParams.FundingPubKey, TransactionSignature(msg.Signature.Value, SigHash.All) ])
                    >>= fun finalizedTx ->
                        let maybeLocalFee =
                            state.ClosingTxProposed
                            |> List.tryHead
                            |> Option.bind (List.tryHead)
                            |> Option.map (fun v -> v.LocalClosingSigned.FeeSatoshis)
                        let areWeInDeal = Some(msg.FeeSatoshis) = maybeLocalFee
                        let hasTooManyNegotiationDone =
                            (state.ClosingTxProposed |> List.collect (id) |> List.length) >= cs.Config.PeerChannelConfigLimits.MaxClosingNegotiationIterations
                        if (areWeInDeal || hasTooManyNegotiationDone) then
                            Closing.handleMutualClose (finalizedTx, Choice1Of2({ state with MaybeBestUnpublishedTx = Some(finalizedTx) }))
                        else
                            let lastLocalClosingFee = state.ClosingTxProposed |> List.tryHead |> Option.bind (List.tryHead) |> Option.map (fun txp -> txp.LocalClosingSigned.FeeSatoshis)
                            let nextClosingFeeRR =
                                match lastLocalClosingFee with | Some v -> Good v | None -> (Closing.firstClosingFee (state.Commitments, state.LocalShutdown.ScriptPubKey, state.RemoteShutdown.ScriptPubKey, cs.FeeEstimator, cs.Network))
                                |>> fun localF ->
                                    Closing.nextClosingFee (localF, msg.FeeSatoshis)
                            nextClosingFeeRR
                            >>= fun nextClosingFee ->
                                if (Some nextClosingFee = lastLocalClosingFee) then
                                    Closing.handleMutualClose (finalizedTx, Choice1Of2({ state with MaybeBestUnpublishedTx = Some(finalizedTx) }))
                                else if (nextClosingFee = msg.FeeSatoshis) then
                                    // we have reached on agreement!
                                    let closingTxProposed1 =
                                        let newProposed = [ { ClosingTxProposed.UnsignedTx = closingTx
                                                              LocalClosingSigned = closingSignedMsg } ]
                                        newProposed :: state.ClosingTxProposed
                                    let negoData = { state with ClosingTxProposed = closingTxProposed1
                                                                MaybeBestUnpublishedTx = Some(finalizedTx) }
                                    Closing.handleMutualClose (finalizedTx, Choice1Of2(negoData))
                                else
                                    let closingTxProposed1 =
                                        let newProposed = [ { ClosingTxProposed.UnsignedTx = closingTx
                                                              LocalClosingSigned = closingSignedMsg } ]
                                        newProposed :: state.ClosingTxProposed
                                    let nextState = { state with ClosingTxProposed = closingTxProposed1; MaybeBestUnpublishedTx = Some(finalizedTx) }
                                    [ WeProposedNewClosingSigned(closingSignedMsg, nextState) ]
                                    |> Good
            >>>= fun e -> e.Describe() |> RRClose
        | Closing state, FulfillHTLC cmd ->
            cs.Logger (LogLevel.Info) (sprintf "got valid payment preimage, recalculating txs to redeem the corresponding htlc on-chain")
            let cm = state.Commitments
            cm |> Commitments.sendFulfill cmd
            >>= fun (msgToSend, newCommitments) ->
                let localCommitPublished =
                    state.LocalCommitPublished
                    |> Option.map (fun localCommitPublished -> Closing.claimCurrentLocalCommitTxOutputs (cs.KeysRepository, newCommitments.LocalParams.ChannelPubKeys, newCommitments, localCommitPublished.CommitTx))
                failwith ""
        | state, cmd ->
            sprintf "DotNetLightning does not know how to handle command (%A) while in state (%A)" cmd state
            |> RRApiMisuse

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
            let channelUpdate = ChannelHelpers.makeChannelUpdate (c.Network.GenesisHashRev,
                                                           c.LocalNodeSecret,
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
        | WeAcceptedCMDAddHTLC(_, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }
        | WeAcceptedUpdateAddHTLC(newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }

        | WeAcceptedCMDFulfillHTLC(_, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }
        | WeAcceptedFulfillHTLC(_msg, _origin, _htlc, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }

        | WeAcceptedCMDFailHTLC(_msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }
        | WeAcceptedFailHTLC(_origin, _msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }

        | WeAcceptedCMDFailMalformedHTLC(_msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }
        | WeAcceptedFailMalformedHTLC(_origin, _msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }

        | WeAcceptedCMDUpdateFee(_msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }
        | WeAcceptedUpdateFee(_msg), ChannelState.Normal _d -> c

        | WeAcceptedCMDSign(_msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }
        | WeAcceptedCommitmentSigned(_msg, newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }

        | WeAcceptedRevokeAndACK(newCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with Commitments = newCommitments }) }

        // -----  closing ------
        | AcceptedShutdownCMD msg, ChannelState.Normal d ->
            { c with State = ChannelState.Normal({ d with LocalShutdown = Some msg }) }
        | AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs(remoteShutdown, nextCommitments), ChannelState.Normal d ->
            { c with State = ChannelState.Normal ({ d with RemoteShutdown = Some remoteShutdown; Commitments = nextCommitments }) }
        | AcceptedShutdownWhenNoPendingHTLCs(_maybeMsg, nextState), ChannelState.Normal _d ->
            { c with State = Negotiating nextState }
        | AcceptedShutdownWhenWeHavePendingHTLCs(nextState), ChannelState.Normal _d ->
            { c with State = Shutdown nextState }
        | MutualClosePerformed nextState, ChannelState.Negotiating _d ->
            { c with State = Closing nextState }
        | WeProposedNewClosingSigned(_msg, nextState), ChannelState.Negotiating _d ->
            { c with State = Negotiating(nextState) }
        // ----- else -----
        | otherEvent -> c
