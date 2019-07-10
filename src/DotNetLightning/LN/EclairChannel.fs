namespace DotNetLightning.LN
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Chain
open DotNetLightning.Crypto
open DotNetLightning.Transactions
open DotNetLightning.Serialize.Msgs
open NBitcoin
open System.Linq

type Channel = internal {
    Config: UserConfig
    UserId: UserId
    ChainListener: IChainListener
    KeysRepository: IKeysRepository
    FeeEstimator: IFeeEstimator
    FundingTxProvider: (IDestination * Money * FeeRatePerKw) -> RResult<FinalizedTx * TxOutIndex>
    Logger: Logger
    RemoteNodeId: NodeId
    State: ChannelState
    Network: Network
}
    with
        static member Create (config, userId, logger, chainListener, keysRepo, feeEstimator, nodeId, fundingTxProvider, n) =
            {
                Config = config
                UserId = userId
                ChainListener = chainListener
                KeysRepository = keysRepo
                FeeEstimator = feeEstimator
                Logger = logger
                FundingTxProvider = fundingTxProvider
                RemoteNodeId = nodeId
                State = WaitForInitInternal
                Network = n
            }
        static member CreateCurried  = curry9 (Channel.Create)

module Channel =
    /// represents the user has than something wrong with this library
    let private RRApiE(e: APIError) =
        RResult.rbad(RBad.Object(e))

    /// Represents the error that something user can not control (e.g. peer has sent invalid msg).
    let private RRChannelE(ex: ChannelError) =
        RResult.rexn(ChannelException(ex))

    let private RRClose (msg: string) =
        RRChannelE(ChannelError.Close(msg))


    module internal Helpers =
        let deriveOurDustLimitSatoshis(feeEstimator: IFeeEstimator): Money =
            let (FeeRatePerKw atOpenBackGroundFee) = feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Background)
            (Money.Satoshis((uint64 atOpenBackGroundFee) * B_OUTPUT_PLUS_SPENDING_INPUT_WEIGHT / 1000UL), Money.Satoshis(546UL))
            |> Money.Max

        let getOurChannelReserve (channelValue: Money) =
            let q = channelValue / 100L
            Money.Min(channelValue, Money.Max(q, Money.Satoshis(1L)))

        let buildLocalCommitmentSecret (n) =
            failwith "not impl"

        let getFundingRedeemScript (ck: ChannelKeys) (theirFundingPubKey: PubKey) : Script =
            let ourFundingKey = ck.FundingKey.PubKey
            let pks = if ourFundingKey.ToBytes() < theirFundingPubKey.ToBytes() then
                          [| ourFundingKey; theirFundingPubKey |]
                      else
                          [| theirFundingPubKey; ourFundingKey |]
            PayToMultiSigTemplate.Instance.GenerateScriptPubKey(2, pks)


        let makeFirstCommitTxs(localParams: LocalParams)
                              (remoteParams: RemoteParams)
                              (fundingSatoshis: Money)
                              (pushMSat: LNMoney)
                              (initialFeeRatePerKw: FeeRatePerKw)
                              (fundingOutputIndex: TxOutIndex)
                              (fundingTx: Transaction)
                              (remoteFirstperCommitmetnPoint: PubKey)
                              (n: Network): RResult<(CommitmentSpec * CommitTx * CommitmentSpec * CommitTx)> =
            let toLocal = if (localParams.IsFunder) then fundingSatoshis.ToLNMoney() - pushMSat else pushMSat
            let toRemote = if (localParams.IsFunder) then pushMSat else fundingSatoshis.ToLNMoney() - pushMSat
            let localSpec = CommitmentSpec.Create toLocal toRemote initialFeeRatePerKw
            let remoteSpec = CommitmentSpec.Create toRemote toLocal initialFeeRatePerKw
            let checkTheyCanAffordFee () =
                let toRemote = remoteSpec.ToLocal
                let fees = Transactions.commitTxFee remoteParams.DustLimitSatoshis remoteSpec
                let missing = toRemote.ToMoney() - localParams.ChannelReserveSatoshis - fees
                if missing < Money.Zero then
                    RResult.rmsg (sprintf "they are funder but cannot affored there fee. to_remote output is: %A; actual fee is %A; channel_reserve_satoshis: %A" toRemote fees localParams.ChannelReserveSatoshis)
                else
                    Good()
            let makeFirstCommitTxCore () =
                let redeem = getFundingRedeemScript localParams.ChannelKeys remoteParams.FundingPubKey
                let scoin = fundingTx.Outputs.AsIndexedOutputs().ElementAt(int fundingOutputIndex.Value) |> fun o -> ScriptCoin(o, redeem)
                let localCommitTx =
                    Transactions.makeCommitTx scoin
                                              0UL
                                              localParams.ChannelKeys.PaymentBaseKey.PubKey 
                                              remoteParams.PaymentBasePoint
                                              localParams.IsFunder
                                              localParams.DustLimitSatoshis
                                              localParams.ChannelKeys.RevocationBaseKey.PubKey
                                              localParams.ToSelfDelay
                                              (localParams.ChannelKeys.DelayedPaymentBaseKey.PubKey)
                                              (remoteParams.PaymentBasePoint)
                                              (localParams.ChannelKeys.HTLCBaseKey.PubKey)
                                              (remoteParams.HTLCBasePoint)
                                              localSpec
                                              n
                let remoteCommitTx =
                    Transactions.makeCommitTx scoin
                                              0UL
                                              remoteParams.PaymentBasePoint
                                              localParams.ChannelKeys.PaymentBaseKey.PubKey
                                              (not localParams.IsFunder)
                                              (remoteParams.DustLimitSatoshis)
                                              (remoteParams.RevocationBasePoint)
                                              (remoteParams.ToSelfDelay)
                                              (remoteParams.DelayedPaymentBasePoint)
                                              (localParams.ChannelKeys.PaymentBaseKey.PubKey)
                                              (remoteParams.HTLCBasePoint)
                                              (localParams.ChannelKeys.HTLCBaseKey.PubKey)
                                              remoteSpec
                                              n

                (localSpec, localCommitTx, remoteSpec, remoteCommitTx) |> Good

            if (not localParams.IsFunder) then
                checkTheyCanAffordFee() *>  (makeFirstCommitTxCore())
            else
                makeFirstCommitTxCore()

    module internal Validation =
        let internal checkSigHash (expected: SigHash) (txSig: TransactionSignature) =
            if txSig.SigHash <> expected then
                RRClose(sprintf "tx sighash must be %A . but it was %A" expected txSig.SigHash)
            else
                Good (txSig.Signature)
        module private OpenChannelRequest =
            let checkFundingSatoshisLessThanMax (msg: OpenChannel) =
                if (msg.FundingSatoshis >= ChannelConstants.MAX_FUNDING_SATOSHIS) then
                    RRClose("Funding value > 2^24")
                else
                    Good ()

            let checkChannelReserveSatohisLessThanFundingSatoshis (msg: OpenChannel) =
                if (msg.ChannelReserveSatoshis > msg.FundingSatoshis) then
                    RRClose("Bogus channel_reserve_satoshis. Must be bigger than funding_satoshis")
                else
                    Good()

            let checkPushMSatLesserThanFundingValue msg =
                if (msg.PushMSat.ToMoney() > (msg.FundingSatoshis - msg.ChannelReserveSatoshis)) then
                    RRClose("push_msat larger than funding value")
                else
                    Good ()

            let checkFundingSatoshisLessThanDustLimitSatoshis (msg: OpenChannel) =
                if (msg.DustLimitSatoshis > msg.FundingSatoshis) then
                    RRClose(sprintf "Peer never wants payout outputs? dust_limit_satoshis: %A; funding_satoshi %A" msg.DustLimitSatoshis msg.FundingSatoshis)
                else
                    Good ()

            let checkRemoteFee (feeEstimator: IFeeEstimator) (feeRate: FeeRatePerKw) =
                if (feeRate) < feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Background) then
                    RRClose("Peer's feerate much too low")
                else if (feeRate.Value > feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.HighPriority).Value * 2u) then
                    RRClose("Peer's feerate much too high")
                else
                    Good ()

            let checkToSelfDelayIsInAcceptableRange (msg: OpenChannel) =
                if (msg.ToSelfDelay > MAX_LOCAL_BREAKDOWN_TIMEOUT) then
                    RRClose("They wanted our payments to be delayed by a needlessly long period")
                else
                    Good ()

            let checkMaxAcceptedHTLCsInMearningfulRange (maxAcceptedHTLCs: uint16) =
                if (maxAcceptedHTLCs < 1us) then
                    RRClose("max_accepted_htlcs must be not 0")
                else if (maxAcceptedHTLCs < 483us) then
                    RRClose("max_accepted_htlcs must be less than 483")
                else
                    Good ()

            let checkConfigPermits (config: ChannelHandshakeLimits) (msg: OpenChannel) =
            
                if (msg.FundingSatoshis < config.MinFundingSatoshis) then
                    RRClose(sprintf "funding satoshis is less than the user specified limit. received: %A; limit: %A" msg.FundingSatoshis config.MinFundingSatoshis)
                else if (msg.HTLCMinimumMsat.ToMoney() > config.MinFundingSatoshis) then
                    RRClose(sprintf "htlc minimum msat is higher than the users specified limit. received %A; limit: %A" msg.FundingSatoshis config.MaxHTLCMinimumMSat)
                else if (msg.MaxHTLCValueInFlightMsat < config.MinMaxHTLCValueInFlightMSat) then
                    RRClose ("max htlc value in light msat is less than the user specified limit")
                else if  (msg.ChannelReserveSatoshis > config.MaxChannelReserveSatoshis) then
                    RRClose(sprintf "channel reserve satoshis is higher than the user specified limit. received %A; limit: %A" msg.ChannelReserveSatoshis msg.ChannelReserveSatoshis)
                else if (msg.MaxAcceptedHTLCs < config.MinMaxAcceptedHTLCs) then
                    RRClose(sprintf "max accepted htlcs is less than the user specified limit. received: %A; limit: %A" msg.MaxAcceptedHTLCs config.MinMaxAcceptedHTLCs)
                else if (msg.DustLimitSatoshis < config.MinDustLimitSatoshis) then
                    RRClose(sprintf "dust_limit_satoshis is less than the user specified limit. received: %A; limit: %A" msg.DustLimitSatoshis config.MinDustLimitSatoshis)
                else if (msg.DustLimitSatoshis > config.MaxDustLimitSatoshis) then 
                    RRClose(sprintf "dust_limit_satoshis is greater than the user specified limit. received: %A; limit: %A" msg.DustLimitSatoshis config.MaxDustLimitSatoshis)
                else
                    Good ()

            let checkChannelAnnouncementPreferenceAcceptable (config: UserConfig) (msg) =
                let theirAnnounce = (msg.ChannelFlags &&& 1uy) = 1uy
                if (config.PeerChannelConfigLimits.ForceAnnouncedChannelPreference) && config.ChannelOptions.AnnouncedChannel <> theirAnnounce then
                    RRClose("Peer tried to open channel but their announcement preference is different from ours")
                else
                    Good ()

            let checkIsAccpetableByCurrentFeeRate (feeEstimator: IFeeEstimator) msg = 
                let ourDustLimit = Helpers.deriveOurDustLimitSatoshis feeEstimator
                let ourChannelReserve = Helpers.getOurChannelReserve (msg.FundingSatoshis)
                if (ourChannelReserve < ourDustLimit) then
                    RRClose("Suitable channel reserve not found. Aborting")
                else if (msg.ChannelReserveSatoshis < ourDustLimit) then
                    RRClose (sprintf "channel_reserve_satoshis too small. It was: %A; dust_limit: %A" msg.ChannelReserveSatoshis ourDustLimit )
                else if (ourChannelReserve < msg.DustLimitSatoshis) then
                    RRClose(sprintf "Dust limit too high for our channel reserve. our channel reserve: %A received dust limit: %A" ourChannelReserve msg.DustLimitSatoshis)
                else
                    Good ()

            let checkIfFundersAmountSufficient (feeEst: IFeeEstimator) msg =
                let fundersAmount = LNMoney.Satoshis(msg.FundingSatoshis.Satoshi) - msg.PushMSat
                let (backgroundFeeRate) = feeEst.GetEstSatPer1000Weight (ConfirmationTarget.Background)
                let backgroundFee = backgroundFeeRate.ToFee COMMITMENT_TX_BASE_WEIGHT
                if (fundersAmount.ToMoney() < backgroundFee) then
                    RRClose(sprintf "Insufficient funding amount for initial commitment. BackgroundFee %A. funders amount %A" backgroundFee fundersAmount)
                else
                    let ourChannelReserve = Helpers.getOurChannelReserve msg.FundingSatoshis
                    let toLocalMSat = msg.PushMSat
                    let toRemoteMSat = fundersAmount - backgroundFeeRate.ToFee(COMMITMENT_TX_BASE_WEIGHT).ToLNMoney()
                    if (toLocalMSat <= (msg.ChannelReserveSatoshis.ToLNMoney()) && toRemoteMSat <= ourChannelReserve.ToLNMoney()) then
                        RRClose("Insufficient funding amount for iniial commitment. ")
                    else
                        Good ()


        let internal checkOpenChannelMsgAcceptable cs (msg: OpenChannel) =
            OpenChannelRequest.checkFundingSatoshisLessThanMax msg
            *> OpenChannelRequest.checkChannelReserveSatohisLessThanFundingSatoshis msg
            *> OpenChannelRequest.checkPushMSatLesserThanFundingValue msg
            *> OpenChannelRequest.checkFundingSatoshisLessThanDustLimitSatoshis msg
            *> OpenChannelRequest.checkRemoteFee cs.FeeEstimator msg.FeeRatePerKw
            *> OpenChannelRequest.checkToSelfDelayIsInAcceptableRange msg
            *> OpenChannelRequest.checkMaxAcceptedHTLCsInMearningfulRange msg.MaxAcceptedHTLCs
            *> OpenChannelRequest.checkConfigPermits cs.Config.PeerChannelConfigLimits msg
            *> OpenChannelRequest.checkChannelAnnouncementPreferenceAcceptable cs.Config msg
            *> OpenChannelRequest.checkIsAccpetableByCurrentFeeRate cs.FeeEstimator msg

        module private AcceptChannelValidator =
            let checkDustLimit msg =
                if msg.DustLimitSatoshis > Money.Satoshis(21000000L * 100000L) then
                    RRClose(sprintf "Peer never wants payout outputs? dust_limit_satoshis was: %A" msg.DustLimitSatoshis)
                else
                    Good ()

            let checkChannelReserveSatoshis (c: Channel) (state: Data.WaitForAcceptChannelData) msg =
                if msg.ChannelReserveSatoshis > state.LastSent.FundingSatoshis then
                    sprintf "bogus channel_reserve_satoshis %A . Must be larger than funding_satoshis %A" (msg.ChannelReserveSatoshis) (state.InputInitFunder.FundingSatoshis)
                    |> RRClose
                else if msg.DustLimitSatoshis > state.LastSent.ChannelReserveSatoshis then
                    sprintf "Bogus channel_serserve and dust_limit. dust_limit: %A; channel_reserve %A" msg.DustLimitSatoshis (state.LastSent.ChannelReserveSatoshis)
                    |> RRClose
                else if msg.ChannelReserveSatoshis < state.LastSent.DustLimitSatoshis then
                    sprintf "Peer never wants payout outputs? channel_reserve_satoshis are %A; dust_limit_satoshis in our last sent msg is %A" msg.ChannelReserveSatoshis (state.LastSent.DustLimitSatoshis)
                    |> RRClose
                else
                    Good ()

            let checkDustLimitIsLargerThanOurChannelReserve (state: Data.WaitForAcceptChannelData) msg =
                let reserve = Helpers.getOurChannelReserve state.LastSent.FundingSatoshis
                if (msg.DustLimitSatoshis < reserve) then
                    sprintf "dust limit (%A) is bigger than our channel reserve (%A)" msg.DustLimitSatoshis reserve
                    |> RRClose
                else
                    Good ()

            let checkMinimumHTLCValueIsAcceptable (state: Data.WaitForAcceptChannelData) (msg: AcceptChannel) =
                if (msg.HTLCMinimumMSat.ToMoney() >= (state.LastSent.FundingSatoshis - msg.ChannelReserveSatoshis)) then
                    sprintf "Minimum HTLC value is greater than full channel value HTLCMinimum %A satoshi; funding_satoshis %A; channel_reserve: %A" (msg.HTLCMinimumMSat.ToMoney()) (state.LastSent.FundingSatoshis) (msg.ChannelReserveSatoshis)
                    |> RRClose
                else
                    Good ()

            let checkToSelfDelayIsAcceptable (msg) =
                if (msg.ToSelfDelay > MAX_LOCAL_BREAKDOWN_TIMEOUT) then
                    sprintf "They wanted our payments to be delayed by a needlessly long period (%A)" msg.ToSelfDelay
                    |> RRClose
                else
                    Good ()

            let checkConfigPermits (config: ChannelHandshakeLimits) (msg: AcceptChannel) =
                let check left predicate right msg =
                    if predicate left right then
                        sprintf msg left right |> RRClose
                    else
                        Good ()
                let check1 = check msg.HTLCMinimumMSat (>) config.MaxHTLCMinimumMSat "HTLC Minimum msat in accept_channel (%A) is higher than the user specified limit (%A)"
                let check2 = check msg.MaxHTLCValueInFlightMsat (<) config.MinMaxHTLCValueInFlightMSat  "max htlc value in flight msat (%A) is less than the user specified limit (%A)"
                let check3 = check msg.ChannelReserveSatoshis (>) config.MaxChannelReserveSatoshis "max reserve_satoshis (%A) is higher than the user specified limit (%A)"
                let check4 = check msg.MaxAcceptedHTLCs (<) config.MinMaxAcceptedHTLCs "max accpeted htlcs (%A) is less than the user specified limit (%A)"
                let check5 = check msg.DustLimitSatoshis (<) config.MinDustLimitSatoshis "dust limit satoshis (%A) is less then the user specified limit (%A)"
                let check6 = check msg.DustLimitSatoshis (>) config.MinDustLimitSatoshis "dust limit satoshis (%A) is greater then the user specified limit (%A)"
                let check7 = check msg.MinimumDepth (>) config.MaxMinimumDepth "We consider the minimum depth (%A) to be unreasonably large. Our max minimum depth is (%A)"

                check1 *> check2 *> check3 *> check4 *> check5 *> check6 *> check7
        
        let internal checkAcceptChannelMsgAccpetable c (state) (msg: AcceptChannel) =
            AcceptChannelValidator.checkDustLimit msg
            *> AcceptChannelValidator.checkChannelReserveSatoshis c state msg
            *> AcceptChannelValidator.checkDustLimitIsLargerThanOurChannelReserve state msg
            *> AcceptChannelValidator.checkMinimumHTLCValueIsAcceptable state msg
            *> AcceptChannelValidator.checkToSelfDelayIsAcceptable msg
            *> OpenChannelRequest.checkMaxAcceptedHTLCsInMearningfulRange msg.MaxAcceptedHTLCs
            *> AcceptChannelValidator.checkConfigPermits c.Config.PeerChannelConfigLimits msg


    open Events
    let executeCommand (cs: Channel) (command: ChannelCommand): RResult<ChannelEvent list> =
        match cs.State, command with
        | WaitForOpenChannel state, ApplyOpenChannel msg ->
            Validation.checkOpenChannelMsgAcceptable cs msg
            |>> fun _ ->
                let localParams = state.InitFundee.LocalParams
                let localCommitmentSecret = Helpers.buildLocalCommitmentSecret ()
                let channelKeys = cs.KeysRepository.GetChannelKeys(true)
                let acceptChannel = { AcceptChannel.TemporaryChannelId = msg.TemporaryChannelId
                                      DustLimitSatoshis = localParams.DustLimitSatoshis
                                      MaxHTLCValueInFlightMsat = localParams.MaxHtlcValueInFlightMSat
                                      ChannelReserveSatoshis = localParams.ChannelReserveSatoshis
                                      HTLCMinimumMSat = localParams.HTLCMinimumMSat
                                      MinimumDepth = cs.Config.OwnChannelConfig.MinimumDepth
                                      ToSelfDelay = localParams.ToSelfDelay
                                      MaxAcceptedHTLCs = localParams.MaxAcceptedHTLCs
                                      FundingPubKey = channelKeys.FundingKey.PubKey
                                      RevocationBasepoint = channelKeys.RevocationBaseKey.PubKey
                                      PaymentBasepoint = channelKeys.PaymentBaseKey.PubKey
                                      DelayedPaymentBasepoint = channelKeys.DelayedPaymentBaseKey.PubKey
                                      HTLCBasepoint = channelKeys.HTLCBaseKey.PubKey
                                      FirstPerCommitmentPoint = localCommitmentSecret.PubKey
                                      ShutdownScriptPubKey = None }

                let remoteParams = RemoteParams.FromOpenChannel cs.RemoteNodeId state.InitFundee.RemoteInit msg
                let data = Data.WaitForFundingCreatedData.Create localParams remoteParams msg acceptChannel
                [ WeAcceptedOpenChannel (acceptChannel, data) ]
        | WaitForOpenChannel state, Close spk ->  [Closed] |> Good
        | WaitForOpenChannel state, cmd -> sprintf "Channel cannot handle %A while waiting for open_channel" cmd |> RRClose
        | WaitForAcceptChannel state, ApplyAcceptChannel msg ->
            Validation.checkAcceptChannelMsgAccpetable cs state msg
            >>= fun _ ->
                let redeem = cs.KeysRepository.GetChannelKeys(false) |> Helpers.getFundingRedeemScript <| (msg.FundingPubKey)
                cs.FundingTxProvider(redeem.WitHash :> IDestination, state.InputInitFunder.FundingSatoshis, state.InputInitFunder.FundingTxFeeRatePerKw)
            >>= fun (fundingTx, outIndex) ->
                let remoteParams = RemoteParams.FromAcceptChannel cs.RemoteNodeId (state.InputInitFunder.RemoteInit) msg
                let localParams = state.InputInitFunder.LocalParams
                let commitmentSpec = state.InputInitFunder.DeriveCommitmentSpec()
                Helpers.makeFirstCommitTxs localParams
                                           remoteParams
                                           state.LastSent.FundingSatoshis
                                           state.LastSent.PushMSat
                                           state.LastSent.FeeRatePerKw
                                           outIndex
                                           fundingTx.Value
                                           msg.FirstPerCommitmentPoint
                                           cs.Network
                >>= fun (localSpec, localCommitTx, remoteSpec, remoteCommitTx) ->
                    let (CommitTx psbt) = remoteCommitTx
                    Validation.checkSigHash SigHash.All (cs.KeysRepository.GetSignature(psbt))
                    |>> fun (localSigOfRemoteCommit) ->
                        let nextMsg = { FundingCreated.TemporaryChannelId = msg.TemporaryChannelId
                                        FundingTxId = fundingTx.Value.GetTxId()
                                        FundingOutputIndex = outIndex.Value
                                        Signature = localSigOfRemoteCommit }

                        let data = { Data.WaitForFundingSignedData.ChannelId = msg.TemporaryChannelId
                                     LocalParams = localParams
                                     RemoteParams = remoteParams
                                     Data.WaitForFundingSignedData.FundingTx = fundingTx
                                     Data.WaitForFundingSignedData.LocalSpec =  commitmentSpec
                                     LocalCommitTx = localCommitTx
                                     RemoteCommit = { RemoteCommit.Index = outIndex
                                                      Spec = remoteSpec
                                                      Tx = remoteCommitTx
                                                      RemotePerCommitmentPoint = msg.FirstPerCommitmentPoint }
                                     ChannelFlags = state.InputInitFunder.ChannelFlags
                                     LastSent = nextMsg }
                        [WeAcceptedAcceptChannel(nextMsg, data)]
        | WaitForFundingSigned state, ApplyFundingSigned msg ->
            let finalLocalCommitTxRR =
                let theirFundingPk = state.RemoteParams.FundingPubKey
                let localSigPairOfLocalTx = (theirFundingPk, TransactionSignature(state.LastSent.Signature, SigHash.All))
                let remoteSigPairOfLocalTx = (state.LocalParams.ChannelKeys.FundingKey.PubKey, TransactionSignature(msg.Signature, SigHash.All))
                let sigPairs = seq [ remoteSigPairOfLocalTx; localSigPairOfLocalTx ]
                let fundingOutIndex = state.LastSent.FundingOutputIndex |> TxOutIndex
                let redeem = Helpers.getFundingRedeemScript state.LocalParams.ChannelKeys theirFundingPk
                Transactions.checkTxFinalized state.LocalCommitTx fundingOutIndex sigPairs (redeem)
            finalLocalCommitTxRR
            >>>= fun e -> RRClose(e.Describe())
            |>> fun finalizedLocalCommitTx ->
                let commitments = { Commitments.LocalParams = state.LocalParams
                                    RemoteParams = state.RemoteParams
                                    ChannelFlags = state.ChannelFlags
                                    LocalCommit = { Index = 0u;
                                                    Spec = state.LocalSpec;
                                                    PublishableTxs = { PublishableTxs.CommitTx = finalizedLocalCommitTx
                                                                       HTLCTxs = [] } }
                                    RemoteCommit = state.RemoteCommit
                                    LocalChanges = { LocalChanges.ACKed = []
                                                     Proposed = []
                                                     Signed = [] }
                                    RemoteChanges = { RemoteChanges.ACKed = []
                                                      Proposed = []
                                                      Signed = [] }
                                    LocalNextHTLCId = 0UL
                                    RemoteNextHTLCId = 0UL
                                    OriginChannels = Map.empty
                                    // we will receive their next per-commitment point in the next msg, so we temporarily put a random byte array
                                    RemoteNextCommitInfo = DataEncoders.HexEncoder().DecodeData("0101010101010101010101010101010101010101010101010101010101010101") |> Key |> fun k -> k.PubKey |> Choice2Of2
                                    RemotePerCommitmentSecrets = ShaChain.Zero
                                    ChannelId =
                                        cs.Logger(sprintf "Channel id has been set to %A" msg.ChannelId, LogLevel.Info)
                                        msg.ChannelId }
                let nextState = { WaitForFundingConfirmedData.Commitments = commitments
                                  Deferred = None
                                  LastSent = Choice1Of2 state.LastSent }
                [WeAcceptedFundingSigned (state.FundingTx, nextState)]
        | WaitForFundingConfirmed state, ApplyFundingLocked msg ->
            [ WeAcceptedFundingLockedMsgWhenNotReady msg ] |> Good
        | WaitForFundingConfirmed { Commitments = commitments; Deferred = deferred }, ApplyFundingConfirmedEvent event ->
            cs.Logger(sprintf "ChannelId %A was confirmed at blockheight %A" commitments.ChannelId event.Value, LogLevel.Info)
            let nextPerCommitmentPoint = ChannelUtils.buildCommitmentSecret()
            failwith ""
        | WaitForFundingLocked state, ApplyFundingLocked msg ->
            failwith ""

    let applyEvent c (e: ChannelEvent): Channel=
        match e with
        | WeAcceptedOpenChannel (nextMsg, data) ->
            let state = WaitForFundingCreated data
            { c with State = state }
        | WeAcceptedAcceptChannel (nextMsg, data) ->
            {c with State = WaitForFundingSigned data}
        | WeAcceptedFundingSigned (txToPublish, data) ->
            { c with State = WaitForFundingConfirmed data}
        | WeAcceptedFundingLockedMsgWhenNotReady msg ->
            match c.State with
            | WaitForFundingConfirmed s -> { c with State = WaitForFundingConfirmed( { s with Deferred = Some(msg)} ) }
            | _ -> failwith "Unreachable! "