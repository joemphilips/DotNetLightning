namespace DotNetLightning.LN
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Chain
open DotNetLightning.Serialize.Msgs
open NBitcoin

type Channel = {
    Config: UserConfig
    RemoteNodeId: NodeId
    ChainListener: IChainListener
    KeysRepository: IKeysRepository
    FeeEstimator: IFeeEstimator
    Router: MailboxProcessor<Route>
    MaybeOrigin: MailboxProcessor<unit> option
    State: ChannelState
    CurrentLocalCommitmentTxNumber: uint64
}

module Channel =
    /// represents the user has than something wrong with this library
    let private RRApiE(e: APIError) =
        RResult.rbad(RBad.Object(e))

    /// Represents the error that something user can not control (e.g. peer has sent invalid msg).
    let private RRChannelE(ex: ChannelError) =
        RResult.rexn(ChannelException(ex))

    let private RRClose (msg: string) =
        RRChannelE(ChannelError.Close(msg))


    let deriveOurDustLimitSatoshis(feeEstimator: IFeeEstimator): Money =
        let (FeeRatePerKw atOpenBackGroundFee) = feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Background)
        (Money.Satoshis((uint64 atOpenBackGroundFee) * B_OUTPUT_PLUS_SPENDING_INPUT_WEIGHT / 1000UL), Money.Satoshis(546UL))
        |> Money.Max

    let getOurChannelReserve (channelValue: Money) =
        let q = channelValue / 100L
        Money.Min(channelValue, Money.Max(q, Money.Satoshis(1L)))

    let buildLocalCommitmentSecret (n) =
        failwith "not impl"

    module internal Validation =
        module OpenChannelRequest =
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

            let checkMaxAcceptedHTLCsInMearningfulRange (msg: OpenChannel) =
                if (msg.MaxAcceptedHTLCs < 1us) then
                    RRClose("max_accepted_htlcs must be not 0")
                else if (msg.MaxAcceptedHTLCs < 483us) then
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
                let ourDustLimit = deriveOurDustLimitSatoshis feeEstimator
                let ourChannelReserve = getOurChannelReserve (msg.FundingSatoshis)
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
                    let ourChannelReserve = getOurChannelReserve msg.FundingSatoshis
                    let toLocalMSat = msg.PushMSat
                    let toRemoteMSat = fundersAmount - backgroundFeeRate.ToFee(COMMITMENT_TX_BASE_WEIGHT).ToLNMoney()
                    if (toLocalMSat <= (msg.ChannelReserveSatoshis.ToLNMoney()) && toRemoteMSat <= ourChannelReserve.ToLNMoney()) then
                        RRClose("Insufficient funding amount for iniial commitment. ")
                    else
                        Good ()


        let checkOpenChannelMsgAcceptable cs (msg: OpenChannel) =
            OpenChannelRequest.checkFundingSatoshisLessThanMax msg
            *> OpenChannelRequest.checkChannelReserveSatohisLessThanFundingSatoshis msg
            *> OpenChannelRequest.checkPushMSatLesserThanFundingValue msg
            *> OpenChannelRequest.checkFundingSatoshisLessThanDustLimitSatoshis msg
            *> OpenChannelRequest.checkRemoteFee cs.FeeEstimator msg.FeeRatePerKw
            *> OpenChannelRequest.checkToSelfDelayIsInAcceptableRange msg
            *> OpenChannelRequest.checkMaxAcceptedHTLCsInMearningfulRange msg
            *> OpenChannelRequest.checkConfigPermits cs.Config.PeerChannelConfigLimits msg
            *> OpenChannelRequest.checkChannelAnnouncementPreferenceAcceptable cs.Config msg
            *> OpenChannelRequest.checkIsAccpetableByCurrentFeeRate cs.FeeEstimator msg

    open Events
    let executeCommand (cs: Channel) (command: ChannelCommand): RResult<ChannelEvent list> =
        match cs.State, command with
        | WaitForOpenChannel state, OpenChannel msg ->
            Validation.checkOpenChannelMsgAcceptable cs msg
            >>= fun _ ->
                let localParams = state.InitFundee.LocalParams
                let localCommitmentSecret = buildLocalCommitmentSecret ()
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

                let remoteParams = { RemoteParams.NodeId = cs.RemoteNodeId
                                     DustLimitSatoshis = msg.DustLimitSatoshis
                                     MaxHTLCValueInFlightMSat = msg.MaxHTLCValueInFlightMsat
                                     ChannelReserveSatoshis = msg.ChannelReserveSatoshis
                                     HTLCMinimumMSat = msg.HTLCMinimumMsat
                                     ToSelfDelay = msg.ToSelfDelay
                                     MaxAcceptedHTLCs = msg.MaxAcceptedHTLCs
                                     FundingPubKey = msg.FundingPubKey
                                     RevocationBasePoint = msg.RevocationBasepoint
                                     DelayedPaymentBasePoint = msg.DelayedPaymentBasepoint
                                     HTLCBasePoint = msg.HTLCBasepoint
                                     GlobalFeatures = state.InitFundee.RemoteInit.GlobalFeatures
                                     LocalFeatures = state.InitFundee.RemoteInit.LocalFeatures }
                let data = Data.WaitForFundingCreatedData.Create localParams remoteParams msg acceptChannel
                ([WeAcceptedOpenChannel (acceptChannel, data) ] |> Good)
        | WaitForOpenChannel state, Close spk
        | WaitForOpenChannel state, DisconnectPeer -> [ChannelEvent.Closed] |> Good
        | WaitForOpenChannel  state, ChannelCommand.Error -> 
        | _ -> failwith ""

    let applyEvent c (e: ChannelEvent): Channel=
        match e with
        | WeAcceptedOpenChannel (acceptChannel, data) ->
            let state = WaitForFundingCreated data
            { c with State = state }