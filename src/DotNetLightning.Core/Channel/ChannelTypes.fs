namespace DotNetLightning.Channel
open DotNetLightning.Utils
open DotNetLightning.Utils.Aether
open DotNetLightning.DomainUtils.Types
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Transactions
open NBitcoin

(*
    based on eclair's channel state management
*)

//    8888888b.        d8888 88888888888     d8888
//    888  "Y88b      d88888     888        d88888
//    888    888     d88P888     888       d88P888
//    888    888    d88P 888     888      d88P 888
//    888    888   d88P  888     888     d88P  888
//    888    888  d88P   888     888    d88P   888
//    888  .d88P d8888888888     888   d8888888888
//    8888888P" d88P     888     888  d88P     888

[<AutoOpen>]
module Data =
    type ClosingTxProposed = {
        UnsignedTx: ClosingTx
        LocalClosingSigned: ClosingSigned
    }
    with
        static member LocalClosingSigned_: Lens<_ ,_> =
            (fun p -> p.LocalClosingSigned),
            (fun v p -> { p with LocalClosingSigned = v })

    type LocalCommitPublished = {
        CommitTx: CommitTx
        ClaimMainDelayedOutputTx: ClaimDelayedOutputTx option
        HTLCSuccessTxs: HTLCSuccessTx list
        HTLCTimeoutTxs: HTLCTimeoutTx list
    }

    type RemoteCommitPublished = {
        CommitTx: CommitTx
        ClaimMainOutputTx: ClaimP2WPKHOutputTx
        ClaimHTLCSuccessTxs: ClaimHTLCSuccessTx list
        ClaimHTLCTimeoutTxs: ClaimHTLCTimeoutTx list
    }

    type RevokedCommitPublished = {
        CommitTx: CommitTx
        ClaimMainOutputTx: ClaimP2WPKHOutputTx option
        MainPenaltyTx: MainPenaltyTx option
        ClaimHTLCTimeoutTxs: ClaimHTLCTimeoutTx list
        HTLCTimeoutTxs: HTLCTimeoutTx list
        HTLCPenaltyTxs: HTLCPenaltyTx list
    }

    type IChannelStateData = interface inherit IStateData end
    type IHasCommitments =
        inherit IChannelStateData
        abstract member ChannelId: ChannelId
        abstract member Commitments: Commitments


    type WaitForOpenChannelData = { InitFundee: InputInitFundee }
        with interface IChannelStateData

    type WaitForAcceptChannelData = {
            InputInitFunder: InputInitFunder;
            LastSent: OpenChannel
        }
        with interface IChannelStateData

    type WaitForFundingInternalData = {
                                            TemporaryChannelId: ChannelId
                                            LocalParams: LocalParams
                                            RemoteParams:RemoteParams
                                            FundingSatoshis: Money
                                            PushMsat: LNMoney
                                            InitialFeeRatePerKw: FeeRatePerKw
                                            RemoteFirstPerCommitmentPoint: PubKey
                                            LastSent: OpenChannel
                                        }
        with interface IChannelStateData

    type WaitForFundingCreatedData = {
                                            TemporaryFailure: ChannelId
                                            LocalParams: LocalParams
                                            RemoteParams: RemoteParams
                                            FundingSatoshis: Money
                                            PushMSat: LNMoney
                                            InitialFeeRatePerKw: FeeRatePerKw
                                            RemoteFirstPerCommitmentPoint: PubKey
                                            ChannelFlags: uint8
                                            LastSent: AcceptChannel
                                      }
        with
            interface IChannelStateData
            static member Create (localParams) (remoteParams) (msg: OpenChannel) acceptChannel =
                { ChannelFlags = msg.ChannelFlags
                  TemporaryFailure = msg.TemporaryChannelId
                  LocalParams = localParams
                  RemoteParams = remoteParams
                  FundingSatoshis = msg.FundingSatoshis
                  PushMSat = msg.PushMSat
                  InitialFeeRatePerKw = msg.FeeRatePerKw
                  RemoteFirstPerCommitmentPoint = msg.FirstPerCommitmentPoint
                  LastSent = acceptChannel }
    type WaitForFundingSignedData = {
                                            ChannelId: ChannelId
                                            LocalParams: LocalParams
                                            RemoteParams: RemoteParams
                                            FundingTx: FinalizedTx
                                            LocalSpec: CommitmentSpec
                                            LocalCommitTx: CommitTx
                                            RemoteCommit: RemoteCommit
                                            ChannelFlags: uint8
                                            LastSent: FundingCreated
                                            InitialFeeRatePerKw: FeeRatePerKw
                                       }
        with interface IChannelStateData

    type WaitForFundingConfirmedData = {
                                            Commitments: Commitments
                                            Deferred: FundingLocked option
                                            LastSent: Choice<FundingCreated, FundingSigned>
                                            InitialFeeRatePerKw: FeeRatePerKw
                                            ChannelId: ChannelId
                                          }
        with
            interface IHasCommitments with
                member this.ChannelId: ChannelId = 
                    this.ChannelId
                member this.Commitments: Commitments = 
                    this.Commitments


    type WaitForFundingLockedData = { Commitments: Commitments;
                                      ShortChannelId: ShortChannelId;
                                      OurMessage: FundingLocked
                                      TheirMessage: FundingLocked option
                                      InitialFeeRatePerKw: FeeRatePerKw
                                      HaveWeSentFundingLocked:bool
                                      ChannelId: ChannelId }
        with interface IHasCommitments with
                member this.ChannelId: ChannelId = 
                    this.ChannelId
                member this.Commitments: Commitments = 
                    this.Commitments

    type NormalData =   {
                            Commitments: Commitments;
                            ShortChannelId: ShortChannelId;
                            Buried: bool;
                            ChannelAnnouncement: ChannelAnnouncement option
                            ChannelUpdate: ChannelUpdate
                            LocalShutdown: Shutdown option
                            RemoteShutdown: Shutdown option
                            ChannelId: ChannelId
                        }
        with
            static member Commitments_: Lens<_, _> =
                (fun nd -> nd.Commitments), (fun v nd -> { nd with Commitments = v })

            interface IHasCommitments with
                member this.ChannelId: ChannelId = 
                    this.ChannelId
                member this.Commitments: Commitments = 
                    this.Commitments

    type ShutdownData = { Commitments: Commitments; LocalShutdown: Shutdown; RemoteShutdown: Shutdown; ChannelId: ChannelId }
        with interface IHasCommitments with
                member this.ChannelId: ChannelId = 
                    this.ChannelId
                member this.Commitments: Commitments = 
                    this.Commitments

    type NegotiatingData = {
                            Commitments: Commitments;
                            LocalShutdown: Shutdown;
                            RemoteShutdown: Shutdown;
                            ClosingTxProposed: ClosingTxProposed list list
                            MaybeBestUnpublishedTx: FinalizedTx option
                            ChannelId: ChannelId
                          }
        with
            interface IHasCommitments with
                member this.ChannelId: ChannelId = 
                    this.ChannelId
                member this.Commitments: Commitments = 
                    this.Commitments

    type ClosingData = internal {
                        ChannelId: ChannelId
                        Commitments: Commitments
                        MaybeFundingTx: Transaction option
                        WaitingSince: System.DateTime
                        MutualCloseProposed: ClosingTx list
                        MutualClosePublished: FinalizedTx list
                        LocalCommitPublished: LocalCommitPublished option
                        RemoteCommitPublished: RemoteCommitPublished option
                        NextRemoteCommitPublished: RemoteCommitPublished option
                        FutureRemoteCommitPublished: RemoteCommitPublished option
                        RevokedCommitPublished: RevokedCommitPublished list
                      }
        with
            interface IHasCommitments with
                member this.ChannelId: ChannelId = 
                    this.ChannelId
                member this.Commitments: Commitments = 
                    this.Commitments
            static member Create(channelId, commitments, maybeFundingTx, waitingSince, mutualCloseProposed, ?mutualClosePublished) =
                {
                    ChannelId = channelId
                    Commitments = commitments
                    MaybeFundingTx = maybeFundingTx
                    WaitingSince = waitingSince
                    MutualCloseProposed = mutualCloseProposed
                    MutualClosePublished = defaultArg mutualClosePublished []
                    LocalCommitPublished = None
                    RemoteCommitPublished = None
                    NextRemoteCommitPublished = None
                    FutureRemoteCommitPublished = None
                    RevokedCommitPublished = []
                }

    type WaitForRemotePublishFutureCommitmentData = {
                                                    Commitments: Commitments;
                                                    RemoteChannelReestablish: ChannelReestablish
                                                    ChannelId: ChannelId
                                                   }
        with interface IHasCommitments with
                member this.ChannelId: ChannelId = 
                    this.ChannelId
                member this.Commitments: Commitments = 
                    this.Commitments

//     8888888888 888     888 8888888888 888b    888 88888888888 .d8888b.
//     888        888     888 888        8888b   888     888    d88P  Y88b
//     888        888     888 888        88888b  888     888    Y88b.
//     8888888    Y88b   d88P 8888888    888Y88b 888     888     "Y888b.
//     888         Y88b d88P  888        888 Y88b888     888        "Y88b.
//     888          Y88o88P   888        888  Y88888     888          "888
//     888           Y888P    888        888   Y8888     888    Y88b  d88P
//     8888888888     Y8P     8888888888 888    Y888     888     "Y8888P"


/// The one include `CMD` in its name is the event which we are the initiator
type ChannelEvent =
    // --- ln events ---
    // --------- init fundee --------
    | NewInboundChannelStarted of nextState: Data.WaitForOpenChannelData
    | WeAcceptedOpenChannel of nextMsg: AcceptChannel * nextState: Data.WaitForFundingCreatedData
    | WeAcceptedFundingCreated of nextMsg: FundingSigned * nextState: Data.WaitForFundingConfirmedData

    // --------- init fender --------
    | NewOutboundChannelStarted of nextMsg: OpenChannel * nextState: Data.WaitForAcceptChannelData
    | WeAcceptedAcceptChannel of nextMsg: FundingCreated * nextState: Data.WaitForFundingSignedData
    | WeAcceptedFundingSigned of txToPublish: FinalizedTx * nextState: Data.WaitForFundingConfirmedData

    /// -------- init both -----
    | FundingConfirmed of nextState: Data.WaitForFundingLockedData
    | TheySentFundingLocked of msg: FundingLocked
    | WeSentFundingLocked of msg: FundingLocked
    | WeResumedDelayedFundingLocked of msg: FundingLocked
    | BothFundingLocked of nextState: Data.NormalData

    // -------- normal operation ------
    | WeAcceptedCMDAddHTLC of msg: UpdateAddHTLC * newCommitments: Commitments
    | WeAcceptedUpdateAddHTLC of newCommitments: Commitments

    | WeAcceptedCMDFulfillHTLC of msg: UpdateFulfillHTLC * newCommitments: Commitments
    | WeAcceptedFulfillHTLC of msg: UpdateFulfillHTLC * origin: HTLCSource * htlc: UpdateAddHTLC * newCommitments: Commitments

    | WeAcceptedCMDFailHTLC of msg: UpdateFailHTLC * newCommitments: Commitments
    | WeAcceptedFailHTLC of origin: HTLCSource * msg: UpdateAddHTLC * nextCommitments: Commitments

    | WeAcceptedCMDFailMalformedHTLC of msg: UpdateFailMalformedHTLC * newCommitments: Commitments
    | WeAcceptedFailMalformedHTLC of origin: HTLCSource * msg: UpdateAddHTLC * newCommitments: Commitments

    | WeAcceptedCMDUpdateFee of msg: UpdateFee  * nextCommitments: Commitments
    | WeAcceptedUpdateFee of msg: UpdateFee 

    | WeAcceptedCMDSign of msg: CommitmentSigned * nextCommitments: Commitments
    | WeAcceptedCommitmentSigned of msg: RevokeAndACK * nextCommitments: Commitments

    | WeAcceptedRevokeAndACK of nextCommitments: Commitments

    | AcceptedShutdownCMD of msg: Shutdown
    | AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs of remoteShutdown: Shutdown * nextCommitments: Commitments
    /// We have to send closing_signed to initiate the negotiation only when if we are the funder
    | AcceptedShutdownWhenNoPendingHTLCs of msgToSend: ClosingSigned option * nextState: NegotiatingData
    | AcceptedShutdownWhenWeHavePendingHTLCs of nextState: ShutdownData

    // ------ closing ------
    | MutualClosePerformed of nextState : ClosingData
    | WeProposedNewClosingSigned of msgToSend: ClosingSigned * nextState: NegotiatingData
    // -------- else ---------
    | Closed
    | Disconnected
    | ChannelStateRequestedSignCommitment
    | WeReplyToChannelReestablish of msg: ChannelReestablish


//      .d8888b. 88888888888     d8888 88888888888 8888888888 .d8888b.
//     d88P  Y88b    888        d88888     888     888       d88P  Y88b
//     Y88b.         888       d88P888     888     888       Y88b.
//      "Y888b.      888      d88P 888     888     8888888    "Y888b.
//         "Y88b.    888     d88P  888     888     888           "Y88b.
//           "888    888    d88P   888     888     888             "888
//     Y88b  d88P    888   d8888888888     888     888       Y88b  d88P
//      "Y8888P"     888  d88P     888     888     8888888888 "Y8888P"
    // --- setup ---

open Data
type ChannelState =
    /// Establishing
    | WaitForInitInternal
    | WaitForOpenChannel of WaitForOpenChannelData
    | WaitForAcceptChannel of WaitForAcceptChannelData
    | WaitForFundingCreated of WaitForFundingCreatedData
    | WaitForFundingSigned of WaitForFundingSignedData
    | WaitForFundingConfirmed of WaitForFundingConfirmedData
    | WaitForFundingLocked of WaitForFundingLockedData

    /// normal
    | Normal of NormalData

    /// Closing
    | Shutdown of ShutdownData
    | Negotiating of NegotiatingData
    | Closing of ClosingData
    | Closed of IChannelStateData

    /// Abnormal
    | Offline of IChannelStateData
    | Syncing of IChannelStateData
    | WaitForRemotePublishFutureCommitment of WaitForRemotePublishFutureCommitmentData

    /// Error
    | ErrFundingLost of IChannelStateData
    | ErrFundingTimeOut of IChannelStateData
    | ErrInformationLeak of IChannelStateData
    with
        interface IState 

        static member Zero = WaitForInitInternal
        static member Normal_: Prism<_, _> =
            (fun cc -> match cc with
                       | Normal s -> Some s
                       | _ -> None ),
            (fun v cc -> match cc with
                         | Normal _ -> Normal v
                         | _ -> cc )
            
            
[<StructuredFormatDisplay("{ToString}")>]
type MessageValidationError =
    | FeeDeltaTooHigh of actual: float * maxAccepted: float
    with
        override this.ToString() =
            match this with
            | FeeDeltaTooHigh (actualDelta, maxAccepted) ->
                sprintf "delta is %.2f%% . But it must be lower than %.2f%%" (actualDelta * 100.0) (maxAccepted * 100.0)
