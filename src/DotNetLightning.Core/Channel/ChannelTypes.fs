namespace DotNetLightning.Channel

open DotNetLightning.Utils
open DotNetLightning.Utils.Aether
open DotNetLightning.DomainUtils.Types
open DotNetLightning.Serialization.Msgs
open DotNetLightning.Transactions
open DotNetLightning.Crypto
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
        LocalClosingSigned: ClosingSignedMsg
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
            LastSent: OpenChannelMsg
        }
        with interface IChannelStateData

    type WaitForFundingInternalData = {
                                            TemporaryChannelId: ChannelId
                                            LocalParams: LocalParams
                                            RemoteParams:RemoteParams
                                            FundingSatoshis: Money
                                            PushMsat: LNMoney
                                            InitialFeeRatePerKw: FeeRatePerKw
                                            RemoteFirstPerCommitmentPoint: PerCommitmentPoint
                                            LastSent: OpenChannelMsg
                                        }
        with interface IChannelStateData

    type WaitForFundingCreatedData = {
                                            TemporaryFailure: ChannelId
                                            LocalParams: LocalParams
                                            RemoteParams: RemoteParams
                                            FundingSatoshis: Money
                                            PushMSat: LNMoney
                                            InitialFeeRatePerKw: FeeRatePerKw
                                            RemoteFirstPerCommitmentPoint: PerCommitmentPoint
                                            ChannelFlags: uint8
                                            LastSent: AcceptChannelMsg
                                      }
        with
            interface IChannelStateData
            static member Create (localParams) (remoteParams) (msg: OpenChannelMsg) acceptChannelMsg =
                { ChannelFlags = msg.ChannelFlags
                  TemporaryFailure = msg.TemporaryChannelId
                  LocalParams = localParams
                  RemoteParams = remoteParams
                  FundingSatoshis = msg.FundingSatoshis
                  PushMSat = msg.PushMSat
                  InitialFeeRatePerKw = msg.FeeRatePerKw
                  RemoteFirstPerCommitmentPoint = msg.FirstPerCommitmentPoint
                  LastSent = acceptChannelMsg }
    type WaitForFundingSignedData = {
                                            ChannelId: ChannelId
                                            LocalParams: LocalParams
                                            RemoteParams: RemoteParams
                                            FundingTx: FinalizedTx
                                            LocalSpec: CommitmentSpec
                                            LocalCommitTx: CommitTx
                                            RemoteCommit: RemoteCommit
                                            ChannelFlags: uint8
                                            LastSent: FundingCreatedMsg
                                            InitialFeeRatePerKw: FeeRatePerKw
                                       }
        with interface IChannelStateData

    type WaitForFundingConfirmedData = {
                                            Commitments: Commitments
                                            Deferred: FundingLockedMsg option
                                            LastSent: Choice<FundingCreatedMsg, FundingSignedMsg>
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
                                      OurMessage: FundingLockedMsg
                                      TheirMessage: FundingLockedMsg option
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
                            ChannelAnnouncement: ChannelAnnouncementMsg option
                            ChannelUpdate: ChannelUpdateMsg
                            LocalShutdown: ShutdownMsg option
                            RemoteShutdown: ShutdownMsg option
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

    type ShutdownData = { Commitments: Commitments; LocalShutdown: ShutdownMsg; RemoteShutdown: ShutdownMsg; ChannelId: ChannelId }
        with interface IHasCommitments with
                member this.ChannelId: ChannelId = 
                    this.ChannelId
                member this.Commitments: Commitments = 
                    this.Commitments

    type NegotiatingData = {
                            Commitments: Commitments;
                            LocalShutdown: ShutdownMsg;
                            RemoteShutdown: ShutdownMsg;
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

    type ClosingData = {
                        ChannelId: ChannelId
                        Commitments: Commitments
                        MaybeFundingTx: Transaction option
                        WaitingSince: System.DateTime
                        MutualCloseProposed: ClosingTx list
                        MutualClosePublished: FinalizedTx
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
                    
            member this.FinalizedTx =
                this.MutualClosePublished
            static member Create(channelId, commitments, maybeFundingTx, waitingSince, mutualCloseProposed, mutualClosePublished) =
                {
                    ChannelId = channelId
                    Commitments = commitments
                    MaybeFundingTx = maybeFundingTx
                    WaitingSince = waitingSince
                    MutualCloseProposed = mutualCloseProposed
                    MutualClosePublished = mutualClosePublished
                    LocalCommitPublished = None
                    RemoteCommitPublished = None
                    NextRemoteCommitPublished = None
                    FutureRemoteCommitPublished = None
                    RevokedCommitPublished = []
                }

    type WaitForRemotePublishFutureCommitmentData = {
                                                    Commitments: Commitments;
                                                    RemoteChannelReestablish: ChannelReestablishMsg
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


/// The one that includes `Operation` in its name is the event which we are the initiator
type ChannelEvent =
    // --- ln events ---
    // --------- init fundee --------
    | NewInboundChannelStarted of nextState: Data.WaitForOpenChannelData
    | WeAcceptedOpenChannel of nextMsg: AcceptChannelMsg * nextState: Data.WaitForFundingCreatedData
    | WeAcceptedFundingCreated of nextMsg: FundingSignedMsg * nextState: Data.WaitForFundingConfirmedData

    // --------- init fender --------
    | NewOutboundChannelStarted of nextMsg: OpenChannelMsg * nextState: Data.WaitForAcceptChannelData
    | WeAcceptedAcceptChannel of nextMsg: FundingCreatedMsg * nextState: Data.WaitForFundingSignedData
    | WeAcceptedFundingSigned of txToPublish: FinalizedTx * nextState: Data.WaitForFundingConfirmedData

    /// -------- init both -----
    | FundingConfirmed of nextState: Data.WaitForFundingLockedData
    | TheySentFundingLocked of msg: FundingLockedMsg
    | WeSentFundingLocked of msg: FundingLockedMsg
    | WeResumedDelayedFundingLocked of msg: FundingLockedMsg
    | BothFundingLocked of nextState: Data.NormalData

    // -------- normal operation ------
    | WeAcceptedOperationMonoHopUnidirectionalPayment of msg: MonoHopUnidirectionalPaymentMsg * newCommitments: Commitments
    | WeAcceptedMonoHopUnidirectionalPayment of newCommitments: Commitments

    | WeAcceptedOperationAddHTLC of msg: UpdateAddHTLCMsg * newCommitments: Commitments
    | WeAcceptedUpdateAddHTLC of newCommitments: Commitments

    | WeAcceptedOperationFulfillHTLC of msg: UpdateFulfillHTLCMsg * newCommitments: Commitments
    | WeAcceptedFulfillHTLC of msg: UpdateFulfillHTLCMsg * origin: HTLCSource * htlc: UpdateAddHTLCMsg * newCommitments: Commitments

    | WeAcceptedOperationFailHTLC of msg: UpdateFailHTLCMsg * newCommitments: Commitments
    | WeAcceptedFailHTLC of origin: HTLCSource * msg: UpdateAddHTLCMsg * nextCommitments: Commitments

    | WeAcceptedOperationFailMalformedHTLC of msg: UpdateFailMalformedHTLCMsg * newCommitments: Commitments
    | WeAcceptedFailMalformedHTLC of origin: HTLCSource * msg: UpdateAddHTLCMsg * newCommitments: Commitments

    | WeAcceptedOperationUpdateFee of msg: UpdateFeeMsg  * nextCommitments: Commitments
    | WeAcceptedUpdateFee of msg: UpdateFeeMsg 

    | WeAcceptedOperationSign of msg: CommitmentSignedMsg * nextCommitments: Commitments
    | WeAcceptedCommitmentSigned of msg: RevokeAndACKMsg * nextCommitments: Commitments

    | WeAcceptedRevokeAndACK of nextCommitments: Commitments

    | AcceptedOperationShutdown of msg: ShutdownMsg
    | AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs of remoteShutdown: ShutdownMsg * nextCommitments: Commitments
    /// We have to send closing_signed to initiate the negotiation only when if we are the funder
    | AcceptedShutdownWhenNoPendingHTLCs of msgToSend: ClosingSignedMsg option * nextState: NegotiatingData
    | AcceptedShutdownWhenWeHavePendingHTLCs of nextState: ShutdownData

    // ------ closing ------
    | MutualClosePerformed of txToPublish: FinalizedTx * nextState : ClosingData
    | WeProposedNewClosingSigned of msgToSend: ClosingSignedMsg * nextState: NegotiatingData
    // -------- else ---------
    | Closed
    | Disconnected
    | ChannelStateRequestedSignCommitment
    | WeSentChannelReestablish of msg: ChannelReestablishMsg


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
type ChannelStatePhase =
    | Opening
    | Normal
    | Closing
    | Closed
    | Abnormal
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
        member this.ChannelId: Option<ChannelId> =
            match this with
            | WaitForInitInternal
            | WaitForOpenChannel _
            | WaitForAcceptChannel _
            | WaitForFundingCreated _ -> None
            | WaitForFundingSigned data -> Some data.ChannelId
            | WaitForFundingConfirmed data -> Some data.ChannelId
            | WaitForFundingLocked data -> Some data.ChannelId
            | Normal data -> Some data.ChannelId
            | Shutdown data -> Some data.ChannelId
            | Negotiating data -> Some data.ChannelId
            | Closing data -> Some data.ChannelId
            | Closed _
            | Offline _
            | Syncing _
            | ErrFundingLost _
            | ErrFundingTimeOut _
            | ErrInformationLeak _ -> None

        member this.Phase =
            match this with
            | WaitForInitInternal
            | WaitForOpenChannel _ 
            | WaitForAcceptChannel _
            | WaitForFundingCreated _
            | WaitForFundingSigned _
            | WaitForFundingConfirmed _
            | WaitForFundingLocked _ -> Opening
            | Normal _ -> ChannelStatePhase.Normal
            | Shutdown _
            | Negotiating _
            | Closing _ -> ChannelStatePhase.Closing
            | Closed _ -> ChannelStatePhase.Closed
            | Offline _
            | Syncing _
            | ErrFundingLost _
            | ErrFundingTimeOut _
            | ErrInformationLeak _ -> Abnormal

        member this.Commitments: Option<Commitments> =
            match this with
            | WaitForInitInternal
            | WaitForOpenChannel _
            | WaitForAcceptChannel _
            | WaitForFundingCreated _
            | WaitForFundingSigned _ -> None
            | WaitForFundingConfirmed data -> Some (data :> IHasCommitments).Commitments
            | WaitForFundingLocked data -> Some (data :> IHasCommitments).Commitments
            | Normal data -> Some (data :> IHasCommitments).Commitments
            | Shutdown data -> Some (data :> IHasCommitments).Commitments
            | Negotiating data -> Some (data :> IHasCommitments).Commitments
            | Closing data -> Some (data :> IHasCommitments).Commitments
            | Closed _
            | Offline _
            | Syncing _
            | ErrFundingLost _
            | ErrFundingTimeOut _
            | ErrInformationLeak _ -> None

