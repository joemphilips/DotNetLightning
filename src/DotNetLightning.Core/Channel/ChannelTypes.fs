namespace DotNetLightning.Channel

open DotNetLightning.Chain
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

    type WaitForFundingConfirmedData = {
        Deferred: Option<FundingLockedMsg>
        InitialFeeRatePerKw: FeeRatePerKw
    }

    type WaitForFundingLockedData = {
        ShortChannelId: ShortChannelId
        OurMessage: FundingLockedMsg
        TheirMessage: Option<FundingLockedMsg>
        InitialFeeRatePerKw: FeeRatePerKw
        HaveWeSentFundingLocked: bool
    }

    type NormalData = {
        ShortChannelId: ShortChannelId
        Buried: bool
        ChannelAnnouncement: Option<ChannelAnnouncementMsg>
        ChannelUpdate: ChannelUpdateMsg
        LocalShutdown: Option<ShutdownMsg>
        RemoteShutdown: Option<ShutdownMsg>
    }

    type ShutdownData = {
        LocalShutdown: ShutdownMsg
        RemoteShutdown: ShutdownMsg
    }

    type NegotiatingData = {
        LocalShutdown: ShutdownMsg
        RemoteShutdown: ShutdownMsg
        ClosingTxProposed: List<List<ClosingTxProposed>>
        MaybeBestUnpublishedTx: Option<FinalizedTx>
    }

    type ClosingData = {
        MaybeFundingTx: Option<Transaction>
        WaitingSince: System.DateTime
        MutualCloseProposed: List<ClosingTx>
        MutualClosePublished: FinalizedTx
        LocalCommitPublished: Option<LocalCommitPublished>
        RemoteCommitPublished: Option<RemoteCommitPublished>
        NextRemoteCommitPublished: Option<RemoteCommitPublished>
        FutureRemoteCommitPublished: Option<RemoteCommitPublished>
        RevokedCommitPublished: List<RevokedCommitPublished>
    } with
        member this.FinalizedTx =
            this.MutualClosePublished
        
        static member Create (maybeFundingTx: Option<Transaction>)
                             (waitingSince: System.DateTime)
                             (mutualCloseProposed: List<ClosingTx>)
                             (mutualClosePublished: FinalizedTx)
                                 : ClosingData = {
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
    /// -------- init both -----
    | FundingConfirmed of nextState: Data.WaitForFundingLockedData
    | TheySentFundingLocked of msg: FundingLockedMsg
    | WeSentFundingLocked of msg: FundingLockedMsg
    | WeResumedDelayedFundingLocked of msg: FundingLockedMsg
    | BothFundingLocked of nextState: Data.NormalData * nextCommitments: Commitments

    // -------- normal operation ------
    | WeAcceptedOperationAddHTLC of msg: UpdateAddHTLCMsg * newCommitments: Commitments
    | WeAcceptedUpdateAddHTLC of newCommitments: Commitments

    | WeAcceptedOperationFulfillHTLC of msg: UpdateFulfillHTLCMsg * newCommitments: Commitments
    | WeAcceptedFulfillHTLC of msg: UpdateFulfillHTLCMsg * origin: HTLCSource * htlc: UpdateAddHTLCMsg * newCommitments: Commitments

    | WeAcceptedOperationFailHTLC of msg: UpdateFailHTLCMsg * newCommitments: Commitments
    | WeAcceptedFailHTLC of origin: HTLCSource * msg: UpdateAddHTLCMsg * nextCommitments: Commitments

    | WeAcceptedOperationFailMalformedHTLC of msg: UpdateFailMalformedHTLCMsg * newCommitments: Commitments
    | WeAcceptedFailMalformedHTLC of origin: HTLCSource * msg: UpdateAddHTLCMsg * newCommitments: Commitments

    | WeAcceptedOperationUpdateFee of msg: UpdateFeeMsg  * nextCommitments: Commitments
    | WeAcceptedUpdateFee of msg: UpdateFeeMsg * newCommitments: Commitments

    | WeAcceptedOperationSign of msg: CommitmentSignedMsg * nextCommitments: Commitments
    | WeAcceptedCommitmentSigned of msg: RevokeAndACKMsg * nextCommitments: Commitments

    | WeAcceptedRevokeAndACK of nextCommitments: Commitments

    | AcceptedOperationShutdown of msg: ShutdownMsg
    | AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs of remoteShutdown: ShutdownMsg * nextCommitments: Commitments
    /// We have to send closing_signed to initiate the negotiation only when if we are the funder
    | AcceptedShutdownWhenNoPendingHTLCs of msgToSend: ClosingSignedMsg option * nextState: NegotiatingData
    | AcceptedShutdownWhenWeHavePendingHTLCs of nextState: ShutdownData

    // ------ closing ------
    | MutualClosePerformed of txToPublish: FinalizedTx * nextState : ClosingData * nextMsgToSend: Option<ClosingSignedMsg>
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
type ChannelState =
    /// Establishing
    | WaitForFundingConfirmed of WaitForFundingConfirmedData
    | WaitForFundingLocked of WaitForFundingLockedData

    /// normal
    | Normal of NormalData

    /// Closing
    | Shutdown of ShutdownData
    | Negotiating of NegotiatingData
    | Closing of ClosingData
    with
        interface IState 

        static member Normal_: Prism<_, _> =
            (fun cc -> match cc with
                       | Normal s -> Some s
                       | _ -> None ),
            (fun v cc -> match cc with
                         | Normal _ -> Normal v
                         | _ -> cc )
        member this.Phase =
            match this with
            | WaitForFundingConfirmed _
            | WaitForFundingLocked _ -> Opening
            | Normal _ -> ChannelStatePhase.Normal
            | Shutdown _
            | Negotiating _
            | Closing _ -> ChannelStatePhase.Closing
