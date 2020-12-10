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

    type ShutdownState = {
        ShutdownScriptPubKey: ShutdownScriptPubKey
        HasRequestedShutdown: bool
    } with
        static member ForNewChannel (shutdownScriptPubKeyOpt: Option<ShutdownScriptPubKey>)
                                        : Option<ShutdownState> =
            match shutdownScriptPubKeyOpt with
            | None -> None
            | Some shutdownScriptPubKey -> Some {
                ShutdownScriptPubKey = shutdownScriptPubKey
                HasRequestedShutdown = false
            }

    type NormalData = {
        ShortChannelId: Option<ShortChannelId>
        LocalShutdownState: Option<ShutdownState>
        RemoteShutdownState: Option<ShutdownState>
        RemoteNextCommitInfo: Option<RemoteNextCommitInfo>
    } with
        member self.HasEnteredShutdown(): bool =
            let localEnteredShutdown =
                match self.LocalShutdownState with
                | None -> false
                | Some shutdownState -> shutdownState.HasRequestedShutdown
            let remoteEnteredShutdown =
                match self.RemoteShutdownState with
                | None -> false
                | Some shutdownState -> shutdownState.HasRequestedShutdown
            localEnteredShutdown || remoteEnteredShutdown

    type NegotiatingData = {
        RemoteNextCommitInfo: Option<RemoteNextCommitInfo>
        LocalShutdown: ShutdownScriptPubKey
        RemoteShutdown: ShutdownScriptPubKey
        ClosingTxProposed: List<List<ClosingTxProposed>>
        MaybeBestUnpublishedTx: Option<FinalizedTx>
    }

    type ClosingData = {
        RemoteNextCommitInfo: Option<RemoteNextCommitInfo>
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
                             (remoteNextCommitInfo: Option<RemoteNextCommitInfo>)
                                 : ClosingData = {
            RemoteNextCommitInfo= remoteNextCommitInfo
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
    | FundingConfirmed of FundingLockedMsg * shortChannelId: ShortChannelId
    | TheySentFundingLocked of msg: FundingLockedMsg
    | WeResumedDelayedFundingLocked of remoteNextPerCommitmentPoint: PerCommitmentPoint
    | BothFundingLocked of nextState: Data.NormalData

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

    | WeAcceptedOperationSign of msg: CommitmentSignedMsg * nextCommitments: Commitments * nextRemoteCommit: RemoteCommit
    | WeAcceptedCommitmentSigned of msg: RevokeAndACKMsg * nextCommitments: Commitments

    | WeAcceptedRevokeAndACK of nextCommitments: Commitments * remoteNextPerCommitmentPoint: PerCommitmentPoint

    | AcceptedOperationShutdown of msg: ShutdownMsg * nextLocalShutdownState: ShutdownState
    | AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs of nextRemoteShutdownState: ShutdownState
    /// We have to send closing_signed to initiate the negotiation only when if we are the funder
    | AcceptedShutdownWhenNoPendingHTLCs of msgToSend: ClosingSignedMsg option * nextState: NegotiatingData
    | AcceptedShutdownWhenWeHavePendingHTLCs of localShutdown: ShutdownMsg * nextState: NormalData

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
    /// normal
    | Normal of NormalData

    /// Closing
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
            | Normal normalData ->
                if normalData.HasEnteredShutdown() then
                    ChannelStatePhase.Closing
                else
                    if normalData.ShortChannelId.IsNone || normalData.RemoteNextCommitInfo.IsNone then
                        ChannelStatePhase.Opening
                    else
                        ChannelStatePhase.Normal
            | Negotiating _
            | Closing _ -> ChannelStatePhase.Closing
