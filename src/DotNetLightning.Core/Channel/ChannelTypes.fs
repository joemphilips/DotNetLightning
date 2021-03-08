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
    type IChannelStateData = interface inherit IStateData end

    type NegotiatingState = {
        LocalRequestedShutdown: Option<ShutdownScriptPubKey>
        RemoteRequestedShutdown: Option<ShutdownScriptPubKey>
        LocalClosingFeesProposed: List<Money>
        RemoteClosingFeeProposed: Option<Money * LNECDSASignature>
    } with
        static member New(): NegotiatingState = {
            LocalRequestedShutdown = None
            RemoteRequestedShutdown = None
            LocalClosingFeesProposed = List.empty
            RemoteClosingFeeProposed = None
        }
        member self.HasEnteredShutdown(): bool =
            self.LocalRequestedShutdown.IsSome && self.RemoteRequestedShutdown.IsSome


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
    // -------- normal operation ------
    | WeAcceptedRevokeAndACK of nextCommitments: Commitments * remoteNextPerCommitmentPoint: PerCommitmentPoint

    | AcceptedOperationShutdown of msg: ShutdownMsg * localShutdownScriptPubKey: ShutdownScriptPubKey
    | AcceptedShutdownWhileWeHaveUnsignedOutgoingHTLCs of remoteShutdownScriptPubKey: ShutdownScriptPubKey
    /// We have to send closing_signed to initiate the negotiation only when if we are the funder
    | AcceptedShutdownWhenNoPendingHTLCs of msgToSend: ClosingSignedMsg option * nextState: NegotiatingState
    | AcceptedShutdownWhenWeHavePendingHTLCs of localShutdown: ShutdownMsg * localShutdownScript: ShutdownScriptPubKey * remoteShutdownScript: ShutdownScriptPubKey

    // ------ closing ------
    | MutualClosePerformed of txToPublish: FinalizedTx * nextMsgToSend: Option<ClosingSignedMsg>
    | WeProposedNewClosingSigned of msgToSend: ClosingSignedMsg * nextState: NegotiatingState
    // -------- else ---------
    | Closed
    | Disconnected
    | ChannelStateRequestedSignCommitment

