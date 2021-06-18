namespace DotNetLightning.Channel

open DotNetLightning.Chain
open DotNetLightning.Utils
open DotNetLightning.Utils.Aether
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
        member this.HasEnteredShutdown(): bool =
            this.LocalRequestedShutdown.IsSome && this.RemoteRequestedShutdown.IsSome

type ClosingSignedResponse =
    | NewClosingSigned of ClosingSignedMsg
    | MutualClose of FinalizedTx * Option<ClosingSignedMsg>

type SavedChannelState = {
    StaticChannelConfig: StaticChannelConfig
    RemotePerCommitmentSecrets: PerCommitmentSecretStore
    ShortChannelId: Option<ShortChannelId>
    LocalCommit: LocalCommit
    RemoteCommit: RemoteCommit
    LocalChanges: LocalChanges
    RemoteChanges: RemoteChanges
} with
    member internal this.HasNoPendingHTLCs (remoteNextCommitInfo: RemoteNextCommitInfo) =
        this.LocalCommit.Spec.OutgoingHTLCs.IsEmpty
        && this.LocalCommit.Spec.IncomingHTLCs.IsEmpty
        && this.RemoteCommit.Spec.OutgoingHTLCs.IsEmpty
        && this.RemoteCommit.Spec.IncomingHTLCs.IsEmpty
        && (remoteNextCommitInfo |> function Waiting _ -> false | Revoked _ -> true)

    member internal this.GetOutgoingHTLCCrossSigned (remoteNextCommitInfo: RemoteNextCommitInfo)
                                                    (htlcId: HTLCId)
                                                        : Option<UpdateAddHTLCMsg> =
        let remoteSigned =
            Map.tryFind htlcId this.LocalCommit.Spec.OutgoingHTLCs
        let localSigned =
            let remoteCommit =
                match remoteNextCommitInfo with
                | Revoked _ -> this.RemoteCommit
                | Waiting nextRemoteCommit -> nextRemoteCommit
            Map.tryFind htlcId remoteCommit.Spec.IncomingHTLCs
        match remoteSigned, localSigned with
        | Some _, Some htlcIn -> htlcIn |> Some
        | _ -> None

    member internal this.GetIncomingHTLCCrossSigned (remoteNextCommitInfo: RemoteNextCommitInfo)
                                                    (htlcId: HTLCId)
                                                        : Option<UpdateAddHTLCMsg> =
        let remoteSigned =
            Map.tryFind htlcId this.LocalCommit.Spec.IncomingHTLCs
        let localSigned =
            let remoteCommit =
                match remoteNextCommitInfo with
                | Revoked _ -> this.RemoteCommit
                | Waiting nextRemoteCommit -> nextRemoteCommit
            Map.tryFind htlcId remoteCommit.Spec.OutgoingHTLCs
        match remoteSigned, localSigned with
        | Some _, Some htlcIn -> htlcIn |> Some
        | _ -> None

