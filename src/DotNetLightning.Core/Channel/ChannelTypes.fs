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

type ClosingSignedResponse =
    | NewClosingSigned of ClosingSignedMsg
    | MutualClose of FinalizedTx * Option<ClosingSignedMsg>

type SavedChannelState = {
    StaticChannelConfig: StaticChannelConfig
    RemotePerCommitmentSecrets: PerCommitmentSecretStore
    ShortChannelId: Option<ShortChannelId>
}

