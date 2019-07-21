namespace DotNetLightning.LN
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Utils.Error
open DotNetLightning.Chain
open DotNetLightning.DomainUtils.Types
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Transactions
open NBitcoin

(*
    based on eclair's channel state management
*)

type InputInitFunder = {
    TemporaryChannelId: ChannelId
    FundingSatoshis: Money
    PushMSat: LNMoney
    InitFeeRatePerKw: FeeRatePerKw
    FundingTxFeeRatePerKw: FeeRatePerKw
    LocalParams: LocalParams
    RemoteInit: Init
    ChannelFlags: uint8
    ChannelKeys: ChannelKeys
}
    with
        static member FromOpenChannel (localParams) (remoteInit) (channelKeys) (o: OpenChannel) =
            {
                InputInitFunder.TemporaryChannelId = o.TemporaryChannelId
                FundingSatoshis = o.FundingSatoshis
                PushMSat = o.PushMSat
                InitFeeRatePerKw = o.FeeRatePerKw
                FundingTxFeeRatePerKw = o.FeeRatePerKw
                LocalParams = localParams
                RemoteInit = remoteInit
                ChannelFlags = o.ChannelFlags
                ChannelKeys = channelKeys
            }

        member this.DeriveCommitmentSpec() =
            CommitmentSpec.Create this.ToLocal this.PushMSat this.FundingTxFeeRatePerKw

        member this.ToLocal =
            this.FundingSatoshis.ToLNMoney() - this.PushMSat

and InputInitFundee = {
    TemporaryChannelId: ChannelId
    LocalParams: LocalParams
    RemoteInit: Init
    ToLocal: LNMoney
    ChannelKeys: ChannelKeys
}


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
        unsignedTx: Transaction 
        LocalClosingSigned: ClosingSigned
    }

    type LocalCommitPublished = {
        CommitTx: CommitTx
        ClaimMainDelayedOutputTx: ClaimDelayedOutputTx option
        HTLCSuccessTxs: HTLCSuccessTx list
        HTLCTimeoutTxs: HTLCTimeoutTx list
        IrrevocablySpent: Map<OutPoint, byte[]>
    }

    type RemoteCommitPublished = {
        CommitTx: CommitTx
        ClaimMainOutputTx: ClaimP2WPKHOutputTx
        ClaimHTLCSuccessTxs: ClaimHTLCSuccessTx list
        ClaimHTLCTimeoutTxs: ClaimHTLCTimeoutTx list
        IrrevocablySpent: Map<OutPoint, byte[]>
    }

    type RevokedCommitPublished = {
        CommitTx: CommitTx
        ClaimMainOutputTx: ClaimP2WPKHOutputTx option
        MainPenaltyTx: MainPenaltyTx option
        ClaimHTLCTimeoutTxs: ClaimHTLCTimeoutTx list
        HTLCTimeoutTxs: HTLCTimeoutTx list
        HTLCPenaltyTxs: HTLCPenaltyTx list
        IrrevocalySpent: Map<OutPoint, byte[]>
    }

    type IChannelStateData = interface inherit IStateData end

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
                                          }
        with interface IChannelStateData

    type WaitForFundingLockedData = { Commitments: Commitments;
                                      ShortChannelId: ShortChannelId;
                                      OurMessage: FundingLocked
                                      TheirMessage: FundingLocked option
                                      InitialFeeRatePerKw: FeeRatePerKw
                                      HaveWeSentFundingLocked:bool }
        with interface IChannelStateData

    type NormalData =   {
                            Commitments: Commitments;
                            ShortChannelId: ShortChannelId;
                            Buried: bool;
                            ChannelAnnouncement: ChannelAnnouncement option
                            ChannelUpdate: ChannelUpdate
                            LocalShutdown: Shutdown option
                            RemoteShutdown: Shutdown option
                        }
        with interface IChannelStateData

    type ShutdownData = { Commitments: Commitments; LocalShutdown: Shutdown; RemoteShutdown: Shutdown }
        with interface IChannelStateData

    type NegotiatingData = {
                            Commitments: Commitments;
                            LocalShutdown: Shutdown;
                            RemoteShutdown: Shutdown;
                            ClosingTxProposed: ClosingTxProposed list list
                            MaybeBestUnpublishedTx: Transaction option
                          }
        with interface IChannelStateData

    type ClosingData = {
                        Commitments: Commitments
                        MutualCloseProposed: Transaction list
                        MutualClosePublished: Transaction list
                        LocalCommitPublished: LocalCommitPublished option
                      }
        with interface IChannelStateData

    type WaitForRemotePublishFutureCommitmentData = {
                                                    Commitments: Commitments;
                                                    RemoteChannelReestablish: ChannelReestablish
                                                   }
        with interface IChannelStateData

//     8888888888 888     888 8888888888 888b    888 88888888888 .d8888b.
//     888        888     888 888        8888b   888     888    d88P  Y88b
//     888        888     888 888        88888b  888     888    Y88b.
//     8888888    Y88b   d88P 8888888    888Y88b 888     888     "Y888b.
//     888         Y88b d88P  888        888 Y88b888     888        "Y88b.
//     888          Y88o88P   888        888  Y88888     888          "888
//     888           Y888P    888        888   Y8888     888    Y88b  d88P
//     8888888888     Y8P     8888888888 888    Y888     888     "Y8888P"


type ChannelEvent =
    // --- ln events ---
    // --------- init fundee --------
    | WeAcceptedOpenChannel of nextMsg: AcceptChannel * nextState: Data.WaitForFundingCreatedData
    | WeAcceptedFundingCreated of nextMsg: FundingSigned * nextState: Data.WaitForFundingConfirmedData

    // --------- init fender --------
    | WeAcceptedAcceptChannel of nextMsg: FundingCreated * nextState: Data.WaitForFundingSignedData
    | WeAcceptedFundingSigned of txToPublish: FinalizedTx * nextState: Data.WaitForFundingConfirmedData
    | OpenChannelFromSelf of InputInitFunder


    /// -------- init both -----
    | FundingConfirmed of nextState: Data.WaitForFundingLockedData
    | TheySentFundingLockedMsgBeforeUs of msg: FundingLocked
    | WeSentFundingLockedMsgBeforeThem of msg: FundingLocked
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

    // -------- else ---------
    | Closed
    | Disconnected
    | NewBlockVerified of height: BlockHeight


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

//       .d8888b.   .d88888b.  888b     d888 888b     d888        d8888 888b    888 8888888b.   .d8888b.
//      d88P  Y88b d88P" "Y88b 8888b   d8888 8888b   d8888       d88888 8888b   888 888  "Y88b d88P  Y88b
//      888    888 888     888 88888b.d88888 88888b.d88888      d88P888 88888b  888 888    888 Y88b.
//      888        888     888 888Y88888P888 888Y88888P888     d88P 888 888Y88b 888 888    888  "Y888b.
//      888        888     888 888 Y888P 888 888 Y888P 888    d88P  888 888 Y88b888 888    888     "Y88b.
//      888    888 888     888 888  Y8P  888 888  Y8P  888   d88P   888 888  Y88888 888    888       "888
//      Y88b  d88P Y88b. .d88P 888   "   888 888   "   888  d8888888888 888   Y8888 888  .d88P Y88b  d88P
//       "Y8888P"   "Y88888P"  888       888 888       888 d88P     888 888    Y888 8888888P"   "Y8888P"

type CMDAddHTLC = {
    AmountMSat: LNMoney
    PaymentHash: PaymentHash
    Expiry: BlockHeight
    Onion: OnionPacket
    Upstream: UpdateAddHTLC option
    Origin: HTLCSource option
}
    with
        static member Create amountMSat paymentHash expiry onion upstream commit origin =
            {
                AmountMSat = amountMSat
                PaymentHash = paymentHash
                Expiry = expiry
                Onion = onion
                Upstream = upstream
                Origin = origin
            }


type CMDFulfillHTLC = {
    Id: HTLCId
    PaymentPreimage: PaymentPreimage
    Commit: bool
}

type CMDFailHTLC = {
    Id: HTLCId
    Reason: Choice<byte[], FailureMsg>
}

type CMDFailMalformedHTLC = {
    Id: HTLCId
    Sha256OfOnion: uint256
    FailureCode: ErrorCode
}

type CMDUpdateFee = {
    FeeRatePerKw: FeeRatePerKw
}

type CMDClose = { ScriptPubKey: Script option }

type CMDResGetInfo = {
    NodeId: NodeId
    ChannelId: ChannelId
    State: IState
    Data: IChannelStateData
}


/// possible input to the channel. Command prefixed from `Apply` is passive. i.e.
/// it has caused by the outside world and not by the user. Mostly this is a message sent
/// from this channel's remote peer.
/// others are active commands which is caused by the user.
/// However, hese two kinds of command has no difference from architectural viewpoint.
/// It is just an input to the state.
type ChannelCommand =
    // open: funder
    | InputInitFunder of InputInitFunder
    | ApplyAcceptChannel of AcceptChannel
    | ApplyFundingSigned of FundingSigned
    | ApplyFundingLocked of FundingLocked
    | ApplyFundingConfirmedOnBC of height: BlockHeight * txIndex: TxIndexInBlock * depth: uint32

    // open: fundee
    | InputInitFundee of InputInitFundee
    | ApplyOpenChannel of OpenChannel
    | ApplyFundingCreated of FundingCreated

    // normal
    | AddHTLC of CMDAddHTLC
    | ApplyUpdateAddHTLC of UpdateAddHTLC
    | FulfillHTLC of CMDFulfillHTLC
    | ApplyUpdateFulfillHLTC of UpdateFulfillHTLC
    | FailHTLC of CMDFailHTLC
    | ApplyUpdateFailHTLC of UpdateFailHTLC
    | FailMalformedHTLC of CMDFailMalformedHTLC
    | ApplyUpdateFailMalformedHTLC of UpdateFailMalformedHTLC
    | UpdateFee of CMDUpdateFee
    | ApplyUpdateFee of UpdateFee

    | SignCommitment
    | ApplyCommitmentSigned of CommitmentSigned

    // close
    | Close of CMDClose
    | ForceClose
    | GetState
    | GetStateData
    | ResGetInfo of CMDResGetInfo
