namespace DotNetLightning.LN
open DotNetLightning.Utils
open DotNetLightning.Utils.Error
open DotNetLightning.Chain
open DotNetLightning.DomainUtils.Types
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Transactions
open NBitcoin

(*
    based on eclair's channel state management
*)


type LocalParams = {
    NodeId: NodeId
    ChannelKeys: ChannelKeys
    DustLimitSatoshis: Money
    MaxHtlcValueInFlightMSat: LNMoney
    ChannelReserveSatoshis: Money
    HTLCMinimumMSat: LNMoney
    ToSelfDelay: BlockHeightOffset
    MaxAcceptedHTLCs: uint16
    IsFunder: bool
    DefaultFinalScriptPubKey: Script
    GlobalFeatuers: GlobalFeatures
    LocalFeatures: LocalFeatures
}

type RemoteParams = {
    NodeId: NodeId
    DustLimitSatoshis: Money
    MaxHTLCValueInFlightMSat: LNMoney
    ChannelReserveSatoshis: Money
    HTLCMinimumMSat: LNMoney
    ToSelfDelay: BlockHeightOffset
    MaxAcceptedHTLCs: uint16
    FundingPubKey: PubKey
    RevocationBasePoint: PubKey
    DelayedPaymentBasePoint: PubKey
    HTLCBasePoint: PubKey
    GlobalFeatures: GlobalFeatures
    LocalFeatures: LocalFeatures
}
type InputInitFunder = {
    TemporaryChannelId: ChannelId
    FundingSatoshis: Money
    PushMSat: LNMoney
    InitFeeRatePerKw: FeeRatePerKw
    FundingTxFeeRatePerKw: FeeRatePerKw
    LocalParams: LocalParams
    RemoteInit: Init
    ChannelFlags: LocalFeatures
}
    with
        static member FromOpenChannel (localParams) (remoteInit) (o: OpenChannel) =
            {
                InputInitFunder.TemporaryChannelId = o.TemporaryChannelId
                FundingSatoshis = o.FundingSatoshis
                PushMSat = o.PushMSat
                InitFeeRatePerKw = o.FeeRatePerKw
                FundingTxFeeRatePerKw = o.FeeRatePerKw
                LocalParams = localParams
                RemoteInit = remoteInit
                ChannelFlags = localParams.LocalFeatures
            }
and InputInitFundee = {
    TemporaryChannelId: ChannelId
    LocalParams: LocalParams
    Remote: MailboxProcessor<ILightningMsg>
    RemoteInit: Init
}


//    8888888b.        d8888 88888888888     d8888
//    888  "Y88b      d88888     888        d88888
//    888    888     d88P888     888       d88P888
//    888    888    d88P 888     888      d88P 888
//    888    888   d88P  888     888     d88P  888
//    888    888  d88P   888     888    d88P   888
//    888  .d88P d8888888888     888   d8888888888
//    8888888P" d88P     888     888  d88P     888

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
                                            FundingTx: Transaction
                                            LocalSpec: CommitmentSpec
                                            LocalCommitTx: CommitTx
                                            RemoteCommit: RemoteCommit
                                            ChannelFlags: byte
                                            LastSent: FundingCreated
                                       }
        with interface IChannelStateData

    type WaitForFundingConfirmedData = {
                                            Commitments: Commitments
                                            Deferred: FundingLocked option
                                            LastSent: Choice<FundingCreated, FundingSigned>
                                          }
        with interface IChannelStateData

    type WaitForFundingLockedData = { Commitments: Commitments; ShortChannelId: ShortChannelId; LastSent: FundingLocked }
        with interface IChannelStateData

    type WaitNormalData = {
                            Commitments: Commitments;
                            ShortChannelId: ShortChannelId;
                            Buried: bool;
                            ChannelAnnouncement: ChannelAnnouncement option
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

module Events =

    type ChannelEvent =
        /// --- ln events ---
        | WeAcceptedOpenChannel of AcceptChannel * Data.WaitForFundingCreatedData
        | OpenChannelFromSelf of InputInitFunder
        | Closed
        /// Wen requesting a mutual close, we wait for as much as this timeout, then unilateral close
        | InputCloseCompleteTimeout
        | InputDisconnected
        | InputReconnected of Remote: MailboxProcessor<ILightningMsg>

        /// ---- onchain events -----
        | BitcoinFundingPublishFailed
        | BitcoinFundingDepthOk
        | BitcoinFundingDeployBuried
        | BitcoinFundingLost
        | BitcoinFundingTimeout
        | BitcoinFundingSpent
        | BitcoinOutputSpent
        | BitcoinTxConfirmed of Transaction
        | BitcoinFundingExternalChannelSpent of ShortChannelId: ShortChannelId
        | BitcoinParentTxConfirmed of ChildTx: Transaction
        with interface IEvent




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
    | WaitForFundingInternal of WaitForFundingInternalData
    | WaitForFundingCreated of WaitForFundingCreatedData
    | WaitForFundingSigned of WaitForFundingSignedData
    | WaitForFundingConfirmed of WaitForFundingConfirmedData
    | WaitForFundingLocked of WaitForFundingLockedData

    /// normal
    | Normal of WaitNormalData

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

type AddHTLC = {
    AmountMSat: LNMoney
    PaymentHash: PaymentHash
    Expiry: BlockHeightOffset
    Onion: byte[]
    Upstream:UpdateAddHTLC option
    Commit: bool
}
    with
        static member Create amountMSat paymentHash expiry onion upstream commit =
            {
                AmountMSat = amountMSat
                PaymentHash = paymentHash
                Expiry = expiry
                Onion = defaultArg onion [||]
                Upstream = upstream
                Commit = defaultArg commit false
            }

type FulfillHTLC = {
    Id: HTLCId
    PaymentPreimage: PaymentPreimage
    Commit: bool
}

type FailHTLC = {
    Id: HTLCId
    Reason: OnionErrorPacket
    Commit: bool
}

type FailMalformedHTLC = {
    Id: HTLCId
    Sha256OfOnion: uint256
    FailureCode: ErrorCode
    Commit: bool
}

type UpdateFee = {
    FeeRatePerKw: FeeRatePerKw
    Commit: bool
}

type Close = { ScriptPubKey: Script option }

type ResGetInfo = {
    NodeId: NodeId
    ChannelId: ChannelId
    State: IState
    Data: IChannelStateData
}

type ChannelCommand =
    | OpenChannel of OpenChannel
    | InputInitFunder of InputInitFunder
    | InputInitFundee of InputInitFundee
    | AddHTLC of AddHTLC
    | FulfillHTLC of FulfillHTLC
    | FailHTLC of FailHTLC
    | FailMalformedHTLC of FailMalformedHTLC
    | UpdateFee of UpdateFee
    | Sign
    | Close of Close
    | ForceClose
    | GetState
    | GetStateData
    | ResGetInfo of ResGetInfo