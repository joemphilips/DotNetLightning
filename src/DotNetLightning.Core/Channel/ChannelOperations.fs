namespace DotNetLightning.Channel

open ResultUtils
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Utils.OnionError
open DotNetLightning.Serialization.Msgs
open DotNetLightning.Chain
open DotNetLightning.Crypto
open DotNetLightning.Transactions

open DotNetLightning.Serialization

open NBitcoin

type OperationMonoHopUnidirectionalPayment = {
    Amount: LNMoney
}

type OperationAddHTLC = {
    Amount: LNMoney
    PaymentHash: PaymentHash
    Expiry: BlockHeight
    Onion: OnionPacket
    Upstream: UpdateAddHTLCMsg option
    Origin: HTLCSource option
    CurrentHeight: BlockHeight
}
    with
        static member Create amountMSat paymentHash expiry onion upstream origin currentHeight =
            {
                Amount = amountMSat
                PaymentHash = paymentHash
                Expiry = expiry
                Onion = onion
                Upstream = upstream
                Origin = origin
                CurrentHeight = currentHeight
            }


type OperationFulfillHTLC = {
    Id: HTLCId
    PaymentPreimage: PaymentPreimage
    Commit: bool
}

type OperationFailHTLC = {
    Id: HTLCId
    Reason: Choice<byte[], FailureMsg>
}

type OperationFailMalformedHTLC = {
    Id: HTLCId
    Sha256OfOnion: uint256
    FailureCode: FailureCode
}

type OperationUpdateFee = {
    FeeRatePerKw: FeeRatePerKw
}

type OperationClose = private { ScriptPubKey: Script option }
    with
    static member Zero = { ScriptPubKey = None }
    static member Create scriptPubKey =
        result {
            do! Scripts.checkIsValidFinalScriptPubKey scriptPubKey
            return  { ScriptPubKey = Some scriptPubKey }
        }

module OperationClose =
    let value cmdClose =
        cmdClose.ScriptPubKey

type LocalParams = {
    NodeId: NodeId
    ChannelPubKeys: ChannelPubKeys
    DustLimitSatoshis: Money
    MaxHTLCValueInFlightMSat: LNMoney
    ChannelReserveSatoshis: Money
    HTLCMinimumMSat: LNMoney
    ToSelfDelay: BlockHeightOffset16
    MaxAcceptedHTLCs: uint16
    IsFunder: bool
    DefaultFinalScriptPubKey: Script
    Features: FeatureBits
}

type RemoteParams = {
    NodeId: NodeId
    DustLimitSatoshis: Money
    MaxHTLCValueInFlightMSat: LNMoney
    ChannelReserveSatoshis: Money
    HTLCMinimumMSat: LNMoney
    ToSelfDelay: BlockHeightOffset16
    MaxAcceptedHTLCs: uint16
    ChannelPubKeys: ChannelPubKeys
    Features: FeatureBits
    MinimumDepth: BlockHeightOffset32
}
    with
        static member FromAcceptChannel nodeId (remoteInit: InitMsg) (msg: AcceptChannelMsg) =
            let channelPubKeys = {
                FundingPubKey = msg.FundingPubKey
                RevocationBasepoint = msg.RevocationBasepoint
                PaymentBasepoint = msg.PaymentBasepoint
                DelayedPaymentBasepoint = msg.DelayedPaymentBasepoint
                HtlcBasepoint = msg.HTLCBasepoint
            }
            {
                NodeId = nodeId
                DustLimitSatoshis = msg.DustLimitSatoshis
                MaxHTLCValueInFlightMSat = msg.MaxHTLCValueInFlightMsat
                ChannelReserveSatoshis = msg.ChannelReserveSatoshis
                HTLCMinimumMSat = msg.HTLCMinimumMSat
                ToSelfDelay = msg.ToSelfDelay
                MaxAcceptedHTLCs = msg.MaxAcceptedHTLCs
                ChannelPubKeys = channelPubKeys
                Features = remoteInit.Features
                MinimumDepth = msg.MinimumDepth
            }

        static member FromOpenChannel (nodeId) (remoteInit: InitMsg) (msg: OpenChannelMsg) (channelHandshakeConfig: ChannelHandshakeConfig) =
            let channelPubKeys = {
                FundingPubKey = msg.FundingPubKey
                RevocationBasepoint = msg.RevocationBasepoint
                PaymentBasepoint = msg.PaymentBasepoint
                DelayedPaymentBasepoint = msg.DelayedPaymentBasepoint
                HtlcBasepoint = msg.HTLCBasepoint
            }
            {
                NodeId = nodeId
                DustLimitSatoshis = msg.DustLimitSatoshis
                MaxHTLCValueInFlightMSat = msg.MaxHTLCValueInFlightMsat
                ChannelReserveSatoshis = msg.ChannelReserveSatoshis
                HTLCMinimumMSat = msg.HTLCMinimumMsat
                ToSelfDelay = msg.ToSelfDelay
                MaxAcceptedHTLCs = msg.MaxAcceptedHTLCs
                ChannelPubKeys = channelPubKeys
                Features = remoteInit.Features
                MinimumDepth = channelHandshakeConfig.MinimumDepth
            }

type InputInitFunder = {
    TemporaryChannelId: ChannelId
    FundingSatoshis: Money
    PushMSat: LNMoney
    InitFeeRatePerKw: FeeRatePerKw
    FundingTxFeeRatePerKw: FeeRatePerKw
    LocalParams: LocalParams
    RemoteInit: InitMsg
    ChannelFlags: uint8
    ChannelPrivKeys: ChannelPrivKeys
}
    with
        static member FromOpenChannel (localParams) (remoteInit) (channelPrivKeys) (o: OpenChannelMsg) =
            {
                InputInitFunder.TemporaryChannelId = o.TemporaryChannelId
                FundingSatoshis = o.FundingSatoshis
                PushMSat = o.PushMSat
                InitFeeRatePerKw = o.FeeRatePerKw
                FundingTxFeeRatePerKw = o.FeeRatePerKw
                LocalParams = localParams
                RemoteInit = remoteInit
                ChannelFlags = o.ChannelFlags
                ChannelPrivKeys = channelPrivKeys
            }

        member this.DeriveCommitmentSpec() =
            CommitmentSpec.Create this.ToLocal this.PushMSat this.FundingTxFeeRatePerKw

        member this.ToLocal =
            this.FundingSatoshis.ToLNMoney() - this.PushMSat

and InputInitFundee = {
    TemporaryChannelId: ChannelId
    LocalParams: LocalParams
    RemoteInit: InitMsg
    ToLocal: LNMoney
    ChannelPrivKeys: ChannelPrivKeys
}


/// possible input to the channel. Command prefixed from `Apply` is passive. i.e.
/// it has caused by the outside world and not by the user. Mostly this is a message sent
/// from this channel's remote peer.
/// others are active commands which is caused by the user.
/// However, these two kinds of command has no difference from architectural viewpoint.
/// It is just an input to the state.
type ChannelCommand =
    // open: funder
    | CreateOutbound of InputInitFunder
    | ApplyAcceptChannel of AcceptChannelMsg
    | ApplyFundingSigned of FundingSignedMsg
    | ApplyFundingLocked of FundingLockedMsg
    | ApplyFundingConfirmedOnBC of height: BlockHeight * txIndex: TxIndexInBlock * depth: BlockHeightOffset32

    // open: fundee
    | CreateInbound of InputInitFundee
    | ApplyOpenChannel of OpenChannelMsg
    | ApplyFundingCreated of FundingCreatedMsg

    | CreateChannelReestablish

    // normal
    | MonoHopUnidirectionalPayment of OperationMonoHopUnidirectionalPayment
    | ApplyMonoHopUnidirectionalPayment of msg: MonoHopUnidirectionalPaymentMsg
    | AddHTLC of OperationAddHTLC
    | ApplyUpdateAddHTLC of msg: UpdateAddHTLCMsg * currentHeight: BlockHeight
    | FulfillHTLC of OperationFulfillHTLC
    | ApplyUpdateFulfillHTLC of UpdateFulfillHTLCMsg
    | FailHTLC of OperationFailHTLC
    | ApplyUpdateFailHTLC of UpdateFailHTLCMsg
    | FailMalformedHTLC of OperationFailMalformedHTLC
    | ApplyUpdateFailMalformedHTLC of UpdateFailMalformedHTLCMsg
    | UpdateFee of OperationUpdateFee
    | ApplyUpdateFee of UpdateFeeMsg

    | SignCommitment
    | ApplyCommitmentSigned of CommitmentSignedMsg
    | ApplyRevokeAndACK of RevokeAndACKMsg

    // close
    | Close of OperationClose
    | ApplyClosingSigned of ClosingSignedMsg
    | RemoteShutdown of ShutdownMsg

    // else
    | ForceClose
    | GetState
    | GetStateData
