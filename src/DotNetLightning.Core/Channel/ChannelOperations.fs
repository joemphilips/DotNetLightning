namespace DotNetLightning.Channel

open ResultUtils
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Utils.OnionError
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Chain
open DotNetLightning.Transactions

open DotNetLightning.Serialize

open NBitcoin

type OperationAddHTLC = {
    Amount: LNMoney
    PaymentHash: PaymentHash
    Expiry: BlockHeight
    Onion: OnionPacket
    Upstream: UpdateAddHTLC option
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
    Features: FeatureBit
}

type RemoteParams = {
    NodeId: NodeId
    DustLimitSatoshis: Money
    MaxHTLCValueInFlightMSat: LNMoney
    ChannelReserveSatoshis: Money
    HTLCMinimumMSat: LNMoney
    ToSelfDelay: BlockHeightOffset16
    MaxAcceptedHTLCs: uint16
    PaymentBasePoint: PubKey
    FundingPubKey: PubKey
    RevocationBasePoint: PubKey
    DelayedPaymentBasePoint: PubKey
    HTLCBasePoint: PubKey
    Features: FeatureBit
    MinimumDepth: BlockHeightOffset32
}
    with
        static member FromAcceptChannel nodeId (remoteInit: Init) (msg: AcceptChannel) =
            {
                NodeId = nodeId
                DustLimitSatoshis = msg.DustLimitSatoshis
                MaxHTLCValueInFlightMSat = msg.MaxHTLCValueInFlightMsat
                ChannelReserveSatoshis = msg.ChannelReserveSatoshis
                HTLCMinimumMSat = msg.HTLCMinimumMSat
                ToSelfDelay = msg.ToSelfDelay
                MaxAcceptedHTLCs = msg.MaxAcceptedHTLCs
                PaymentBasePoint = msg.PaymentBasepoint
                FundingPubKey = msg.FundingPubKey
                RevocationBasePoint = msg.RevocationBasepoint
                DelayedPaymentBasePoint = msg.DelayedPaymentBasepoint
                HTLCBasePoint = msg.HTLCBasepoint
                Features = remoteInit.Features
                MinimumDepth = msg.MinimumDepth
            }

        static member FromOpenChannel (nodeId) (remoteInit: Init) (msg: OpenChannel) (channelHandshakeConfig: ChannelHandshakeConfig) =
            {
                NodeId = nodeId
                DustLimitSatoshis = msg.DustLimitSatoshis
                MaxHTLCValueInFlightMSat = msg.MaxHTLCValueInFlightMsat
                ChannelReserveSatoshis = msg.ChannelReserveSatoshis
                HTLCMinimumMSat = msg.HTLCMinimumMsat
                ToSelfDelay = msg.ToSelfDelay
                MaxAcceptedHTLCs = msg.MaxAcceptedHTLCs
                PaymentBasePoint = msg.PaymentBasepoint
                FundingPubKey = msg.FundingPubKey
                RevocationBasePoint = msg.RevocationBasepoint
                DelayedPaymentBasePoint = msg.DelayedPaymentBasepoint
                HTLCBasePoint = msg.HTLCBasepoint
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


/// possible input to the channel. Command prefixed from `Apply` is passive. i.e.
/// it has caused by the outside world and not by the user. Mostly this is a message sent
/// from this channel's remote peer.
/// others are active commands which is caused by the user.
/// However, these two kinds of command has no difference from architectural viewpoint.
/// It is just an input to the state.
type ChannelCommand =
    // open: funder
    | CreateOutbound of InputInitFunder
    | ApplyAcceptChannel of AcceptChannel
    | ApplyFundingSigned of FundingSigned
    | ApplyFundingLocked of FundingLocked
    | ApplyFundingConfirmedOnBC of height: BlockHeight * txIndex: TxIndexInBlock * depth: BlockHeightOffset32

    // open: fundee
    | CreateInbound of InputInitFundee
    | ApplyOpenChannel of OpenChannel
    | ApplyFundingCreated of FundingCreated

    | CreateChannelReestablish

    // normal
    | AddHTLC of OperationAddHTLC
    | ApplyUpdateAddHTLC of msg: UpdateAddHTLC * currentHeight: BlockHeight
    | FulfillHTLC of OperationFulfillHTLC
    | ApplyUpdateFulfillHTLC of UpdateFulfillHTLC
    | FailHTLC of OperationFailHTLC
    | ApplyUpdateFailHTLC of UpdateFailHTLC
    | FailMalformedHTLC of OperationFailMalformedHTLC
    | ApplyUpdateFailMalformedHTLC of UpdateFailMalformedHTLC
    | UpdateFee of OperationUpdateFee
    | ApplyUpdateFee of UpdateFee

    | SignCommitment
    | ApplyCommitmentSigned of CommitmentSigned
    | ApplyRevokeAndACK of RevokeAndACK

    // close
    | Close of OperationClose
    | ApplyClosingSigned of ClosingSigned
    | RemoteShutdown of Shutdown

    // else
    | ForceClose
    | GetState
    | GetStateData