namespace DotNetLightning.Channel

open ResultUtils
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Utils.OnionError
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Chain
open DotNetLightning.Transactions

open NBitcoin

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
    CurrentHeight: BlockHeight
}
    with
        static member Create amountMSat paymentHash expiry onion upstream origin currentHeight =
            result {
                return {
                    AmountMSat = amountMSat
                    PaymentHash = paymentHash
                    Expiry = expiry
                    Onion = onion
                    Upstream = upstream
                    Origin = origin
                    CurrentHeight = currentHeight
                }
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
    FailureCode: FailureCode
}

type CMDUpdateFee = {
    FeeRatePerKw: FeeRatePerKw
}

type CMDClose = private { ScriptPubKey: Script option }
    with
    static member Zero = { ScriptPubKey = None }
    static member Create scriptPubKey =
        result {
            do! Scripts.checkIsValidFinalScriptPubKey scriptPubKey
            return  { ScriptPubKey = Some scriptPubKey }
        }

module CMDClose =
    let value cmdClose =
        cmdClose.ScriptPubKey

type LocalParams = {
    NodeId: NodeId
    ChannelPubKeys: ChannelPubKeys
    DustLimitSatoshis: Money
    MaxHTLCValueInFlightMSat: LNMoney
    ChannelReserveSatoshis: Money
    HTLCMinimumMSat: LNMoney
    ToSelfDelay: BlockHeightOffset
    MaxAcceptedHTLCs: uint16
    IsFunder: bool
    DefaultFinalScriptPubKey: Script
    GlobalFeatures: GlobalFeatures
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
    PaymentBasePoint: PubKey
    FundingPubKey: PubKey
    RevocationBasePoint: PubKey
    DelayedPaymentBasePoint: PubKey
    HTLCBasePoint: PubKey
    GlobalFeatures: GlobalFeatures
    LocalFeatures: LocalFeatures
    MinimumDepth: BlockHeightOffset
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
                GlobalFeatures = remoteInit.GlobalFeatures
                LocalFeatures = remoteInit.LocalFeatures
                MinimumDepth = BlockHeightOffset <| uint16 msg.MinimumDepth.Value
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
                GlobalFeatures = remoteInit.GlobalFeatures
                LocalFeatures = remoteInit.LocalFeatures
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
    | ApplyFundingConfirmedOnBC of height: BlockHeight * txIndex: TxIndexInBlock * depth: BlockHeightOffset

    // open: fundee
    | CreateInbound of InputInitFundee
    | ApplyOpenChannel of OpenChannel
    | ApplyFundingCreated of FundingCreated

    | ApplyChannelReestablish of ChannelReestablish

    // normal
    | AddHTLC of CMDAddHTLC
    | ApplyUpdateAddHTLC of msg: UpdateAddHTLC * currentHeight: BlockHeight
    | FulfillHTLC of CMDFulfillHTLC
    | ApplyUpdateFulfillHTLC of UpdateFulfillHTLC
    | FailHTLC of CMDFailHTLC
    | ApplyUpdateFailHTLC of UpdateFailHTLC
    | FailMalformedHTLC of CMDFailMalformedHTLC
    | ApplyUpdateFailMalformedHTLC of UpdateFailMalformedHTLC
    | UpdateFee of CMDUpdateFee
    | ApplyUpdateFee of UpdateFee

    | SignCommitment
    | ApplyCommitmentSigned of CommitmentSigned
    | ApplyRevokeAndACK of RevokeAndACK

    // close
    | Close of CMDClose
    | ApplyClosingSigned of ClosingSigned
    | RemoteShutdown of Shutdown

    // else
    | ForceClose
    | GetState
    | GetStateData