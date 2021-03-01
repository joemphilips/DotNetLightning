namespace DotNetLightning.Channel

open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Utils.OnionError
open DotNetLightning.Serialization.Msgs
open DotNetLightning.Chain
open DotNetLightning.Crypto
open DotNetLightning.Transactions

open DotNetLightning.Serialization

open NBitcoin

open ResultUtils
open ResultUtils.Portability

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

type LocalParams = {
    DustLimitSatoshis: Money
    MaxHTLCValueInFlightMSat: LNMoney
    ChannelReserveSatoshis: Money
    HTLCMinimumMSat: LNMoney
    ToSelfDelay: BlockHeightOffset16
    MaxAcceptedHTLCs: uint16
    Features: FeatureBits
}

type RemoteParams = {
    DustLimitSatoshis: Money
    MaxHTLCValueInFlightMSat: LNMoney
    ChannelReserveSatoshis: Money
    HTLCMinimumMSat: LNMoney
    ToSelfDelay: BlockHeightOffset16
    MaxAcceptedHTLCs: uint16
    Features: FeatureBits
}
    with
        static member FromAcceptChannel (remoteInit: InitMsg) (msg: AcceptChannelMsg) =
            {
                DustLimitSatoshis = msg.DustLimitSatoshis
                MaxHTLCValueInFlightMSat = msg.MaxHTLCValueInFlightMsat
                ChannelReserveSatoshis = msg.ChannelReserveSatoshis
                HTLCMinimumMSat = msg.HTLCMinimumMSat
                ToSelfDelay = msg.ToSelfDelay
                MaxAcceptedHTLCs = msg.MaxAcceptedHTLCs
                Features = remoteInit.Features
            }

        static member FromOpenChannel (remoteInit: InitMsg)
                                      (msg: OpenChannelMsg)
                                          : RemoteParams =
            {
                DustLimitSatoshis = msg.DustLimitSatoshis
                MaxHTLCValueInFlightMSat = msg.MaxHTLCValueInFlightMsat
                ChannelReserveSatoshis = msg.ChannelReserveSatoshis
                HTLCMinimumMSat = msg.HTLCMinimumMsat
                ToSelfDelay = msg.ToSelfDelay
                MaxAcceptedHTLCs = msg.MaxAcceptedHTLCs
                Features = remoteInit.Features
            }

/// possible input to the channel. Command prefixed from `Apply` is passive. i.e.
/// it has caused by the outside world and not by the user. Mostly this is a message sent
/// from this channel's remote peer.
/// others are active commands which is caused by the user.
/// However, these two kinds of command has no difference from architectural viewpoint.
/// It is just an input to the state.
type ChannelCommand =
    // open: funder
    | ApplyAcceptChannel of AcceptChannelMsg
    | CreateFundingTx of fundingTx: FinalizedTx * outIndex: TxOutIndex
    | ApplyFundingSigned of FundingSignedMsg
    | ApplyFundingLocked of FundingLockedMsg
    | ApplyFundingConfirmedOnBC of height: BlockHeight * txIndex: TxIndexInBlock * depth: BlockHeightOffset32

    // open: fundee
    | ApplyOpenChannel of OpenChannelMsg
    | ApplyFundingCreated of FundingCreatedMsg

    | CreateChannelReestablish

    // normal
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
    | Close of ShutdownScriptPubKey
    | ApplyClosingSigned of ClosingSignedMsg
    | RemoteShutdown of ShutdownMsg * ShutdownScriptPubKey

    // else
    | ForceClose
    | GetState
    | GetStateData


/// Channel config which is static, ie. config parameters which are established
/// during the channel handshake and persist unchagned through the lifetime of
/// the channel.
type StaticChannelConfig = {
    AnnounceChannel: bool
    RemoteNodeId: NodeId
    Network: Network
    FundingTxMinimumDepth: BlockHeightOffset32
    LocalStaticShutdownScriptPubKey: Option<ShutdownScriptPubKey>
    RemoteStaticShutdownScriptPubKey: Option<ShutdownScriptPubKey>
    IsFunder: bool
    FundingScriptCoin: ScriptCoin
    LocalParams: LocalParams
    RemoteParams: RemoteParams
    RemoteChannelPubKeys: ChannelPubKeys
}
    with
        member this.ChannelId(): ChannelId =
            this.FundingScriptCoin.Outpoint.ToChannelId()

