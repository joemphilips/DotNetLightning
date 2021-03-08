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
open Aether

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

type ChannelOptions = {
    MaxFeeRateMismatchRatio: float
    // Amount (in millionth of a satoshi) the channel will charge per transferred satoshi.
    // This may be allowed to change at runtime in a later update, however doing so must result in
    // update messages sent to notify all nodes of our updated relay fee.
    FeeProportionalMillionths: uint32
    /// We don't exchange more than this many signatures when negotiating the closing fee
    MaxClosingNegotiationIterations: int32
    FeeEstimator: IFeeEstimator
 }
    with

    static member FeeProportionalMillionths_: Lens<_, _> =
        (fun cc -> cc.FeeProportionalMillionths),
        (fun v cc -> { cc with FeeProportionalMillionths = v })
        
