namespace DotNetLightning.LN

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Utils.Error
open DotNetLightning.Serialize.Msgs

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


/// possible input to the channel. Command prefixed from `Apply` is passive. i.e.
/// it has caused by the outside world and not by the user. Mostly this is a message sent
/// from this channel's remote peer.
/// others are active commands which is caused by the user.
/// However, hese two kinds of command has no difference from architectural viewpoint.
/// It is just an input to the state.
type ChannelCommand =
    // open: funder
    | ApplyAcceptChannel of AcceptChannel
    | ApplyFundingSigned of FundingSigned
    | ApplyFundingLocked of FundingLocked
    | ApplyFundingConfirmedOnBC of height: BlockHeight * txIndex: TxIndexInBlock * depth: uint32

    // open: fundee
    | ApplyOpenChannel of OpenChannel
    | ApplyFundingCreated of FundingCreated

    // normal
    | AddHTLC of CMDAddHTLC
    | ApplyUpdateAddHTLC of UpdateAddHTLC
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