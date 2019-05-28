namespace DotNetLightning.LN
open BTCPayServer.Lightning
open DotNetLightning.Utils.Primitives
open NBitcoin


type HTLCOutputInCommitment = {
    Offered: bool
    Amount: LightMoney
    CLTVExpiry: uint32
    PaymentHash: PaymentHash
    TransactionOutputIndex: uint32 option
}
