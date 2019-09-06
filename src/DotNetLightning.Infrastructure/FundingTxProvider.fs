namespace DotNetLightning.Infrastructure

open DotNetLightning.LN
open DotNetLightning.Utils
open DotNetLightning.Transactions
open NBitcoin

type IFundingTxProvider =
    abstract member ProvideFundingTx: IDestination * Money * FeeRatePerKw -> RResult<FinalizedTx * TxOutIndex> 
