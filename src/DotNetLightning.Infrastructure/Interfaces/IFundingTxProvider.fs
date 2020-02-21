namespace DotNetLightning.Infrastructure

open DotNetLightning.Utils
open DotNetLightning.Transactions
open NBitcoin

type IFundingTxProvider =
    abstract member ProvideFundingTx: IDestination * Money * FeeRatePerKw -> Result<FinalizedTx * TxOutIndex, string> 
