namespace DotNetLightning.Channel

open DotNetLightning.Chain
open DotNetLightning.Utils
open DotNetLightning.Channel

open NBitcoin

/// cousin of `ChannelHelpers` module which only includes very primitive function.
module internal ChannelConstantHelpers =
    let deriveOurDustLimitSatoshis (feeEstimator: IFeeEstimator): Money =
        let (FeeRatePerKw atOpenBackGroundFee) = feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Background)
        (Money.Satoshis((uint64 atOpenBackGroundFee) * B_OUTPUT_PLUS_SPENDING_INPUT_WEIGHT / 1000UL), Money.Satoshis(546UL))
        |> Money.Max
        
    let getOurChannelReserve (channelValue: Money) =
        let q = channelValue / 100L
        Money.Min(channelValue, Money.Max(q, Money.Satoshis(1L)))
        
