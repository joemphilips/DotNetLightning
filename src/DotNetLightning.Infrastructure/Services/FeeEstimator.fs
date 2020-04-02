namespace DotNetLightning.Infrastructure.Services

open DotNetLightning.Chain


type BitcoindRPCFeeEstimator() =
    interface IFeeEstimator with
        member this.GetEstSatPer1000Weight(confirmationTarget) =
            failwith "Not implemented: BitcoindRPCFeeEstimator::GetEstSatPer1000Weight"

