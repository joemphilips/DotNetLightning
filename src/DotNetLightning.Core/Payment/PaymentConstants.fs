namespace DotNetLightning.Payment

open DotNetLightning.Utils.Primitives
open System

module internal PaymentConstants =
    let DEFAULT_EXPIRY_SECONDS = TimeSpan.FromSeconds(3600.0)
    let DEFAULT_MINIMUM_CLTVEXPIRY = 9u |> BlockHeightOffset32
