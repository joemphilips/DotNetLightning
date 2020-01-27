namespace DotNetLightning.Payment

open System

module internal PaymentConstants =
    let DEFAULT_EXPIRY_SECONDS = TimeSpan.FromSeconds(3600.0)
    
