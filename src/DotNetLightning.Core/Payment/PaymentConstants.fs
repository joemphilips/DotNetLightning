namespace DotNetLightning.Payment

open DotNetLightning.Utils.Primitives
open System

/// <namespacedoc>
///     <summary>
///         This namespace contains utilities for working with LN payment. e.g.
///         1. BOLT11 invoice (PaymentRequest)
///         2. LSAT authentication token
///     </summary>
/// </namespacedoc>
/// <exclude />
module NamespaceDocDummy =
    ()

module internal PaymentConstants =
    let DEFAULT_EXPIRY_SECONDS = TimeSpan.FromSeconds(3600.0)
    let DEFAULT_MINIMUM_CLTVEXPIRY = 9u |> BlockHeightOffset32
