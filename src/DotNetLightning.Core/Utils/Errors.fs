namespace DotNetLightning.Utils

open System

/// Indicates an error on the client's part ( usually some variant of attempting to use too-low or
/// too high values)
type APIError =
    /// Indicates the API was wholly misused (see err for more). Cases where these can be returned
    /// are documented, but generally indicates some precondition of a function was violated.
    | APIMisuseError of string

    /// Due to a high feerate, we were unable to complete the request.
    /// For example, this may be returned if the feerate implies we cannot open a channel at the
    /// requested value, but opening a larget channel would succeed.

    | FeeRateTooHigh of FeeRateTooHighContent
    /// a malformed Route was provided ( e.g. overfloed value, node id mismatch, overly-loope route,
    /// too-many-hops, etc).
    | RouteError of string
    /// An attempt to call add_update_monitor returned Err ( ie you did this!), causing the
    /// attempted action to fail.
    | MonitorUpdateFailed

and FeeRateTooHighContent =
    {
        Msg: string
        FeeRate: FeeRatePerKw
    }


/// TODO: rename?
module OnionError =
    [<Literal>]
    let BADONION = 0x8000us

    [<Literal>]
    let PERM = 0x4000us

    [<Literal>]
    let NODE = 0x2000us

    [<Literal>]
    let UPDATE = 0x1000us

    [<Literal>]
    let INVALID_REALM = PERM ||| 1us

    [<Literal>]
    let TEMPORARY_NODE_FAILURE = NODE ||| 2us

    [<Literal>]
    let PERMANENT_NODE_FAILURE = PERM ||| NODE ||| 2us

    [<Literal>]
    let REQUIRED_NODE_FEATURE_MISSING = PERM ||| NODE ||| 3us

    [<Literal>]
    let INVALID_ONION_VERSION = BADONION ||| PERM ||| 4us

    [<Literal>]
    let INVALID_ONION_HMAC = BADONION ||| PERM ||| 5us

    [<Literal>]
    let INVALID_ONION_KEY = BADONION ||| PERM ||| 6us

    [<Literal>]
    let TEMPORARY_CHANNEL_FAILURE = UPDATE ||| 7us

    [<Literal>]
    let PERMANENT_CHANNEL_FAILURE = PERM ||| 8us

    [<Literal>]
    let REQUIRED_CHANNEL_FEATURE_MISSING = PERM ||| 9us

    [<Literal>]
    let UNKNOWN_NEXT_PEER = PERM ||| 10us

    [<Literal>]
    let AMOUNT_BELOW_MINIMUM = UPDATE ||| 11us

    [<Literal>]
    let FEE_INSUFFICIENT = UPDATE ||| 12us

    [<Literal>]
    let INOCCORRECT_CLTV_EXPIRY = UPDATE ||| 13us

    [<Literal>]
    let EXPIRY_TOO_SOON = UPDATE ||| 14us

    [<Literal>]
    let UNKNOWN_PAYMENT_HASH = PERM ||| 15us

    [<Literal>]
    let INCORRECT_PAYMENT_AMOUNT = PERM ||| 16us

    [<Literal>]
    let FINAL_EXPIRY_TOO_SOON = 17us

    [<Literal>]
    let FINAL_INCORRECT_CLTV_EXPIRY = 18us

    [<Literal>]
    let FINAL_INCORRECT_HTLC_AMOUNT = 19us

    [<Literal>]
    let EXPIRY_TOO_FAR = 21us

    [<Literal>]
    let CHANNEL_DISABLED = UPDATE ||| 20us

    /// P2P onion msg error.
    /// See [bolt02](https://github.com/lightning/bolts/blob/f6c4d7604150986894bcb46d67c5c88680740b12/04-onion-routing.md#failure-messages)
    /// For the detail.
    [<Flags>]
#if !NoDUsAsStructs
    [<Struct>]
#endif
    type FailureCode =
        | FailureCode of uint16

        member this.Value = let (FailureCode v) = this in v

        member this.GetOnionErrorDescription() =
            match this.Value with
            | (INVALID_REALM) ->
                ("The realm byte was not understood by the processing node",
                 "invalid_realm")
            | (TEMPORARY_NODE_FAILURE) ->
                ("Node indicated temporary node failure",
                 "temporary_node_failure")
            | (PERMANENT_NODE_FAILURE) ->
                ("Node indicated permanent node failure",
                 "permanent_node_failure")
            | (REQUIRED_NODE_FEATURE_MISSING) ->
                ("Node indicated the required node feature is missing in the onion",
                 "required_node_feature_missing")
            | (INVALID_ONION_VERSION) ->
                ("Node indicated the version by is not understood",
                 "invalid_onion_version")
            | (INVALID_ONION_HMAC) ->
                ("Node indicated the HMAC of the onion is incorrect",
                 "invalid_onion_hmac")
            | (INVALID_ONION_KEY) ->
                ("Node indicated the ephemeral public keys is not parseable",
                 "invalid_onion_key")
            | (TEMPORARY_CHANNEL_FAILURE) ->
                ("Node indicated the outgoing channel is unable to handle the HTLC temporarily",
                 "temporary_channel_failure")
            | (PERMANENT_CHANNEL_FAILURE) ->
                ("Node indicated the outgoing channel is unable to handle the HTLC permanently",
                 "permanent_channel_failure")
            | (REQUIRED_CHANNEL_FEATURE_MISSING) ->
                ("Node indicated the required feature for the outgoing channel is not satisfied",
                 "required_channel_feature_missing")
            | (UNKNOWN_NEXT_PEER) ->
                ("Node indicated the outbound channel is not found for the specified short_channel_id in the onion packet",
                 "unknown_next_peer")
            | (AMOUNT_BELOW_MINIMUM) ->
                ("Node indicated the HTLC amount was below the required minimum for the outbound channel",
                 "amount_below_minimum")
            | (FEE_INSUFFICIENT) ->
                ("Node indicated the fee amount does not meet the required level",
                 "fee_insufficient")
            | (INOCCORRECT_CLTV_EXPIRY) ->
                ("Node indicated the cltv_expiry does not comply with the cltv_expiry_delta required by the outgoing channel",
                 "incorrect_cltv_expiry")
            | (EXPIRY_TOO_SOON) ->
                ("Node indicated the CLTV expiry too close to the current block height for safe handling",
                 "expiry_too_soon")
            | (UNKNOWN_PAYMENT_HASH) ->
                ("The final node indicated the payment hash is unknown or amount is incorrect",
                 "incorrect_or_unknown_payment_details")
            | (INCORRECT_PAYMENT_AMOUNT) ->
                ("The final node indicated the payment amount is incorrect",
                 "incorrect_payment_amount")
            | FINAL_EXPIRY_TOO_SOON ->
                ("The final node indicated the CLTV expiry is too close to the current block height for safe handling",
                 "final_expiry_too_soon")
            | (FINAL_INCORRECT_CLTV_EXPIRY) ->
                ("The final node indicated the CLTV expiry in the HTLC does not match the value in the onion",
                 "final_incorrect_cltv_expiry")
            | (FINAL_INCORRECT_HTLC_AMOUNT) ->
                ("The final node indicated the amount in the HTLC does not match the value in the onion",
                 "final_incorrect_htlc_amount")
            | (EXPIRY_TOO_FAR) ->
                ("Node indicated the CLTV expiry in the HTLC is too far in the future",
                 "expiry_too_far")
            | (CHANNEL_DISABLED) ->
                ("Node indicated the outbound channel has been disabled",
                 "channel_disabled")
            | _ -> ("Unknown", "")
