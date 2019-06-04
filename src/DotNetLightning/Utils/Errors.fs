namespace DotNetLightning.Utils
open System


module Error =
    [<Literal>]
    let BADONION = 0x8000us
    [<Literal>]
    let PERM = 0x4000us
    [<Literal>]
    let NODE = 0x2000us
    [<Literal>]
    let UPDATE = 0x1000us

    [<Flags>]
    type ErrorCode = ErrorCode of uint16
        with 
            member this.Value = let (ErrorCode v) = this in v
            member this.GetOnionErrorDescription() =
                match this.Value with
                | _c when _c = (PERM|||1us) -> ("The realm byte was not understood by the processing node", "invalid_realm")
                | _c when _c = (NODE|||2us) -> ("Node indicated temporary node failure", "temporary_node_failure")
                | _c when _c = (PERM|||NODE|||2us) -> ("Node indicated permanent node failure", "permanent_node_failure")
                | _c when _c = (PERM|||NODE|||3us) -> ("Node indicated the required node feature is missing in the onion", "required_node_feature_missing")
                | _c when _c = (BADONION|||PERM|||4us) -> ("Node indicated the version by is not understood", "invalid_onion_version")
                | _c when _c = (BADONION|||PERM|||5us)  -> ("Node indicated the HMAC of the onion is incorrect", "invalid_onion_hmac")
                | _c when _c = (BADONION|||PERM|||6us) -> ("Node indicated the ephemeral public keys is not parseable", "invalid_onion_key")
                | _c when _c = (UPDATE|||7us) -> ("Node indicated the outgoing channel is unable to handle the HTLC temporarily", "temporary_channel_failure")
                | _c when _c = (PERM|||8us) -> ("Node indicated the outgoing channel is unable to handle the HTLC peramanently", "permanent_channel_failure")
                | _c when _c = (PERM|||9us) -> ("Node indicated the required feature for the outgoing channel is not satisfied", "required_channel_feature_missing")
                | _c when _c = (PERM|||10us) -> ("Node indicated the outbound channel is not found for the specwhenied short_channel_id in the onion packet", "unknown_next_peer")
                | _c when _c = (UPDATE|||11us) -> ("Node indicated the HTLC amount was below the required minmum for the outbound channel", "amount_below_minimum")
                | _c when _c = (UPDATE|||12us) -> ("Node indicated the fee amount does not meet the required level", "fee_insufficient")
                | _c when _c = (UPDATE|||13us) -> ("Node indicated the cltv_expiry does not comply with the cltv_expiry_delta required by the outgoing channel", "incorrect_cltv_expiry")
                | _c when _c = (UPDATE|||14us) -> ("Node indicated the CLTV expiry too close to the current block height for safe handling", "expiry_too_soon")
                | _c when _c = (PERM|||15us) -> ("The final node indicated the payment hash is unknown or amount is incorrect", "incorrect_or_unknown_payment_details")
                | _c when _c = (PERM|||16us) -> ("The final node indicated the payment amount is incorrect", "incorrect_payment_amount")
                | _c when _c = (17us) -> ("The final node indicated the CLTV expiry is too close to the current block height for safe handling", "final_expiry_too_soon")
                | _c when _c = (18us) -> ("The final node indicated the CLTV expiry in the HTLC does not match the value in the onion", "final_incorrect_cltv_expiry")
                | _c when _c = (19us) -> ("The final node indicated the amount in the HTLC does not match the value in the onion", "final_incorrect_htlc_amount")
                | _c when _c = (UPDATE|||20us) -> ("Node indicated the outbound channel has been disabled", "channel_disabled")
                | _c when _c = (21us) -> ("Node indicated the CLTV expiry in the HTLC is too far in the future", "expiry_too_far")
                | _ -> ("Unknown", "")