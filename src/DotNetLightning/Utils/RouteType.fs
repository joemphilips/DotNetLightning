namespace DotNetLightning.Utils

open NBitcoin

/// A hop in route
type RouteHop = {
    /// The node_id of the node at this hop.
    PubKey: PubKey
    /// The channel that should be used from the previous hop to reach this node.
    ShortChannelId: ShortChannelId
    /// The fee of this hop. FOr the last hop, this should be the full value of the payment.
    Fee: LNMoney
    /// The CLTV delta added for this hop. For the last hop, this should be the full CLTV value
    /// expected at the destination, in excess of the current block height.
    CLTVExpiryDelta: uint32
}

/// This should not exceed 20 (as a protocol rule).
type Route = Route of RouteHop list
    with
    member this.Value = let (Route r) = this in r