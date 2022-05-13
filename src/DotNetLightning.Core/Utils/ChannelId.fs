namespace DotNetLightning.Utils

open NBitcoin

/// <summary>
///     see [bolt2](https://github.com/lightning/bolts/blob/master/02-peer-protocol.md#definition-of-channel_id)
///     for the detail.
///     This is mainly used when the channel funding tx is not confirmed in to the
///     blockchain. After confirmation, `ShortChannelId` is used
/// </summary>
/// <seealso cref="ShortChannelId" />
type ChannelId =
    | ChannelId of uint256

    member this.Value = let (ChannelId v) = this in v

    static member Zero = uint256.Zero |> ChannelId
