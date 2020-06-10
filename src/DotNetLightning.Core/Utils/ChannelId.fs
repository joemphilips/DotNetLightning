namespace DotNetLightning.Utils

open NBitcoin

type ChannelId = | ChannelId of uint256 with
    member x.Value = let (ChannelId v) = x in v

    static member Zero = uint256.Zero |> ChannelId

