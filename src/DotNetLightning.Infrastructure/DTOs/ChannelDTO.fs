namespace DotNetLightning.Infrastructure.DTOs

open System.Net
open DotNetLightning.Channel
open DotNetLightning.Utils.Primitives
open NBitcoin

type OpenChannelRequest = {
    NodeInfo: {| Id: NodeId; Host: string; Port: int |}
    Amount: Money
    FeeRate: FeeRate
}

type OpenChannelResponse = Result<ShortChannelId, ChannelError>
