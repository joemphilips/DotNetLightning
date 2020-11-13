namespace DotNetLightning.Routing

open DotNetLightning.Utils
open DotNetLightning.Utils.Primitives
open NBitcoin

open ResultUtils
open ResultUtils.Portability

type Stats<'T> = {
    Median: 'T
    Percentile5: 'T
    Percentile10: 'T
    Percentile25: 'T
    Percentile75: 'T
    Percentile90: 'T
    Percentile95: 'T
}
type NetworkStats = {
    Channels: int
    Nodes: int
    Cap: Stats<Money>
    CLTVExpiryDelta: Stats<BlockHeightOffset16>
    FeeBase: Stats<LNMoney>
    FeeProportional: Stats<uint32>
}
    with
    member this.ComputeStats(publicChannels: PublicChannel seq): NetworkStats option =
        if (publicChannels |> Seq.isEmpty || publicChannels |> Seq.collect(fun pc -> pc.GetChannelUpdateField(fun _ -> true)) |> Seq.isEmpty) then
            None
        else
            failwith "TODO"
        

