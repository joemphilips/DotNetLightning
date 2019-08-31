namespace DotNetLightning.Infrastructure.Tests

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Chain
open DotNetLightning.Infrastructure
open System.Net
open Moq

type TestEntity =
    {
        Seed: uint256
        KeyRepo: IKeysRepository
        NodeParams: NodeParams
    }

module Constants =
    let getAliceParam() =
        let p = NodeParams()
        p.Alias <- "alice"
        p.Color <- { RGB.Red = 1uy; Green = 2uy; Blue = 3uy }
        p.PublicAddresses <- [IPEndPoint.Parse("127.0.0.1:9731")]
        p.MaxHTLCValueInFlightMSat <- LNMoney.MilliSatoshis(150000000UL)
        p.MaxAcceptedHTLCs <- 100us
        // p.ExpirtyDeltaBlocks <- 144
        p.HTLCMinimumMSat <- LNMoney.Zero
        p.MinDepthBlocks <- 3u |> BlockHeight
        // p.SmartFeeNBlocks <- 3
        p.ToRemoteDelayBlocks <- BlockHeight 720u
        p.MaxToLocalDelayBlocks <- BlockHeight 1000u
        p.FeeBaseMSat <- 546000UL |> LNMoney.MilliSatoshis
        p.FeeProportionalMillionths <- 10u
        p.ReserveToFundingRatio <- 0.01
        p.MaxReserveToFundingRatio <- 0.05
        p.DBType <- SupportedDBType.Null
        let keyRepoMock = new Mock<IKeysRepository>()

        {
            TestEntity.Seed = [| for _ in 0..31 -> 0uy |] |> uint256
            KeyRepo = keyRepoMock.Object
            NodeParams = p
        }