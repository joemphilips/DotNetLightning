module TestConstants

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Chain
open DotNetLightning.Infrastructure
open System.Net
open Foq

type TestEntity =
    {
        Seed: uint256
        KeyRepo: IKeysRepository
        NodeParams: NodeParams
    }

let fundingSatoshis = 1000000L |> Money.Satoshis
let pushMsat = 200000000L |> LNMoney.MilliSatoshis
let feeratePerKw = 10000u |> FeeRatePerKw 
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
    p.ToRemoteDelayBlocks <- BlockHeightOffset 720us
    p.MaxToLocalDelayBlocks <- BlockHeightOffset 1000us
    p.FeeBaseMSat <- 546000UL |> LNMoney.MilliSatoshis
    p.FeeProportionalMillionths <- 10u
    p.ReserveToFundingRatio <- 0.01
    p.DBType <- SupportedDBType.Null
    let keyRepoMock = new Mock<IKeysRepository>()

    {
        TestEntity.Seed = [| for _ in 0..31 -> 0uy |] |> uint256
        KeyRepo = keyRepoMock.Create()
        NodeParams = p
    }
    
let getBobParam() =
    let p = NodeParams()
    let keyRepoMock = new Mock<IKeysRepository>()
    {
        TestEntity.Seed = [| for _ in 0..31 -> 1uy |] |> uint256
        KeyRepo = keyRepoMock.Create()
        NodeParams = p
    }
