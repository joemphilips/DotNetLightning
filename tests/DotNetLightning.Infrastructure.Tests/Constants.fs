namespace DotNetLightning.Infrastructure

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Chain
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
        p.PublicAddresses <- failwith ""
        let keyRepoMock = new Mock<IKeysRepository>()

        {
            TestEntity.Seed = [| for _ in 1..31 -> 0uy |] |> uint256
            KeyRepo = keyRepoMock.Object
            NodeParams = p
        }