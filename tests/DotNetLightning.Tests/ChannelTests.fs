module ChannelTests
open Expecto
open NBitcoin
open DotNetLightning.LN
open DotNetLightning
open DotNetLightning.Utils.Primitives
open DotNetLightning.Chain
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Utils.Aether.Operators
open DotNetLightning.Utils.Aether
open DotNetLightning.Tests.Utils
open NBitcoin.Crypto
open Expecto.Logging
open Expecto.Logging.Message

let hex = NBitcoin.DataEncoders.HexEncoder()
let logger = Log.create "Channel tests"
let log = eventX >> logger.info 

type DummyFeeEstimator =
    {
        FeeEst: FeeRatePerKw
    }
    with
        interface IFeeEstimator with
            member this.GetEstSatPer1000Weight(arg1: ConfirmationTarget): FeeRatePerKw = 
                this.FeeEst

type HTLCSource with
    /// For test
    static member internal Dummy() =
        OutboundRoute {
            Route = Route []
            SessionPriv = Key([| for _ in 0..31 -> 1uy|])
            FirstHopHTLC = LNMoney.Zero
        }


[<Tests>]
let tests =
    testList "Channel tests" [
        testCase "test_max_funding_satoshis" <| fun _ ->
            ()
    ]