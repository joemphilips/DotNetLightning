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

type DummyKeyRepository =
    {
        ChanKeys: ChannelKeys
    }
    with
        interface IKeysRepository with

            member this.GetChannelId(): Utils.Primitives.ChannelId = 
                uint256.Zero |> ChannelId

            member this.GetChannelKeys(_inbound): ChannelKeys = 
                this.ChanKeys

            member this.GetDestinationScript(): Script = 
                let channelMonitorClaminKey = Key(hex.DecodeData("0fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))
                channelMonitorClaminKey.PubKey.Hash.ScriptPubKey

            member this.GetSessionKey(): Key = 
                failwith "Not Implemented"

            member this.GetShutdownPubKey(): PubKey = 
                let channelCloseKey = Key(hex.DecodeData("0fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))
                channelCloseKey.PubKey

            member this.GetNodeSecret() =
                failwith "Not implemented"

            member this.GetSignature(psbt) = failwith ""

[<Tests>]
let tests =
    testList "Channel tests" [
        testCase "test_max_funding_satoshis" <| fun _ ->
            Expect.isLessThan MAX_FUNDING_SATOSHIS (Money.Satoshis(21_000_000L * 100_000_000L)) "MAX_FUNDING_SATOSHIS is greater than all satoshis in existance"

        /// Test vectors from BOLT3 Appendix C:
        testCase "Outbound commitment test" <| fun _ ->
            let feeest = { DummyFeeEstimator.FeeEst = FeeRatePerKw 15000u }
            let logger = TestLogger.Zero
            let n = Network.RegTest
            let channelKeys = {
                    ChannelKeys.FundingKey = Key(hex.DecodeData("30ff4956bbdd3222d44cc5e8a1261dab1e07957bdac5ae88fe3261ef321f3749"))
                    PaymentBaseKey = Key(hex.DecodeData("1111111111111111111111111111111111111111111111111111111111111111"))
                    DelayedPaymentBaseKey = Key(hex.DecodeData("3333333333333333333333333333333333333333333333333333333333333333"))
                    HTLCBaseKey=Key(hex.DecodeData("1111111111111111111111111111111111111111111111111111111111111111"))
                    RevocationBaseKey=Key(hex.DecodeData("0fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))
                    CommitmentSeed = uint256([| for _ in 0..31 -> 0xffuy |])
                }

            Expect.equal channelKeys.FundingKey.PubKey (PubKey(hex.DecodeData("023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb"))) ""
            let keysProvider = { DummyKeyRepository.ChanKeys = channelKeys }
            let theirNodeId = NodeId (Key([| for _ in 0..31 -> 0x42uy |]).PubKey)
            let l = UserConfig.ChannelOptions_ >-> ChannelConfig.AnnouncedChannel_
            let config = Optic.set l false UserConfig.Zero
            ()
    ]