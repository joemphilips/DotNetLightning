module internal ChannelTestUtils

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Chain
open DotNetLightning.LN
open FsCheck
open Generators
open System

module Constants =
    let fundingSatohis = Money.Satoshis(1000000L)
    let pushMSat = LNMoney.MilliSatoshis(200000000L)
    let feeratePerKw = FeeRatePerKw 10000u

    type ChannelTestEntity = {
        Seed: uint256
        KeysRepo: IKeysRepository
        Config: ChannelConfig
    }
    let logger level msg = printfn msg
    let alice log =
        let aliceConfigConstructor
            announceChannel
            feeRateMismatch
            chargeFeeRate
            forceAnnounceChannelPref = {
            ChannelConfig.ChannelOptions = { ChannelOptions.AnnounceChannel = announceChannel
                                             MaxFeeRateMismatchRatio = feeRateMismatch
                                             FeeProportionalMillionths = chargeFeeRate }
            ChannelHandshakeConfig = { ChannelHandshakeConfig.MinimumDepth = BlockHeight(10u) }
            PeerChannelConfigLimits = { ChannelHandshakeLimits.ForceAnnounceChannelPreference = forceAnnounceChannelPref
                                        MinFundingSatoshis = 1000L |> Money.Satoshis
                                        MaxHTLCMinimumMSat = 1000L |> LNMoney.MilliSatoshis
                                        MinMaxHTLCValueInFlightMSat = 1000L |> LNMoney.MilliSatoshis
                                        MaxChannelReserveSatoshis = 20000L |> Money.Satoshis
                                        MinMaxAcceptedHTLCs = 100us
                                        MinDustLimitSatoshis = failwith "Not Implemented"
                                        MaxDustLimitSatoshis = failwith "Not Implemented"
                                        MaxMinimumDepth = failwith "Not Implemented"
                                        MaxNegotiationIterations = failwith "Not Implemented"}
        }
        let aliceConfigGen =
            aliceConfigConstructor
            <!> Arb.generate<bool>
            <*> (Arb.generate<NormalFloat> |> Gen.filter(fun x -> 1.0001 < x.Get && x.Get < 1.8) |> Gen.map(fun x -> x.Get))
            <*> (Arb.generate<uint32> |> Gen.filter(fun x -> x < 10000u))
            <*> Arb.generate<bool>

        let aliceEntityConstructor aliceConfig = {
            Seed = uint256.One
            KeysRepo = DefaultKeyRepository(uint256.Zero, Network.RegTest, log)
            Config = aliceConfig
        }

        aliceEntityConstructor <!> aliceConfigGen

    let bob log =
        let bobConfigConstructor
            announceChannel =
                {
                    ChannelConfig.ChannelOptions = {
                        ChannelOptions.AnnounceChannel = announceChannel
                        MaxFeeRateMismatchRatio = failwith "Not Implemented"
                        FeeProportionalMillionths = failwith "Not Implemented"
                    }
                    ChannelHandshakeConfig = failwith ""
                    PeerChannelConfigLimits = { ChannelHandshakeLimits.ForceAnnounceChannelPreference = true
                                                MinFundingSatoshis = failwith "Not Implemented"
                                                MaxHTLCMinimumMSat = failwith "Not Implemented"
                                                MinMaxHTLCValueInFlightMSat = UInt64.MaxValue |> LNMoney.MilliSatoshis
                                                MaxChannelReserveSatoshis = failwith "Not Implemented"
                                                MinMaxAcceptedHTLCs = failwith "Not Implemented"
                                                MinDustLimitSatoshis = failwith "Not Implemented"
                                                MaxDustLimitSatoshis = failwith "Not Implemented"
                                                MaxMinimumDepth = failwith "Not Implemented"
                                                MaxNegotiationIterations = failwith "Not Implemented" }
                }

        let bobConfigGen =
            bobConfigConstructor
            <!> Arb.generate<bool>

        let bobEntityConstructor bobConfig = {
            Seed = [||] |> uint256
            KeysRepo = DefaultKeyRepository(uint256.Zero, Network.RegTest, log)
            Config = bobConfig
        }
        bobEntityConstructor <!> bobConfigGen