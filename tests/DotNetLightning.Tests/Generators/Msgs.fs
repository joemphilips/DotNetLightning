module MsgGenerators

open DotNetLightning.Serialize.Msgs
open PrimitiveGenerators
open NBitcoin
open FsCheck
open DotNetLightning.Utils.Primitives

let (<*>) = Gen.apply

let private globalFeaturesGen =
    Arb.generate<uint8[]> |> Gen.map GlobalFeatures.Flags

let private localFeaturesGen =
    Arb.generate<uint8[]> |> Gen.map LocalFeatures.Flags

let initGen =
    Gen.map2 (fun g l -> { GlobalFeatures = g; LocalFeatures = l})
        globalFeaturesGen
        localFeaturesGen

let pingGen =
    Gen.map2(fun pLen bLen -> { PongLen = pLen; BytesLen = bLen })
        Arb.generate<uint16>
        Arb.generate<uint16>

let pongGen =
    Gen.map(fun bLen -> { BytesLen = bLen })
        Arb.generate<uint16>

let openChannelGen =
    let constructor = fun arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19 -> {
            OpenChannel.Chainhash = arg1
            TemporaryChannelId = arg2
            FundingSatoshis = arg3
            PushMSat = arg4
            DustLimitSatoshis = arg5
            MaxHTLCValueInFlightMsat = arg6
            ChannelReserveSatoshis = arg7
            HTLCMinimumMsat = arg8
            FeeRatePerKw = arg9
            ToSelfDelay = arg10
            MaxAcceptedHTLCs = arg11
            FundingPubKey = arg12
            RevocationBasepoint = arg13
            PaymentBasepoint = arg14
            DelayedPaymentBasepoint = arg15
            HTLCBasepoint = arg16
            FirstPerCommitmentPoint = arg17
            ChannelFlags = arg18
            ShutdownScriptPubKey = arg19
        }
    constructor
        <!> (uint256Gen)
        <*> (temporaryChannelGen)
        <*> (moneyGen)
        <*> (lnMoneyGen)
        <*> (moneyGen)
        <*> (lnMoneyGen)
        <*> (moneyGen)
        <*> (lnMoneyGen)
        <*> (FeeRatePerKw <!> Arb.generate<uint32>)
        <*> (BlockHeightOffset <!> Arb.generate<uint16>)
        <*> Arb.generate<uint16>
        <*> pubKeyGen
        <*> pubKeyGen
        <*> pubKeyGen
        <*> pubKeyGen
        <*> pubKeyGen
        <*> pubKeyGen
        <*> Arb.generate<uint8>
        <*> (Gen.optionOf pushScriptGen)