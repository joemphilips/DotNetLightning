module PaymentGenerators

open DotNetLightning.Serialization
open NBitcoin
open NBitcoin
open NBitcoin.Crypto
open NBitcoin.DataEncoders
open PrimitiveGenerators
open DotNetLightning.Utils
open DotNetLightning.Payment
open System
open FsCheck
open ResultUtils
open MsgGenerators

let controlStrGen =
    Arb.generate<char> // It seems that `Arb.generate<string>` has a bug which causes stack overflow.
    |> Gen.arrayOf
    |> Gen.filter(Seq.exists(Char.IsControl) >> not)
    |> Gen.map(String)

let descTagGen =
    let ascii = ASCIIEncoder()
    let s = seq [
        controlStrGen |> Gen.map(TaggedField.DescriptionTaggedField)
        controlStrGen |> Gen.map(ascii.DecodeData >> Hashes.DoubleSHA256 >> TaggedField.DescriptionHashTaggedField)
    ]
    s |> Gen.oneof

let fallbackAddrGen =
    seq [
        Gen.constant(BitcoinAddress.Create("2Myzv4XfqMyNZFKHUq3Jqv7zbw1AAHY3a2X", Network.RegTest))
        Gen.constant (BitcoinAddress.Create("bcrt1q5a75p74lj2269jcj83tlrzcl6vf50atn3dnt7p", Network.RegTest))
    ]
    |> Gen.oneof
    |> Gen.map(FallbackAddress.FromAddress)
    |> Gen.filter(Result.isOk) |> Gen.map(Result.deref)
    |> Gen.map(TaggedField.FallbackAddressTaggedField)

let routingInfoGen =
    let g = gen {
        let! nodeId = pubKeyGen |> Gen.map(NodeId)
        let! shortChannelId = shortChannelIdsGen
        let! feeBase = lnMoneyGen
        let! feeProportional = Arb.generate<uint32>
        let! expiryDelta = Arb.generate<uint16>
        return ExtraHop.TryCreate(nodeId, shortChannelId, feeBase, feeProportional, BlockHeightOffset16 expiryDelta)
    }
    let g = g |> Gen.filter(Result.isOk) |> Gen.map(Result.deref)
    seq [
        (20, g |> Gen.listOfLength 1)
        (20, g |> Gen.listOfLength 2)
        (1, g |> Gen.listOfLength 3)
    ]
    |> Gen.frequency
    |> Gen.map(TaggedField.RoutingInfoTaggedField)

let paymentRequestGen =
    let taggedFieldGen =
        seq [
            descTagGen
            uint256Gen |> Gen.map(PaymentHash >> PaymentHashTaggedField)
            Arb.generate<DateTimeOffset>
                |> Gen.filter(fun d -> d.IsValidUnixTime())
                |> Gen.map(TaggedField.ExpiryTaggedField)
            completeFeaturesGen |> Gen.map(FeaturesTaggedField)
            fallbackAddrGen
            routingInfoGen
            uint256Gen |> Gen.map PaymentSecretTaggedField
            Arb.generate<uint32> |> Gen.map(BlockHeightOffset32 >> MinFinalCltvExpiryTaggedField)
        ]
        |> Gen.sequence
    let taggedFieldsGen = gen {
        let! f = taggedFieldGen
        return { TaggedFields.Fields = f }
    }
    let prefixGen = Gen.oneof[
        Gen.constant "lnbcrt"
        Gen.constant "lntb"
        Gen.constant "lnbc"
    ]
    gen {
        let! p = prefixGen
        let! m = moneyGen |> Gen.map(fun m -> m.ToLNMoney()) |> Gen.optionOf
        let! t = Arb.generate<DateTimeOffset> |> Gen.filter(fun d -> d.IsValidUnixTime())
        let! nodeSecret = keyGen
        let! tags = taggedFieldsGen
        let! shouldUseExplicitNodeId = Arb.generate<bool>
        let tags =
            if shouldUseExplicitNodeId then
                { tags with Fields = NodeIdTaggedField(nodeSecret.PubKey |> NodeId) :: tags.Fields }
            else
                tags
        return PaymentRequest.TryCreate(p, m, t, tags, nodeSecret)
    }
    |> Gen.filter(Result.isOk)
    |> Gen.map(Result.deref)

open DotNetLightning.Payment.LSAT
open DotNetLightning.Utils.Primitives
open FsCheck
open PrimitiveGenerators
#if BouncyCastle

let private macaroonIdV1Gen =
    (uint256Gen |> Gen.map(PaymentHash.PaymentHash), uint256Gen)
    ||> Gen.map2(fun p u -> { MacaroonIdentifierV0.PaymentHash = p
                              TokenId = u })
    |> Gen.map(MacaroonIdentifier.V0)
let private macaroonUnknownIdGen(knownVersions: uint16 seq) =
    gen {
        let! t =
            Arb.generate<uint16>
            |> Gen.filter(fun v -> not <| (Seq.exists(fun knownInt -> v = knownInt) knownVersions))
        let! v = Arb.generate<NonNull<byte[]>>
        return MacaroonIdentifier.UnknownVersion(t, v.Get)
    }

let macaroonIdGen: Gen<MacaroonIdentifier> =
    Gen.oneof [
        macaroonIdV1Gen
        macaroonUnknownIdGen([0us])
    ]

#endif
