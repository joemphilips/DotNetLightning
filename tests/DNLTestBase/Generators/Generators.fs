module Generators

open DotNetLightning.Payment.LSAT
open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Serialization.Msgs
open FsCheck
open NBitcoin.Scripting
open PrimitiveGenerators
open MsgGenerators
open PaymentGenerators


type PrimitiveGenerators =
    static member ECDSASignature() : Arbitrary<LNECDSASignature> =
        Arb.fromGen(signatureGen)

    static member UInt256() : Arbitrary<uint256> =
        Arb.fromGen(uint256Gen)

    static member PubKey() =
        Arb.fromGen(pubKeyGen)

    static member NodeId() =
        Arb.fromGen(NodeId <!> pubKeyGen)

    static member Key() =
        Arb.fromGen(keyGen)

    static member OutPoint() =
        Arb.fromGen(outPointGen)

    /// Taken from bitcoin-core unit test.
    static member OutputDescriptor() =
        let testVectors =
            seq {
                "addr(2N7nD1pG3kK3DYaP34jQKbxB3JnEfMbVea7)"
                "pk(0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798)"
                "pkh(02c6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee5)"
                "wpkh(02f9308a019258c31049344f85f89d5229b531c845836f99b08601f113bce036f9)"
                "sh(wpkh(03fff97bd5755eeea420453a14355235d382f6472f8568a18b2f057a1460297556))"
                "combo(0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798)"
                "sh(wsh(pkh(02e493dbf1c10d80f3581e4904930b1404cc6c13900ee0758474fa94abe8c4cd13)))"
                "multi(1,022f8bde4d1a07209355b4a7250a5c5128e88b84bddc619ab7cba8d569b240efe4,025cbdf0646e5db4eaa398f365f2ea7a0e3d419b7e0330e39ce92bddedcac4f9bc)"
                "sh(multi(2,022f01e5e15cca351daff3843fb70f3c2f0a1bdd05e5af888a67784ef3e10a2a01,03acd484e2f0c7f65309ad178a9f559abde09796974c57e714c35f110dfc27ccbe))"
                "wsh(multi(2,03a0434d9e47f3c86235477c7b1ae6ae5d3442d49b1943c2b752a68e2a47e247c7,03774ae7f858a9411e5ef4246b70c65aac5649980be5c17891bbec17895da008cb,03d01115d548e7561b15c38f004d734633687cf4419620095bc5b0f47070afe85a))"
                "sh(wsh(multi(1,03f28773c2d975288bc7d1d205c3748651b075fbc6610e58cddeeddf8f19405aa8,03499fdf9e895e719cfd64e67f07d38e3226aa7b63678949e6e49b241a60e823e4,02d7924d4f7d43ea965a465ae3095ff41131e5946f3c85f79e44adbcf8e27e080e)))"
            }
            |> Seq.map(
                (fun x -> OutputDescriptor.Parse(x, Network.RegTest, false))
                >> Gen.constant
            )

        Gen.oneof testVectors |> Arb.fromGen

type NonNullOptionGenerator =
    static member NonNullOptionString() =
        Arb.Default.Option<string>()
        |> Arb.convert
            (Option.bind(fun v ->
                if v |> isNull then
                    None
                else
                    Some v
            ))
            id

    static member NonNullOptionArray<'T>() =
        Arb.Default.Option<'T []>()
        |> Arb.convert
            (Option.bind(fun v ->
                if v |> isNull then
                    None
                else
                    Some v
            ))
            id

type P2PMsgGenerators =
    static member Init() : Arbitrary<InitMsg> =
        Arb.fromGen(initGen)

    static member ErrorMsg() : Arbitrary<ErrorMsg> =
        Arb.fromGen(errorMsgGen)

    static member Ping() : Arbitrary<PingMsg> =
        Arb.fromGen(pingGen)

    static member Pong() : Arbitrary<PongMsg> =
        Arb.fromGen(pongGen)

    static member OpenChannel() : Arbitrary<OpenChannelMsg> =
        Arb.fromGen(openChannelGen)

    static member AcceptChannel() : Arbitrary<AcceptChannelMsg> =
        Arb.fromGen(acceptChannelGen)

    static member FundingCreated() : Arbitrary<FundingCreatedMsg> =
        Arb.fromGen(fundingCreatedGen)

    static member FundingSigned() : Arbitrary<FundingSignedMsg> =
        Arb.fromGen(fundingSignedGen)

    static member FundingLocked() : Arbitrary<FundingLockedMsg> =
        Arb.fromGen(fundingLockedGen)

    static member Shutdown() : Arbitrary<ShutdownMsg> =
        Arb.fromGen(shutdownGen)

    static member ClosingSigned() : Arbitrary<ClosingSignedMsg> =
        Arb.fromGen(closingSignedGen)

    static member OnionPacket() : Arbitrary<OnionPacket> =
        Arb.fromGen(onionPacketGen)

    static member UpdateAddHTLC() : Arbitrary<UpdateAddHTLCMsg> =
        Arb.fromGen(updateAddHTLCGen)

    static member UpdateFulfillHTLC() : Arbitrary<UpdateFulfillHTLCMsg> =
        Arb.fromGen(updateFulfillHTLCGen)

    static member UpdateFailHTLC() : Arbitrary<UpdateFailHTLCMsg> =
        Arb.fromGen(updateFailHTLCGen)

    static member UpdateFailMalformedHTLC
        ()
        : Arbitrary<UpdateFailMalformedHTLCMsg> =
        Arb.fromGen(updateFailMalformedHTLCGen)

    static member CommitmentSigned() : Arbitrary<CommitmentSignedMsg> =
        Arb.fromGen(commitmentSignedGen)

    static member RevokeAndACK() : Arbitrary<RevokeAndACKMsg> =
        Arb.fromGen(revokeAndACKGen)

    static member UpdateFee() : Arbitrary<UpdateFeeMsg> =
        Arb.fromGen(updateFeeGen)

    static member ChannelReestablish() : Arbitrary<ChannelReestablishMsg> =
        Arb.fromGen(channelReestablishGen)

    static member AnnouncementSignatures
        ()
        : Arbitrary<AnnouncementSignaturesMsg> =
        Arb.fromGen(announcementSignaturesGen)

    static member UnsignedNodeAnnouncement
        ()
        : Arbitrary<UnsignedNodeAnnouncementMsg> =
        Arb.fromGen unsignedNodeAnnouncementGen

    static member NodeAnnouncement() : Arbitrary<NodeAnnouncementMsg> =
        Arb.fromGen nodeAnnouncementGen

    static member ChannelAnnouncement() : Arbitrary<ChannelAnnouncementMsg> =
        Arb.fromGen channelAnnouncementGen

    static member ChannelUpdate() : Arbitrary<ChannelUpdateMsg> =
        Arb.fromGen channelUpdateGen

    static member QueryShortChannelIds() : Arbitrary<QueryShortChannelIdsMsg> =
        Arb.fromGen queryShortChannelIdsGen

    static member ReplyShortChannelIds() =
        Arb.fromGen(replyShortChannelIdsEndGen)

    static member QueryChannelRange() =
        Arb.fromGen queryChannelRangeGen

    static member ReplyChannelRange = Arb.fromGen replyChannelRangeGen
    static member GossipTimestampFilter = Arb.fromGen gossipTimestampFilterGen

    static member OnionPayload() =
        Arb.fromGen(onionPayloadGen)

    static member P2PMsg() : Arbitrary<ILightningMsg> =
        Gen.oneof
            [
                initGen |> Gen.map(fun i -> i :> ILightningMsg)
                errorMsgGen |> Gen.map(fun i -> i :> ILightningMsg)
                pingGen |> Gen.map(fun i -> i :> ILightningMsg)
                pongGen |> Gen.map(fun i -> i :> ILightningMsg)
                openChannelGen |> Gen.map(fun i -> i :> ILightningMsg)
                acceptChannelGen |> Gen.map(fun i -> i :> ILightningMsg)
                fundingCreatedGen |> Gen.map(fun i -> i :> ILightningMsg)
                fundingSignedGen |> Gen.map(fun i -> i :> ILightningMsg)
                fundingLockedGen |> Gen.map(fun i -> i :> ILightningMsg)
                shutdownGen |> Gen.map(fun i -> i :> ILightningMsg)
                closingSignedGen |> Gen.map(fun i -> i :> ILightningMsg)
                updateAddHTLCGen |> Gen.map(fun i -> i :> ILightningMsg)
                updateFulfillHTLCGen |> Gen.map(fun i -> i :> ILightningMsg)
                updateFailHTLCGen |> Gen.map(fun i -> i :> ILightningMsg)
                updateFailMalformedHTLCGen
                |> Gen.map(fun i -> i :> ILightningMsg)
                commitmentSignedGen |> Gen.map(fun i -> i :> ILightningMsg)
                revokeAndACKGen |> Gen.map(fun i -> i :> ILightningMsg)
                updateFeeGen |> Gen.map(fun i -> i :> ILightningMsg)
                channelReestablishGen |> Gen.map(fun i -> i :> ILightningMsg)
                announcementSignaturesGen
                |> Gen.map(fun i -> i :> ILightningMsg)
                nodeAnnouncementGen |> Gen.map(fun i -> i :> ILightningMsg)
                channelAnnouncementGen |> Gen.map(fun i -> i :> ILightningMsg)
                channelUpdateGen |> Gen.map(fun i -> i :> ILightningMsg)
                queryShortChannelIdsGen |> Gen.map(fun i -> i :> ILightningMsg)
                replyShortChannelIdsEndGen
                |> Gen.map(fun i -> i :> ILightningMsg)
                queryChannelRangeGen |> Gen.map(fun i -> i :> ILightningMsg)
                replyChannelRangeGen |> Gen.map(fun i -> i :> ILightningMsg)
                gossipTimestampFilterGen |> Gen.map(fun i -> i :> ILightningMsg)
            ]
        |> Arb.fromGen
