module Generators

open DotNetLightning.Payment
open DotNetLightning.Payment.LSAT
open DotNetLightning.Serialize
open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open FsCheck
open PrimitiveGenerators
open MsgGenerators
open PaymentGenerators


type PrimitiveGenerators =
    static member ECDSASignature() : Arbitrary<LNECDSASignature> =
        Arb.fromGen(signatureGen)

    static member UInt256(): Arbitrary<uint256> =
        Arb.fromGen(uint256Gen)
        
    static member PubKey() = Arb.fromGen(pubKeyGen)
    
    static member NodeId() = Arb.fromGen(NodeId <!> pubKeyGen)
    
type P2PMsgGenerators =
    static member Init() : Arbitrary<Init> =
        Arb.fromGen(initGen)

    static member ErrorMsg(): Arbitrary<ErrorMessage> =
        Arb.fromGen(errorMsgGen)

    static member Ping() : Arbitrary<Ping> =
        Arb.fromGen(pingGen)

    static member Pong() : Arbitrary<Pong> =
        Arb.fromGen(pongGen)

    static member OpenChannel(): Arbitrary<OpenChannel> =
        Arb.fromGen(openChannelGen)

    static member AcceptChannel(): Arbitrary<AcceptChannel> =
        Arb.fromGen(acceptChannelGen)

    static member FundingCreated(): Arbitrary<FundingCreated> =
        Arb.fromGen(fundingCreatedGen)

    static member FundingSigned(): Arbitrary<FundingSigned> =
        Arb.fromGen(fundingSignedGen)

    static member FundingLocked(): Arbitrary<FundingLocked> =
        Arb.fromGen(fundingLockedGen)

    static member Shutdown(): Arbitrary<Shutdown> =
        Arb.fromGen(shutdownGen)

    static member ClosingSigned(): Arbitrary<ClosingSigned> =
        Arb.fromGen(closingSignedGen)

    static member OnionPacket(): Arbitrary<OnionPacket> =
        Arb.fromGen(onionPacketGen)

    static member UpdateAddHTLC(): Arbitrary<UpdateAddHTLC> =
        Arb.fromGen(updateAddHTLCGen)

    static member UpdateFulfillHTLC(): Arbitrary<UpdateFulfillHTLC> =
        Arb.fromGen(updateFulfillHTLCGen)

    static member UpdateFailHTLC(): Arbitrary<UpdateFailHTLC> =
        Arb.fromGen(updateFailHTLCGen)

    static member UpdateFailMalformedHTLC(): Arbitrary<UpdateFailMalformedHTLC> =
        Arb.fromGen(updateFailMalformedHTLCGen)

    static member CommitmentSigned(): Arbitrary<CommitmentSigned> =
        Arb.fromGen(commitmentSignedGen)

    static member RevokeAndACK(): Arbitrary<RevokeAndACK> =
        Arb.fromGen(revokeAndACKGen)

    static member UdpateFee(): Arbitrary<UpdateFee> =
        Arb.fromGen(updateFeeGen)

    static member ChannelReestablish(): Arbitrary<ChannelReestablish> =
        Arb.fromGen(channelReestablishGen)

    static member AnnouncementSignature(): Arbitrary<AnnouncementSignatures> =
        Arb.fromGen(announcementSignaturesGen)

    static member UnsignedNodeAnnouncement(): Arbitrary<UnsignedNodeAnnouncement> =
        Arb.fromGen unsignedNodeAnnouncementGen

    static member NodeAnnouncement(): Arbitrary<NodeAnnouncement> =
        Arb.fromGen nodeAnnouncementGen

    static member ChannelAnnouncement(): Arbitrary<ChannelAnnouncement> =
        Arb.fromGen channelAnnouncementGen

    static member ChannelUpdate(): Arbitrary<ChannelUpdate> =
        Arb.fromGen channelUpdateGen
        
    static member QueryShortChannelIds(): Arbitrary<QueryShortChannelIds> =
        Arb.fromGen queryShortChannelIdsGen

    static member ReplyShortChannelIds() =
        Arb.fromGen(replyShortChannelIdsEndGen)
        
    static member QueryChannelRange() = Arb.fromGen queryChannelRangeGen
    
    static member ReplyChannelRange = Arb.fromGen replyChannelRangeGen
    static member GossipTimestampFilter = Arb.fromGen gossipTimestampFilterGen
    static member OnionPayload() = Arb.fromGen(onionPayloadGen)

    static member P2PMsg(): Arbitrary<ILightningMsg> =
        Gen.oneof [
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
            updateFailMalformedHTLCGen |> Gen.map(fun i -> i :> ILightningMsg)
            commitmentSignedGen |> Gen.map(fun i -> i :> ILightningMsg)
            revokeAndACKGen |> Gen.map(fun i -> i :> ILightningMsg)
            updateFeeGen |> Gen.map(fun i -> i :> ILightningMsg)
            channelReestablishGen |> Gen.map(fun i -> i :> ILightningMsg)
            announcementSignaturesGen |> Gen.map(fun i -> i :> ILightningMsg)
            nodeAnnouncementGen |> Gen.map(fun i -> i :> ILightningMsg)
            channelAnnouncementGen |> Gen.map(fun i -> i :> ILightningMsg)
            channelUpdateGen |> Gen.map(fun i -> i :> ILightningMsg)
            queryShortChannelIdsGen |> Gen.map(fun i -> i :> ILightningMsg)
            replyShortChannelIdsEndGen |> Gen.map(fun i -> i :> ILightningMsg)
            queryChannelRangeGen |> Gen.map(fun i -> i :> ILightningMsg)
            replyChannelRangeGen |> Gen.map(fun i -> i :> ILightningMsg)
            gossipTimestampFilterGen |> Gen.map(fun i -> i :> ILightningMsg)
        ]
        |> Arb.fromGen
        
        
type PaymentGenerators =
    static member MacaroonIdentifier: Arbitrary<MacaroonIdentifier> =
        macaroonIdGen |> Arb.fromGen
