module Generators

open DotNetLightning.Serialize.Msgs
open FsCheck
open MsgGenerators


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

    static member NodeAnnouncement(): Arbitrary<NodeAnnouncement> =
        Arb.fromGen nodeAnnouncementGen

    static member ChannelAnnouncement(): Arbitrary<ChannelAnnouncement> =
        Arb.fromGen channelAnnouncementGen

    static member ChannelUpdate(): Arbitrary<ChannelUpdate> =
        Arb.fromGen channelUpdateGen