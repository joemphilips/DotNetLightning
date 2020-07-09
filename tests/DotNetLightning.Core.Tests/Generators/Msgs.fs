module MsgGenerators

open DotNetLightning.Serialize.Msgs
open DotNetLightning.Serialize
open DotNetLightning.Utils.OnionError
open PrimitiveGenerators
open FsCheck
open DotNetLightning.Utils.Primitives
open DotNetLightning.Utils

let (<*>) = Gen.apply

let private featuresGen =
    Gen.constant (1L <<< Feature.InitialRoutingSync.OptionalBitPosition)
    |> Gen.map(FeatureBit.CreateUnsafe)

let private chainHashGen =
    Gen.oneof(seq {
        yield Gen.constant NBitcoin.Consensus.Main.HashGenesisBlock;
        yield Gen.constant NBitcoin.Consensus.TestNet.HashGenesisBlock;
        yield Gen.constant NBitcoin.Consensus.RegTest.HashGenesisBlock;
    })
    
let genericTLVGen (known: uint64 list) =
    gen {
        let! t =
            Arb.generate<uint64>
            |> Gen.filter(fun v -> not <| (List.exists(fun knownInt -> v = knownInt) known))
        let! v = Arb.generate<NonNull<byte[]>>
        return { GenericTLV.Type = t; Value = v.Get }
    }
let initTLVGen =
    Gen.frequency[|
        (1, chainHashGen |> Gen.map(Array.singleton >> InitTLV.Networks))
        (1, genericTLVGen([1UL]) |> Gen.map(InitTLV.Unknown))
    |]
    
let initGen =
    Gen.map2 (fun f tlvS -> { Features = f; TLVStream = tlvS })
        featuresGen
        (initTLVGen |> Gen.map(Array.singleton))

let errorMsgGen = gen {
    let specificC = SpecificChannel <!> (ChannelId <!> uint256Gen)
    let allC = Gen.constant WhichChannel.All
    let! c = Gen.oneof [specificC; allC]
    let! d = bytesGen
    return {ChannelId = c; Data = d}
}

let pingGen =
    Gen.map2(fun pLen bLen -> { PongLen = pLen; BytesLen = bLen })
        Arb.generate<uint16>
        Arb.generate<uint16>

let pongGen =
    Gen.map(fun bLen -> { BytesLen = bLen })
        Arb.generate<uint16>

let openChannelGen =
    let constructor = fun arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19 -> {
            OpenChannelMsg.Chainhash = arg1
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
        <*> (BlockHeightOffset16 <!> Arb.generate<uint16>)
        <*> Arb.generate<uint16>
        <*> pubKeyGen
        <*> pubKeyGen
        <*> pubKeyGen
        <*> pubKeyGen
        <*> pubKeyGen
        <*> commitmentPubKeyGen
        <*> Arb.generate<uint8>
        <*> (Gen.optionOf pushScriptGen)

let acceptChannelGen =
    let constructor = fun a b c d e f g h i j k l m n o ->
        {
            TemporaryChannelId = a
            DustLimitSatoshis = b
            MaxHTLCValueInFlightMsat = c
            ChannelReserveSatoshis = d
            HTLCMinimumMSat = e
            MinimumDepth = f
            ToSelfDelay = g
            MaxAcceptedHTLCs = h
            FundingPubKey = i
            RevocationBasepoint = j
            PaymentBasepoint = k
            DelayedPaymentBasepoint = l
            HTLCBasepoint = m
            FirstPerCommitmentPoint = n
            ShutdownScriptPubKey = o
        }

    constructor
        <!> temporaryChannelGen
        <*> moneyGen
        <*> lnMoneyGen
        <*> moneyGen
        <*> lnMoneyGen
        <*> (Arb.generate<uint32> |> Gen.map(BlockHeightOffset32))
        <*> (BlockHeightOffset16 <!> Arb.generate<uint16>)
        <*> Arb.generate<uint16>
        <*> pubKeyGen
        <*> pubKeyGen
        <*> pubKeyGen
        <*> pubKeyGen
        <*> pubKeyGen
        <*> commitmentPubKeyGen
        <*> (Gen.optionOf pushScriptGen)

let fundingCreatedGen =
    let constructor = fun a b c d ->
        {
            TemporaryChannelId = a
            FundingTxId = b
            FundingOutputIndex = c
            Signature = d
        }

    constructor
        <!> temporaryChannelGen
        <*> (TxId <!> uint256Gen)
        <*> (TxOutIndex <!> Arb.generate<uint16>)
        <*> signatureGen

let fundingSignedGen = gen {
    let! c = ChannelId <!> uint256Gen
    let! s = signatureGen
    return {
        FundingSignedMsg.ChannelId = c
        Signature = s
    }
}

let fundingLockedGen = gen {
    let! c = ChannelId <!> uint256Gen
    let! pk = commitmentPubKeyGen
    return {ChannelId = c; NextPerCommitmentPoint = pk}
}

let shutdownGen = gen {
    let! c = ChannelId <!> uint256Gen
    let! sc = pushScriptGen 
    return { ChannelId = c; ScriptPubKey = sc }
}

let closingSignedGen = gen {
    let! c = ChannelId <!> uint256Gen
    let! m = moneyGen
    let! s = signatureGen
    return { ChannelId=c; FeeSatoshis=m; Signature=s}
}

let onionPacketGen = gen {
    // let! v = Arb.generate<uint8>
    let! pk = pubKeyGen |> Gen.map(fun pk -> pk.ToBytes())
    let! hopData = bytesOfNGen(1300)
    let! hmac = uint256Gen
    return { Version = 0uy; PublicKey=pk; HopData=hopData; HMAC=hmac }
}

let updateAddHTLCGen = gen {
    let! c = ChannelId <!> uint256Gen
    let! htlc = HTLCId <!> Arb.generate<uint64>
    let! amount = lnMoneyGen
    let! paymentHash = PaymentHash <!> uint256Gen
    let! cltvE = Arb.generate<uint32>
    let! onionRoutingPacket = onionPacketGen
    return {
        ChannelId = c
        HTLCId = htlc
        Amount = amount
        PaymentHash = paymentHash
        CLTVExpiry = cltvE |> BlockHeight
        OnionRoutingPacket = onionRoutingPacket
    }
}

let updateFulfillHTLCGen = gen {
    let! c = ChannelId <!> uint256Gen
    let! htlc = HTLCId <!> Arb.generate<uint64>
    let! paymentPreimage = PaymentPreimage.Create <!> bytesOfNGen PaymentPreimage.LENGTH
    return { ChannelId = c; HTLCId = htlc; PaymentPreimage = paymentPreimage }
}


let updateFailHTLCGen = gen {
    let! c = ChannelId <!> uint256Gen
    let! htlc = HTLCId <!> Arb.generate<uint64>
    let! reason = bytesGen |> Gen.map (fun bs -> { Data = bs})
    return {
        ChannelId = c
        HTLCId = htlc
        Reason = reason
    }
}

let updateFailMalformedHTLCGen = gen {
    let! c = ChannelId <!> uint256Gen
    let! htlc = HTLCId <!> Arb.generate<uint64>
    let! sha256 = uint256Gen
    let! ec = FailureCode <!> Arb.generate<uint16>
    return {
        ChannelId = c
        HTLCId = htlc
        Sha256OfOnion = sha256
        FailureCode = ec
    }
}

let commitmentSignedGen = gen {
    let! c = ChannelId <!> uint256Gen
    let! s = signatureGen
    let! n = Arb.generate<uint16>
    let! ss = Gen.listOfLength (int n) signatureGen
    return {
        ChannelId = c
        Signature = s
        HTLCSignatures = ss
    }
}

let revokeAndACKGen = gen {
    let! c = ChannelId <!> uint256Gen
    let! revocationKey = revocationKeyGen
    let! pk = commitmentPubKeyGen 
    return {
        ChannelId = c
        PerCommitmentSecret = revocationKey
        NextPerCommitmentPoint = pk
    }
}
let updateFeeGen = gen {
    let! c = ChannelId <!> uint256Gen
    let! fr = FeeRatePerKw <!> Arb.generate<uint32>
    return {
        ChannelId = c
        FeeRatePerKw = fr
    }
}

let private dataLossProtectGen = gen {
    let! revocationKey = revocationKeyGen
    let! pk = commitmentPubKeyGen
    return {
        YourLastPerCommitmentSecret = Some revocationKey
        MyCurrentPerCommitmentPoint = pk
    }
}

let channelReestablishGen = gen {
    let! c = ChannelId <!> uint256Gen
    let! n1 = commitmentNumberGen
    let! n2 = commitmentNumberGen
    let! d = Gen.optionOf dataLossProtectGen

    return {
        ChannelId = c
        NextCommitmentNumber = n1
        NextRevocationNumber = n2
        DataLossProtect = d
    }
}

let announcementSignaturesGen = gen {
    let! c = ChannelId <!> uint256Gen
    let! s = ShortChannelId.FromUInt64 <!> Arb.generate<uint64>
    let! ns = signatureGen
    let! bs = signatureGen
    return {
        ChannelId = c
        ShortChannelId = s
        NodeSignature = ns
        BitcoinSignature = bs
    }
}

let private ipV4AddressGen = gen {
    let! addr = bytesOfNGen(4)
    let! port = Arb.generate<uint16>
    return NetAddress.IPv4 { IPv4Or6Data.Addr = addr; Port = port }
}

let private ipV6AddressGen = gen {
    let! addr = bytesOfNGen(16)
    let! port = Arb.generate<uint16>
    return NetAddress.IPv6 { IPv4Or6Data.Addr = addr; Port = port }
}

let private onionV2AddressGen = gen {
    let! addr = bytesOfNGen(10)
    let! port = Arb.generate<uint16>
    return NetAddress.OnionV2 { OnionV2EndPoint.Addr = addr; Port = port }
}

let private onionV3AddressGen = gen {
    let! cs = Arb.generate<uint16>
    let! pk = bytesOfNGen(32)
    let! port = Arb.generate<uint16>
    let! v = Arb.generate<uint8>
    return NetAddress.OnionV3 { OnionV3EndPoint.CheckSum = cs
                                ed25519PubKey = pk
                                Version = v
                                Port = port }
}


let private netAddressesGen = gen {
    let! ipv4 = Gen.optionOf(ipV4AddressGen)
    let! ipv6 = Gen.optionOf(ipV6AddressGen)
    let! onionv2 = Gen.optionOf(onionV2AddressGen)
    let! onionv3 = Gen.optionOf(onionV3AddressGen)
    return [|ipv4; ipv6; onionv2; onionv3|] 
} 

let unsignedNodeAnnouncementGen = gen {
    let! f = featuresGen
    let! t = Arb.generate<uint32>
    let! nodeId = NodeId <!> pubKeyGen
    let! rgb = (fun r g b -> {Red = r; Green = g; Blue = b})
                <!> Arb.generate<uint8>
                <*> Arb.generate<uint8>
                <*> Arb.generate<uint8>

    let! a = uint256Gen
    let! addrs = netAddressesGen |> Gen.map (Array.choose id)
    let! eAddrs = bytesGen |> Gen.filter(fun b -> b.Length = 0 || (b.[0] <> 1uy && b.[0] <> 2uy && b.[0] <> 3uy && b.[0] <> 4uy))
    let! ed = bytesGen

    return {
        Features = f
        Timestamp = t
        NodeId = nodeId
        RGB = rgb
        Alias = a
        Addresses = addrs
        ExcessAddressData = eAddrs
        ExcessData = ed
    }
}

let nodeAnnouncementGen = gen {
    let! c = unsignedNodeAnnouncementGen
    let! s = signatureGen
    return {
        NodeAnnouncementMsg.Contents = c
        Signature = s
    }
}

let private unsignedChannelAnnouncementGen = gen {
    let! g = featuresGen
    let! ch = uint256Gen
    let! s = ShortChannelId.FromUInt64 <!> Arb.generate<uint64>
    let! n1 = NodeId <!> pubKeyGen
    let! n2 = NodeId <!> pubKeyGen
    let! b1 = pubKeyGen |> Gen.map ComparablePubKey
    let! b2 = pubKeyGen |> Gen.map ComparablePubKey
    let! e = bytesGen

    return {
        Features = g
        ChainHash = ch
        ShortChannelId = s
        NodeId1 = n1
        NodeId2 = n2
        BitcoinKey1 = b1
        BitcoinKey2 = b2
        ExcessData = e
    }
}

let channelAnnouncementGen = gen {
    let! n1 = signatureGen
    let! n2 = signatureGen
    let! b1 = signatureGen
    let! b2 = signatureGen
    let! c = unsignedChannelAnnouncementGen
    return {
        NodeSignature1 = n1
        NodeSignature2 = n2
        BitcoinSignature1 = b1
        BitcoinSignature2 = b2
        Contents = c
    }
}

let private unsignedChannelUpdateGen = gen {
    let! ch = uint256Gen
    let! s = ShortChannelId.FromUInt64 <!> Arb.generate<uint64>
    let! ts = Arb.generate<uint32>
    let! messageFlags = Arb.generate<uint8>
    let! channelFlags = Arb.generate<uint8>
    let! cltvE = BlockHeightOffset16 <!> Arb.generate<uint16>
    let! htlcMin = lnMoneyGen
    let! feeBase = lnMoneyGen
    let! feePM = Arb.generate<uint32>
    let! maximum =
        if ((messageFlags &&& 0b00000001uy) = 1uy) then
            lnMoneyGen |> Gen.map Some
        else
            Gen.constant None

    return {
        ChainHash = ch
        ShortChannelId = s
        Timestamp = ts
        MessageFlags = messageFlags
        ChannelFlags = channelFlags
        CLTVExpiryDelta = cltvE
        HTLCMinimumMSat = htlcMin
        FeeBaseMSat = feeBase
        FeeProportionalMillionths = feePM
        HTLCMaximumMSat = maximum
    }
}

let channelUpdateGen = gen {
    let! s = signatureGen
    let! c = unsignedChannelUpdateGen

    return {
        Signature = s
        Contents = c
    }
}

let private encodingTypeGen =
    Gen.oneof[
        Gen.constant(EncodingType.SortedPlain)
        Gen.constant(EncodingType.ZLib)
    ]

let private queryFlagGen: Gen<QueryFlags> =
    Arb.generate<uint8>
    |> Gen.filter(fun x -> 0xfduy > x) // query flags should be represented as 1 byte in VarInt encoding
    |> Gen.map QueryFlags.Create
    
let private queryFlagsGen (length: int): Gen<QueryShortChannelIdsTLV> =
    gen {
        let! ty = encodingTypeGen
        let! flags = Gen.arrayOfLength length queryFlagGen
        return QueryShortChannelIdsTLV.QueryFlags(ty, flags)
    }
    
let queryShortChannelIdsGen = gen {
    let! n = Arb.generate<PositiveInt>
    let! s = chainHashGen
    let! ty = encodingTypeGen
    let! ids = Gen.arrayOfLength n.Get shortChannelIdsGen
    let! knownTLVs = queryFlagsGen n.Get |> Gen.map Array.singleton
    let! unknownTLVs = (genericTLVGen([1UL]) |> Gen.map(QueryShortChannelIdsTLV.Unknown)) |> Gen.optionOf |> Gen.map(Option.toArray)
    let tlvs = [knownTLVs; unknownTLVs] |> Array.concat
    return { QueryShortChannelIdsMsg.ChainHash = s
             ShortIdsEncodingType = ty
             ShortIds = ids
             TLVs = tlvs }
}

let replyShortChannelIdsEndGen = gen {
    let! b = Arb.generate<bool>
    let! chainHash = chainHashGen
    return {
        ReplyShortChannelIdsEndMsg.Complete = b
        ChainHash = chainHash
    }
}

let private queryChannelRangeTLVGen =
    Gen.frequency[|
        (1, Arb.generate<uint8>  |> Gen.filter(fun x -> 0xfduy > x) |> Gen.map(QueryOption.Create >> QueryChannelRangeTLV.Opt))
        (1, genericTLVGen([1UL]) |> Gen.map(QueryChannelRangeTLV.Unknown))
    |]
let queryChannelRangeGen: Gen<QueryChannelRangeMsg> = gen {
    let! chainHash = chainHashGen
    let! firstBlockNum = Arb.generate<uint32> |> Gen.map(BlockHeight)
    let! nBlocks = Arb.generate<uint32>
    let! tlvs = queryChannelRangeTLVGen |> Gen.arrayOf
    return {
        QueryChannelRangeMsg.ChainHash = chainHash
        FirstBlockNum = firstBlockNum
        NumberOfBlocks = nBlocks
        TLVs = tlvs
    }
}

let private timestampPairsGen (length: int): Gen<ReplyChannelRangeTLV> =
    gen {
        let! ty = encodingTypeGen
        let! timestampPairs =
            (Arb.generate<uint32>, Arb.generate<uint32>)
            ||> Gen.map2(fun a b -> { TwoTimestamps.NodeId1 = a; NodeId2 = b })
            |> Gen.arrayOfLength length
        return ReplyChannelRangeTLV.Timestamp(ty, timestampPairs)
    }
    
let private checksumPairsGen n = gen {
    let! pairs =
        (Arb.generate<uint32>, Arb.generate<uint32>)
        ||> Gen.map2(fun a b -> { TwoChecksums.NodeId1 = a; NodeId2 = b })
        |> Gen.arrayOfLength n
    return ReplyChannelRangeTLV.Checksums(pairs)
}
    
let private replyChannelRangeTLVGen n =
    Gen.frequency [
        (1, timestampPairsGen n)
        (1, checksumPairsGen n)
        (1, genericTLVGen([1UL; 3UL]) |> Gen.map(ReplyChannelRangeTLV.Unknown))
    ]
let replyChannelRangeGen: Gen<ReplyChannelRangeMsg> = gen {
    let! ch = chainHashGen
    let! firstBlockNum = Arb.generate<uint32> |> Gen.map(BlockHeight)
    let! nBlocks = Arb.generate<uint32>
    let! complete = Arb.generate<bool>
    let! n = Arb.generate<PositiveInt>
    let! eType = encodingTypeGen
    let! shortChannelIds = shortChannelIdsGen |> Gen.arrayOfLength n.Get
    let! tlvs = replyChannelRangeTLVGen n.Get |> Gen.map(Array.singleton)
    return {
        ReplyChannelRangeMsg.ChainHash = ch
        FirstBlockNum = firstBlockNum
        NumOfBlocks = nBlocks
        Complete = complete
        ShortIdsEncodingType = eType
        ShortIds = shortChannelIds
        TLVs = tlvs
    }
}

let gossipTimestampFilterGen: Gen<GossipTimestampFilterMsg> = gen {
    let! ch = chainHashGen
    let! tsFirst = Arb.generate<uint32>
    let! tsRange = Arb.generate<uint32>
    return { ChainHash = ch; FirstTimestamp = tsFirst; TimestampRange = tsRange }
}

let private hopPayloadTlvGen =
    let paymentDataGen: Gen<_ * _> =
        ((PaymentPreimage.Create <!> bytesOfNGen(32)), (lnMoneyGen))
        ||> Gen.map2(fun a b -> (a, b))
    Gen.frequency
        [(1, AmountToForward <!> lnMoneyGen)
         (1, OutgoingCLTV <!> Arb.generate<uint32>)
         (1, HopPayloadTLV.ShortChannelId <!> shortChannelIdsGen)
         (1, PaymentData <!> paymentDataGen)]
let private onionPayloadTlvGen =
    Gen.frequency
        [(1, genericTLVGen([2UL; 4UL; 6UL; 8UL]) |> Gen.map(HopPayloadTLV.Unknown))
         (4, hopPayloadTlvGen)]
let tlvOnionPayloadGen =
    gen {
        let! tlv = Gen.nonEmptyListOf onionPayloadTlvGen |> Gen.map(List.toArray)
        let! hmac = uint256Gen
        return (tlv, hmac) |> OnionPayload.TLVPayload
    }
let private legacyOnionPayloadGen =
    gen {
        let! scid = shortChannelIdsGen
        let! amt = lnMoneyGen
        let! cltv = Arb.generate<uint32>
        return { ShortChannelId = scid; AmtToForward = amt; OutgoingCLTVValue = cltv } |> OnionPayload.Legacy
    }
let onionPayloadGen =
    Gen.frequency
        [(3, tlvOnionPayloadGen)
         (1, legacyOnionPayloadGen)]
