module MsgGenerators

open DotNetLightning.Serialize.Msgs
open DotNetLightning.Utils.OnionError
open PrimitiveGenerators
open FsCheck
open DotNetLightning.Utils.Primitives

let (<*>) = Gen.apply

let private globalFeaturesGen =
    Gen.constant ([|0b00000000uy|]) |> Gen.map GlobalFeatures.Flags

let private localFeaturesGen =
    Gen.constant ([|0b01010101uy|]) |> Gen.map LocalFeatures.Flags

let initGen =
    Gen.map2 (fun g l -> { GlobalFeatures = g; LocalFeatures = l})
        globalFeaturesGen
        localFeaturesGen

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
        <*> (Arb.generate<uint32> |> Gen.map(BlockHeight))
        <*> (BlockHeightOffset <!> Arb.generate<uint16>)
        <*> Arb.generate<uint16>
        <*> pubKeyGen
        <*> pubKeyGen
        <*> pubKeyGen
        <*> pubKeyGen
        <*> pubKeyGen
        <*> pubKeyGen
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
        FundingSigned.ChannelId = c
        Signature = s
    }
}

let fundingLockedGen = gen {
    let! c = ChannelId <!> uint256Gen
    let! pk = pubKeyGen
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
        AmountMSat = amount
        PaymentHash = paymentHash
        CLTVExpiry = cltvE |> BlockHeight
        OnionRoutingPacket = onionRoutingPacket
    }
}

let updateFulfillHTLCGen = gen {
    let! c = ChannelId <!> uint256Gen
    let! htlc = HTLCId <!> Arb.generate<uint64>
    let! paymentPreimage = PaymentPreimage <!> bytesOfNGen(32)
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
    let! paymentPreimage = PaymentPreimage <!> bytesOfNGen(32)
    let! pk = pubKeyGen 
    return {
        ChannelId = c
        PerCommitmentSecret = paymentPreimage
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
    let! paymentPreimage = PaymentPreimage <!> bytesOfNGen(32)
    let! pk = pubKeyGen
    return {
        YourLastPerCommitmentSecret = paymentPreimage
        MyCurrentPerCommitmentPoint = pk
    }
}

let channelReestablishGen = gen {
    let! c = ChannelId <!> uint256Gen
    let! n1 = Arb.generate<uint64>
    let! n2 = Arb.generate<uint64>
    let! d = Gen.optionOf dataLossProtectGen

    return {
        ChannelId = c
        NextLocalCommitmentNumber = n1
        NextRemoteCommitmentNumber = n2
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
    let! f = globalFeaturesGen
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
        NodeAnnouncement.Contents = c
        Signature = s
    }
}

let private unsignedChannelAnnouncementGen = gen {
    let! g = globalFeaturesGen
    let! ch = uint256Gen
    let! s = ShortChannelId.FromUInt64 <!> Arb.generate<uint64>
    let! n1 = NodeId <!> pubKeyGen
    let! n2 = NodeId <!> pubKeyGen
    let! b1 = pubKeyGen
    let! b2 = pubKeyGen
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
    let! f = Arb.generate<uint16>
    let! cltvE = BlockHeightOffset <!> Arb.generate<uint16>
    let! htlcMin = lnMoneyGen
    let! feeBase = lnMoneyGen
    let! feePM = Arb.generate<uint32>
    let! ed = bytesGen


    return {
        ChainHash = ch
        ShortChannelId = s
        Timestamp = ts
        Flags = f
        CLTVExpiryDelta = cltvE
        HTLCMinimumMSat = htlcMin
        FeeBaseMSat = feeBase
        FeeProportionalMillionths = feePM
        ExcessData = ed
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