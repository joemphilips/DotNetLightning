module PrimitiveGenerators
open FsCheck
open NBitcoin
open DotNetLightning.Utils.Primitives
open DotNetLightning.Utils
open DotNetLightning.Crypto
open System
open DotNetLightning.Crypto

let boolGen = Gen.oneof [ gen { return true }; gen { return false } ]
let byteGen = byte <!> Gen.choose(0, 127)
let bytesGen = Gen.listOf(byteGen) |> Gen.map(List.toArray)
let bytesOfNGen(n) = Gen.listOfLength n byteGen |> Gen.map(List.toArray)
let uint48Gen = bytesOfNGen(6) |> Gen.map(fun bs -> UInt48.FromBytesBigEndian bs)
let uint256Gen = bytesOfNGen(32) |> Gen.map(fun bs -> uint256(bs))
let temporaryChannelGen = uint256Gen |> Gen.map ChannelId
let moneyGen = Arb.generate<uint64> |> Gen.map(Money.Satoshis)
let lnMoneyGen = Arb.generate<uint64> |> Gen.map(LNMoney.MilliSatoshis)

let shortChannelIdsGen = Arb.generate<uint64> |> Gen.map(ShortChannelId.FromUInt64)
// crypto stuffs

let keyGen = Gen.fresh (fun () -> new Key())

let pubKeyGen = gen {
    let! key = keyGen
    return key.PubKey
}

let perCommitmentSecretGen = gen {
    let! key = keyGen
    return PerCommitmentSecret key
}

let perCommitmentPointGen = gen {
    let! pubKey = pubKeyGen
    return PerCommitmentPoint pubKey
}

let fundingPubKeyGen = gen {
    let! pubKey = pubKeyGen
    return FundingPubKey pubKey
}

let paymentBasepointGen = gen {
    let! pubKey = pubKeyGen
    return PaymentBasepoint pubKey
}

let revocationBasepointGen = gen {
    let! pubKey = pubKeyGen
    return RevocationBasepoint pubKey
}

let delayedPaymentBasepointGen = gen {
    let! pubKey = pubKeyGen
    return DelayedPaymentBasepoint pubKey
}

let htlcBasepointGen = gen {
    let! pubKey = pubKeyGen
    return HtlcBasepoint pubKey
}

let commitmentNumberGen = gen {
    let! n = uint48Gen
    return CommitmentNumber n
}

let signatureGen: Gen<LNECDSASignature> = gen {
    let! h = uint256Gen
    let! k = keyGen
    return k.Sign(h, false) |> LNECDSASignature
}

let channelFlagsGen = gen {
    let! announceChannel = boolGen
    return {
        AnnounceChannel = announceChannel
    }
}

// scripts

let pushOnlyOpcodeGen = bytesOfNGen(4) |> Gen.map(Op.GetPushOp)
let pushOnlyOpcodesGen = Gen.listOf pushOnlyOpcodeGen

let pushScriptGen = Gen.nonEmptyListOf pushOnlyOpcodeGen |> Gen.map(fun ops -> Script(ops))
let shutdownScriptPubKeyGen = gen {
    let! pubKey = pubKeyGen
    return ShutdownScriptPubKey.FromPubKeyP2pkh pubKey
}

let cipherSeedGen = gen {
    let! v = Arb.generate<uint8>
    let! now = Arb.generate<uint16>
    let! entropy = bytesOfNGen 16
    let! salt = bytesOfNGen 5
    return {
        CipherSeed.InternalVersion = v
        Birthday = now
        Entropy = entropy
        Salt = salt
    }
}

