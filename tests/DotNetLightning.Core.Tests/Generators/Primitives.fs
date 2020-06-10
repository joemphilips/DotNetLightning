module PrimitiveGenerators
open FsCheck
open NBitcoin
open DotNetLightning.Utils.Primitives
open DotNetLightning.Utils
open System

let byteGen = byte <!> Gen.choose(0, 127)
let bytesGen = Gen.listOf(byteGen) |> Gen.map(List.toArray)
let bytesOfNGen(n) = Gen.listOfLength n byteGen |> Gen.map(List.toArray)
let uint256Gen = bytesOfNGen(32) |> Gen.map(fun bs -> uint256(bs))
let temporaryChannelGen = uint256Gen |> Gen.map ChannelId
let moneyGen = Arb.generate<uint64> |> Gen.map(Money.Satoshis)
let lnMoneyGen = Arb.generate<uint64> |> Gen.map(LNMoney.MilliSatoshis)

let shortChannelIdsGen = Arb.generate<uint64> |> Gen.map(ShortChannelId.FromUInt64)
// crypto stuffs

let keyGen = Gen.fresh (fun () -> Key())

let pubKeyGen = gen {
    let! key = keyGen
    return key.PubKey
}

let revocationKeyGen = gen {
    let! key = keyGen
    return RevocationKey key
}

let commitmentPubKeyGen = gen {
    let! pubKey = pubKeyGen
    return CommitmentPubKey pubKey
}

let signatureGen: Gen<LNECDSASignature> = gen {
    let! h = uint256Gen
    let! k = keyGen
    return k.Sign(h, false) |> LNECDSASignature
}

// scripts

let pushOnlyOpcodeGen = bytesOfNGen(4) |> Gen.map(Op.GetPushOp)
let pushOnlyOpcodesGen = Gen.listOf pushOnlyOpcodeGen

let pushScriptGen = Gen.nonEmptyListOf pushOnlyOpcodeGen |> Gen.map(fun ops -> Script(ops))
