namespace DotNetLightning.Transactions

open NBitcoin
open NBitcoin.Crypto
open DotNetLightning.Utils
open DotNetLightning.Crypto

open ResultUtils
open ResultUtils.Portability

/// Functions to work wth LN-specific bitcoin scripts.
module Scripts =

    let inline private encodeInt n =
        Op.GetPushOp(int64 n).ToString()

    let funding
        (fundingPubKey0: FundingPubKey)
        (fundingPubKey1: FundingPubKey)
        =
        PayToMultiSigTemplate.Instance.GenerateScriptPubKey(
            2,
            true,
            [|
                fundingPubKey0.RawPubKey()
                fundingPubKey1.RawPubKey()
            |]
        )

    let toLocalDelayed
        (revocationPubKey: RevocationPubKey)
        (BlockHeightOffset16 toSelfDelay)
        (localDelayedPaymentPubKey: DelayedPaymentPubKey)
        : Script =
        let opList = ResizeArray<Op>()
        opList.Add(!>OpcodeType.OP_IF)
        opList.Add(Op.GetPushOp(revocationPubKey.ToBytes()))
        opList.Add(!>OpcodeType.OP_ELSE)
        opList.Add(Op.GetPushOp(int64 toSelfDelay))
        opList.Add(!>OpcodeType.OP_CHECKSEQUENCEVERIFY)
        opList.Add(!>OpcodeType.OP_DROP)
        opList.Add(Op.GetPushOp(localDelayedPaymentPubKey.ToBytes()))
        opList.Add(!>OpcodeType.OP_ENDIF)
        opList.Add(!>OpcodeType.OP_CHECKSIG)
        Script(opList)

    let htlcOffered
        (localHtlcPubKey: HtlcPubKey)
        (remoteHtlcPubKey: HtlcPubKey)
        (revocationPubKey: RevocationPubKey)
        (ph: PaymentHash)
        : Script =
        let revocationPubKeyHash =
            let p = revocationPubKey.ToBytes()
            Hashes.Hash160(p, 0, p.Length)

        let paymentHashHash = ph.GetRIPEMD160()
        let opList = new ResizeArray<Op>()
        opList.Add(!>OpcodeType.OP_DUP)
        opList.Add(!>OpcodeType.OP_HASH160)
        opList.Add(Op.GetPushOp(revocationPubKeyHash.ToBytes()))
        opList.Add(!>OpcodeType.OP_EQUAL)
        opList.Add(!>OpcodeType.OP_IF)
        opList.Add(!>OpcodeType.OP_CHECKSIG)
        opList.Add(!>OpcodeType.OP_ELSE)
        opList.Add(Op.GetPushOp(remoteHtlcPubKey.ToBytes()))
        opList.Add(!>OpcodeType.OP_SWAP)
        opList.Add(!>OpcodeType.OP_SIZE)
        opList.Add(Op.GetPushOp(32L))
        opList.Add(!>OpcodeType.OP_EQUAL)
        opList.Add(!>OpcodeType.OP_NOTIF)
        opList.Add(!>OpcodeType.OP_DROP)
        opList.Add(!>OpcodeType.OP_2)
        opList.Add(!>OpcodeType.OP_SWAP)
        opList.Add(Op.GetPushOp(localHtlcPubKey.ToBytes()))
        opList.Add(!>OpcodeType.OP_2)
        opList.Add(!>OpcodeType.OP_CHECKMULTISIG)
        opList.Add(!>OpcodeType.OP_ELSE)
        opList.Add(!>OpcodeType.OP_HASH160)
        opList.Add(Op.GetPushOp(paymentHashHash))
        opList.Add(!>OpcodeType.OP_EQUALVERIFY)
        opList.Add(!>OpcodeType.OP_CHECKSIG)
        opList.Add(!>OpcodeType.OP_ENDIF)
        opList.Add(!>OpcodeType.OP_ENDIF)
        Script(opList)

    let htlcReceived
        (localHTLCPubKey: HtlcPubKey)
        (remoteHTLCPubKey: HtlcPubKey)
        (revocationPubKey: RevocationPubKey)
        (ph: PaymentHash)
        (lockTime: uint32)
        : Script =
        let revocationPubKeyHash =
            let p = revocationPubKey.ToBytes()
            Hashes.Hash160(p, 0, p.Length)

        let paymentHashHash = ph.GetRIPEMD160()
        let opList = ResizeArray<Op>()

        opList.Add(!>OpcodeType.OP_DUP)
        opList.Add(!>OpcodeType.OP_HASH160)
        opList.Add(Op.GetPushOp(revocationPubKeyHash.ToBytes()))
        opList.Add(!>OpcodeType.OP_EQUAL)
        opList.Add(!>OpcodeType.OP_IF)
        opList.Add(!>OpcodeType.OP_CHECKSIG)
        opList.Add(!>OpcodeType.OP_ELSE)
        opList.Add(Op.GetPushOp(remoteHTLCPubKey.ToBytes()))
        opList.Add(!>OpcodeType.OP_SWAP)
        opList.Add(!>OpcodeType.OP_SIZE)
        opList.Add(Op.GetPushOp(32L))
        opList.Add(!>OpcodeType.OP_EQUAL)
        opList.Add(!>OpcodeType.OP_IF)
        opList.Add(!>OpcodeType.OP_HASH160)
        opList.Add(Op.GetPushOp(paymentHashHash))
        opList.Add(!>OpcodeType.OP_EQUALVERIFY)
        opList.Add(!>OpcodeType.OP_2)
        opList.Add(!>OpcodeType.OP_SWAP)
        opList.Add(Op.GetPushOp(localHTLCPubKey.ToBytes()))
        opList.Add(!>OpcodeType.OP_2)
        opList.Add(!>OpcodeType.OP_CHECKMULTISIG)
        opList.Add(!>OpcodeType.OP_ELSE)
        opList.Add(!>OpcodeType.OP_DROP)
        opList.Add(Op.GetPushOp(int64 lockTime))
        opList.Add(!>OpcodeType.OP_CHECKLOCKTIMEVERIFY)
        opList.Add(!>OpcodeType.OP_DROP)
        opList.Add(!>OpcodeType.OP_CHECKSIG)
        opList.Add(!>OpcodeType.OP_ENDIF)
        opList.Add(!>OpcodeType.OP_ENDIF)
        Script(opList)
