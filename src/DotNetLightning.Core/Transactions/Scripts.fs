namespace DotNetLightning.Transactions
open NBitcoin
open NBitcoin.Crypto
open DotNetLightning.Utils


module Scripts =

    let inline private encodeInt (n) =
        Op.GetPushOp(int64 n).ToString()

    let multiSigOfM_2 (sort) (pks) =
        PayToMultiSigTemplate.Instance.GenerateScriptPubKey(2, sort, pks)

    let toLocalDelayed  (revocationPubKey: PubKey) (BlockHeightOffset toSelfDelay) (localDelayedPaymentPubkey: PubKey): Script =
        sprintf
            "OP_IF %s OP_ELSE %s OP_CSV OP_DROP %s OP_ENDIF OP_CHECKSIG"
            (revocationPubKey.ToHex()) (encodeInt toSelfDelay) (localDelayedPaymentPubkey.ToHex())
            |> Script

    let htlcOffered (localHtlcPubKey: PubKey) (remoteHtlcPubKey: PubKey) (revocationPubKey: PubKey) (PaymentHash ph): Script =
        let revocationPubKeyHash =
            let p = revocationPubKey.ToBytes()
            Hashes.Hash160(p, 0, p.Length)
        let paymentHashHash =
            let p = ph.ToBytes()
            Hashes.RIPEMD160(p, 0, p.Length)
        sprintf
            "OP_DUP OP_HASH160 %s OP_EQUAL
             OP_IF
                 OP_CHECKSIG
             OP_ELSE
                 %s OP_SWAP OP_SIZE %s OP_EQUAL
                 OP_NOTIF
                     OP_DROP OP_2 OP_SWAP %s OP_2 OP_CHECKMULTISIG
                 OP_ELSE
                     OP_HASH160 %s OP_EQUALVERIFY
                     OP_CHECKSIG
                 OP_ENDIF
             OP_ENDIF
             "
            (revocationPubKeyHash.ToString()) (remoteHtlcPubKey.ToHex()) (encodeInt 32) (localHtlcPubKey.ToHex())
            (paymentHashHash.ToHexString())
            |> Script

    let htlcReceived (localHTLCPubKey: PubKey) (remoteHTLCPubKey: PubKey) (revocationPubKey: PubKey) (PaymentHash ph) (lockTime: uint32): Script =
        let revocationPubKeyHash =
            let p = revocationPubKey.ToBytes()
            Hashes.Hash160(p, 0, p.Length)
        let paymentHashHash =
            let p = ph.ToBytes()
            Hashes.RIPEMD160(p, 0, p.Length)
        sprintf
            "OP_DUP OP_HASH160 %s OP_EQUAL 
             OP_IF 
                 OP_CHECKSIG 
             OP_ELSE 
                 %s OP_SWAP OP_SIZE %s OP_EQUAL 
                 OP_IF 
                     OP_HASH160 %s OP_EQUALVERIFY  
                     2 OP_SWAP %s 2 OP_CHECKMULTISIG 
                 OP_ELSE 
                     OP_DROP %s OP_CLTV OP_DROP 
                     OP_CHECKSIG 
                 OP_ENDIF 
             OP_ENDIF
             "
            (revocationPubKeyHash.ToString()) (remoteHTLCPubKey.ToHex()) (encodeInt 32) (paymentHashHash.ToHexString())
            (localHTLCPubKey.ToHex()) (encodeInt lockTime)
            |> fun s -> printfn "going to encoding \n %s" s; s
            |> Script

    let isValidFinalScriptPubKey(spk: Script) =
        (PayToPubkeyHashTemplate.Instance.CheckScriptPubKey(spk))
        || (PayToScriptHashTemplate.Instance.CheckScriptPubKey(spk))
        || (PayToWitPubKeyHashTemplate.Instance.CheckScriptPubKey(spk))
        || (PayToWitScriptHashTemplate.Instance.CheckScriptPubKey(spk))