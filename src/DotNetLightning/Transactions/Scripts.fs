namespace DotNetLightning.Transactions
open NBitcoin
open NBitcoin.Crypto
open DotNetLightning.Utils


module Scripts =
    let multiSig (sort) (pks) =
        PayToMultiSigTemplate.Instance.GenerateScriptPubKey(2, sort, pks)

    let toLocalDelayed  (revocationPubKey: PubKey) (BlockHeightOffset toSelfDelay) (localDelayedPaymentPubkey: PubKey): Script =
        failwith ""

    let htlcOffered (localHtlcPubKey: PubKey) (remoteHtlcPubKey: PubKey) (revocationPubKey: PubKey) (PaymentHash ph): Script =
        let paymentHashHash =
            let p = ph.ToBytes()
            Hashes.RIPEMD160(p, 0, p.Length)
        failwith ""

    let htlcReceived (localHTLCPubKey: PubKey) (remoteHTLCPubKey: PubKey) (revocationPubKey: PubKey) (PaymentHash ph) (lockTime: uint32): Script =
        let paymentHashHash =
            let p = ph.ToBytes()
            Hashes.RIPEMD160(p, 0, p.Length)
        failwith ""