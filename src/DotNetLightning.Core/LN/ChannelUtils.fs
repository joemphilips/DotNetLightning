namespace DotNetLightning.LN
open DotNetLightning.Utils.Primitives
open DotNetLightning.Utils
open DotNetLightning.Crypto
open NBitcoin
open NBitcoin.Crypto

/// It has three ways to spend.
/// 1. Use old commitment tx
/// 2. revoke after timeout
/// 3. revoke using preimage
type HTLCOutputInCommitment = {
    /// There are two types of htlc 1. Offered, 2. Received
    Offered: bool
    Amount: LNMoney
    CLTVExpiry: uint32
    PaymentHash: PaymentHash
    TransactionOutputIndex: uint32 option
}


module ChannelUtils =
    [<Literal>]
    let HTLC_SUCCESS_TX_WEIGHT = 703L;

    [<Literal>]
    let HTLC_TIMEOUT_TX_WEIGHT = 663L;

    /// Various funcitons for key derivation and tx creation for use within channels. Primarily used in Channel
    /// and ChannelMonitor
    let buildCommitmentSecret (commitmentSeed: uint256, index: uint64): Key =
        let mutable res = commitmentSeed.ToBytes()
        for i in 0..47 do
            let bitpos = 47 - i
            if index &&& (1UL <<< bitpos) = (1UL <<< bitpos) then
                res.[bitpos / 8] <- (res.[bitpos / 8] ^^^ (1uy <<< (7 &&& bitpos)))
                res <- Hashes.SHA256(res)
        res |> Key
    
    let buildCommitmentPoint(seed: uint256, index: uint64) =
        buildCommitmentSecret(seed, index) |> fun k -> k.PubKey

    let derivePrivateKey (perCommitmentPoint: PubKey) (baseSecret: Key): Key =
        let res = Hashes.SHA256(Array.concat[|perCommitmentPoint.ToBytes(); baseSecret.PubKey.ToBytes() |])
        Key(res)

    let inline private encodeInt (n) =
        Op.GetPushOp(int64 n).ToString()


    /// Gets the "to_local" output redeemScript, ie the script which is time-locked or spendable
    /// by the revocation key
    let getRevokableRedeemScript (revocationKey: PubKey) (BlockHeightOffset toSelfDelay) (delayedPaymentKey: PubKey)  =
        let str = sprintf "OP_IF %s OP_ELSE %s OP_CSV OP_DROP %s OP_ENDIF OP_CHECKSIG"
                          (revocationKey.ToHex())
                          (encodeInt toSelfDelay)
                          (delayedPaymentKey.ToHex()) 
        Script(str)

    let public getHTLCRedeemScriptWithExplicitKeys (htlc: HTLCOutputInCommitment) (aHTLCKey: PubKey) (bHTLCKey: PubKey) (revocationKey: PubKey) =
        let paymentHash160 = uint160(Hashes.RIPEMD160(htlc.PaymentHash.ToBytes(), 0, 32), false)
        if htlc.Offered then
            let str = sprintf "OP_DUP OP_HASH160 %s OP_EQUAL
                              OP_IF OP_CHECKSIG
                              OP_ELSE %s OP_SWAP OP_SIZE %s OP_EQUAL
                              OP_NOTIF OP_DROP 2 OP_SWAP %s 2 OP_CHECKMULTISIG
                              OP_ELSE OP_HASH160 %s OP_EQUALVERIFY OP_CHECKSIG OP_ENDIF OP_ENDIF"
                              (revocationKey.ToHex())
                              (bHTLCKey.ToHex())
                              (encodeInt 32)
                              (aHTLCKey.ToHex())
                              (paymentHash160.ToString())
            Script(str)
        else
            let str = sprintf "OP_DUP OP_HASH160 %s OP_EQUAL
                              OP_IF OP_CHECKSIG
                              OP_ELSE %s OP_SWAP OP_SIZE %s OP_EQUAL
                              OP_IF OP_HASH160 %s OP_EQUALVERIFY 2 OP_SWAP %s 2 OP_CHECKMULTISIG
                              OP_ELSE OP_DROP %s OP_CLTV OP_DROP OP_CHECKSIG OP_ENDIF OP_ENDIF"
                              (revocationKey.ToHex())
                              (bHTLCKey.ToHex())
                              (encodeInt 32)
                              (paymentHash160.ToString())
                              (aHTLCKey.ToHex())
                              (encodeInt htlc.CLTVExpiry)
            Script(str)
    let public getHTLCTransaction (prevHash: TxId)
                                  (feeRatePerKw: Money)
                                  (toSelfDelay: BlockHeightOffset)
                                  (htlc: HTLCOutputInCommitment)
                                  (aDelayedPaymentKey: PubKey)
                                  (revocationKey: PubKey)
                                  (network: Network): Transaction =
        let txin = TxIn()
        match htlc.TransactionOutputIndex with
        | None _ -> failwith "Can't build an htlc transaction for a dust output"
        | Some v -> 
            txin.PrevOut <- OutPoint(prevHash.Value, v)
            txin.Sequence <- (!> 0u)
            txin.ScriptSig <- Script()
            txin.WitScript <- WitScript()

        let totalFee = if htlc.Offered then
                           feeRatePerKw.Satoshi * HTLC_TIMEOUT_TX_WEIGHT / 1000L
                       else 
                           feeRatePerKw.Satoshi * HTLC_SUCCESS_TX_WEIGHT / 1000L
        let txOut = TxOut()
        txOut.ScriptPubKey <- (getRevokableRedeemScript(revocationKey) toSelfDelay aDelayedPaymentKey).WitHash.ScriptPubKey
        // BOLT 3 does not specify if we should add amount_msat before deviding or if we should devide by 1000 before subtracting (as we do here)
        txOut.Value <- Money.Satoshis(htlc.Amount.MilliSatoshi / 1000L  - totalFee)

        let tx = network.CreateTransaction()
        tx.Inputs.Add(txin) |> ignore
        tx.Outputs.Add(txOut) |> ignore
        tx.Version <- 2u
        tx.LockTime <- if htlc.Offered then !> htlc.CLTVExpiry else LockTime.Zero
        tx

    let buildHTLCTransaction (TxId prevHash)
                             (FeeRatePerKw feeRate)
                             (toSelfDelay: BlockHeightOffset)
                             (htlc: HTLCOutputInCommitment)
                             (aDelayedPaymentKey: PubKey)
                             (revocationKey: PubKey)
                             (n: Network): Transaction =
        let txIn = TxIn()
        txIn.PrevOut <- OutPoint(prevHash, htlc.TransactionOutputIndex.Value)

        let totalFee = if htlc.Offered then
                           (int64 feeRate) * HTLC_TIMEOUT_TX_WEIGHT / 1000L
                       else
                           (int64 feeRate) * HTLC_SUCCESS_TX_WEIGHT / 1000L

        let txOut = TxOut(!>(htlc.Amount.MilliSatoshi / 1000L - totalFee),
                          (getRevokableRedeemScript revocationKey toSelfDelay aDelayedPaymentKey).WitHash.ScriptPubKey)
        let tx = n.CreateTransaction()
        tx.Inputs.Add(txIn) |> ignore
        tx.Outputs.Add(txOut) |> ignore
        tx.LockTime <- !> (if htlc.Offered then htlc.CLTVExpiry else 0u)
        tx