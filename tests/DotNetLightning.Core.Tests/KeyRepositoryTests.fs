module KeyRepositoryTests

open ResultUtils
open DotNetLightning.Serialization.Msgs
open DotNetLightning.Chain
open DotNetLightning.Channel
open DotNetLightning.Transactions
open DotNetLightning.Utils
open DotNetLightning.Crypto
open NBitcoin
open Expecto

let hex = NBitcoin.DataEncoders.HexEncoder()
let n = Network.RegTest

/// same with bolt 3
let paymentPreImages =
    let _s = ([
            ("0000000000000000000000000000000000000000000000000000000000000000")
            ("0101010101010101010101010101010101010101010101010101010101010101")
            ("0202020202020202020202020202020202020202020202020202020202020202")
            ("0303030303030303030303030303030303030303030303030303030303030303")
            ("0404040404040404040404040404040404040404040404040404040404040404")
        ])
    _s |> List.map(hex.DecodeData) |> List.map(PaymentPreimage.Create)
    
/// same with bolt 3
let htlcs = [
    { DirectedHTLC.Direction = In;
      Add = { UpdateAddHTLCMsg.ChannelId = ChannelId.Zero;
              HTLCId = HTLCId.Zero;
              Amount = LNMoney.MilliSatoshis 1000000L
              PaymentHash = paymentPreImages.[0].Hash
              CLTVExpiry = 500u |> BlockHeight;
              OnionRoutingPacket = OnionPacket.LastPacket } }
    { DirectedHTLC.Direction = In;
      Add = { UpdateAddHTLCMsg.ChannelId = ChannelId.Zero;
              HTLCId = HTLCId(1UL);
              Amount = LNMoney.MilliSatoshis 2000000L
              PaymentHash = paymentPreImages.[1].Hash
              CLTVExpiry = 501u |> BlockHeight;
              OnionRoutingPacket = OnionPacket.LastPacket } }
    { DirectedHTLC.Direction = Out;
      Add = { UpdateAddHTLCMsg.ChannelId = ChannelId.Zero;
              HTLCId = HTLCId(2UL);
              Amount = LNMoney.MilliSatoshis 2000000L
              PaymentHash = paymentPreImages.[2].Hash
              CLTVExpiry = 502u |> BlockHeight;
              OnionRoutingPacket = OnionPacket.LastPacket } }
    { DirectedHTLC.Direction = Out;
      Add = { UpdateAddHTLCMsg.ChannelId = ChannelId.Zero;
              HTLCId = HTLCId(3UL);
              Amount = LNMoney.MilliSatoshis 3000000L
              PaymentHash = paymentPreImages.[3].Hash
              CLTVExpiry = 503u |> BlockHeight;
              OnionRoutingPacket = OnionPacket.LastPacket } }
    { DirectedHTLC.Direction = In;
      Add = { UpdateAddHTLCMsg.ChannelId = ChannelId.Zero;
              HTLCId = HTLCId(4UL);
              Amount = LNMoney.MilliSatoshis 4000000L
              PaymentHash = paymentPreImages.[4].Hash
              CLTVExpiry = 504u |> BlockHeight;
              OnionRoutingPacket = OnionPacket.LastPacket } }
]
let htlcMap = htlcs |> List.map(fun htlc ->  htlc.Add.HTLCId, htlc) |> Map.ofList

[<Tests>]
let tests =
    testList "KeyRepository tests" [
        testCase "should create valid signature" <| fun _ ->
            let fundingTxId = [| for _ in 0..31 -> 1uy |] |> uint256 |> TxId
            let fundingAmount = Money.Satoshis 10000000L
            
            let localNodeMasterPrivKey = NodeMasterPrivKey <| ExtKey("00000000000000000000000000000000")
            let localChannelIndex = 0
            let localPrivKeys = localNodeMasterPrivKey.ChannelPrivKeys localChannelIndex
            let localPubKeys = localPrivKeys.ToChannelPubKeys()
            
            let remoteNodeMasterPrivKey = NodeMasterPrivKey <| ExtKey("88888888888888888888888888888888")
            let remoteChannelIndex = 1
            let remotePrivKeys = remoteNodeMasterPrivKey.ChannelPrivKeys remoteChannelIndex
            let remotePubKeys = remotePrivKeys.ToChannelPubKeys()

            let fundingScriptCoin =
                ChannelHelpers.getFundingScriptCoin
                    localPubKeys.FundingPubKey
                    remotePubKeys.FundingPubKey
                    fundingTxId
                    (TxOutIndex 0us)
                    fundingAmount
            
            let localDustLimit = Money.Satoshis(546L)
            let toLocalDelay = 200us |> BlockHeightOffset16
            let specBase = { CommitmentSpec.HTLCs = htlcMap; FeeRatePerKw = 15000u |> FeeRatePerKw;
                             ToLocal = LNMoney.MilliSatoshis(6988000000L); ToRemote =  3000000000L |> LNMoney.MilliSatoshis}
            let commitTx =
                Transactions.makeCommitTx fundingScriptCoin
                                          CommitmentNumber.FirstCommitment
                                          localPubKeys.PaymentBasepoint
                                          remotePubKeys.PaymentBasepoint
                                          (true)
                                          localDustLimit
                                          (RevocationPubKey <| localPubKeys.RevocationBasepoint.RawPubKey())            // FIXME: basepoint being used as pubkey here?
                                          toLocalDelay
                                          (DelayedPaymentPubKey <| localPubKeys.DelayedPaymentBasepoint.RawPubKey())    // FIXME: basepoint being used as pubkey here?
                                          (PaymentPubKey <| remotePubKeys.PaymentBasepoint.RawPubKey())                 // FIXME: basepoint being used as pubkey here?
                                          (HtlcPubKey <| localPubKeys.HtlcBasepoint.RawPubKey())
                                          (HtlcPubKey <| remotePubKeys.HtlcBasepoint.RawPubKey())
                                          specBase
                                          n
            let _remoteSigForLocalCommit, commitTx2 = remotePrivKeys.SignWithFundingPrivKey commitTx.Value
            let _localSigForLocalCommit, commitTx3 = localPrivKeys.SignWithFundingPrivKey commitTx2
            commitTx3.Finalize() |> ignore
            Expect.isTrue (commitTx3.CanExtractTransaction()) (sprintf "failed to finalize commitTx %A" commitTx3)
            
            let remoteDustLimit = Money.Satoshis(1000L)
            let remoteDelay = 160us |> BlockHeightOffset16
            let remoteCommitTx =
                Transactions.makeCommitTx fundingScriptCoin
                                          CommitmentNumber.FirstCommitment
                                          remotePubKeys.PaymentBasepoint
                                          localPubKeys.PaymentBasepoint
                                          false
                                          remoteDustLimit
                                          (RevocationPubKey <| remotePubKeys.RevocationBasepoint.RawPubKey())           // FIXME: basepoint being used as pubkey here?
                                          remoteDelay
                                          (DelayedPaymentPubKey <| remotePubKeys.DelayedPaymentBasepoint.RawPubKey())   // FIXME: basepoint being used as pubkey here?
                                          (PaymentPubKey <| localPubKeys.PaymentBasepoint.RawPubKey())                  // FIXME: basepoint being used as pubkey here?
                                          (HtlcPubKey <| remotePubKeys.HtlcBasepoint.RawPubKey())
                                          (HtlcPubKey <| localPubKeys.HtlcBasepoint.RawPubKey())
                                          specBase
                                          n
            
            let _remoteSigForRemoteCommit, remoteCommitTx2 = remotePrivKeys.SignWithFundingPrivKey remoteCommitTx.Value
            let localSigForRemoteCommit, _commitTx3 = localPrivKeys.SignWithFundingPrivKey remoteCommitTx2
            
            let localSigs = seq [(localPubKeys.FundingPubKey.RawPubKey(), TransactionSignature(localSigForRemoteCommit.Signature, SigHash.All))]
            let _finalizedTx = Transactions.checkTxFinalized remoteCommitTx2 0 localSigs |> Result.deref
            ()
    ]
