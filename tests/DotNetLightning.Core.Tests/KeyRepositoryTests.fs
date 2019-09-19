module KeyRepositoryTests

open DotNetLightning.Serialize.Msgs
open DotNetLightning.Chain
open DotNetLightning.LN
open DotNetLightning.Tests.Utils
open DotNetLightning.Transactions
open DotNetLightning.Utils
open NBitcoin
open Expecto

let hex = NBitcoin.DataEncoders.HexEncoder()
let n = Network.RegTest

let logger = TestLogger.Create("KeyRepository tests")
let log = logger.LogSimple

/// same with bolt 3
let paymentPreImages =
    let _s = ([
            ("0000000000000000000000000000000000000000000000000000000000000000")
            ("0101010101010101010101010101010101010101010101010101010101010101")
            ("0202020202020202020202020202020202020202020202020202020202020202")
            ("0303030303030303030303030303030303030303030303030303030303030303")
            ("0404040404040404040404040404040404040404040404040404040404040404")
        ])
    _s |> List.map(hex.DecodeData) |> List.map(PaymentPreimage)
/// same with bolt 3
let htlcs = [
    { DirectedHTLC.Direction = In;
      Add = { UpdateAddHTLC.ChannelId = ChannelId.Zero;
              HTLCId = HTLCId.Zero;
              AmountMSat = LNMoney.MilliSatoshis(1000000L);
              PaymentHash = paymentPreImages.[0].GetSha256();
              CLTVExpiry = 500u |> BlockHeight;
              OnionRoutingPacket = OnionPacket.LastPacket } }
    { DirectedHTLC.Direction = In;
      Add = { UpdateAddHTLC.ChannelId = ChannelId.Zero;
              HTLCId = HTLCId(1UL);
              AmountMSat = LNMoney.MilliSatoshis(2000000L);
              PaymentHash = paymentPreImages.[1].GetSha256();
              CLTVExpiry = 501u |> BlockHeight;
              OnionRoutingPacket = OnionPacket.LastPacket } }
    { DirectedHTLC.Direction = Out;
      Add = { UpdateAddHTLC.ChannelId = ChannelId.Zero;
              HTLCId = HTLCId(2UL);
              AmountMSat = LNMoney.MilliSatoshis(2000000L);
              PaymentHash = paymentPreImages.[2].GetSha256();
              CLTVExpiry = 502u |> BlockHeight;
              OnionRoutingPacket = OnionPacket.LastPacket } }
    { DirectedHTLC.Direction = Out;
      Add = { UpdateAddHTLC.ChannelId = ChannelId.Zero;
              HTLCId = HTLCId(3UL);
              AmountMSat = LNMoney.MilliSatoshis(3000000L);
              PaymentHash = paymentPreImages.[3].GetSha256();
              CLTVExpiry = 503u |> BlockHeight;
              OnionRoutingPacket = OnionPacket.LastPacket } }
    { DirectedHTLC.Direction = In;
      Add = { UpdateAddHTLC.ChannelId = ChannelId.Zero;
              HTLCId = HTLCId(4UL);
              AmountMSat = LNMoney.MilliSatoshis(4000000L);
              PaymentHash = paymentPreImages.[4].GetSha256();
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
            
            let localSeed = [| for _ in 0..31 -> 0uy |] |> uint256
            let localRepo = DefaultKeyRepository(localSeed) :> IKeysRepository
            let localKeys = localRepo.GetChannelKeys(true)
            let localPubKeys = localKeys.ToChannelPubKeys()
            
            let remoteSeed = [| for _ in 0..31 -> 8uy |] |> uint256
            let remoteRepo = DefaultKeyRepository(remoteSeed) :> IKeysRepository
            let remoteKeys = remoteRepo.GetChannelKeys(false)
            let remotePubKeys = remoteKeys.ToChannelPubKeys()
            
            let fundingSCoin = Channel.Helpers.getFundingSCoin(localPubKeys)
                                                              (remotePubKeys.FundingPubKey)
                                                              (fundingTxId)
                                                              (TxOutIndex 0us)
                                                              fundingAmount
            
            let localDustLimit = Money.Satoshis(546L)
            let toLocalDelay = 200us |> BlockHeightOffset
            let specBase = { CommitmentSpec.HTLCs = htlcMap; FeeRatePerKw = 15000u |> FeeRatePerKw;
                             ToLocal = LNMoney.MilliSatoshis(6988000000L); ToRemote =  3000000000L |> LNMoney.MilliSatoshis}
            let commitTx =
                Transactions.makeCommitTx fundingSCoin
                                          0UL
                                          (localPubKeys.PaymentBasePubKey)
                                          (remotePubKeys.PaymentBasePubKey)
                                          (true)
                                          localDustLimit
                                          (localPubKeys.RevocationBasePubKey)
                                          toLocalDelay
                                          localPubKeys.DelayedPaymentBasePubKey
                                          remotePubKeys.PaymentBasePubKey
                                          localPubKeys.HTLCBasePubKey
                                          remotePubKeys.HTLCBasePubKey
                                          specBase
                                          n
            let _remoteSigForLocalCommit, commitTx2 = remoteRepo.GetSignatureFor(commitTx.Value, remotePubKeys.FundingPubKey)
            let _localSigForLocalCommit, commitTx3 = localRepo.GetSignatureFor(commitTx2, localPubKeys.FundingPubKey)
            commitTx3.Finalize() |> ignore
            Expect.isTrue (commitTx3.CanExtractTransaction()) (sprintf "failed to finalize commitTx %A" commitTx3)
            
            let remoteDustLimit = Money.Satoshis(1000L)
            let remoteDelay = 160us |> BlockHeightOffset
            let remoteCommitTx =
                Transactions.makeCommitTx fundingSCoin
                                          0UL
                                          remotePubKeys.PaymentBasePubKey
                                          localPubKeys.PaymentBasePubKey
                                          false
                                          remoteDustLimit
                                          remotePubKeys.RevocationBasePubKey
                                          remoteDelay
                                          remotePubKeys.DelayedPaymentBasePubKey
                                          localPubKeys.PaymentBasePubKey
                                          remotePubKeys.HTLCBasePubKey
                                          localPubKeys.HTLCBasePubKey
                                          specBase
                                          n
            
            let _remoteSigForRemoteCommit, remoteCommitTx2 = remoteRepo.GetSignatureFor(remoteCommitTx.Value, remotePubKeys.FundingPubKey)
            let localSigForRemoteCommit, commitTx3 = localRepo.GetSignatureFor(remoteCommitTx2, localPubKeys.FundingPubKey)
            
            let localSigs = seq [(localPubKeys.FundingPubKey, TransactionSignature(localSigForRemoteCommit.Signature, SigHash.All))]
            let finalizedTx = Transactions.checkTxFinalized remoteCommitTx2 0 localSigs |> function Good tx -> tx | Bad e -> failwithf "%A" e
            ()
    ]
