module ChannelTests
open Expecto
open NBitcoin
open DotNetLightning.LN
open DotNetLightning
open DotNetLightning.Utils.Primitives
open DotNetLightning.Chain
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Utils.Aether.Operators
open DotNetLightning.Utils.Aether
open DotNetLightning.Tests.Utils
open DotNetLightning.LN
open NBitcoin
open NBitcoin.Crypto
open NBitcoin
open System.Security.Cryptography
open DotNetLightning.LN
open DotNetLightning.LN

let hex = NBitcoin.DataEncoders.HexEncoder()

type DummyFeeEstimator =
    {
        FeeEst: FeeRatePerKw
    }
    with
        interface IFeeEstimator with
            member this.GetEstSatPer1000Weight(arg1: ConfirmationTarget): FeeRatePerKw = 
                this.FeeEst

type HTLCSource with
    /// For test
    static member internal Dummy() =
        OutboundRoute {
            Route = Route []
            SessionPriv = Key([| for _ in 0..31 -> 1uy|])
            FirstHopHTLC = LNMoney.Zero
        }

type DummyKeyRepository =
    {
        ChanKeys: ChannelKeys
    }
    with
        interface IKeysRepository with

            member this.GetChannelId(): Utils.Primitives.ChannelId = 
                uint256.Zero |> ChannelId

            member this.GetChannelKeys(): ChannelKeys = 
                this.ChanKeys

            member this.GetDestinationScript(): Script = 
                let channelMonitorClaminKey = Key(hex.DecodeData("0fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))
                channelMonitorClaminKey.PubKey.Hash.ScriptPubKey

            member this.GetSessionKey(): Key = 
                failwith "Not Implemented"

            member this.GetShutdownPubKey(): PubKey = 
                let channelCloseKey = Key(hex.DecodeData("0fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))
                channelCloseKey.PubKey

            member this.GetNodeSecret() =
                failwith "Not implemented"

[<Tests>]
let tests =
    ftestList "Channel tests" [
        testCase "test_max_funding_satoshis" <| fun _ ->
            Expect.isLessThan MAX_FUNDING_SATOSHIS (Money.Satoshis(21_000_000L * 100_000_000L)) "MAX_FUNDING_SATOSHIS is greater than all satoshis in existance"

        /// Test vectors from BOLt3 Appendix C:
        testCase "Outbound commitment test" <| fun _ ->
            let feeest = { DummyFeeEstimator.FeeEst = FeeRatePerKw 15000u }
            let logger = TestLogger.Zero
            let n = Network.RegTest
            let channelKeys = {
                    ChannelKeys.FundingKey = Key(hex.DecodeData("30ff4956bbdd3222d44cc5e8a1261dab1e07957bdac5ae88fe3261ef321f3749"))
                    PaymentBaseKey = Key(hex.DecodeData("1111111111111111111111111111111111111111111111111111111111111111"))
                    DelayedPaymentBaseKey = Key(hex.DecodeData("3333333333333333333333333333333333333333333333333333333333333333"))
                    HTLCBaseKey=Key(hex.DecodeData("1111111111111111111111111111111111111111111111111111111111111111"))
                    RevocationBaseKey=Key(hex.DecodeData("0fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))
                    CommitmentSeed = uint256([| for _ in 0..31 -> 0xffuy |])
                }

            Expect.equal channelKeys.FundingKey.PubKey (PubKey(hex.DecodeData("023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb"))) ""
            let keysProvider = { DummyKeyRepository.ChanKeys = channelKeys }
            let theirNodeId = NodeId (Key([| for _ in 0..31 -> 0x42uy |]).PubKey)
            let l = UserConfig.ChannelOptions_ >-> ChannelConfig.AnnouncedChannel_
            let config = Optic.set l false UserConfig.Zero
            let channelR =
                Channel.newOutBound(feeest, keysProvider, theirNodeId, Money.Satoshis(100000L), LNMoney.MilliSatoshis(100000L), UserId(42UL), logger, config)
                |>> fun c -> { c with TheirToSelfDelay = (BlockHeightOffset 144us) }
                |>> fun c -> { c with OurDustLimit = Money.Satoshis(546L) }
                |>> fun c ->
                        let prevOut = uint256("8984484a580b825b9972d7adb15050b3ab624ccd731946b3eeddb92f4e7ef6be")
                        let fundingInfo = OutPoint(prevOut, 0)
                        Optic.set (Channel.ChannelMonitor_ >-> ChannelMonitor.KeyStorage_ >-> Storage.Local_ >?> LocalStorage.FundingInfo_) (fundingInfo, Script()) c
                |>> fun c ->
                        let theirPaymentBasePoint = Key(hex.DecodeData("4444444444444444444444444444444444444444444444444444444444444444")).PubKey
                        { c with TheirPaymentBasePoint = Some (theirPaymentBasePoint) }
                |>> fun c ->
                        Expect.equal (c.TheirPaymentBasePoint.Value.ToBytes()) (hex.DecodeData("032c0b7cf95324a07d05398b240174dc0c2be444d96b159aa6c7f7b1e668680991")) ""
                        let fundingPk = Key(hex.DecodeData("1552dfba4f6cf29a62a0af13c8d6981d36d0ef8d61ba10fb0fe90da7634d7e13")).PubKey
                        { c with TheirFundingPubKey = Some(fundingPk) }
                |>> fun c ->
                        Expect.equal (c.TheirFundingPubKey.Value.ToBytes()) (hex.DecodeData("030e9f7b623d2ccc7c9bd44d66d5ce21ce504c0acf6385a132cec6d3c39fa711c1")) ""
                        let theirHtlcBasePoint = Key(hex.DecodeData("4444444444444444444444444444444444444444444444444444444444444444")).PubKey
                        { c with TheirHTLCBasePoint = Some (theirHtlcBasePoint)}
                |>> fun c ->
                        Expect.equal (c.TheirHTLCBasePoint.Value.ToBytes()) (hex.DecodeData("032c0b7cf95324a07d05398b240174dc0c2be444d96b159aa6c7f7b1e668680991")) ""
                        let theirRevocationBP = PubKey(hex.DecodeData("02466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f27"))
                        { c with TheirRevocationBasePoint = Some (theirRevocationBP)}

            Expect.isOk (RResult.rtoResult channelR) ""

            // We can't just use BuildLocalTransactionKeys here as perCommitmentSecret is not
            // derived from a CommitmentSeed
            let baseChannel = RResult.rderef channelR

            let keys =
                let delayedPaymentBase = baseChannel.LocalKeys.DelayedPaymentBaseKey.PubKey
                let perCommitmentSecret = Key(hex.DecodeData("1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a09080706050403020100"))
                let perCommitmentPoint = perCommitmentSecret.PubKey
                let htlcBasePoint = baseChannel.LocalKeys.HTLCBaseKey.PubKey
                TxCreationKeys.Create(perCommitmentPoint,
                                      delayedPaymentBase,
                                      htlcBasePoint,
                                      baseChannel.TheirRevocationBasePoint.Value,
                                      baseChannel.TheirPaymentBasePoint.Value,
                                      baseChannel.TheirHTLCBasePoint.Value)
            let getUnsignedTx chan =
                let res = chan |> Channel.buildCommitmentTransaction (0xffffffffffffUL - 42UL) keys true false chan.FeeRatePerKw
                let tx, _ , is = res
                let htlcs = is |> List.choose(fun (htlc, _) -> if htlc.TransactionOutputIndex.IsSome then Some htlc else None )
                (tx, htlcs)

            let testCommitment (theirSigHex: string) (ourSigHex: string) (txHex: string) (n: Network) (unsignedTx: Transaction * HTLCOutputInCommitment list) (chan) =
                let tx, _ = unsignedTx
                /// given their signature...
                let theirSig = ECDSASignature.FromDER(hex.DecodeData(theirSigHex))
                let redeem = chan |> Channel.getFundingRedeemScript
                let theirPk = chan.TheirFundingPubKey.Value
                let b =
                    let coin  = ScriptCoin(coin = Coin(fromOutpoint = tx.Inputs.[0].PrevOut, fromTxOut = TxOut(chan.ChannelValueSatoshis, redeem.WitHash.ScriptPubKey)), redeem=redeem)
                    n.CreateTransactionBuilder().AddCoins(coin :> ICoin).AddKnownSignature(theirPk, theirSig, tx.Inputs.[0].PrevOut)
                Expect.isEmpty (b.Check(fst unsignedTx)) "their signature is invalid"

                let signedTxRR = chan |> Channel.signCommitmentTransaction (tx, theirSig, n, Some true)
                let signedTxR = RResult.rtoResult signedTxRR
                Expect.isOk (signedTxR) "result of signing operation is not good."
                let signedTx = RResult.rderef signedTxRR
                Expect.equal (signedTx.ToBytes()) (hex.DecodeData(txHex)) "signed tx does not matches the one expected"
                (signedTx, snd unsignedTx)

            let testHtlcOutput (htlcIdx: int32) (theirSigHex: string) (ourSigHex: string) (txHex: string) (n: Network) (unsignedTx: Transaction * HTLCOutputInCommitment list) chan =
                let remoteSig = ECDSASignature(hex.DecodeData(theirSigHex))
                let tx, htlcs = unsignedTx
                let htlc = htlcs.[htlcIdx]
                let htlcTx = chan |> Channel.buildHTLCTransaction (tx.GetTxId()) (htlc) (true) (keys) (chan.FeeRatePerKw) <| n
                let htlcRedeemScript = ChannelUtils.getHTLCRedeemScript htlc keys

                // check their sig 
                let theirPk = chan.TheirFundingPubKey.Value
                let coin = ScriptCoin(Coin(tx.Inputs.[0].PrevOut, TxOut(chan.ChannelValueSatoshis, htlcRedeemScript.WitHash.ScriptPubKey)), htlcRedeemScript)
                let b = n.CreateTransactionBuilder().AddCoins(coin).AddKnownSignature(theirPk, remoteSig, (fst unsignedTx).Inputs.[0].PrevOut)
                Expect.isEmpty (b.Check(fst unsignedTx)) "their signature is invalid"
                let mutable preImage = None
                if (not htlc.Offered) then
                    for i in 0uy..5uy do
                        let out = PaymentHash([| for _ in 1..31 -> i |] |> Hashes.SHA256 |> uint256 )
                        if (out = htlc.PaymentHash) then
                            preImage <- Some (PaymentPreimage([| for _ in 1..31 -> i|] |> uint256))
                Expect.isSome preImage "No preimage found in htlc.PaymentHash"
                let signedTxRR = chan |> Channel.signHTLCTransaction (htlcTx) (remoteSig) (preImage) htlc keys n
                let signedTxR = RResult.rtoResult signedTxRR
                Expect.isOk (signedTxR) "Failed to sign HTLC transaction"
                let signedTx = RResult.rderef signedTxRR
                Expect.equal (signedTx.ToBytes()) (hex.DecodeData(txHex)) "signed tx does not match to expected one."
                (signedTx, snd unsignedTx)

            let testCase1 (channelLocal) =
                let unsignedTx = getUnsignedTx channelLocal
                testCommitment("3045022100f51d2e566a70ba740fc5d8c0f07b9b93d2ed741c3c0860c613173de7d39e7968022041376d520e9c0e1ad52248ddf4b22e12be8763007df977253ef45a4ca3bdb7c0")
                              ("3044022051b75c73198c6deee1a875871c3961832909acd297c6b908d59e3319e5185a46022055c419379c5051a78d00dbbce11b5b664a0c22815fbcc6fcef6b1937c3836939")
                              ("02000000000101bef67e4e2fb9ddeeb3461973cd4c62abb35050b1add772995b820b584a488489000000000038b02b8002c0c62d0000000000160014ccf1af2f2aabee14bb40fa3851ab2301de84311054a56a00000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e0400473044022051b75c73198c6deee1a875871c3961832909acd297c6b908d59e3319e5185a46022055c419379c5051a78d00dbbce11b5b664a0c22815fbcc6fcef6b1937c383693901483045022100f51d2e566a70ba740fc5d8c0f07b9b93d2ed741c3c0860c613173de7d39e7968022041376d520e9c0e1ad52248ddf4b22e12be8763007df977253ef45a4ca3bdb7c001475221023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb21030e9f7b623d2ccc7c9bd44d66d5ce21ce504c0acf6385a132cec6d3c39fa711c152ae3e195220")
                              n
                              unsignedTx
                              channelLocal
            testCase1 ({ baseChannel with ValueToSelf = LNMoney.MilliSatoshis(7000000000L)}) |> ignore

            let additionalInboundHtlcs = [
                {
                    InboundHTLCOutput.HTLCId = 0UL |> Primitives.HTLCId
                    Amount = LNMoney.MilliSatoshis(1000000L)
                    CLTVExpiry = 500u
                    PaymentHash = hex.DecodeData("0000000000000000000000000000000000000000000000000000000000000000") |> Hashes.SHA256 |> uint256 |> PaymentHash
                    State = InboundHTLCState.Commited
                }
                {
                    InboundHTLCOutput.HTLCId = 1UL |> Primitives.HTLCId
                    Amount = LNMoney.MilliSatoshis(2000000L)
                    CLTVExpiry = 501u
                    PaymentHash = hex.DecodeData("0101010101010101010101010101010101010101010101010101010101010101") |> Hashes.SHA256 |> uint256 |> PaymentHash
                    State = InboundHTLCState.Commited
                }
                {
                    InboundHTLCOutput.HTLCId = 4UL |> Primitives.HTLCId
                    Amount = LNMoney.MilliSatoshis(4000000L)
                    CLTVExpiry = 504u
                    PaymentHash = hex.DecodeData("0404040404040404040404040404040404040404040404040404040404040404") |> Hashes.SHA256 |> uint256 |> PaymentHash
                    State = InboundHTLCState.Commited
                }
            ]
            let additionalOutboundHTLCs = [
                {
                    OutboundHTLCOutput.HTLCId = 2UL |> Primitives.HTLCId
                    Amount = 2000000L |> LNMoney.MilliSatoshis
                    CLTVExpiry = 502u
                    PaymentHash = hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202") |> Hashes.SHA256 |> uint256 |> PaymentHash
                    State = OutboundHTLCState.Commited
                    Source = HTLCSource.Dummy()
                }
                {
                    OutboundHTLCOutput.HTLCId = 3UL |> Primitives.HTLCId
                    Amount = 3000000L |> LNMoney.MilliSatoshis
                    CLTVExpiry = 503u
                    PaymentHash = hex.DecodeData("0303030303030303030303030303030303030303030303030303030303030303") |> Hashes.SHA256 |> uint256 |> PaymentHash
                    State = OutboundHTLCState.Commited
                    Source = HTLCSource.Dummy()
                }
            ]
            let chan = baseChannel
                       |> Optic.map (Channel.PendingInboundHTLCs_) (fun l -> l @ additionalInboundHtlcs)
                       |> Optic.map (Channel.PendingOutboundHTLCs_) (fun l -> l @ additionalOutboundHTLCs)
                       |> fun c -> { c with ValueToSelf = LNMoney.MilliSatoshis 6993000000L } // 7000000000 - 7000000
                       |> fun c -> { c with FeeRatePerKw = FeeRatePerKw 0u }

            /// Commitment tx with all five HTLCs untrimmed (minimum feerate)
            let testCase2 (channelLocal) =
                let mutable tx = getUnsignedTx channelLocal
                tx <- testCommitment("304402204fd4928835db1ccdfc40f5c78ce9bd65249b16348df81f0c44328dcdefc97d630220194d3869c38bc732dd87d13d2958015e2fc16829e74cd4377f84d215c0b70606")
                                    ("30440220275b0c325a5e9355650dc30c0eccfbc7efb23987c24b556b9dfdd40effca18d202206caceb2c067836c51f296740c7ae807ffcbfbf1dd3a0d56b6de9a5b247985f06")
                                    ("02000000000101bef67e4e2fb9ddeeb3461973cd4c62abb35050b1add772995b820b584a488489000000000038b02b8007e80300000000000022002052bfef0479d7b293c27e0f1eb294bea154c63a3294ef092c19af51409bce0e2ad007000000000000220020403d394747cae42e98ff01734ad5c08f82ba123d3d9a620abda88989651e2ab5d007000000000000220020748eba944fedc8827f6b06bc44678f93c0f9e6078b35c6331ed31e75f8ce0c2db80b000000000000220020c20b5d1f8584fd90443e7b7b720136174fa4b9333c261d04dbbd012635c0f419a00f0000000000002200208c48d15160397c9731df9bc3b236656efb6665fbfe92b4a6878e88a499f741c4c0c62d0000000000160014ccf1af2f2aabee14bb40fa3851ab2301de843110e0a06a00000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e04004730440220275b0c325a5e9355650dc30c0eccfbc7efb23987c24b556b9dfdd40effca18d202206caceb2c067836c51f296740c7ae807ffcbfbf1dd3a0d56b6de9a5b247985f060147304402204fd4928835db1ccdfc40f5c78ce9bd65249b16348df81f0c44328dcdefc97d630220194d3869c38bc732dd87d13d2958015e2fc16829e74cd4377f84d215c0b7060601475221023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb21030e9f7b623d2ccc7c9bd44d66d5ce21ce504c0acf6385a132cec6d3c39fa711c152ae3e195220")
                                    n
                                    tx
                                    channelLocal
                Expect.equal (snd tx).Length 5 ""
                tx <- testHtlcOutput(0)
                                    ("304402206a6e59f18764a5bf8d4fa45eebc591566689441229c918b480fb2af8cc6a4aeb02205248f273be447684b33e3c8d1d85a8e0ca9fa0bae9ae33f0527ada9c162919a6")
                                    ("304402207cb324fa0de88f452ffa9389678127ebcf4cabe1dd848b8e076c1a1962bf34720220116ed922b12311bd602d67e60d2529917f21c5b82f25ff6506c0f87886b4dfd5")
                                    ("020000000001018154ecccf11a5fb56c39654c4deb4d2296f83c69268280b94d021370c94e219700000000000000000001e8030000000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e050047304402206a6e59f18764a5bf8d4fa45eebc591566689441229c918b480fb2af8cc6a4aeb02205248f273be447684b33e3c8d1d85a8e0ca9fa0bae9ae33f0527ada9c162919a60147304402207cb324fa0de88f452ffa9389678127ebcf4cabe1dd848b8e076c1a1962bf34720220116ed922b12311bd602d67e60d2529917f21c5b82f25ff6506c0f87886b4dfd5012000000000000000000000000000000000000000000000000000000000000000008a76a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c8201208763a914b8bcb07f6344b42ab04250c86a6e8b75d3fdbbc688527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae677502f401b175ac686800000000")
                                    n
                                    tx
                                    channelLocal

                tx <- testHtlcOutput(1)
                                    ("3045022100d5275b3619953cb0c3b5aa577f04bc512380e60fa551762ce3d7a1bb7401cff9022037237ab0dac3fe100cde094e82e2bed9ba0ed1bb40154b48e56aa70f259e608b")
                                    ("3045022100c89172099507ff50f4c925e6c5150e871fb6e83dd73ff9fbb72f6ce829a9633f02203a63821d9162e99f9be712a68f9e589483994feae2661e4546cd5b6cec007be5")
                                    ("020000000001018154ecccf11a5fb56c39654c4deb4d2296f83c69268280b94d021370c94e219701000000000000000001d0070000000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e0500483045022100d5275b3619953cb0c3b5aa577f04bc512380e60fa551762ce3d7a1bb7401cff9022037237ab0dac3fe100cde094e82e2bed9ba0ed1bb40154b48e56aa70f259e608b01483045022100c89172099507ff50f4c925e6c5150e871fb6e83dd73ff9fbb72f6ce829a9633f02203a63821d9162e99f9be712a68f9e589483994feae2661e4546cd5b6cec007be501008576a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c820120876475527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae67a914b43e1b38138a41b37f7cd9a1d274bc63e3a9b5d188ac6868f6010000")
                                    n
                                    tx
                                    channelLocal

                tx <- testHtlcOutput(2)
                                    ("304402201b63ec807771baf4fdff523c644080de17f1da478989308ad13a58b51db91d360220568939d38c9ce295adba15665fa68f51d967e8ed14a007b751540a80b325f202")
                                    ("3045022100def389deab09cee69eaa1ec14d9428770e45bcbe9feb46468ecf481371165c2f022015d2e3c46600b2ebba8dcc899768874cc6851fd1ecb3fffd15db1cc3de7e10da")
                                    ("020000000001018154ecccf11a5fb56c39654c4deb4d2296f83c69268280b94d021370c94e219702000000000000000001d0070000000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e050047304402201b63ec807771baf4fdff523c644080de17f1da478989308ad13a58b51db91d360220568939d38c9ce295adba15665fa68f51d967e8ed14a007b751540a80b325f20201483045022100def389deab09cee69eaa1ec14d9428770e45bcbe9feb46468ecf481371165c2f022015d2e3c46600b2ebba8dcc899768874cc6851fd1ecb3fffd15db1cc3de7e10da012001010101010101010101010101010101010101010101010101010101010101018a76a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c8201208763a9144b6b2e5444c2639cc0fb7bcea5afba3f3cdce23988527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae677502f501b175ac686800000000")
                                    n
                                    tx
                                    channelLocal

                tx <- testHtlcOutput(3)
                                    ("3045022100daee1808f9861b6c3ecd14f7b707eca02dd6bdfc714ba2f33bc8cdba507bb182022026654bf8863af77d74f51f4e0b62d461a019561bb12acb120d3f7195d148a554")
                                    ("30440220643aacb19bbb72bd2b635bc3f7375481f5981bace78cdd8319b2988ffcc6704202203d27784ec8ad51ed3bd517a05525a5139bb0b755dd719e0054332d186ac08727")
                                    ("020000000001018154ecccf11a5fb56c39654c4deb4d2296f83c69268280b94d021370c94e219703000000000000000001b80b0000000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e0500483045022100daee1808f9861b6c3ecd14f7b707eca02dd6bdfc714ba2f33bc8cdba507bb182022026654bf8863af77d74f51f4e0b62d461a019561bb12acb120d3f7195d148a554014730440220643aacb19bbb72bd2b635bc3f7375481f5981bace78cdd8319b2988ffcc6704202203d27784ec8ad51ed3bd517a05525a5139bb0b755dd719e0054332d186ac0872701008576a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c820120876475527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae67a9148a486ff2e31d6158bf39e2608864d63fefd09d5b88ac6868f7010000")
                                    n
                                    tx
                                    channelLocal

                tx <- testHtlcOutput(4)
                                    ("304402207e0410e45454b0978a623f36a10626ef17b27d9ad44e2760f98cfa3efb37924f0220220bd8acd43ecaa916a80bd4f919c495a2c58982ce7c8625153f8596692a801d")
                                    ("30440220549e80b4496803cbc4a1d09d46df50109f546d43fbbf86cd90b174b1484acd5402205f12a4f995cb9bded597eabfee195a285986aa6d93ae5bb72507ebc6a4e2349e")
                                    ("020000000001018154ecccf11a5fb56c39654c4deb4d2296f83c69268280b94d021370c94e219704000000000000000001a00f0000000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e050047304402207e0410e45454b0978a623f36a10626ef17b27d9ad44e2760f98cfa3efb37924f0220220bd8acd43ecaa916a80bd4f919c495a2c58982ce7c8625153f8596692a801d014730440220549e80b4496803cbc4a1d09d46df50109f546d43fbbf86cd90b174b1484acd5402205f12a4f995cb9bded597eabfee195a285986aa6d93ae5bb72507ebc6a4e2349e012004040404040404040404040404040404040404040404040404040404040404048a76a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c8201208763a91418bc1a114ccf9c052d3d23e28d3b0a9d1227434288527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae677502f801b175ac686800000000")
                                    n
                                    tx
                                    channelLocal
            testCase2 ({ chan with ValueToSelf = LNMoney.MilliSatoshis(6993000000L); FeeRatePerKw = FeeRatePerKw 0u })

            /// Commitment tx with seven outputs untrimmed (maximum feerate)
            let testCase3 (channelLocal) =
                let mutable tx = getUnsignedTx channelLocal
                tx <- testCommitment "3045022100a5c01383d3ec646d97e40f44318d49def817fcd61a0ef18008a665b3e151785502203e648efddd5838981ef55ec954be69c4a652d021e6081a100d034de366815e9b"
                                     "304502210094bfd8f5572ac0157ec76a9551b6c5216a4538c07cd13a51af4a54cb26fa14320220768efce8ce6f4a5efac875142ff19237c011343670adf9c7ac69704a120d1163"
                                     "02000000000101bef67e4e2fb9ddeeb3461973cd4c62abb35050b1add772995b820b584a488489000000000038b02b8007e80300000000000022002052bfef0479d7b293c27e0f1eb294bea154c63a3294ef092c19af51409bce0e2ad007000000000000220020403d394747cae42e98ff01734ad5c08f82ba123d3d9a620abda88989651e2ab5d007000000000000220020748eba944fedc8827f6b06bc44678f93c0f9e6078b35c6331ed31e75f8ce0c2db80b000000000000220020c20b5d1f8584fd90443e7b7b720136174fa4b9333c261d04dbbd012635c0f419a00f0000000000002200208c48d15160397c9731df9bc3b236656efb6665fbfe92b4a6878e88a499f741c4c0c62d0000000000160014ccf1af2f2aabee14bb40fa3851ab2301de843110e09c6a00000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e040048304502210094bfd8f5572ac0157ec76a9551b6c5216a4538c07cd13a51af4a54cb26fa14320220768efce8ce6f4a5efac875142ff19237c011343670adf9c7ac69704a120d116301483045022100a5c01383d3ec646d97e40f44318d49def817fcd61a0ef18008a665b3e151785502203e648efddd5838981ef55ec954be69c4a652d021e6081a100d034de366815e9b01475221023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb21030e9f7b623d2ccc7c9bd44d66d5ce21ce504c0acf6385a132cec6d3c39fa711c152ae3e195220"
                                     n
                                     tx
                                     channelLocal
                Expect.equal ((snd tx).Length) 5 ""

                tx <- testHtlcOutput 0
                                    "30440220385a5afe75632f50128cbb029ee95c80156b5b4744beddc729ad339c9ca432c802202ba5f48550cad3379ac75b9b4fedb86a35baa6947f16ba5037fb8b11ab343740"
                                    "304402205999590b8a79fa346e003a68fd40366397119b2b0cdf37b149968d6bc6fbcc4702202b1e1fb5ab7864931caed4e732c359e0fe3d86a548b557be2246efb1708d579a"
                                    "020000000001018323148ce2419f21ca3d6780053747715832e18ac780931a514b187768882bb60000000000000000000122020000000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e05004730440220385a5afe75632f50128cbb029ee95c80156b5b4744beddc729ad339c9ca432c802202ba5f48550cad3379ac75b9b4fedb86a35baa6947f16ba5037fb8b11ab3437400147304402205999590b8a79fa346e003a68fd40366397119b2b0cdf37b149968d6bc6fbcc4702202b1e1fb5ab7864931caed4e732c359e0fe3d86a548b557be2246efb1708d579a012000000000000000000000000000000000000000000000000000000000000000008a76a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c8201208763a914b8bcb07f6344b42ab04250c86a6e8b75d3fdbbc688527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae677502f401b175ac686800000000"
                                    n
                                    tx
                                    channelLocal

                tx <- testHtlcOutput 1
                                     "304402207ceb6678d4db33d2401fdc409959e57c16a6cb97a30261d9c61f29b8c58d34b90220084b4a17b4ca0e86f2d798b3698ca52de5621f2ce86f80bed79afa66874511b0"
                                     "304402207ff03eb0127fc7c6cae49cc29e2a586b98d1e8969cf4a17dfa50b9c2647720b902205e2ecfda2252956c0ca32f175080e75e4e390e433feb1f8ce9f2ba55648a1dac"
                                     "020000000001018323148ce2419f21ca3d6780053747715832e18ac780931a514b187768882bb60100000000000000000124060000000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e050047304402207ceb6678d4db33d2401fdc409959e57c16a6cb97a30261d9c61f29b8c58d34b90220084b4a17b4ca0e86f2d798b3698ca52de5621f2ce86f80bed79afa66874511b00147304402207ff03eb0127fc7c6cae49cc29e2a586b98d1e8969cf4a17dfa50b9c2647720b902205e2ecfda2252956c0ca32f175080e75e4e390e433feb1f8ce9f2ba55648a1dac01008576a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c820120876475527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae67a914b43e1b38138a41b37f7cd9a1d274bc63e3a9b5d188ac6868f6010000"
                                     n
                                     tx
                                     channelLocal

                tx <- testHtlcOutput 2
                                     "304402206a401b29a0dff0d18ec903502c13d83e7ec019450113f4a7655a4ce40d1f65ba0220217723a084e727b6ca0cc8b6c69c014a7e4a01fcdcba3e3993f462a3c574d833"
                                     "3045022100d50d067ca625d54e62df533a8f9291736678d0b86c28a61bb2a80cf42e702d6e02202373dde7e00218eacdafb9415fe0e1071beec1857d1af3c6a201a44cbc47c877"
                                     "020000000001018323148ce2419f21ca3d6780053747715832e18ac780931a514b187768882bb6020000000000000000010a060000000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e050047304402206a401b29a0dff0d18ec903502c13d83e7ec019450113f4a7655a4ce40d1f65ba0220217723a084e727b6ca0cc8b6c69c014a7e4a01fcdcba3e3993f462a3c574d83301483045022100d50d067ca625d54e62df533a8f9291736678d0b86c28a61bb2a80cf42e702d6e02202373dde7e00218eacdafb9415fe0e1071beec1857d1af3c6a201a44cbc47c877012001010101010101010101010101010101010101010101010101010101010101018a76a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c8201208763a9144b6b2e5444c2639cc0fb7bcea5afba3f3cdce23988527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae677502f501b175ac686800000000"
                                     n
                                     tx
                                     channelLocal

                tx <- testHtlcOutput 3
                                     "30450221009b1c987ba599ee3bde1dbca776b85481d70a78b681a8d84206723e2795c7cac002207aac84ad910f8598c4d1c0ea2e3399cf6627a4e3e90131315bc9f038451ce39d"
                                     "3045022100db9dc65291077a52728c622987e9895b7241d4394d6dcb916d7600a3e8728c22022036ee3ee717ba0bb5c45ee84bc7bbf85c0f90f26ae4e4a25a6b4241afa8a3f1cb"
                                     "020000000001018323148ce2419f21ca3d6780053747715832e18ac780931a514b187768882bb6030000000000000000010c0a0000000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e05004830450221009b1c987ba599ee3bde1dbca776b85481d70a78b681a8d84206723e2795c7cac002207aac84ad910f8598c4d1c0ea2e3399cf6627a4e3e90131315bc9f038451ce39d01483045022100db9dc65291077a52728c622987e9895b7241d4394d6dcb916d7600a3e8728c22022036ee3ee717ba0bb5c45ee84bc7bbf85c0f90f26ae4e4a25a6b4241afa8a3f1cb01008576a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c820120876475527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae67a9148a486ff2e31d6158bf39e2608864d63fefd09d5b88ac6868f7010000"
                                     n
                                     tx
                                     channelLocal

                tx <- testHtlcOutput 4
                                     "3045022100cc28030b59f0914f45b84caa983b6f8effa900c952310708c2b5b00781117022022027ba2ccdf94d03c6d48b327f183f6e28c8a214d089b9227f94ac4f85315274f0"
                                     "304402202d1a3c0d31200265d2a2def2753ead4959ae20b4083e19553acfffa5dfab60bf022020ede134149504e15b88ab261a066de49848411e15e70f9e6a5462aec2949f8f"
                                     "020000000001018323148ce2419f21ca3d6780053747715832e18ac780931a514b187768882bb604000000000000000001da0d0000000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e0500483045022100cc28030b59f0914f45b84caa983b6f8effa900c952310708c2b5b00781117022022027ba2ccdf94d03c6d48b327f183f6e28c8a214d089b9227f94ac4f85315274f00147304402202d1a3c0d31200265d2a2def2753ead4959ae20b4083e19553acfffa5dfab60bf022020ede134149504e15b88ab261a066de49848411e15e70f9e6a5462aec2949f8f012004040404040404040404040404040404040404040404040404040404040404048a76a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c8201208763a91418bc1a114ccf9c052d3d23e28d3b0a9d1227434288527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae677502f801b175ac686800000000"
                                     n
                                     tx
                                     channelLocal
                ()
            
            testCase3 ({ chan with ValueToSelf = LNMoney.MilliSatoshis 6993000000L; FeeRatePerKw = FeeRatePerKw 647u })

            /// commitment tx with six outputs untrimmed (minimum feerate)
            let testCase4 channelLocal =
                let mutable tx = getUnsignedTx channelLocal
                tx <- testCommitment "3044022072714e2fbb93cdd1c42eb0828b4f2eff143f717d8f26e79d6ada4f0dcb681bbe02200911be4e5161dd6ebe59ff1c58e1997c4aea804f81db6b698821db6093d7b057"
                                     "3045022100a2270d5950c89ae0841233f6efea9c951898b301b2e89e0adbd2c687b9f32efa02207943d90f95b9610458e7c65a576e149750ff3accaacad004cd85e70b235e27de"
                                     "02000000000101bef67e4e2fb9ddeeb3461973cd4c62abb35050b1add772995b820b584a488489000000000038b02b8006d007000000000000220020403d394747cae42e98ff01734ad5c08f82ba123d3d9a620abda88989651e2ab5d007000000000000220020748eba944fedc8827f6b06bc44678f93c0f9e6078b35c6331ed31e75f8ce0c2db80b000000000000220020c20b5d1f8584fd90443e7b7b720136174fa4b9333c261d04dbbd012635c0f419a00f0000000000002200208c48d15160397c9731df9bc3b236656efb6665fbfe92b4a6878e88a499f741c4c0c62d0000000000160014ccf1af2f2aabee14bb40fa3851ab2301de8431104e9d6a00000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e0400483045022100a2270d5950c89ae0841233f6efea9c951898b301b2e89e0adbd2c687b9f32efa02207943d90f95b9610458e7c65a576e149750ff3accaacad004cd85e70b235e27de01473044022072714e2fbb93cdd1c42eb0828b4f2eff143f717d8f26e79d6ada4f0dcb681bbe02200911be4e5161dd6ebe59ff1c58e1997c4aea804f81db6b698821db6093d7b05701475221023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb21030e9f7b623d2ccc7c9bd44d66d5ce21ce504c0acf6385a132cec6d3c39fa711c152ae3e195220"
                                     n
                                     tx
                                     channelLocal

                tx <- testHtlcOutput 0
                                     "3044022062ef2e77591409d60d7817d9bb1e71d3c4a2931d1a6c7c8307422c84f001a251022022dad9726b0ae3fe92bda745a06f2c00f92342a186d84518588cf65f4dfaada8"
                                     "3045022100a4c574f00411dd2f978ca5cdc1b848c311cd7849c087ad2f21a5bce5e8cc5ae90220090ae39a9bce2fb8bc879d7e9f9022df249f41e25e51f1a9bf6447a9eeffc098"
                                     "02000000000101579c183eca9e8236a5d7f5dcd79cfec32c497fdc0ec61533cde99ecd436cadd10000000000000000000123060000000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e0500473044022062ef2e77591409d60d7817d9bb1e71d3c4a2931d1a6c7c8307422c84f001a251022022dad9726b0ae3fe92bda745a06f2c00f92342a186d84518588cf65f4dfaada801483045022100a4c574f00411dd2f978ca5cdc1b848c311cd7849c087ad2f21a5bce5e8cc5ae90220090ae39a9bce2fb8bc879d7e9f9022df249f41e25e51f1a9bf6447a9eeffc09801008576a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c820120876475527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae67a914b43e1b38138a41b37f7cd9a1d274bc63e3a9b5d188ac6868f6010000"
                                     n
                                     tx
                                     channelLocal

                tx <- testHtlcOutput 1
                                     "3045022100e968cbbb5f402ed389fdc7f6cd2a80ed650bb42c79aeb2a5678444af94f6c78502204b47a1cb24ab5b0b6fe69fe9cfc7dba07b9dd0d8b95f372c1d9435146a88f8d4"
                                     "304402207679cf19790bea76a733d2fa0672bd43ab455687a068f815a3d237581f57139a0220683a1a799e102071c206b207735ca80f627ab83d6616b4bcd017c5d79ef3e7d0"
                                     "02000000000101579c183eca9e8236a5d7f5dcd79cfec32c497fdc0ec61533cde99ecd436cadd10100000000000000000109060000000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e0500483045022100e968cbbb5f402ed389fdc7f6cd2a80ed650bb42c79aeb2a5678444af94f6c78502204b47a1cb24ab5b0b6fe69fe9cfc7dba07b9dd0d8b95f372c1d9435146a88f8d40147304402207679cf19790bea76a733d2fa0672bd43ab455687a068f815a3d237581f57139a0220683a1a799e102071c206b207735ca80f627ab83d6616b4bcd017c5d79ef3e7d0012001010101010101010101010101010101010101010101010101010101010101018a76a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c8201208763a9144b6b2e5444c2639cc0fb7bcea5afba3f3cdce23988527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae677502f501b175ac686800000000"
                                     n
                                     tx
                                     channelLocal

                tx <- testHtlcOutput 2
                                     "3045022100aa91932e305292cf9969cc23502bbf6cef83a5df39c95ad04a707c4f4fed5c7702207099fc0f3a9bfe1e7683c0e9aa5e76c5432eb20693bf4cb182f04d383dc9c8c2"
                                     "304402200df76fea718745f3c529bac7fd37923e7309ce38b25c0781e4cf514dd9ef8dc802204172295739dbae9fe0474dcee3608e3433b4b2af3a2e6787108b02f894dcdda3"
                                     "02000000000101579c183eca9e8236a5d7f5dcd79cfec32c497fdc0ec61533cde99ecd436cadd1020000000000000000010b0a0000000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e0500483045022100aa91932e305292cf9969cc23502bbf6cef83a5df39c95ad04a707c4f4fed5c7702207099fc0f3a9bfe1e7683c0e9aa5e76c5432eb20693bf4cb182f04d383dc9c8c20147304402200df76fea718745f3c529bac7fd37923e7309ce38b25c0781e4cf514dd9ef8dc802204172295739dbae9fe0474dcee3608e3433b4b2af3a2e6787108b02f894dcdda301008576a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c820120876475527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae67a9148a486ff2e31d6158bf39e2608864d63fefd09d5b88ac6868f7010000"
                                     n
                                     tx
                                     channelLocal

                tx <- testHtlcOutput 3
                                     "3044022035cac88040a5bba420b1c4257235d5015309113460bc33f2853cd81ca36e632402202fc94fd3e81e9d34a9d01782a0284f3044370d03d60f3fc041e2da088d2de58f"
                                     "304402200daf2eb7afd355b4caf6fb08387b5f031940ea29d1a9f35071288a839c9039e4022067201b562456e7948616c13acb876b386b511599b58ac1d94d127f91c50463a6"
                                     "02000000000101579c183eca9e8236a5d7f5dcd79cfec32c497fdc0ec61533cde99ecd436cadd103000000000000000001d90d0000000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e0500473044022035cac88040a5bba420b1c4257235d5015309113460bc33f2853cd81ca36e632402202fc94fd3e81e9d34a9d01782a0284f3044370d03d60f3fc041e2da088d2de58f0147304402200daf2eb7afd355b4caf6fb08387b5f031940ea29d1a9f35071288a839c9039e4022067201b562456e7948616c13acb876b386b511599b58ac1d94d127f91c50463a6012004040404040404040404040404040404040404040404040404040404040404048a76a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c8201208763a91418bc1a114ccf9c052d3d23e28d3b0a9d1227434288527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae677502f801b175ac686800000000"
                                     n
                                     tx
                                     channelLocal
                ()

            testCase4 ({ chan with ValueToSelf = LNMoney.MilliSatoshis(6993000000L); FeeRatePerKw = FeeRatePerKw 648u })

            /// commitment tx with six outputs untrimmed (maximum feerate)
            let testCase5 channelLocal =
                let mutable tx = getUnsignedTx channelLocal
                testCommitment "3044022001d55e488b8b035b2dd29d50b65b530923a416d47f377284145bc8767b1b6a75022019bb53ddfe1cefaf156f924777eaaf8fdca1810695a7d0a247ad2afba8232eb4"
                                "304402203ca8f31c6a47519f83255dc69f1894d9a6d7476a19f498d31eaf0cd3a85eeb63022026fd92dc752b33905c4c838c528b692a8ad4ced959990b5d5ee2ff940fa90eea"
                                "02000000000101bef67e4e2fb9ddeeb3461973cd4c62abb35050b1add772995b820b584a488489000000000038b02b8006d007000000000000220020403d394747cae42e98ff01734ad5c08f82ba123d3d9a620abda88989651e2ab5d007000000000000220020748eba944fedc8827f6b06bc44678f93c0f9e6078b35c6331ed31e75f8ce0c2db80b000000000000220020c20b5d1f8584fd90443e7b7b720136174fa4b9333c261d04dbbd012635c0f419a00f0000000000002200208c48d15160397c9731df9bc3b236656efb6665fbfe92b4a6878e88a499f741c4c0c62d0000000000160014ccf1af2f2aabee14bb40fa3851ab2301de84311077956a00000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e040047304402203ca8f31c6a47519f83255dc69f1894d9a6d7476a19f498d31eaf0cd3a85eeb63022026fd92dc752b33905c4c838c528b692a8ad4ced959990b5d5ee2ff940fa90eea01473044022001d55e488b8b035b2dd29d50b65b530923a416d47f377284145bc8767b1b6a75022019bb53ddfe1cefaf156f924777eaaf8fdca1810695a7d0a247ad2afba8232eb401475221023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb21030e9f7b623d2ccc7c9bd44d66d5ce21ce504c0acf6385a132cec6d3c39fa711c152ae3e195220"
                                n
                                tx
                                channelLocal

                Expect.equal (snd tx).Length 4 ""

                testHtlcOutput 0
                               "3045022100d1cf354de41c1369336cf85b225ed033f1f8982a01be503668df756a7e668b66022001254144fb4d0eecc61908fccc3388891ba17c5d7a1a8c62bdd307e5a513f992"
                               "3044022056eb1af429660e45a1b0b66568cb8c4a3aa7e4c9c292d5d6c47f86ebf2c8838f022065c3ac4ebe980ca7a41148569be4ad8751b0a724a41405697ec55035dae66402"
                               "02000000000101ca94a9ad516ebc0c4bdd7b6254871babfa978d5accafb554214137d398bfcf6a0000000000000000000175020000000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e0500483045022100d1cf354de41c1369336cf85b225ed033f1f8982a01be503668df756a7e668b66022001254144fb4d0eecc61908fccc3388891ba17c5d7a1a8c62bdd307e5a513f99201473044022056eb1af429660e45a1b0b66568cb8c4a3aa7e4c9c292d5d6c47f86ebf2c8838f022065c3ac4ebe980ca7a41148569be4ad8751b0a724a41405697ec55035dae6640201008576a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c820120876475527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae67a914b43e1b38138a41b37f7cd9a1d274bc63e3a9b5d188ac6868f6010000"
                               n
                               tx
                               channelLocal

                testHtlcOutput 1
                               "3045022100d065569dcb94f090345402736385efeb8ea265131804beac06dd84d15dd2d6880220664feb0b4b2eb985fadb6ec7dc58c9334ea88ce599a9be760554a2d4b3b5d9f4"
                               "3045022100914bb232cd4b2690ee3d6cb8c3713c4ac9c4fb925323068d8b07f67c8541f8d9022057152f5f1615b793d2d45aac7518989ae4fe970f28b9b5c77504799d25433f7f"
                               "02000000000101ca94a9ad516ebc0c4bdd7b6254871babfa978d5accafb554214137d398bfcf6a0100000000000000000122020000000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e0500483045022100d065569dcb94f090345402736385efeb8ea265131804beac06dd84d15dd2d6880220664feb0b4b2eb985fadb6ec7dc58c9334ea88ce599a9be760554a2d4b3b5d9f401483045022100914bb232cd4b2690ee3d6cb8c3713c4ac9c4fb925323068d8b07f67c8541f8d9022057152f5f1615b793d2d45aac7518989ae4fe970f28b9b5c77504799d25433f7f012001010101010101010101010101010101010101010101010101010101010101018a76a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c8201208763a9144b6b2e5444c2639cc0fb7bcea5afba3f3cdce23988527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae677502f501b175ac686800000000"
                               n
                               tx
                               channelLocal

                testHtlcOutput 2
                               "3045022100d4e69d363de993684eae7b37853c40722a4c1b4a7b588ad7b5d8a9b5006137a102207a069c628170ee34be5612747051bdcc087466dbaa68d5756ea81c10155aef18"
                               "304402200e362443f7af830b419771e8e1614fc391db3a4eb799989abfc5ab26d6fcd032022039ab0cad1c14dfbe9446bf847965e56fe016e0cbcf719fd18c1bfbf53ecbd9f9"
                               "02000000000101ca94a9ad516ebc0c4bdd7b6254871babfa978d5accafb554214137d398bfcf6a020000000000000000015d060000000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e0500483045022100d4e69d363de993684eae7b37853c40722a4c1b4a7b588ad7b5d8a9b5006137a102207a069c628170ee34be5612747051bdcc087466dbaa68d5756ea81c10155aef180147304402200e362443f7af830b419771e8e1614fc391db3a4eb799989abfc5ab26d6fcd032022039ab0cad1c14dfbe9446bf847965e56fe016e0cbcf719fd18c1bfbf53ecbd9f901008576a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c820120876475527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae67a9148a486ff2e31d6158bf39e2608864d63fefd09d5b88ac6868f7010000"
                               n
                               tx
                               channelLocal

                testHtlcOutput 3
                               "30450221008ec888e36e4a4b3dc2ed6b823319855b2ae03006ca6ae0d9aa7e24bfc1d6f07102203b0f78885472a67ff4fe5916c0bb669487d659527509516fc3a08e87a2cc0a7c"
                               "304402202c3e14282b84b02705dfd00a6da396c9fe8a8bcb1d3fdb4b20a4feba09440e8b02202b058b39aa9b0c865b22095edcd9ff1f71bbfe20aa4993755e54d042755ed0d5"
                               "02000000000101ca94a9ad516ebc0c4bdd7b6254871babfa978d5accafb554214137d398bfcf6a03000000000000000001f2090000000000002200204adb4e2f00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e05004830450221008ec888e36e4a4b3dc2ed6b823319855b2ae03006ca6ae0d9aa7e24bfc1d6f07102203b0f78885472a67ff4fe5916c0bb669487d659527509516fc3a08e87a2cc0a7c0147304402202c3e14282b84b02705dfd00a6da396c9fe8a8bcb1d3fdb4b20a4feba09440e8b02202b058b39aa9b0c865b22095edcd9ff1f71bbfe20aa4993755e54d042755ed0d5012004040404040404040404040404040404040404040404040404040404040404048a76a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c8201208763a91418bc1a114ccf9c052d3d23e28d3b0a9d1227434288527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae677502f801b175ac686800000000"
                               n
                               tx
                               channelLocal
                ()

            testCase5 ({ chan with ValueToSelf = LNMoney.MilliSatoshis(6993000000L); FeeRatePerKw = FeeRatePerKw 2069u })
            ()

    ]