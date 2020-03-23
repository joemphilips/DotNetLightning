module TestConstants

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Chain
open DotNetLightning.Infrastructure
open DotNetLightning.Transactions
open System.Net
open Foq

type TestEntity =
    {
        Seed: uint256
        KeyRepo: IKeysRepository
        NodeParams: ChainConfig
    }

let fundingSatoshis = 1000000L |> Money.Satoshis
let pushMsat = 200000000L |> LNMoney.MilliSatoshis
let feeratePerKw = 10000u |> FeeRatePerKw 
let hex = NBitcoin.DataEncoders.HexEncoder()
let aliceNodeSecret = 
    Key(hex.DecodeData("1111111111111111111111111111111111111111111111111111111111111111"))
        
let aliceChannelKeysSeed = 
    hex.DecodeData("2222222222222222222222222222222222222222222222222222222222222222")
    |> uint256
      
let bobNodeSecret =
    Key(hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202"))
    // Key(hex.DecodeData("3333333333333333333333333333333333333333333333333333333333333333"))
    
let bobChannelKeysSeed =
    hex.DecodeData("4444444444444444444444444444444444444444444444444444444444444444")
    |> uint256
    

type DummyFundingTxProvider(n: Network) =
    member val DummyTx = null with get, set
    interface IFundingTxProvider with
        member this.ProvideFundingTx(dest: IDestination, amount: Money, feerate: FeeRatePerKw) =
            let txb = n.CreateTransactionBuilder()
            txb.ShuffleRandom <- null
            let dummyKey =
                "5555555555555555555555555555555555555555555555555555555555555555"
                |> hex.DecodeData |> Key
            let coin =
                let inputAmount = amount + Money.Satoshis(6000000L)
                let dummyTxid = [| for _ in 0..31 -> 1uy |] |> uint256
                Coin(dummyTxid, 0u, inputAmount, dummyKey.PubKey.WitHash.ScriptPubKey)
            let dummyChange =
                "6666666666666666666666666666666666666666666666666666666666666666"
                |> hex.DecodeData |> Key
            
            txb.AddCoins(coin)
               .AddKeys(dummyKey, dummyChange) |> ignore
            txb.SetChange(dummyChange) |> ignore
            txb.Send(dest, amount) |> ignore
            let fees = txb.EstimateFees(feerate.AsNBitcoinFeeRate())
            txb.SendFees(fees) |> ignore
            this.DummyTx <- txb.BuildTransaction(true)
            (this.DummyTx |> FinalizedTx, 1us |> TxOutIndex) |> Ok

type DummyBroadCaster() =
    interface IBroadCaster with
        member this.BroadCastTransaction(tx: Transaction) =
            async { return tx.GetTxId() }

let getAliceParam() =
    let p = ChainConfig()
    p.Alias <- "alice"
    p.Color <- { RGB.Red = 1uy; Green = 2uy; Blue = 3uy }
    p.PublicAddresses <- [IPEndPoint.Parse("127.0.0.1:9731")]
    p.MaxHTLCValueInFlightMSat <- LNMoney.MilliSatoshis(150000000UL)
    p.MaxAcceptedHTLCs <- 100us
    // p.ExpirtyDeltaBlocks <- 144
    p.HTLCMinimumMSat <- LNMoney.Zero
    p.MinimumDepth <- 3u |> BlockHeightOffset
    // p.SmartFeeNBlocks <- 3
    p.ToRemoteDelayBlocks <- BlockHeightOffset 720u
    p.MaxToLocalDelayBlocks <- BlockHeightOffset 1000u
    p.FeeBaseMSat <- 546000UL |> LNMoney.MilliSatoshis
    p.FeeProportionalMillionths <- 10u
    p.ReserveToFundingRatio <- 0.01
    p.DBType <- SupportedDBType.Null
    let keyRepo =
        DefaultKeyRepository(aliceChannelKeysSeed)
    {
        TestEntity.Seed = [| for _ in 0..31 -> 0uy |] |> uint256
        KeyRepo = keyRepo
        NodeParams = p
    }
    
let getBobParam() =
    let p = ChainConfig()
    let keyRepo = DefaultKeyRepository(bobChannelKeysSeed)
    {
        TestEntity.Seed = [| for _ in 0..31 -> 1uy |] |> uint256
        KeyRepo = keyRepo
        NodeParams = p
    }
