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
        NodeParams: NodeParams
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
            let fees = txb.EstimateFees(feerate.AsNBitcoinFeeRate())
            txb.SendFees(fees).Send(dest, amount) |> ignore
            this.DummyTx <- txb.BuildTransaction(true)
            (this.DummyTx |> FinalizedTx, 0us |> TxOutIndex) |> RResult.Good

type DummyBroadCaster() =
    interface IBroadCaster with
        member this.BroadCastTransaction(tx: Transaction) =
            async { return tx.GetTxId() }

let getAliceParam() =
    let p = NodeParams()
    p.Alias <- "alice"
    p.Color <- { RGB.Red = 1uy; Green = 2uy; Blue = 3uy }
    p.PublicAddresses <- [IPEndPoint.Parse("127.0.0.1:9731")]
    p.MaxHTLCValueInFlightMSat <- LNMoney.MilliSatoshis(150000000UL)
    p.MaxAcceptedHTLCs <- 100us
    // p.ExpirtyDeltaBlocks <- 144
    p.HTLCMinimumMSat <- LNMoney.Zero
    p.MinimumDepth <- 3us |> BlockHeightOffset
    // p.SmartFeeNBlocks <- 3
    p.ToRemoteDelayBlocks <- BlockHeightOffset 720us
    p.MaxToLocalDelayBlocks <- BlockHeightOffset 1000us
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
    let p = NodeParams()
    let keyRepo = DefaultKeyRepository(bobChannelKeysSeed)
    {
        TestEntity.Seed = [| for _ in 0..31 -> 1uy |] |> uint256
        KeyRepo = keyRepo
        NodeParams = p
    }
