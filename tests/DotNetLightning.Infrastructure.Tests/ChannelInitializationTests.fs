module ChannelInitializationTests

open FSharp.Control.Reactive
open System.Reactive.Threading.Tasks
open System.Reactive.Linq
open System.Reactive.Linq
open DotNetLightning.Utils.Primitives
open Expecto
open NBitcoin
open CustomEventAggregator
open DotNetLightning.LN


module internal Helper =
    let hex = NBitcoin.DataEncoders.HexEncoder()
    let bobNodeSecret =
        Key(hex.DecodeData("0202020202020202020202020202020202020202020202020202020202020202"))
    let bobNodeId = bobNodeSecret.PubKey |> NodeId
    let eventAggregator = new ReactiveEventAggregator() :> IEventAggregator
      
    let getBob() = {
        PeerManagerEntity.Id = failwith ""
        PM = failwith "todo"
    }
    
    let getAlice() = {
        PeerManagerEntity.Id = failwith ""
        PM = failwith "todo"
    }
    
    let initiateActor() = async {
        let b = getBob()
        let actors = new PeerActors(getAlice(), b)
        return! actors.Launch(bobNodeId) |> Async.AwaitTask
        let! _ = eventAggregator.GetObservable<PeerEvent>()
                     |> Observable.first
                     |> fun o -> o.ToTask() |> Async.AwaitTask
        return ()
    }
    
[<Tests>]
let tests =
    testList "Channel initialization between peers" [
        testAsync "" {
            return ()
        }
    ]

