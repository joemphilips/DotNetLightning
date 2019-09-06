[<AutoOpen>]
module internal PeerActors

open System
open System.IO.Pipelines

open FSharp.Control.Tasks

open CustomEventAggregator
open DotNetLightning.Infrastructure
open DotNetLightning.Utils.Primitives

type internal PeerManagerEntity = {
    PM: PeerManager
    Id: PeerId
    CM: IChannelManager
    EventAggregator: IEventAggregator
}

type internal PeerActors(a, b) =
    let pipePair = DuplexPipe.CreatePair()
    member val Initiator: PeerManagerEntity = a
    member val Responder: PeerManagerEntity = b
    member val InitiatorToTransport: IDuplexPipe = pipePair.Transport with get
    member val ResponderToTransport: IDuplexPipe = pipePair.Application with get
    member val InitiatorTask = null with get, set
    member val ResponderTask = null with get, set
    member this.Launch(nodeIdForResponder: NodeId) = task {
            let! r = this.Initiator.PM.NewOutBoundConnection(nodeIdForResponder, this.Responder.Id, this.InitiatorToTransport.Output)
            match r with Ok r -> () | Result.Error e -> failwithf "%A" e
            this.InitiatorTask <- task {
                while true do
                    do! this.Initiator.PM.ProcessMessageAsync(this.Responder.Id, this.InitiatorToTransport)
            }
            this.ResponderTask <- task {
                while true do
                    do! this.Responder.PM.ProcessMessageAsync(this.Initiator.Id, this.ResponderToTransport)
            }
        }
    interface IDisposable with
        member this.Dispose() =
            ()
            

