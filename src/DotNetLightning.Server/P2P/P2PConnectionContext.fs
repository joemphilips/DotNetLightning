namespace DotNetLightning.Server.P2P

open DotNetLightning.Utils.Primitives
open System.Collections.Generic
open Microsoft.AspNetCore.Connections
open Microsoft.AspNetCore.Connections.Features
open Microsoft.AspNetCore.Http.Features

type A = Microsoft.AspNetCore.Connections.Features.IConnectionTransportFeature

type P2PConnectionContext(peerId: PeerId) as this =
    inherit ConnectionContext()
    let features = FeatureCollection() :> IFeatureCollection
    do
        features.Set<IConnectionHeartbeatFeature>(this)
        features.Set<IConnectionTransportFeature>(this)
        features.Set<IConnectionIdFeature>(this)
    
    override val ConnectionId = peerId.ToString() with get, set
    override val Features = features with get
    override val Items: IDictionary<_ ,_> = failwith "" with get, set
    override val Transport  = failwith "" with get,set
    
    
    interface IConnectionTransportFeature with
        member this.Transport
            with get () = this.Transport
            and set v = this.Transport <- v
        
    interface IConnectionIdFeature with
        member this.ConnectionId
            with get () = this.ConnectionId
            and set v = this.ConnectionId <- v
        
    interface IConnectionHeartbeatFeature with
        member this.OnHeartbeat(action, state) =
            failwith ""

