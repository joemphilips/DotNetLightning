namespace DotNetLightning.Infrastructure.Auth

open NBitcoin.DataEncoders
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Net.WebSockets


type IAuth =
    abstract member RefreshCache: unit -> bool
    abstract member SetAuthorization: HttpRequestMessage -> unit
    abstract member SetWebSocketAuth: ClientWebSocket -> unit
    
type CookieAuthentication(path: string) =
    let mutable _cachedAuth = null
    interface IAuth with
        member this.RefreshCache() =
            try
                let cookieData = File.ReadAllText(path) |> Encoders.ASCII.DecodeData |> Encoders.Base64.EncodeData
                _cachedAuth <- AuthenticationHeaderValue("Basic", cookieData)
                true
            with
            | _ -> false
            
        member this.SetAuthorization(msg) =
            msg.Headers.Authorization <- _cachedAuth
            
        member this.SetWebSocketAuth(socket: ClientWebSocket) =
            if (not <| isNull _cachedAuth) then
                socket.Options.SetRequestHeader("Authorization", sprintf "%s %s" (_cachedAuth.Scheme) (_cachedAuth.Parameter))
                
                
[<AutoOpen>]
module NullAuth =
    let nullAuth = { new IAuth with
                       member this.RefreshCache() = false
                       member this.SetAuthorization(msg) = ()
                       member this.SetWebSocketAuth(sock) = () }
