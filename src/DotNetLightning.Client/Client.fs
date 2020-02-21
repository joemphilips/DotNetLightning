namespace DotNetLightning.Client

open System.Net.Http
open FSharp.Control.Tasks

open DotNetLightning.Client
open System
open System.Net
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks


open DotNetLightning.Infrastructure
open DotNetLightning.Infrastructure.Error
open DotNetLightning.Infrastructure.Auth

open System.Text.Json.Serialization

type DotNetLightningClient(network: DotNetLightningNetwork, ?serverAddress: Uri) =
    let serverAddress =
        Option.defaultValue (DotNetLightningDefaultSettings.getSettings(network.NetworkType)).DefaultUrl serverAddress
    let cryptoCode = network.CryptoCode
    let _client = new HttpClient()
    
    let _opts = 
        let opts = (JsonSerializerOptions())
        opts.Converters.Add(JsonFSharpConverter())
        opts
    let _serializeAsync i =
        JsonSerializer.SerializeAsync(i , _opts)
        
    let _deserializeAsync i =
        JsonSerializer.DeserializeAsync(i, _opts)
        
    let _serialize i =
        JsonSerializer.Serialize(i, _opts)
    let _deserializeString (i: string) =
        JsonSerializer.Deserialize(i, _opts)
        
    
    let mutable _auth = nullAuth
    
    member private this.GetFullUri(relativePath: string, ?parameters: obj[]) =
        let parameters = Option.defaultValue [|obj()|] parameters
        let relativePath = String.Format(relativePath, parameters)
        let mutable uri = serverAddress.AbsoluteUri
        if (not <| uri.EndsWith("/", StringComparison.Ordinal)) then
            uri <- uri + "/"
        uri <- uri + relativePath
        uri
    member private this.CreateMessageAsync(method, body: obj option, relativepath, parameters) =
        let uri = this.GetFullUri(relativepath, parameters)
        let msg = new HttpRequestMessage(method, uri)
        _auth.SetAuthorization(msg)
        task {
            match body with
            | Some b ->
                    match b with
                    | :? array<byte> as byteBody ->  msg.Content <- new ByteArrayContent(byteBody)
                    | _ ->
                        msg.Content <-
                            let jsonBody = _serialize(msg)
                            new StringContent(jsonBody, Encoding.UTF8, "application/json")
            | None -> ()
            return msg
        }
        
    member private this.ParseResponseAsync<'T>(response: HttpResponseMessage): Task<'T> =
        task {
            let returnFailure(r: HttpResponseMessage) =
                task {
                    if (r.StatusCode = HttpStatusCode.InternalServerError) then
                        r.EnsureSuccessStatusCode() |> ignore
                    let! errorJson = response.Content.ReadAsStreamAsync()
                    let! err = _deserializeAsync errorJson
                    let err: DotNetLightningError = err
                    return raise <| err.RaiseAsException()
                }
            use r = response
            if (r.IsSuccessStatusCode) then
                let header = r.Content.Headers
                let cLength = header.ContentLength
                if (not <| cLength.HasValue || cLength.Value = 0L) then
                    return Unchecked.defaultof<'T>
                else if (header.ContentType.MediaType.Equals("application/json", StringComparison.Ordinal)) then
                    let! str = r.Content.ReadAsStringAsync().ConfigureAwait(false)
                    return _deserializeString(str)
                else if (header.ContentType.MediaType.Equals("application/octet-stream")) then
                    let! b = r.Content.ReadAsByteArrayAsync().ConfigureAwait(false)
                    return b :> obj :?> 'T
                else
                    return! returnFailure(r)
            else
                return! returnFailure(r)
        }
    member private this.SendAsync<'T>(method: HttpMethod, body: obj option, relativePath:string, parameters: obj[], ct:CancellationToken) = task {
        let sendAsyncCore() =
            task {
                let! msg = this.CreateMessageAsync(method, body, relativePath, parameters)
                return! _client.SendAsync(msg, ct).ConfigureAwait(false)
            }
        let! result = sendAsyncCore()
        if (result.StatusCode = HttpStatusCode.NotFound) then return Unchecked.defaultof<'T> else
        if (result.StatusCode = HttpStatusCode.Unauthorized) then
            _auth.RefreshCache() |> ignore
            let! result = sendAsyncCore()
            return! this.ParseResponseAsync<'T>(result).ConfigureAwait(false)
        else
            return! this.ParseResponseAsync<'T>(result).ConfigureAwait(false)
    }
    
    member this.SetNullAuth() =
        _auth <- nullAuth
        
    member this.SetCookieAuth(path: string) =
        checkNull path
        _auth <- CookieAuthentication(path)
        _auth.RefreshCache()
        
    member this.GetNewAddress() = task {
        let! response = _client.GetAsync("/api/v1/address")
        let mutable address = null
        if (response.IsSuccessStatusCode) then
            let! content = response.Content.ReadAsStringAsync()
            address <- content
        return address
    }
