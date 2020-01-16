namespace EclairRPCClient

open System
open System.Net
open System.Net.Http
open System.Net.WebSockets
open System.Text.Json

type IAuth =
    abstract member RefreshCode: unit -> bool
    abstract member SetAuthorization: HttpRequestMessage -> HttpRequestMessage
    abstract member SegWebSocketAuth: ClientWebSocket -> ClientWebSocket

type ClientConfig = {
    Host: string
    Port: int
    AuthCredentials: AuthCredentials
    Auth: IAuth
}
    with
    member this.RpcUri =
        this.Host + this.Port.ToString()
        
    member this.HttpClient =
        new HttpClient()
        
    interface IDisposable with
        member this.Dispose() = this.HttpClient.Dispose()
and AuthCredentials = {
    Password: string
}

module Client =
    let private buildRequest(conf: ClientConfig, methodName: string, parameters: (string * string) list) =
        let uri = conf.RpcUri + sprintf "/%s" methodName
        let username = ""
        let pass = conf.AuthCredentials.Password
        let req = new HttpRequestMessage(HttpMethod.Post, uri)
        req
        
    let private sendRequest(conf: ClientConfig, req) =
        conf.HttpClient.SendAsync(req) |> Async.AwaitTask
        
    let private getPayload<'T>(response: HttpResponseMessage) =
        let failureCase(resp)= async {
            return failwith ""
        }
        let successCase(resp: HttpResponseMessage) = async {
            let cl = response.Content.Headers.ContentLength |> Option.ofNullable
            if (cl.IsSome && cl.Value = 0L) then
                return Unchecked.defaultof<'T>
            else if (resp.Content.Headers.ContentType.MediaType.Equals("application/json", StringComparison.Ordinal)) then
                let! content = resp.Content.ReadAsStringAsync() |> Async.AwaitTask
                return JsonSerializer.Deserialize(content)
            else
                return failwithf "Unexpected http content type %A" resp.Content
        }
        
        if (response.IsSuccessStatusCode) then
            successCase(response)
        else if (response.StatusCode = HttpStatusCode.InternalServerError) then
            response.EnsureSuccessStatusCode() |> ignore
            failureCase(response)
        else
            failureCase(response)
        
    let private eclairCall<'T>(conf, command: string, parameters: (string * string) list) =
        let req = buildRequest(conf, command, parameters)
        async {
            let! response = sendRequest(conf, req)
            let! payload = getPayload(response)
            let validated = payload.validate<'T>()
            let parsed = parseResult(validated, payload, command)
            return parsed
        }
        
    let allChannelsImpl() =
        failwith ""
    let rec interpret = function
        | Pure x -> x
        | Free(AllChannels((), next)) -> allChannelsImpl() |>  next |> interpret
