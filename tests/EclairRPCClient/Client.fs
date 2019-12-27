namespace EclairRPCClient

open System
open System.Net
open System.Net.Http

type ClientConfig = {
    Host: string
    Port: int
    AuthCredentials: AuthCredentials
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
        let req = new HttpRequestMessage(HttpMethod.Get, uri)
        let r = conf.HttpClient.PostAsync()
        failwith ""
        
    let private eclairCall<'T>(conf, command: string, parameters: (string * string) list) =
        let req = buildRequest(conf, command, parameters)
        async {
            let! response = sendRequest(req)
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
