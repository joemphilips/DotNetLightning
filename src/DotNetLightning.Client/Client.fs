namespace DotNetLightning.Server.Client

open FSharp.Control.Tasks
open System.Threading.Tasks
open System.Text.Json

open System.Net.Http

type DotNetLightningClient() =
    let _client = new HttpClient()
    member this.GetNewAddress() = task {
        let! response = _client.GetAsync("/api/v1/address")
        let mutable address = null
        if (response.IsSuccessStatusCode) then
            let! content = response.Content.ReadAsStringAsync()
            address <- content
        return address
    }
        
    member this.SendAsync(method: HttpMethod, body: obj, relativePath:string, parameters: obj[]) = task {
        let httpRequestMessage = this.CreateMessage()
        let! result = _client.SendAsync(httpRequestMessage)
        return failwith ""
    }
    
    member this.CreateMessage() =
        failwith ""
