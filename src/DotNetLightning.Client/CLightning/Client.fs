namespace DotNetLightning.Client.CLightning

open NBitcoin
open System
open System.Threading

open DotNetLightning.Client
open DotNetLightning.Client.CLightning.DTO
open System.IO
open System.Net
open System.Net.Sockets
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading.Tasks
open FSharp.Control.Tasks

type private CLightningRequest = {
    [<JsonPropertyName("id")>]
    Id: int
    [<JsonPropertyName("method")>]
    Method: string
    [<JsonPropertyName("params")>]
    Params: obj array
}
    with
    static member Create(method, ?parameters) =
        let parameters = Option.defaultValue [||] parameters
        {
            Id = 0
            Method = method
            Params = parameters
        }
        
type CLightningClient(address: Uri, n: Network) =
    do checkNull address
    do checkNull n
    let mutable addr = address
    do
        if addr.Scheme = "file" then
            addr <-
                let a = UriBuilder(address)
                a.Scheme <- "unix"
                a.Uri
        else
            ()
    member val Network = n with get
    member val Address = addr with get
    
    member private this.Connect(): Task<Socket> =
        task {
            let mutable sock = null
            let mutable endpoint = null
            if (this.Address.Scheme = "tcp") then
                let domain = this.Address.DnsSafeHost
                let mutable addr = null
                match IPAddress.TryParse domain with
                | false, _ ->
                    let! addresses = Dns.GetHostAddressesAsync(domain)
                    addr <- addresses |> Seq.tryHead |> Option.defaultWith (fun _ -> failwith "Host not found")
                | true, a ->
                    addr <- a
                sock <- new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
                endpoint <- IPEndPoint(addr, this.Address.Port) :> EndPoint
            else if (this.Address.Scheme = "unix") then
                let mutable path = this.Address.AbsoluteUri.Remove(0, "unix:".Length)
                if (not <| path.StartsWith("/")) then
                    path <- "/" + path
                while path.Length >= 2 && (path.[0] <> '/' || path.[1] <> '/') do
                    path <- path.Remove(0, 1)
                if (path.Length < 2) then
                    raise <| FormatException("Invalid unix url")
                sock <- new Socket(AddressFamily.Unix, SocketType.Stream, ProtocolType.IP)
                endpoint <- UnixDomainSocketEndPoint(path) :> EndPoint
            else
                raise <| NotSupportedException(sprintf "Protocol %A for clightning not supported" this.Address.Scheme)
            do! sock.ConnectAsync(endpoint)
            return sock
        }
    
    member private this.SendCommandAsync<'T>(command: string, ?parameters: obj array, ?noReturn: bool, ?isArray: bool, ?ct: CancellationToken): Task<'T> =
        let parameters = Option.defaultValue [||] parameters
        let noReturn = Option.defaultValue false noReturn
        let isArray = Option.defaultValue false isArray
        let ct = Option.defaultValue (Unchecked.defaultof<CancellationToken>) ct
        task {
            use! socket = this.Connect()
            use networkStream = new NetworkStream(socket)
            
            let opts = JsonSerializerOptions()
            opts.Converters.Add(JsonFSharpConverter())
            let req = CLightningRequest.Create(command, parameters)
            do! JsonSerializer.SerializeAsync(networkStream, req, opts, ct)
            
            do! networkStream.FlushAsync(ct)
            
            let! result =
                let opts = JsonDocumentOptions()
                JsonDocument.ParseAsync(networkStream, opts, ct)
            use _ = ct.Register(fun _ -> socket.Dispose())
            try
                match result.RootElement.TryGetProperty("error") with
                | true, e ->
                    return
                        LightningRPCException.Create (e.GetProperty("message").GetString()) (e.GetProperty("code").GetInt32())
                        |> raise
                | false, _ ->
                    if noReturn then
                        return Unchecked.defaultof<'T>
                    else if isArray then
                        let arrayEnumerator = result.RootElement.GetProperty("result").EnumerateArray()
                        // get only first data
                        return arrayEnumerator.Current.ToObject<'T>()
                    else
                        return result.RootElement.GetProperty("result").ToObject<'T>()
            with
            | exn when ct.IsCancellationRequested ->
                ct.ThrowIfCancellationRequested()
                return failwith "Unreahable"
        }
    
    member private this.SendAsync(bolt11: string, ?ct: CancellationToken) =
        checkNull bolt11
        let ct = Option.defaultValue (Unchecked.defaultof<CancellationToken>) ct
        let bolt11Shorten = bolt11.Replace("lightning:", "").Replace("LIGHTNING:", "")
        task {
            return! this.SendCommandAsync<obj>("pay", [| bolt11Shorten |], true, ct = ct)
        }

    member this.GetInfoAsync(ct) =
        this.SendCommandAsync<GetInfoResponse>("getinfo", ct = ct)
