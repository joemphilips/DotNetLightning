namespace DotNetLightning.ClnRpc

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Runtime.InteropServices
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Newtonsoft.Json.Linq
open NBitcoin

open DotNetLightning.ClnRpc.SystemTextJsonConverters
open DotNetLightning.ClnRpc.NewtonsoftJsonConverters

// fsharplint:disable enumCasesNames
type CLightningClientErrorCodeEnum =
    // -- errors from `pay`, `sendpay` or `waitsendpay` commands --
    | IN_PROGRESS = 200
    | RHASH_ALREADY_USED = 201
    | UNPARSABLE_ONION = 202
    | DESTINATION_PERM_FAIL = 203
    | TRY_OTHER_ROUTE = 204
    | ROUTE_NOT_FOUND = 205
    | ROUTE_TOO_EXPENSIVE = 206
    | INVOICE_EXPIRED = 207
    | NO_SUCH_PAYMENT = 208
    | UNSPECIFIED_ERROR = 209
    | STOPPED_RETRYING = 210
    | PAY_STATUS_UNEXPECTED = 211
    | PAY_OFFER_INVALID = 212

    // -- `fundchannel` or `withdraw` errors --
    | MAX_EXCEEDED = 300
    | CANNOT_AFFORD = 301
    | FUND_OUTPUT_IS_DUST = 302
    | FUNDING_BROADCAST_FAIL = 303
    | FUNDING_STILL_SYNCING_BITCOIN = 304
    | FUNDING_PEER_NOT_CONNECTED = 305
    | FUNDING_UNKNOWN_PEER = 306
    | FUNDING_NOTHING_TO_CANCEL = 307
    | FUNDING_CANCEL_NOT_SAFE = 308
    | FUNDING_PSBT_INVALID = 309
    | FUNDING_V2_NOT_SUPPORTED = 310
    | FUNDING_UNKNOWN_CHANNEL = 311
    | FUNDING_STATE_INVALID = 312

    // -- `connect` errors --
    | CONNECT_NO_KNOWN_ADDRESS = 400
    | CONNECT_ALL_ADDRESSES_FAILED = 401

    // -- Errors from `invoice` or `delinvoice` commands
    | INVOICE_LABEL_ALREADY_EXISTS = 900
    | INVOICE_PREIMAGE_ALREADY_EXISTS = 901
    | INVOICE_HINTS_GAVE_NO_ROUTES = 902
    | INVOICE_EXPIRED_DURING_WAIT = 903
    | INVOICE_WAIT_TIMED_OUT = 904
    | INVOICE_NOT_FOUND = 905
    | INVOICE_STATUS_UNEXPECTED = 906
    | INVOICE_OFFER_INACTIVE = 907

    // -- Errors from HSM crypto operations. --
    | HSM_ECDH_FAILED = 800

    // -- Errors from `offer` commands --
    | OFFER_ALREADY_EXISTS = 1000
    | OFFER_ALREADY_DISABLED = 1001
    | OFFER_EXPIRED = 1002
    | OFFER_ROUTE_NOT_FOUND = 1003
    | OFFER_BAD_INREQ_REPLY = 1004
    | OFFER_TIMEOUT = 1005

    // -- Errors from datastore command --
    | DATASTORE_DEL_DOES_NOT_EXIST = 1200
    | DATASTORE_DEL_WRONG_GENERATION = 1201
    | DATASTORE_UPDATE_ALREADY_EXISTS = 1201
    | DATASTORE_UPDATE_DOES_NOT_EXIST = 1203
    | DATASTORE_UPDATE_WRONG_GENERATION = 1204
    | DATASTORE_UPDATE_HAS_CHILDREN = 1205
    | DATASTORE_UPDATE_NO_CHILDREN = 1206

    // -- Errors from `wait` commands --
    | WAIT_TIMEOUT = 2000

// fsharplint:enable

/// see 5.1: "Error object" in https://www.jsonrpc.org/specification
type JsonRPCErrorCodeEnum =
    | ParseError = -32700
    | InvalidRequest = -32600
    | MethodNotFound = -32601
    | InvalidParams = -32602
    | InternalError = -32603

[<Struct>]
type CLightningClientErrorCode =
    | Known of knownCode: CLightningClientErrorCodeEnum
    | Unknown of unknownCode: int
    | KnownReservedJsonRPCError of knownJsonErrorCode: JsonRPCErrorCodeEnum
    | UnknownReservedJsonRPCError of unknownJsonErrorCode: int

    static member FromInt(i: int) =
        if Enum.IsDefined(typeof<CLightningClientErrorCodeEnum>, i) then
            Known(LanguagePrimitives.EnumOfValue i)
        else if Enum.IsDefined(typeof<JsonRPCErrorCodeEnum>, i) then
            KnownReservedJsonRPCError(LanguagePrimitives.EnumOfValue i)
        else if -32099 < i && i < -32000 then
            UnknownReservedJsonRPCError i
        else
            Unknown i

    member this.AsInt =
        match this with
        | Known s -> LanguagePrimitives.EnumToValue s
        | Unknown s -> s
        | KnownReservedJsonRPCError s -> LanguagePrimitives.EnumToValue s
        | UnknownReservedJsonRPCError s -> s

[<Struct>]
type CLightningRPCError =
    {
        Code: CLightningClientErrorCode
        Msg: string
    }

type CLightningRPCException(e: CLightningRPCError) =
    inherit Exception($"code: {e.Code}. Msg: {e.Msg}")
    member val Code = e.Code

type JsonLibraryType =
    | SystemTextJson = 0
    | Newtonsoft = 1

/// c-lightning rpc client.
type ClnClient
    /// <param name="network">bitcoin network name</param>
    /// <param name="address">
    ///     Uri to connect for RPC call. it can be either tcp or unix socket.
    /// </param>
    /// <param name="jsonLibrary">Which json serializer you want to use.</param>
    /// <param name="getTransport">Stream getter.
    ///     If you specify <c>address</c>, than it will automatically use NetworkStream.
    ///     Otherwise, you can set other stream. This is useful for testing or STDIN/STDOUT for plugin.
    /// </param>
    (
        network: Network,
        [<Optional; DefaultParameterValue(null: Uri)>] address: Uri,
        [<Optional; DefaultParameterValue(JsonLibraryType.SystemTextJson)>] jsonLibrary: JsonLibraryType,
        [<Optional;
          DefaultParameterValue(null: Func<CancellationToken, Task<Stream>>)>] getTransport: Func<CancellationToken, Task<Stream>>
    ) as this =
    let utf8 = UTF8Encoding()

    let getAddr(domain: string) =
        task {
            match IPAddress.TryParse domain with
            | true, addr -> return addr
            | false, _ ->
                let! a =
                    Dns
                        .GetHostAddressesAsync(domain)
                        .ConfigureAwait false

                return
                    a
                    |> Array.tryHead
                    |> Option.defaultWith(fun () -> failwith "Host not found")
        }

    let getTransportStream =
        if getTransport |> isNull then
            Func<CancellationToken, Task<Stream>>(fun ct ->
                task {
                    let! socket = this.Connect(ct)
                    return new NetworkStream(socket) |> unbox
                }
            )
        else
            getTransport

    let mutable _nextId = 0

    let jsonOpts = JsonSerializerOptions()

    let newtonSoftJsonOpts =
        Newtonsoft.Json.JsonSerializerSettings(
            NullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore
        )

    do
        if address |> isNull && getTransport |> isNull then
            raise
            <| ArgumentException(
                $"you must specify either {nameof(address)} or {nameof getTransport} as {nameof(ClnClient)} constructor option"
            )
        else if address |> isNull |> not && getTransport |> isNull |> not then
            raise
            <| ArgumentException(
                $"you must specify either {nameof(address)} or {nameof getTransport} as {nameof(ClnClient)} constructor option. not both"
            )
        else
            match jsonLibrary with
            | JsonLibraryType.SystemTextJson ->
                jsonOpts.AddDNLJsonConverters(network)
            | JsonLibraryType.Newtonsoft ->
                newtonSoftJsonOpts.AddDNLJsonConverters(network)
            | _ -> invalidArg (nameof(jsonLibrary)) "Unknown json library type"

    member val JsonOpts = jsonOpts
    member val NewtonSoftJsonOpts = newtonSoftJsonOpts

    member this.NextId
        with private get () =
            Interlocked.Increment(ref _nextId) |> ignore
            _nextId

    member private this.Connect(ct) =
        task {
            let! socket, endpoint =
                if address.Scheme = "tcp" then
                    task {
                        let domain = address.DnsSafeHost
                        let! addr = getAddr domain

                        let socket =
                            new Socket(
                                AddressFamily.InterNetwork,
                                SocketType.Stream,
                                ProtocolType.Tcp
                            )

                        let endpoint =
                            IPEndPoint(addr, address.Port) :> EndPoint

                        return socket, endpoint
                    }
                else if address.Scheme = "unix" then
                    task {
                        let mutable path =
                            address.AbsoluteUri.Remove(0, "unix:".Length)

                        if not <| path.StartsWith "/" then
                            path <- $"/{path}"

                        while path.Length >= 2
                              && (path.[0] <> '/' || path.[1] = '/') do
                            path <- path.Remove(0, 1)

                        if path.Length < 2 then
                            raise <| FormatException "Invalid unix url"

                        return
                            (new Socket(
                                AddressFamily.Unix,
                                SocketType.Stream,
                                ProtocolType.IP
                             ),
                             UnixDomainSocketEndPoint(path) :> EndPoint)
                    }
                else
                    raise
                    <| NotSupportedException
                        $"Protocol {address.Scheme} for clightning not supported"

            do! socket.ConnectAsync(endpoint, ct)
            return socket
        }

    member internal this.SendCommandAsync
        (
            methodName: string,
            req: obj,
            returnType: Type,
            [<Optional; DefaultParameterValue(false)>] noReturn: bool,
            [<Optional; DefaultParameterValue(false)>] notification: bool,
            [<Optional; DefaultParameterValue(CancellationToken())>] ct: CancellationToken
        ) : Task<'T> =
        backgroundTask {
            match jsonLibrary with
            | JsonLibraryType.Newtonsoft ->
                use! networkStream = getTransportStream.Invoke(ct)

                use textWriter =
                    new StreamWriter(networkStream, utf8, 1024 * 10, true)

                use jsonWriter = new Newtonsoft.Json.JsonTextWriter(textWriter)

                let! _ =
                    let reqO = JObject()

                    if not <| notification then
                        reqO.Add("id", JToken.op_Implicit(this.NextId))

                    reqO.Add("method", JToken.op_Implicit(methodName))
                    reqO.Add("jsonrpc", JToken.op_Implicit("2.0"))

                    if req |> isNull then
                        reqO.Add("params", JArray())
                    else
                        reqO.Add(
                            "params",
                            JToken.FromObject(
                                req,
                                Newtonsoft.Json.JsonSerializer.Create(
                                    this.NewtonSoftJsonOpts
                                )
                            )
                        )

                    reqO.WriteToAsync(jsonWriter)

                do! jsonWriter.FlushAsync(ct)
                do! textWriter.FlushAsync()
                do! networkStream.FlushAsync(ct)

                if notification then
                    // we don't have to read response in case of notification.
                    return Activator.CreateInstance returnType |> unbox
                else
                    // read response.
                    use textReader =
                        new StreamReader(
                            networkStream,
                            utf8,
                            false,
                            1024 * 10,
                            true
                        )

                    use jsonReader =
                        new Newtonsoft.Json.JsonTextReader(textReader)

                    let resultAsync = JObject.LoadAsync(jsonReader, ct)

                    try
                        let! result = resultAsync

                        match result.Property "error" with
                        | err when err |> isNull |> not ->
                            return
                                raise
                                <| CLightningRPCException
                                    {
                                        Code =
                                            err.Value.["code"].Value<int>()
                                            |> CLightningClientErrorCode.FromInt
                                        Msg =
                                            err.Value.["message"]
                                                .Value<string>()
                                    }
                        | _ ->
                            if noReturn then
                                return
                                    Activator.CreateInstance returnType |> unbox
                            else
                                let jsonSer =
                                    Newtonsoft.Json.JsonSerializer.Create(
                                        this.NewtonSoftJsonOpts
                                    )

                                return
                                    result
                                        .Property("result")
                                        .ToObject<'T>(jsonSer)
                    with
                    | _ when ct.IsCancellationRequested ->
                        ct.ThrowIfCancellationRequested()
                        return failwith "unreachable"

            | JsonLibraryType.SystemTextJson ->
                use! networkStream = getTransportStream.Invoke(ct)
                use jsonWriter = new Utf8JsonWriter(networkStream)

                // -- write --
                let reqObject = JsonObject()

                if not <| notification then
                    reqObject.set_Item("id", JsonNode.op_Implicit(this.NextId))

                reqObject.set_Item("method", JsonNode.op_Implicit(methodName))
                reqObject.set_Item("jsonrpc", JsonNode.op_Implicit("2.0"))

                if req |> isNull then
                    reqObject.set_Item("params", JsonArray())
                else
                    reqObject.set_Item(
                        "params",
                        JsonSerializer.SerializeToNode(req, this.JsonOpts)
                    )

                reqObject.WriteTo(jsonWriter)
                do! jsonWriter.FlushAsync(ct)
                do! networkStream.FlushAsync(ct)
                // -- --

                if notification then
                    // we don't have to read response in case of notification.
                    return Activator.CreateInstance returnType |> unbox
                else
                    let buf = Array.zeroCreate(65535)
                    let length = networkStream.Read(buf.AsSpan())

                    if length = 0 then
                        return Activator.CreateInstance()
                    else
                        let result =
                            let bufSpan = ReadOnlySpan.op_Implicit buf

                            JsonSerializer.Deserialize<JsonElement>(
                                bufSpan.Slice(0, length),
                                this.JsonOpts
                            )

                        try
                            match result.TryGetProperty("error") with
                            | true, err ->
                                let code =
                                    err.GetProperty("code").GetInt32()
                                    |> CLightningClientErrorCode.FromInt

                                let msg = err.GetProperty("message").GetString()

                                return
                                    raise
                                    <| CLightningRPCException
                                        {
                                            Code = code
                                            Msg = msg
                                        }
                            | false, _ ->
                                if noReturn then
                                    return
                                        Activator.CreateInstance returnType
                                        |> unbox
                                else
                                    let jObj = result.GetProperty("result")
                                    return jObj.Deserialize(this.JsonOpts)
                        with
                        | _ when ct.IsCancellationRequested ->
                            ct.ThrowIfCancellationRequested()
                            return failwith "unreachable"
            | x -> return failwith $"Unknown json library type: {x}"
        }

    member this.SendCommandAsync<'T>
        (
            methodName: string,
            req: obj,
            [<Optional; DefaultParameterValue(false)>] noReturn: bool,
            [<Optional; DefaultParameterValue(CancellationToken())>] ct: CancellationToken
        ) : Task<'T> =
        this.SendCommandAsync(methodName, req, typeof<'T>, noReturn, false, ct)

    member this.SendNotification<'T>
        (
            methodName: string,
            req: obj,
            [<Optional; DefaultParameterValue(CancellationToken())>] ct: CancellationToken
        ) : Task =
        this.SendCommandAsync(methodName, req, typeof<obj>, true, true, ct)
        :> Task

    member internal this.SendCommandAsync<'T>(req: Request, ct) =
        this.SendCommandAsync<'T>(req.MethodName, req.Data, false, ct)
