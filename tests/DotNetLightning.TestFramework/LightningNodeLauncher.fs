namespace DotNetLightning.TestFramework

open BTCPayServer.Lightning
open BTCPayServer.Lightning.LND
open FSharp.Control.Tasks
open System
open System.Linq
open System.IO
open System.Net
open System.Net.Sockets
open System.Diagnostics
open System.Threading.Tasks
open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open Microsoft.FSharp.Reflection
open NBitcoin
open NBitcoin.RPC

type LauncherSettings = {
  NETWORK: string
  BITCOIND_RPCPORT: int
  BITCOIND_PORT: int
  DATADIR: string
  BALANCER_RESTPORT: int
  CUSTODY_RESTPORT: int
  THIRDPARTY_RESTPORT: int
}

type Clients = {
  Bitcoin: RPCClient
  LndClient: ILightningClient
  Custody: ILightningClient
  ThirdParty: ILightningClient
}

[<AutoOpen>]
module LightningNodeLauncher =
    // Wrapper for the specific process of docker-compose
    type LightningNodeBuilder(name: string, network: Network, composeFilePath: string) =
        let checkConnection portN =
            let l = TcpListener(IPAddress.Loopback, portN)
            l.Start()
            l.Stop()

        let findEmptyPort (number: int) =
            let mutable i = 0
            let mutable result = Array.create number 0
            while (i < number) do
                let mutable port = RandomUtils.GetInt32() % 4000
                port <- port + 10000
                if not (result |> Array.contains port) then
                    try
                        checkConnection port
                        result.[i] <- port
                        i <- i + 1
                    with
                    | :? SocketException -> ()          
            result

        let ports = findEmptyPort 5
        let settings = {
            NETWORK = network.ToString().ToLower()
            BITCOIND_PORT = ports.[0]
            BITCOIND_RPCPORT = ports.[1]
            DATADIR = name
            BALANCER_RESTPORT = ports.[2]
            CUSTODY_RESTPORT = ports.[3]
            THIRDPARTY_RESTPORT = ports.[4]
        }

        let mutable maybeRunningProcess: Process option = None

        let convertSettingsToEnvInStartInfo (settings: LauncherSettings): ProcessStartInfo =
            let mutable startInfo = ProcessStartInfo()
            let allFields = FSharpType.GetRecordFields (settings.GetType())
            for k in allFields do
                startInfo.EnvironmentVariables.["LNLAUNCHER_" + k.Name] <- k.GetValue(settings).ToString()
            startInfo

        let rec checkConnected clients =
            Task.Delay(2000) |> Async.AwaitTask |> Async.RunSynchronously
            try
                Console.WriteLine("checking connection ...")
                let _ = clients.LndClient.GetInfo() |> Async.AwaitTask |> Async.RunSynchronously
                ()
            with
            | :? SocketException -> checkConnected clients
            | :? AggregateException -> checkConnected clients

        let runDockerComposeDown() =
            let startInfo = convertSettingsToEnvInStartInfo settings
            startInfo.EnvironmentVariables.["COMPOSE_PROJECT_NAME"] <- name
            startInfo.FileName <- "docker-compose"
            startInfo.Arguments <- " -f " + composeFilePath + " down"
            let p = Process.Start(startInfo)
            p.WaitForExit()
            ()

        let waitLnSynced (bitcoin: RPCClient) (lnClient: ILightningClient) =
            task {
                let! lnInfo = lnClient.GetInfo()
                let! height = bitcoin.GetBlockCountAsync()
                let mutable shouldWait = (lnInfo.BlockHeight < height)
                while shouldWait do
                    let! lnInfo = lnClient.GetInfo()
                    let! height = bitcoin.GetBlockCountAsync()
                    shouldWait <- (lnInfo.BlockHeight < height)
                    printf "height in bitcoin is %s and height in lnd is %s \n" (height.ToString()) (lnInfo.BlockHeight.ToString())
                    do! Task.Delay(500)
                return ()
            }

        interface IDisposable with
            member this.Dispose() =
                match maybeRunningProcess with
                | None -> ()
                | Some p ->
                    runDockerComposeDown()
                    p.Dispose()
                    maybeRunningProcess <- None
                    printf "disposed Builder %s " name

        member this.startNode() =
            let startInfo = convertSettingsToEnvInStartInfo settings
            startInfo.EnvironmentVariables.["COMPOSE_PROJECT_NAME"] <- name
            startInfo.FileName <- "docker-compose"
            startInfo.Arguments <- " -f " + composeFilePath + " up"
            // startInfo.ArgumentList.Add("-d")
            startInfo.ErrorDialog <- true
            startInfo.RedirectStandardError <- true
            startInfo.RedirectStandardOutput <- true
            let p = Process.Start(startInfo)
            maybeRunningProcess <- Some p
            // printf "%s" (p.StandardError.ReadToEnd())
            let c = this.GetClients()
            c |> checkConnected
            // lnd requires at least one block in the chain. (otherwise it will return 500 when tried to connect)
            c.Bitcoin.Generate(1) |> ignore
            // end we must wait until lnd can scan that block.
            Async.Sleep 1000 |> Async.RunSynchronously 
            ()


        member this.GetClients(): Clients =
            let fac = LightningClientFactory(network)

            let con1 = sprintf "type=lnd-rest;server=https://lnd:lnd@127.0.0.1:%d;allowinsecure=true" settings.BALANCER_RESTPORT
            Console.WriteLine(con1)
            {
                Bitcoin = RPCClient(RPCCredentialString.Parse("0I5rfLbJEXsg:yJt7h7D8JpQy"),
                                        Uri(sprintf "http://localhost:%d" settings.BITCOIND_RPCPORT),
                                        network)
                LndClient = fac.Create(sprintf "type=lnd-rest;server=https://lnd:lnd@127.0.0.1:%d;allowinsecure=true" settings.BALANCER_RESTPORT)
                Custody = fac.Create(sprintf "type=lnd-rest;server=https://lnd:lnd@127.0.0.1:%d;allowinsecure=true" settings.CUSTODY_RESTPORT)
                ThirdParty = fac.Create(sprintf "type=lnd-rest;server=https://lnd:lnd@127.0.0.1:%d;allowinsecure=true" settings.THIRDPARTY_RESTPORT)
            }

        member this.ConnectAsync(from: ILightningClient, dest: ILightningClient) =
            task {
                let! info = dest.GetInfo()
                let! _ = from.ConnectTo(info.NodeInfoList.FirstOrDefault())
                return ()
            }

         member this.Connect(from: ILightningClient, dest: ILightningClient) =
             this.ConnectAsync(from, dest).GetAwaiter().GetResult()

         member this.ConnectAllAsync() =
             let clients = this.GetClients()
             [|
                 this.ConnectAsync(clients.LndClient, clients.ThirdParty)
                 this.ConnectAsync(clients.LndClient, clients.Custody)
                 this.ConnectAsync(clients.Custody, clients.ThirdParty)
             |] |> Task.WhenAll

         member this.ConnectAll() =
             this.ConnectAllAsync().GetAwaiter().GetResult()


        member this.OpenChannelAsync(cashCow: RPCClient, from: ILightningClient, dest: ILightningClient, amount: Money) =
            task {
                let! destInvoice = dest.CreateInvoice(LightMoney.op_Implicit(1000), "EnsureConnectedToDest", TimeSpan.FromSeconds(5000.0))
                let! info = dest.GetInfo()
                let request = OpenChannelRequest()
                request.NodeInfo <- info.NodeInfoList.FirstOrDefault()
                request.ChannelAmount <- amount
                request.FeeRate <- NBitcoin.FeeRate(0.0004m)
                let! payResult = from.Pay(destInvoice.BOLT11)
                let mutable notOpened = payResult.Result = PayResult.CouldNotFindRoute
                while notOpened do
                    let! payResult = from.Pay(destInvoice.BOLT11)
                    notOpened <- payResult.Result = PayResult.CouldNotFindRoute
                    Console.WriteLine("Openning channel ...")
                    let! response = from.OpenChannel(request)
                    if response.Result = OpenChannelResult.CannotAffordFunding then
                        Console.WriteLine("Cannot afford fund")
                        do! this.PrepareBTCFundsAsync()
                        let! addr = from.GetDepositAddress()
                        let! _ = cashCow.SendToAddressAsync(addr, Money.Coins(0.1m))
                        let! _ = cashCow.GenerateAsync(10)
                        do! waitLnSynced cashCow from
                        do! waitLnSynced cashCow dest
                    if response.Result = OpenChannelResult.PeerNotConnected then
                        Console.WriteLine("Peer not conn")
                        do! from.ConnectTo(info.NodeInfoList.FirstOrDefault())
                    if response.Result = OpenChannelResult.NeedMoreConf then
                        Console.WriteLine("Need more conf")
                        let! _ = cashCow.GenerateAsync(6)
                        do! waitLnSynced cashCow from
                        do! waitLnSynced cashCow dest
                    if response.Result = OpenChannelResult.AlreadyExists then 
                        Console.WriteLine("already exists")
                        do! Task.Delay(1000)
                return ()
            }

        member this.OpenChannel(cashCow: RPCClient, from: ILightningClient, dest: ILightningClient, amount: Money) =
            this.OpenChannelAsync(cashCow, from, dest, amount).GetAwaiter().GetResult()

        member this.PrepareBTCFundsAsync() =
            let clients = this.GetClients()
            task {
                let! count = clients.Bitcoin.GetBlockCountAsync()
                let mat = clients.Bitcoin.Network.Consensus.CoinbaseMaturity
                let notReady = count <= mat
                if notReady then
                    let! _ = clients.Bitcoin.GenerateAsync(mat + 1)
                    return ()
                return ()
            }

        member private this.PrepareLNFundsAsyncPrivate(amount: Money, confirmation: int option, onlyThisClient: ILightningClient option) =
            let clients = this.GetClients()
            let conf = defaultArg confirmation 3
            task {
                do! this.PrepareBTCFundsAsync()
                match onlyThisClient with
                | Some c ->
                    let! addr = c.GetDepositAddress()
                    let! _ = clients.Bitcoin.SendToAddressAsync(addr, amount)
                    return! clients.Bitcoin.GenerateAsync(conf)
                | None ->
                    let! addrs =
                        seq [clients.Custody; clients.LndClient; clients.ThirdParty]
                        |> Seq.map(fun c -> c.GetDepositAddress()) |> Task.WhenAll
                    let! _ = addrs |> Seq.map(fun a -> clients.Bitcoin.SendToAddressAsync(a, amount)) |> Task.WhenAll
                    return! clients.Bitcoin.GenerateAsync(conf)
            }

        member this.PrepareLNFundsAsync(amount: Money, [<Optional>] ?confirmation: int, [<Optional>] ?onlyThisClient: ILightningClient) =
            this.PrepareLNFundsAsyncPrivate(amount, confirmation, onlyThisClient)

        member this.PrepareLNFunds(amount: Money, [<Optional>] ?confirmation: int, [<Optional>] ?onlyThisClient: ILightningClient) =
            this.PrepareLNFundsAsyncPrivate(amount, confirmation, onlyThisClient).GetAwaiter().GetResult()


    type LightningNodeLauncher() =

        let getComposeFilePath() =
            let path1 = Path.GetFullPath(Path.Combine(Directory.GetCurrentDirectory(), "../../../../LNTestFramework/docker-compose.yml")) // for testing
            let path2 = Path.GetFullPath(Path.Combine(getAssemblyDirectory(), "../../contentFiles/any/netstandard2.0/docker-compose.yml")) // for production
            if File.Exists(path1) then path1
            else if File.Exists(path2) then path2
            else failwithf "path not found in %s" path2

        member this.createBuilder ([<CallerMemberName>] [<Optional>] ?caller: string, [<Optional>] ?network: Network) =
             let composeFilePath = getComposeFilePath()
             printf "using compose file %s" composeFilePath
             let name = match caller with
                        | None -> failwith "caller member name not spplyed!"
                        | Some i -> Path.GetFullPath(Path.Combine(Directory.GetCurrentDirectory(), i))
             let net = match network with
                       | Some n -> n
                       | None -> Network.RegTest
             if not (Directory.Exists(name)) then
                Directory.CreateDirectory(name) |> ignore
             else 
                Directory.Delete(name, true)
                Directory.CreateDirectory(name) |> ignore
             new LightningNodeBuilder(name, net, composeFilePath)

    let lnLauncher = LightningNodeLauncher()