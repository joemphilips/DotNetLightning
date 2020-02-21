[<AutoOpen>]
module internal DotNetLightning.Server.Common

open System
open System.IO
open System.Text.Json
open Microsoft.FSharp.Quotations
open NBitcoin

let nameof (q:Expr<_>) = 
  match q with 
  | Patterns.Let(_, _, DerivedPatterns.Lambdas(_, Patterns.Call(_, mi, _))) -> mi.Name
  | Patterns.PropertyGet(_, mi, _) -> mi.Name
  | DerivedPatterns.Lambdas(_, Patterns.Call(_, mi, _)) -> mi.Name
  | _ -> failwith "Unexpected format"

let any<'R> : 'R = failwith "!"

let inline checkNull<'T when 'T : null> (a: 'T) =
    if isNull a then nullArg (sprintf "%A" (nameof <@ any<'T> @> )) else ()
    
type JsonElement
    with
    member this.ToObject<'T>() =
        this.GetRawText() |> JsonSerializer.Deserialize<'T>
        
type JsonDocument
    with
    member this.ToObject<'T>() =
        this.RootElement.GetRawText() |> JsonSerializer.Deserialize<'T>

type NetworkType
    with
    member this.DataDirName =
        match this with
        | NetworkType.Mainnet -> "Main"
        | NetworkType.Testnet -> "TestNet"
        | NetworkType.Regtest -> "RegTest"
        | _ -> failwith "Unreachable"
let private getDefaultDataDirectory(appDirectory: string, subDirectory: string, createIfNotExists: bool option) =
    let createIfNotExists = Option.defaultValue true createIfNotExists
    let home =
        if (Environment.OSVersion.Platform = PlatformID.Unix || Environment.OSVersion.Platform = PlatformID.MacOSX) then
            Environment.GetEnvironmentVariable("HOME")
        else
            Environment.ExpandEnvironmentVariables("%HOMEDRIVE%%HOMEPATH%")
    let dirPath = Path.Join(home, "." + appDirectory.ToLower(), subDirectory.ToLower())
    if (Directory.Exists(dirPath) |> not) then
        if (createIfNotExists) then
            Directory.CreateDirectory(dirPath) |> ignore
        else
            raise <| DirectoryNotFoundException(dirPath)
    dirPath
    
let getDataDirectory(nType: NetworkType) =
    getDefaultDataDirectory("DotNetLightning", nType.DataDirName, None)

