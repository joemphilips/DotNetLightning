(**
---
title: calling c-lightning rpc
category: examples
index: 1
categoryindex: 1
---
*)
(*** condition: prepare ***)
#I "../src/DotNetLightning.ClnRpc/bin/Debug/net6.0"
#I "../src/DotNetLightning.Core/bin/Debug/netstandard2.0"
#r "DotNetLightning.Core.dll"
#r "DotNetLightning.ClnRpc.dll"
#r "nuget: NBitcoin"
#r "/Users/joemiyamoto/.nuget/packages/nbitcoin/6.0.19/lib/netstandard2.0/NBitcoin.dll"

open System
open NBitcoin
open DotNetLightning.ClnRpc

(**
    This guide shows you how to use `DotNetLightning.ClnRpc` to call c-lightning rpc.
*)

let uri = Uri("tcp://127.0.0.1:9835")
// or Uri("unix:///path/to/your/lightning-rpc")

let client = ClnClient(uri, Network.RegTest)

let getPeerTask = client.GetinfoAsync()

let info = getPeerTask.GetAwaiter().GetResult()
(*** include-value: info ***)
