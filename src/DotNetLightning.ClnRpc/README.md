## `DotNetLightning.ClnRpc`

This is a library for calling [Core Lightning](https://github.com/ElementsProject/lightning) Json RPC from your C#/F# projects
in a typesafe way.
It will automatically maps the Core Lightning native types into DotNetLightning types with `System.Text.Json`

Example:

```fsharp

open DotNetLightning.ClnRpc

let client = ClnClient(Uri("unix:///path/to/lightning-rpc"), Network.RegTest)
let info = client.GetinfoAsync()
```

### Todo

* Add `Plugin` class/module for easily building plugin?

### Other choices

You can also use [cln-grpc](https://github.com/ElementsProject/lightning/tree/master/cln-grpc)
And call c-lightning through grpc.

The downside of this is that it is not sufficient for building your own plugins.
Since plugins require you to communicate through json-rpc through stdin/out.

In that case `DotNetLightning.ClnRpc` might be useful.

See following resources for building your own plugins.

* [Building c-lightning plugin with .NET](https://dev.to/joemphilips/building-c-lightning-plugin-with-net-3162)
* [NLoop](https://github.com/bitbankinc/NLoop)

