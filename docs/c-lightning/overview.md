---
title: Overview
category: c-lightning
index: 1
categoryindex: 2
---

Package `DotNetLightning.ClnRpc` contains a utility to work with [c-lightning](https://github.com/ElementsProject/lightning)
under `DotNetLightning.ClnRpc` namespace.

### calling c-lightning json rpc.

The most important type is `ClnClient` which you can use it for calling its Json RPC in a typesafe way.

See [the example](../examples/cln_rpc_example.html) for how to use it.

### building your own plugin

c-lightning has a feature called plugin.
See the [c-lightning document](https://github.com/ElementsProject/lightning/blob/master/doc/PLUGINS.md)
to know what is the plugin (and how it works in general).

`DotNetLightning.ClnRpc.Plugin` namespace contains a utility for building your own plugin.
The most important type is `PluginServerBase`.
By overriding this type, you can easily create your own plugin.

Please read [the API reference](../reference/dotnetlightning-clnrpc-plugin-pluginserverbase.html) for how to use the type.

see its API reference and an [example C# project](https://github.com/joemphilips/DotNetLightning/tree/master/examples/HelloWorldPlugin) for how to use.
