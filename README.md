## DotNetLightning: The utility to work with the Bitcoin Lightning Network (LN) in .NET


![logo](./docs/img/logo.png)

The main API is in `DotNetLightning.Core` project/assembly.

## Installation

The package is compiled and published with three variants:

* [`DotNetLightning`](https://www.nuget.org/packages/DotNetLightning/)
  * This does not use native bindings for cryptographic operations.
  * This is the one you want to use if you run your code everywhere, but possibly slower than below.
* [`DotNetLightning.Core`](https://www.nuget.org/packages/DotNetLightning.Core/)
  * This uses a pre-compiled `libsodium` for cryptographic operations.
  * It only supports `windows`, `mac` and `linux` environments.
  * This is what you want if you need performance and the environments above are the only ones you are planning to support.
* [`DotNetLightning.ClnRpc`](https://www.nuget.org/packages/DotNetLightning.ClnRpc/)
  * The wrapper for `DotNetLightning` which includes a [Core Lightning](https://github.com/ElementsProject/lightning) Json Rpc Client and utilities for making your own plugins.

Run `dotnet add package` with the one you want.

Currently it is in beta, so you probably want to install a latest version by specifying it with `--version`.
The version is prefixed with git commit hash and date. Please take a look at the nuget page.

## Features

See our [API document](https://joemphilips.github.io/DotNetLightning) for full features.

