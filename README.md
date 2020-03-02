## Overview

This repository contains one solution which contains four main project.

1. `DotNetLightning.Core` to perform LN specific logic and low level primitives.
2. `DotNetLightning.Infrastructure` to support data persistancy, altcoins, channel/peer management etc.
3. `DotNetLightning.Server` to run as a server.
4. `DotNetLightning.Client` to query against the server.

## How to configure

Configuration will follow an usual semantics for ASP.NET Core web app.
That is, it will read in following order.

* Read `appsettings.json`

## Supported platforms

The project `DotNetLightning.Core` is published to nuget as two different styles.

* By default, the solution is built with dependencies on Secp256k1.Net and NSec (which have native dependencies that need to be built for every platform, and we right now only provide Linux/MacOS binaries). This version is published as `DotNetLightning.Core` on Nuget.
* For a more portable version (but possibly less performant) you can compile with BouncyCastle support, passing /p:BouncyCastle=True to your build. This version is published as `DotNetLightning` on Nuget.

## Developer notes

![Alt text](images/Package_Dependency_Graph.png?raw=true "Package dependency graph")

Purple background indicates that the package is included in this repository.
Important point here is that we should have the least external dependencies as possible to reduce number of possible
attack vectors. (especially for DotNetLightning.Core)

![Alt text](images/Architecture01.png?raw=true "Infrastructure Architecture in one image")

## footnote

[Slide used for creating images](https://docs.google.com/presentation/d/1GKByCIPef3wwM_RMGQFcdsme2dVyQWHL_kp6t67eFRw/edit?usp=sharing)
