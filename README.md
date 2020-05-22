## Overview

This repository contains one solution which contains four main projects.

1. `DotNetLightning.Core` to perform LN specific logic and low level primitives.
2. `DotNetLightning.Infrastructure` to support data persistency, altcoins, channel/peer management etc.
3. `DotNetLightning.Server` to run as a server.
4. `DotNetLightning.Client` to interact with the server.

## How to configure

Configuration will follow an usual semantics for ASP.NET Core web app.
That is, it will read in following order.

* Read `appsettings.json`

## Supported platforms

Thanks to the BouncyCastle project (nuget dependency), DotNetLightning is cross-platform and doesn't depend on any native libraries.

## Developer notes

![Alt text](images/Package_Dependency_Graph.png?raw=true "Package dependency graph")

Purple background indicates that the package is included in this repository.
Important point here is that we should have the least external dependencies as possible to reduce number of possible
attack vectors. (especially for DotNetLightning.Core)

![Alt text](images/Architecture01.png?raw=true "Infrastructure Architecture in one image")

## Footnotes

[Slide used for creating images](https://docs.google.com/presentation/d/1GKByCIPef3wwM_RMGQFcdsme2dVyQWHL_kp6t67eFRw/edit?usp=sharing)
