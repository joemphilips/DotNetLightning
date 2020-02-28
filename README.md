
## How to configure

Configuration will follow an usual semantics for ASP.NET Core web app.
That is, it will read in following order.

* Read `appsettings.json`

## Supported platforms

* By default, the solution is built with dependencies on Secp256k1.Net and NSec (which have native dependencies that need to be built for every platform, and we right now only provide Linux binaries). This version is published as `DotNetLightning.Core` on Nuget.
* For a more portable version (but possibly less performant) you can compile with BouncyCastle support, passing /p:BouncyCastle=True to your build. This version is published as `DotNetLightning` on Nuget.

## TODO

* Refactor PeerChannelEncryptor to optimize performance (Probably by using byref-like-type and mutable state)
* Update `Channel.deriveOurDustLimitSatoshis`

## Developer notes

![Alt text](images/Package_Dependency_Graph.png?raw=true "Package dependency graph")

Purple background indicates that the package is included in this repository.
Important point here is that we should have at least external dependencies which may become possible
attack vector. (especially for DotNetLightning.Core)

![Alt text](images/Architecture01.png?raw=true "Infrastructure Architecture in one image")

## footnote

[Slide used for creating images](https://docs.google.com/presentation/d/1GKByCIPef3wwM_RMGQFcdsme2dVyQWHL_kp6t67eFRw/edit?usp=sharing)
