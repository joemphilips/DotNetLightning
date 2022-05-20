---
title: Other namespaces
category: Overview
index: 2
categoryindex: 1
---

# Features which are not in the "DotNetLightning.*" namespace

Some sibling assemblies come together when you install `DotNetLightning` or `DotNetLightning.Core`. These are mostly for internal usages but some might be useful for you:

## AEZ

Which contains managed code for aez cipher scheme.

It may be useful if you want to secure your data before saving it to disk.
See official page for more details: https://www.cs.ucdavis.edu/~rogaway/aez/index.html

## Macaroon

Which contains macaroon authentication token.

The API is mostly the same as [libmacaroon](https://github.com/rescrv/libmacaroons) (see libmacaroon's readme for the
usage).

(Currently it is only supported in the BouncyCastle build, that is, not in the `DotNetLightning.Core` nuget package;
see https://github.com/joemphilips/DotNetLightning/issues/153 for more info.)

