---
title: other namespaces
category: overview
index: 2
categoryindex: 2
---

# features which is not in "DotNetLightning.*" namespace

Some sibling assemblies come together when you install `DotNetLightning` or `DotNetLightning.Core`. These are mostly for internal usages but some might be useful for you:

## AEZ

Which contains managed code for aez cipher scheme.

It may be useful if you want to secure your data before saving it to disk.
See official page for more details: https://www.cs.ucdavis.edu/~rogaway/aez/index.html

## Macaroon

Which contains macaroon authentication token.

The API is mostly the same as [libmacaroon](https://github.com/rescrv/libmacaroons) (see libmacaroon's readme for the
usage).

(Currently it is only supported in BouncyCastle build (which means not in `DotNetLightning.Core`),
see https://github.com/joemphilips/DotNetLightning/issues/153 for more info.)

