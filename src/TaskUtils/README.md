# TaskUtils

This is basically an extension to `ResultUtils` for an asynchronous computing.

It is based on following libraries

* [Ply](https://github.com/crowded/ply)
  * For Task and ValueTask computation expression
  * When F# native task support comes out, we probably remove this part.
* [FsToolKit.ErrorHandling](https://github.com/demystifyfp/FsToolkit.ErrorHandling)
  * For TaskResult and related stuffs
