namespace DotNetLightning.Infrastructure

open System.Threading.Tasks
open System.Runtime.CompilerServices
open DotNetLightning.Utils


[<Extension>]
type RResultExtensions() =
    [<Extension>]
    static member RBadIterAsync(this: RResult<_>, m : RBad -> Task): Task =
        match this with
        | RResult.Bad   tbad  -> (tbad.Flatten()) |> Array.map m |> Task.WhenAll
        | RResult.Good  _     -> Task.FromResult () :> Task