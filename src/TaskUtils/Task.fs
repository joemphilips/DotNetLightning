namespace TaskUtils

open System.Threading.Tasks
open FSharp.Control.Tasks


[<RequireQualifiedAccess>]
module Task =
  let singleton v = v |> Task.FromResult

  let bind (f: 'a -> Task<'b>) (x: Task<'a>) = task {
    let! r = x
    return! f r
  }

  let apply f x =
    bind (fun f' -> bind(f' >> singleton) x) f

  let map f x =
    x |> bind (f >> singleton)

  let map2 f x y =
    (apply (apply (singleton f) x) y)

  let map3 f x y z =
    apply(map2 f x y) z