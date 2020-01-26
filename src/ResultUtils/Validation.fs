namespace ResultUtils

[<RequireQualifiedAccess>]
module Validation =
  let ofResult x =
    Result.mapError List.singleton x

  let apply f x =
    match f, x with
    | Ok f, Ok x -> Ok (f x)
    | Error errs, Ok _ | Ok _, Error errs -> Error errs
    | Error errors1, Error errors2 -> Error (errors1 @ errors2)

  let retn x = ofResult (Ok x)

  let map2 f x y =
    apply (apply (retn f ) x ) y

  let map3 f x y z =
    apply (map2 f x y) z
    
  let inline lift2 (f: 'a -> 'b -> 'c) (x: Result<'a, _>) (y: Result<'b, _>): Result<'c, _> =
    apply (apply (Ok f) x) y

