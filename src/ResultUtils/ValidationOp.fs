namespace ResultUtils

[<AutoOpen>]
module ValidationOp =
  let inline (<!>) f (x: Result<'a, 'b list>) = Result.map f x
  let inline (<!^>) f x =
    x
    |> Result.mapError List.singleton
    |> Result.map f
  let inline (<*>) f x = Validation.apply f x
  let inline (<*^>) f x = Validation.apply f (Validation.ofResult x)