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
  
  
  let inline ( *> ) m1 m2 = Validation.lift2 (fun _ y -> y) m1 m2
  let inline ( <* ) m1 m2 = Validation.lift2 (fun x _ -> x) m1 m2
  
  let inline ( *^> ) m1 m2 = Validation.lift2 (fun _ y -> y) m1 (Validation.ofResult m2)
  let inline ( <*^ ) m1 m2 = Validation.lift2 (fun x _ -> x) m1 (Validation.ofResult m2)
