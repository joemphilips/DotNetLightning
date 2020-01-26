namespace ResultUtils

[<AutoOpen>]
module ResultOp =
  let inline (<!>) f x = Result.map f x
  let inline (<*>) f x = Result.apply f x
  let inline (>>=) x f = Result.bind f x
