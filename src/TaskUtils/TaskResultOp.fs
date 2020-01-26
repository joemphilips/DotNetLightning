namespace TaskUtils


[<AutoOpen>]
module TaskResultOp =

  let inline (<!>) f x = TaskResult.map f x
  let inline (<*>) f x = TaskResult.apply f x
  let inline (>>=) x f = TaskResult.bind f x