namespace ResultUtils


[<RequireQualifiedAccess>]
module Seq =
  let rec private traverseResultM' (state : Result<_,_>) (f : _ -> Result<_,_>) (xs: seq<_>) =
    match xs |> Seq.isEmpty with
    | true -> state
    | false -> 
      let x = xs |> Seq.head
      let r = result {
        let! y = f x
        let! ys = state
        return seq { yield! ys; yield y }
      }  
      match r with
      | Ok _ -> traverseResultM' r f (xs |> Seq.skip 1)
      | Error _ -> r

  let traverseResultM f xs =
    traverseResultM' (Ok Seq.empty) f xs
  
  let sequenceResultM xs =
    traverseResultM id xs

  let rec private traverseResultA' state f xs =
    match (xs |> Seq.isEmpty) with
    | true -> state
    | false ->
      let x = xs |> Seq.head
      let xs = xs |> Seq.skip 1
      let fR = 
        f x |> Result.mapError List.singleton
      match state, fR with
      | Ok ys, Ok y -> 
        traverseResultA' (Ok (seq {yield! ys; yield y})) f xs
      | Error errs, Error e -> 
        traverseResultA' (Error (errs @ e)) f xs
      | Ok _, Error e | Error e , Ok _  -> 
        traverseResultA' (Error e) f xs

  let traverseResultA f xs =
    traverseResultA' (Ok Seq.empty) f xs

  let sequenceResultA xs =
    traverseResultA id xs

  let rec traverseValidationA' state f xs =
    match xs |> Seq.isEmpty with
    | true -> state
    | false -> 
      let x = xs |> Seq.head
      let xs = xs |> Seq.skip 1
      let fR = f x
      match state, fR with
      | Ok ys, Ok y -> 
        traverseValidationA' (Ok (seq { yield! ys; yield y })) f xs
      | Error errs1, Error errs2 -> 
        traverseValidationA' (Error (errs2 @ errs1 )) f xs
      | Ok _, Error errs | Error errs, Ok _  -> 
        traverseValidationA' (Error errs) f xs

  let traverseValidationA f xs =
    traverseValidationA' (Ok Seq.empty) f xs

  let sequenceValidationA xs =
    traverseValidationA id xs

