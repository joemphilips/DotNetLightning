namespace ResultUtils

[<RequireQualifiedAccess>]
module List =
    let rec private traverseResultM' (state: Result<_, _>) (f: _ -> Result<_, _>) xs =
        match xs with
        | [] ->
            state
        | x :: xs ->
            let r = result {
                let! y = f x
                let! ys = state
                return ys @ [y]
            }
            match r with
            | Ok _ -> traverseResultM' r f xs
            | Error _ -> r
            
    let traverseResultM f xs =
        traverseResultM' (Ok []) f xs
        
    let sequenceResultM xs =
        traverseResultM id xs

    let rec private traverseResultA' state f xs =
        match xs with
        | [] -> state
        | x :: xs ->
            let fR =
                f x |> Result.mapError List.singleton
            match state, fR with
            | Ok ys, Ok y ->
                traverseResultA' (Ok (ys @ [y])) f xs
            | Error errs, Error e ->
                traverseResultA' (Error(errs @ e)) f xs
            | Ok _, Error e | Error e , Ok _ ->
                traverseResultA' (Error e) f xs

    let traverseResultA f xs =
        traverseResultA' (Ok []) f xs
    let sequenceResultA xs =
        traverseResultA id xs