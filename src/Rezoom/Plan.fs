namespace Rezoom


type DataResponse =
    | RetrievalSuccess of result : obj
    | RetrievalException of exn: exn
    | RetrievalDeferred
    
type Batch<'a> =
    | BatchNone
    | BatchLeaf of 'a
    | BatchPair of 'a Batch * 'a Batch
    | BatchMany of 'a Batch array
    | BatchAbort
    member this.Map(f : 'a -> 'b) =
        match this with
        | BatchNone -> BatchNone
        | BatchLeaf x -> BatchLeaf(f x)
        | BatchPair (l, r) -> BatchPair(l.Map(f), r.Map(f))
        | BatchMany arr -> arr |> Array.map(fun b -> b.Map(f)) |> BatchMany
        | BatchAbort -> BatchAbort
        
type Requests = Errand Batch
type Responses = DataResponse Batch
and Plan<'result> =
    | Result of 'result
    | Step of Requests * (Responses -> Plan<'result>)
    
type BatchHint<'a> = internal | BatchHint of 'a

[<AutoOpen>]
module BatchHints =
    let batch x = BatchHint x
