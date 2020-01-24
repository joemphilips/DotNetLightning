module Rezoom.Plan

open Rezoom.ExceptionTypes
open System.Collections.Generic

let inline internal advance plan =
    match plan with
    | Step(BatchNone, next) -> next BatchNone
    | _ -> plan
    
let internal abort() = raise(PlanAbortException("Plan aborted"))

let internal abortPlan(state: 'a Plan) (reason: exn) : 'b =
    match state with
    | Step(_, resume) ->
        try
            ignore <| resume BatchAbort
            dispatchRaise reason
        with
        | PlanAbortException _ -> dispatchRaise reason
        | exn -> raise(aggregate[|reason; exn|])
    | _ -> dispatchRaise reason

let inline delayed (plan : unit -> 'a Plan) =
    Step(BatchNone, fun _ -> plan())
    
/// Monadic Return for `Plan`
let inline ret (result: 'a) : Plan<'a> = Result result

/// Monoidal identity for `Plan`
let zero = ret()

let ofErrand(request: Errand<'a>) : Plan<'a> =
    let rec onResponse =
        function
        | BatchLeaf RetrievalDeferred -> step
        | BatchLeaf (RetrievalSuccess suc) -> Result(Unchecked.unbox suc : 'a)
        | BatchLeaf(RetrievalException exn) -> dispatchRaise exn
        | BatchAbort -> abort()
        | BatchNone
        | BatchPair _
        | BatchMany _ -> logicFault("Incorrect response shape for data request")
        
    and step : Plan<'a> =
        Step(BatchLeaf(request :> Errand), onResponse)
    step
    
let rec map(f: 'a -> 'b) (plan : 'a Plan) : 'b Plan =
    match plan with
    | Result r -> Result (f r)
    | Step(pending, resume) -> Step(pending, resume >> map f)

let rec bind(plan: 'a Plan) (cont : 'a -> 'b Plan) : 'b Plan =
    match plan with
    | Result r -> cont r
    | Step(pending, resume) ->
        Step(pending, fun responses -> bind (resume responses) cont)
        
let rec combine (plan: 'a Plan) (cont : unit -> 'b Plan) : 'b Plan =
    match plan with
    | Result _ -> cont()
    | Step(pending, resume) ->
        Step(pending, fun responses -> combine(resume responses) cont)
        
/// Applicative functor `apply`
let inline private next2 planF planA proceed =
    let mutable exnF : exn = null
    let mutable exnA : exn = null
    let mutable resF : Plan<'a -> 'b> = Unchecked.defaultof<_>
    let mutable resA : Plan<'a> = Unchecked.defaultof<_>
    try
        resF <- advance(planF())
    with
    | exn ->
        exnF <- exn
    try
        resA <- advance(planA())
    with
    | exn -> exnA <- exn
    if isNull exnF && isNull exnA then
        proceed resF resA
    else if not (isNull exnF) && not (isNull exnA) then
        raise(aggregate[|exnF; exnA|])
    else if isNull exnF then
        abortPlan resF exnA
    else
        abortPlan resA exnF
        
         
let rec private applyState (planF : Plan<'a -> 'b>) (planA : Plan<'a>) : Plan<'b> =
    match planF, planA with
    | Result f, Result a -> Result(f a)
    | Result f, step -> map ((<|) f) step
    | step, Result a -> map ((|>) a) step
    | Step(pendingF, resumeF), Step(pendingA, resumeA) ->
        let pending = BatchPair(pendingF, pendingA)
        let onResponses =
            function
            | BatchPair(rspF, rspA) ->
                next2 (fun () -> resumeF rspF) (fun () -> resumeA rspA) applyState
            | BatchAbort -> abort()
            | _ -> logicFault "Incorrect response shape for applied pair"
        Step(pending, onResponses)
        
        
let apply (planF : Plan<'a -> 'b>) (planA : Plan<'a>) : Plan<'b> =
    next2 (fun () -> planF) (fun () -> planA) applyState
    
let tuple2 (planA : 'a Plan) (planB : 'b Plan) : ('a * 'b) Plan =
    apply
        (map (fun a b -> a, b) planA)
        planB
let tuple3 (planA : 'a Plan) (planB : 'b Plan) (planC: 'c Plan) :('a * 'b * 'c) Plan =
    apply
        (apply
            (map (fun a b c -> a, b, c) planA)
            planB)
        planC
let tuple4
    (planA : 'a Plan)
    (planB : 'b Plan)
    (planC : 'c Plan)
    (planD : 'd Plan)
    : ('a * 'b * 'c * 'd) Plan =
    apply
        (apply
            (apply
                (map (fun a b c d -> a, b, c, d) planA)
                planB)
            planC)
        planD
        
/// Create a plan that runs all the given plans concurrently and combines their results into a list.
let rec concurrentList (plans : 'a Plan list) : 'a list Plan =
    match plans with
    | [] -> ret []
    | head :: tail ->
        apply
            (head |> map (fun head tail -> head :: tail))
            (concurrentList tail)
let rec tryCatch (wrapped : unit -> 'a Plan) (catcher : exn -> 'a Plan) : 'a Plan =
    try
        match advance <| wrapped () with
        | Result _ as r -> r
        | Step (pending, resume) ->
            let onResponses (responses: Responses) =
                tryCatch(fun () -> resume responses) catcher
            Step(pending, onResponses)
    with
    | PlanAbortException _ -> reraise()
    | ex -> catcher ex
    
let rec tryFinally (wrapped: unit -> 'a Plan) (onExit : unit -> unit) : 'a Plan =
    let mutable cleanExit = false
    let plan =
        try
            match advance <| wrapped () with
            | Result _ as r ->
                cleanExit <- true
                r
            | Step(pending, resume) ->
                let onResponses(responses: Responses) =
                    tryFinally(fun () -> resume responses) onExit
                Step(pending, onResponses)
        with
        | ex ->
            try
                onExit()
            with
            | inner ->
                raise (aggregate[|ex; inner|])
            reraise()
    if cleanExit then
        onExit()
    plan
    
let rec private forIterator (enumerator : 'a IEnumerator) (iteration: 'a -> unit Plan) =
    if not <| enumerator.MoveNext() then zero else
    bind (iteration enumerator.Current) (fun () -> forIterator enumerator iteration)
    
let forM (sequence : 'a seq) (iteration: 'a -> unit Plan) : unit Plan =
    let enumerator = sequence.GetEnumerator()
    tryFinally
        (fun _ -> forIterator enumerator iteration)
        (fun _ -> enumerator.Dispose())
        
let private abortSteps (steps: _ seq) (reason : exn) : 'b =
    let exns = ResizeArray<_>()
    exns.Add(reason)
    for _, resume in steps do
        try
            ignore <| resume BatchAbort
        with
        | PlanAbortException _ -> ()
        | exn -> exns.Add(exn)
    if exns.Count > 1 then raise (aggregate exns)
    else dispatchRaise reason
    
let rec private forAs (plans: (unit Plan) seq) : unit Plan =
    let steps =
        let steps = ResizeArray<_>()
        let exns = ResizeArray<_>()
        for plan in plans do
            try
                match advance plan with
                | Step (pending, resume) -> steps.Add(pending, resume)
                | Result _ -> ()
            with
            | exn -> exns.Add(exn)
        if exns.Count > 0 then abortSteps steps (aggregate exns)
        else steps
    if steps.Count <= 0 then Result() else
        let pending =
            let arr = Array.zeroCreate steps.Count
            for i = 0 to steps.Count - 1 do
                arr.[i] <- fst steps.[i]
            BatchMany arr
        let onResponses =
            function
            |  BatchMany responses ->
                responses
                |> Seq.mapi (fun i rsp -> delayed (fun _ -> snd steps.[i] rsp))
                |> forAs
            | BatchAbort -> abort ()
            | BatchNone
            | BatchPair _
            | BatchLeaf _ -> logicFault "Incorrect response shape for applicative batch"
        Step(pending, onResponses)

/// Applicative iteration
/// Create a plan that strictly iterates a sequence, creating a `Plan` for each element
/// using the given `iteration` function, then runs those plans concurrently.
let forA (sequence: 'a seq) (iteration: 'a -> unit Plan) : unit Plan =
    forAs(sequence |> Seq.map(fun e -> iteration e))