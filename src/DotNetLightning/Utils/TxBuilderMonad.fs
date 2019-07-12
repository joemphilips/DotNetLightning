namespace DotNetLightning.Utils
open NBitcoin
open NBitcoin.Crypto

type TxBuildingComputation<'a> =
    TxBuildingComputation of (TransactionBuilder -> 'a * TransactionBuilder)

[<RequireQualifiedAccess>]
module TxBuildingComputation =
    let run (TxBuildingComputation innerFn) state =
        innerFn state

    let returnT x =
        let innerFn state =
            (x, state)
        TxBuildingComputation innerFn

    let getBuilder = TxBuildingComputation(fun state -> (state, state))
    let setBuilder s = TxBuildingComputation(fun _ -> ((), s))

    let bindT f xT =
        let innerFn state =
            let x, state2 = run xT state
            run (f x) state2
        TxBuildingComputation innerFn

    let mapT f =
        bindT (f >> returnT)

    let toComputation f =
        let innerFn state =
            let (result, newState) = f state
            result, newState
        TxBuildingComputation innerFn

    let toUnitComputation f =
        let f2 state =
            (), f state
        toComputation f2


module TxBuilderClient =

    let addSignature (pk) (signature: TransactionSignature) (op) =
        TxBuildingComputation.toUnitComputation (fun state ->
            state.AddKnownSignature(pk, signature, op) |> ignore
            state
        )

    let sign (key: Key) =
        TxBuildingComputation.toComputation(fun state ->
            state.AddKeys(key) |> ignore
            let tx = state.BuildTransaction(true)
            let signature = tx.Inputs
                            |> Seq.collect(fun i -> i.WitScript.Pushes)
                            |> Seq.choose(fun bs -> if TransactionSignature.IsValid(bs) then Some (TransactionSignature(bs)) else None)
            signature, state.ContinueToBuild tx
        )
    ()