namespace DotNetLightning.Utils
open NBitcoin

type TxState =
    | PSBT of PSBT
    | TransactionBuilder of TransactionBuilder
    | Transaction of Transaction

type TxBuildingComputation<'a> =
    TxBuildingComputation of (PSBT -> 'a * PSBT)

module TxBuildingComputation =
    let runT txBuilder state =
        let (TxBuildingComputation innerFn) = txBuilder
        innerFn state

    let returnT x =
        let innerFn state =
            (x, state)
        TxBuildingComputation innerFn

    let bindT f xT =
        let innerFn state =
            let x, state2 = runT xT state
            runT (f x) state2
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
    open TxBuildingComputation

    let addSignature (pk) (signature: TransactionSignature) =
        toUnitComputation (fun state ->
            state.Inputs.[0].PartialSigs.Add(pk, signature)
            state
        )

    ()