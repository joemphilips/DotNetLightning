namespace DotNetLightning.Transactions

open DotNetLightning.Serialize.Msgs
open DotNetLightning.Utils.Primitives
open DotNetLightning.Utils
open DotNetLightning.Utils.Aether

type internal Direction =
    | In
    | Out
    with
        member this.Opposite =
            match this with
            | In -> Out
            | Out -> In


type DirectedHTLC = internal {
    Direction: Direction
    Add: UpdateAddHTLC
}

type CommitmentSpec = internal {
    HTLCs: Map<HTLCId, DirectedHTLC>
    FeeRatePerKw: FeeRatePerKw
    ToLocal: LNMoney
    ToRemote: LNMoney
}
    with
        static member internal HTLCs_: Lens<_, _> =
            (fun cs -> cs.HTLCs),
            (fun v cs -> { cs with HTLCs = v })
        static member internal  FeeRatePerKW_: Lens<_, _> =
            (fun cs -> cs.FeeRatePerKw),
            (fun v cs -> { cs with FeeRatePerKw = v })
        static member internal ToLocal_ : Lens<_,_> =
            (fun cs -> cs.ToLocal),
            (fun v cs -> { cs with ToLocal = v })
        static member internal ToRemote_: Lens<_, _> =
            (fun cs -> cs.ToRemote),
            (fun v cs -> { cs with ToRemote = v })

        member this.TotalFunds =
            this.ToLocal + this.ToRemote + (this.HTLCs |> Seq.sumBy(fun h -> h.Value.Add.AmountMSat))

        member internal this.AddHTLC(direction: Direction, update: UpdateAddHTLC) =
            let htlc = { DirectedHTLC.Direction = direction; Add = update }
            match direction with
            | Out -> { this with ToLocal = (this.ToLocal - htlc.Add.AmountMSat); HTLCs = this.HTLCs.Add(update.HTLCId, htlc)}
            | In ->  { this with ToRemote = this.ToRemote - htlc.Add.AmountMSat; HTLCs = this.HTLCs.Add(update.HTLCId, htlc)}

        member internal this.FulfillHTLC(direction: Direction, htlcId: HTLCId) =
            match this.HTLCs |> Map.filter(fun k v  -> v.Direction <> direction) |>  Map.tryFind(htlcId), direction with
            | Some htlc, Out ->
                { this with ToLocal = this.ToLocal + htlc.Add.AmountMSat; HTLCs = this.HTLCs.Remove htlcId }
                |> Good
            | Some htlc, In ->
                { this with ToRemote = this.ToRemote + htlc.Add.AmountMSat; HTLCs = this.HTLCs.Remove(htlcId) }
                |> Good
            | None, _ ->
                RResult.rmsg(sprintf "cannot find htlc id %A" htlcId)

        member internal this.FailHTLC(direction: Direction, htlcId: HTLCId) =
            match this.HTLCs |> Map.filter(fun k v -> v.Direction <> direction) |> Map.tryFind (htlcId), direction with
            | Some htlc, Out ->
                { this with ToRemote = this.ToRemote + htlc.Add.AmountMSat; HTLCs = this.HTLCs.Remove(htlcId)}
                |> Good
            | Some htlc, In ->
                { this with ToLocal = this.ToLocal + htlc.Add.AmountMSat; HTLCs = this.HTLCs.Remove(htlcId)}
                |> Good
            | None, _ ->
                RResult.rmsg(sprintf "cannot find htlc id %A" htlcId)

        member internal this.Reduce(localChanges: #IUpdateMsg list, remoteChanges: #IUpdateMsg list) =
            let spec1 =
                localChanges
                |> List.fold(fun (acc: CommitmentSpec) updateMsg ->
                        match box updateMsg with
                        | :? UpdateAddHTLC as u -> acc.AddHTLC(Out, u)
                        | _ -> acc
                    )
                    this

            let spec2 =
                remoteChanges
                |> List.fold(fun (acc: CommitmentSpec) updateMsg ->
                        match box updateMsg with
                        | :? UpdateAddHTLC as u -> acc.AddHTLC(In, u)
                        | _ -> acc
                    )
                    spec1

            let spec3RR =
                localChanges
                |> List.fold(fun (acc: RResult<CommitmentSpec>) updateMsg ->
                            match box updateMsg with
                            | :? UpdateFulfillHTLC as u ->
                                acc >>= fun a -> a.FulfillHTLC(Out, u.HTLCId)
                            | :? UpdateFailHTLC as u ->
                                acc >>= fun a -> a.FailHTLC(Out, u.HTLCId)
                            | :? UpdateFailMalformedHTLC as u ->
                                acc >>= fun a -> a.FailHTLC(Out, u.HTLCId)
                            | _ -> acc
                        )
                    (Good spec2)

            let spec4RR =
                remoteChanges
                |> List.fold(fun (acc: RResult<CommitmentSpec>) updateMsg ->
                            match box updateMsg with
                            | :? UpdateFulfillHTLC as u ->
                                acc >>= fun a -> a.FulfillHTLC(In, u.HTLCId)
                            | :? UpdateFailHTLC as u ->
                                acc >>= fun a -> a.FailHTLC(In, u.HTLCId)
                            | :? UpdateFailMalformedHTLC as u ->
                                acc >>= fun a -> a.FailHTLC(In, u.HTLCId)
                            | _ -> acc
                    )
                    spec3RR

            let spec5 = 
                (localChanges @ remoteChanges)
                |> List.fold(fun (acc) updateMsg ->
                        match box updateMsg with
                        | :? UpdateFee as u ->
                            acc |>> fun a -> { a with CommitmentSpec.FeeRatePerKw = u.FeeRatePerKw }
                        | _ -> acc
                    )
                    spec4RR
            spec5