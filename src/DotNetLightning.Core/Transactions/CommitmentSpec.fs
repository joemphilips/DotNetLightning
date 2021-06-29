namespace DotNetLightning.Transactions

open DotNetLightning.Serialization.Msgs
open DotNetLightning.Utils.Primitives
open DotNetLightning.Utils
open DotNetLightning.Utils.Aether

open ResultUtils
open ResultUtils.Portability

type CommitmentSpec = {
    OutgoingHTLCs: Map<HTLCId, UpdateAddHTLCMsg>
    IncomingHTLCs: Map<HTLCId, UpdateAddHTLCMsg>
    FeeRatePerKw: FeeRatePerKw
    ToLocal: LNMoney
    ToRemote: LNMoney
}
    with
        static member Create (toLocal) (toRemote) (feeRate) =
            {
                OutgoingHTLCs = Map.empty
                IncomingHTLCs = Map.empty
                FeeRatePerKw = feeRate
                ToLocal = toLocal
                ToRemote = toRemote
            }
        static member internal  FeeRatePerKW_: Lens<_, _> =
            (fun cs -> cs.FeeRatePerKw),
            (fun v cs -> { cs with FeeRatePerKw = v })
        static member internal ToLocal_ : Lens<_,_> =
            (fun cs -> cs.ToLocal),
            (fun v cs -> { cs with ToLocal = v })
        static member internal ToRemote_: Lens<_, _> =
            (fun cs -> cs.ToRemote),
            (fun v cs -> { cs with ToRemote = v })

        member internal this.AddOutgoingHTLC(update: UpdateAddHTLCMsg) =
            { this with ToLocal = (this.ToLocal - update.Amount); OutgoingHTLCs = this.OutgoingHTLCs.Add(update.HTLCId, update)}

        member internal this.AddIncomingHTLC(update: UpdateAddHTLCMsg) =
            { this with ToRemote = this.ToRemote - update.Amount; IncomingHTLCs = this.IncomingHTLCs.Add(update.HTLCId, update)}

        member internal this.FulfillOutgoingHTLC(htlcId: HTLCId) =
            match this.IncomingHTLCs |> Map.tryFind htlcId with
            | Some htlc ->
                { this with ToLocal = this.ToLocal + htlc.Amount; IncomingHTLCs = this.IncomingHTLCs.Remove htlcId }
                |> Ok
            | None ->
                UnknownHTLC htlcId |> Error

        member internal this.FulfillIncomingHTLC(htlcId: HTLCId) =
            match this.OutgoingHTLCs |> Map.tryFind htlcId with
            | Some htlc ->
                { this with ToRemote = this.ToRemote + htlc.Amount; OutgoingHTLCs = this.OutgoingHTLCs.Remove htlcId }
                |> Ok
            | None ->
                UnknownHTLC htlcId |> Error

        member internal this.FailOutgoingHTLC(htlcId: HTLCId) =
            match this.OutgoingHTLCs |> Map.tryFind htlcId with
            | Some htlc ->
                { this with ToRemote = this.ToRemote + htlc.Amount; OutgoingHTLCs = this.OutgoingHTLCs.Remove htlcId }
                |> Ok
            | None ->
                UnknownHTLC htlcId |> Error

        member internal this.FailIncomingHTLC(htlcId: HTLCId) =
            match this.IncomingHTLCs |> Map.tryFind htlcId with
            | Some htlc ->
                { this with ToLocal = this.ToLocal + htlc.Amount; IncomingHTLCs = this.IncomingHTLCs.Remove htlcId }
                |> Ok
            | None ->
                UnknownHTLC htlcId |> Error

        member internal this.Reduce(localChanges: #IUpdateMsg list, remoteChanges: #IUpdateMsg list) =
            let spec1 =
                localChanges
                |> List.fold(fun (acc: CommitmentSpec) updateMsg ->
                        match box updateMsg with
                        | :? UpdateAddHTLCMsg as u -> acc.AddOutgoingHTLC u
                        | _ -> acc
                    )
                    this

            let spec2 =
                remoteChanges
                |> List.fold(fun (acc: CommitmentSpec) updateMsg ->
                        match box updateMsg with
                        | :? UpdateAddHTLCMsg as u -> acc.AddIncomingHTLC u
                        | _ -> acc
                    )
                    spec1

            let spec3RR =
                localChanges
                |> List.fold(fun (acc: Result<CommitmentSpec, TransactionError>) updateMsg ->
                            match box updateMsg with
                            | :? UpdateFulfillHTLCMsg as u ->
                                acc >>= fun a -> a.FulfillOutgoingHTLC u.HTLCId
                            | :? UpdateFailHTLCMsg as u ->
                                acc >>= fun a -> a.FailOutgoingHTLC u.HTLCId
                            | :? UpdateFailMalformedHTLCMsg as u ->
                                acc >>= fun a -> a.FailOutgoingHTLC u.HTLCId
                            | _ -> acc
                        )
                    (Ok spec2)

            let spec4RR =
                remoteChanges
                |> List.fold(fun (acc: Result<CommitmentSpec, TransactionError>) updateMsg ->
                            match box updateMsg with
                            | :? UpdateFulfillHTLCMsg as u ->
                                acc >>= fun a -> a.FulfillIncomingHTLC u.HTLCId
                            | :? UpdateFailHTLCMsg as u ->
                                acc >>= fun a -> a.FailIncomingHTLC u.HTLCId
                            | :? UpdateFailMalformedHTLCMsg as u ->
                                acc >>= fun a -> a.FailIncomingHTLC u.HTLCId
                            | _ -> acc
                    )
                    spec3RR

            let spec5 = 
                (localChanges @ remoteChanges)
                |> List.fold(fun (acc) updateMsg ->
                        match box updateMsg with
                        | :? UpdateFeeMsg as u ->
                            (fun a -> { a with CommitmentSpec.FeeRatePerKw = u.FeeRatePerKw }) <!> acc
                        | _ -> acc
                    )
                    spec4RR
            spec5
