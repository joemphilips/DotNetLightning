namespace DotNetLightning.Channel

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Utils.Aether
open DotNetLightning.Utils.Aether.Operators
open DotNetLightning.Crypto
open DotNetLightning.Transactions
open DotNetLightning.Serialize.Msgs

type LocalChanges = {
    Proposed: IUpdateMsg list
    Signed: IUpdateMsg list
    ACKed: IUpdateMsg list
}
    with
        static member Zero = { Proposed = []; Signed = []; ACKed = [] }

        // -- lenses
        static member Proposed_: Lens<_, _> =
            (fun lc -> lc.Proposed),
            (fun ps lc -> { lc with Proposed = ps })
        static member Signed_: Lens<_, _> =
            (fun lc -> lc.Signed),
            (fun v lc -> { lc with Signed = v })

        static member ACKed_: Lens<_, _> =
            (fun lc -> lc.ACKed),
            (fun v lc -> { lc with ACKed = v })

        member this.All =
            this.Proposed @ this.Signed @ this.ACKed

type RemoteChanges = { 
    Proposed: IUpdateMsg list
    Signed: IUpdateMsg list
    ACKed: IUpdateMsg list
}
    with
        static member Zero = { Proposed = []; Signed = []; ACKed = [] }
        static member Proposed_: Lens<_, _> =
            (fun lc -> lc.Proposed),
            (fun ps lc -> { lc with Proposed = ps })
        static member Signed_: Lens<_, _> =
            (fun lc -> lc.Signed),
            (fun v lc -> { lc with Signed = v })

        static member ACKed_: Lens<_, _> =
            (fun lc -> lc.ACKed),
            (fun v lc -> { lc with ACKed = v })


type Changes =
    | Local of LocalChanges
    | Remote of RemoteChanges

type PublishableTxs = {
    CommitTx: FinalizedTx
    HTLCTxs: FinalizedTx list
}

type LocalCommit = {
    Index: uint64
    Spec: CommitmentSpec
    PublishableTxs: PublishableTxs
    /// These are not redeemable on-chain until we get a corresponding preimage.
    PendingHTLCSuccessTxs: HTLCSuccessTx list
}
type RemoteCommit = {
    Index: uint64
    Spec: CommitmentSpec
    TxId: TxId
    RemotePerCommitmentPoint: PubKey
}

type WaitingForRevocation = {
    NextRemoteCommit: RemoteCommit
    Sent: CommitmentSigned
    SentAfterLocalCommitmentIndex: uint64
    ReSignASAP: bool
}
    with
        static member NextRemoteCommit_: Lens<_,_> =
            (fun w -> w.NextRemoteCommit),
            (fun v w -> { w with NextRemoteCommit = v})

        static member ReSignASAP_: Lens<_,_> =
            (fun w -> w.ReSignASAP),
            (fun v w -> { w with ReSignASAP = v })

type RemoteNextCommitInfo =
    | Waiting of WaitingForRevocation
    | Revoked of PubKey
    with
        static member Waiting_: Prism<RemoteNextCommitInfo, WaitingForRevocation> =
            (fun remoteNextCommitInfo ->
                match remoteNextCommitInfo with
                | Waiting waitingForRevocation -> Some waitingForRevocation
                | Revoked _ -> None),
            (fun waitingForRevocation remoteNextCommitInfo ->
                match remoteNextCommitInfo with
                | Waiting _ -> Waiting waitingForRevocation
                | Revoked _ -> remoteNextCommitInfo)

        static member Revoked_: Prism<RemoteNextCommitInfo, PubKey> =
            (fun remoteNextCommitInfo ->
                match remoteNextCommitInfo with
                | Waiting _ -> None
                | Revoked pubKey -> Some pubKey),
            (fun pubKey remoteNextCommitInfo ->
                match remoteNextCommitInfo with
                | Waiting _ -> remoteNextCommitInfo
                | Revoked pubKey -> Revoked pubKey)

type Commitments = {
    LocalParams: LocalParams
    RemoteParams: RemoteParams
    ChannelFlags: uint8
    FundingSCoin: ScriptCoin
    LocalCommit: LocalCommit
    RemoteCommit: RemoteCommit
    LocalChanges: LocalChanges
    RemoteChanges: RemoteChanges
    LocalNextHTLCId: HTLCId
    RemoteNextHTLCId: HTLCId
    OriginChannels: Map<HTLCId, HTLCSource>
    RemoteNextCommitInfo: RemoteNextCommitInfo
    RemotePerCommitmentSecrets: ShaChain
    ChannelId: ChannelId
}
    with
        static member LocalChanges_: Lens<_, _> =
            (fun c -> c.LocalChanges),
            (fun v c -> { c with LocalChanges = v })
        static member RemoteChanges_: Lens<_, _> =
            (fun c -> c.RemoteChanges),
            (fun v c -> { c with RemoteChanges = v })
        static member RemoteNextCommitInfo_: Lens<_, _> =
            (fun c -> c.RemoteNextCommitInfo),
            (fun v c -> { c with RemoteNextCommitInfo = v })


        member this.AddLocalProposal(proposal: IUpdateMsg) =
            let lens = Commitments.LocalChanges_ >-> LocalChanges.Proposed_
            Optic.map lens (fun proposalList -> proposal :: proposalList) this

        member this.AddRemoteProposal(proposal: IUpdateMsg) =
            let lens = Commitments.RemoteChanges_ >-> RemoteChanges.Proposed_
            Optic.map lens (fun proposalList -> proposal :: proposalList) this

        member this.IncrLocalHTLCId = { this with LocalNextHTLCId = this.LocalNextHTLCId + 1UL }
        member this.IncrRemoteHTLCId = { this with RemoteNextHTLCId = this.RemoteNextHTLCId + 1UL }

        member this.LocalHasChanges() =
            (not this.RemoteChanges.ACKed.IsEmpty) || (not this.LocalChanges.Proposed.IsEmpty)

        member this.RemoteHasChanges() =
            (not this.LocalChanges.ACKed.IsEmpty) || (not this.RemoteChanges.Proposed.IsEmpty)

        member internal this.LocalHasUnsignedOutgoingHTLCs() =
            this.LocalChanges.Proposed |> List.exists(fun p -> match p with | :? UpdateAddHTLC -> true | _ -> false)

        member internal this.RemoteHasUnsignedOutgoingHTLCs() =
            this.RemoteChanges.Proposed |> List.exists(fun p -> match p with | :? UpdateAddHTLC -> true | _ -> false)

        member internal this.HasNoPendingHTLCs() =
            this.LocalCommit.Spec.HTLCs.IsEmpty && this.RemoteCommit.Spec.HTLCs.IsEmpty && (this.RemoteNextCommitInfo |> function Waiting _ -> false | Revoked _ -> true)

        member internal this.GetHTLCCrossSigned(directionRelativeToLocal: Direction, htlcId: HTLCId): UpdateAddHTLC option =
            let remoteSigned =
                this.LocalCommit.Spec.HTLCs
                |> Map.tryPick (fun k v -> if v.Direction = directionRelativeToLocal && v.Add.HTLCId = htlcId then Some v else None)

            let localSigned =
                let lens =
                    Commitments.RemoteNextCommitInfo_
                    >-> RemoteNextCommitInfo.Waiting_
                    >?> WaitingForRevocation.NextRemoteCommit_
                match Optic.get lens this with
                | Some v -> v
                | None -> this.RemoteCommit
                |> fun v -> v.Spec.HTLCs |> Map.tryPick(fun k v -> if v.Direction = directionRelativeToLocal.Opposite && v.Add.HTLCId = htlcId then Some v else None)
            match remoteSigned, localSigned with
            | Some _, Some htlcIn -> htlcIn.Add |> Some
            | _ -> None
