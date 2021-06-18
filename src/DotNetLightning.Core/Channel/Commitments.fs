namespace DotNetLightning.Channel

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Utils.Aether
open DotNetLightning.Utils.Aether.Operators
open DotNetLightning.Crypto
open DotNetLightning.Transactions
open DotNetLightning.Serialization.Msgs

open ResultUtils
open ResultUtils.Portability
open DotNetLightning.Transactions

type LocalChanges = {
    Signed: IUpdateMsg list
    ACKed: IUpdateMsg list
}
    with
        static member Zero = { Signed = []; ACKed = [] }

        // -- lenses
        static member Signed_: Lens<_, _> =
            (fun lc -> lc.Signed),
            (fun v lc -> { lc with Signed = v })

        static member ACKed_: Lens<_, _> =
            (fun lc -> lc.ACKed),
            (fun v lc -> { lc with ACKed = v })

type RemoteChanges = { 
    Signed: IUpdateMsg list
    ACKed: IUpdateMsg list
}
    with
        static member Zero = { Signed = []; ACKed = [] }

        static member Signed_: Lens<_, _> =
            (fun lc -> lc.Signed),
            (fun v lc -> { lc with Signed = v })

        static member ACKed_: Lens<_, _> =
            (fun lc -> lc.ACKed),
            (fun v lc -> { lc with ACKed = v })


type PublishableTxs = {
    CommitTx: FinalizedTx
    HTLCTxs: FinalizedTx list
}

type LocalCommit = {
    Index: CommitmentNumber
    Spec: CommitmentSpec
    PublishableTxs: PublishableTxs
    /// These are not redeemable on-chain until we get a corresponding preimage.
    PendingHTLCSuccessTxs: HTLCSuccessTx list
}
type RemoteCommit = {
    Index: CommitmentNumber
    Spec: CommitmentSpec
    TxId: TxId
    RemotePerCommitmentPoint: PerCommitmentPoint
}

type RemoteNextCommitInfo =
    | Waiting of RemoteCommit
    | Revoked of PerCommitmentPoint
    with
        static member Waiting_: Prism<RemoteNextCommitInfo, RemoteCommit> =
            (fun remoteNextCommitInfo ->
                match remoteNextCommitInfo with
                | Waiting remoteCommit -> Some remoteCommit
                | Revoked _ -> None),
            (fun waitingForRevocation remoteNextCommitInfo ->
                match remoteNextCommitInfo with
                | Waiting _ -> Waiting waitingForRevocation
                | Revoked _ -> remoteNextCommitInfo)

        static member Revoked_: Prism<RemoteNextCommitInfo, PerCommitmentPoint> =
            (fun remoteNextCommitInfo ->
                match remoteNextCommitInfo with
                | Waiting _ -> None
                | Revoked commitmentPubKey -> Some commitmentPubKey),
            (fun commitmentPubKey remoteNextCommitInfo ->
                match remoteNextCommitInfo with
                | Waiting _ -> remoteNextCommitInfo
                | Revoked _ -> Revoked commitmentPubKey)

        member this.PerCommitmentPoint(): PerCommitmentPoint =
            match this with
            | Waiting remoteCommit -> remoteCommit.RemotePerCommitmentPoint
            | Revoked perCommitmentPoint -> perCommitmentPoint

type Amounts = 
    {
        ToLocal: Money
        ToRemote: Money
    }

type Commitments = {
    ProposedLocalChanges: list<IUpdateMsg>
    ProposedRemoteChanges: list<IUpdateMsg>
    LocalNextHTLCId: HTLCId
    RemoteNextHTLCId: HTLCId
    OriginChannels: Map<HTLCId, HTLCSource>
}
    with
        member this.AddLocalProposal(proposal: IUpdateMsg) =
            {
                this with
                    ProposedLocalChanges = proposal :: this.ProposedLocalChanges
            }

        member this.AddRemoteProposal(proposal: IUpdateMsg) =
            {
                this with
                    ProposedRemoteChanges = proposal :: this.ProposedRemoteChanges
            }

        member this.IncrLocalHTLCId() = { this with LocalNextHTLCId = this.LocalNextHTLCId + 1UL }
        member this.IncrRemoteHTLCId() = { this with RemoteNextHTLCId = this.RemoteNextHTLCId + 1UL }

        member internal this.LocalHasUnsignedOutgoingHTLCs() =
            this.ProposedLocalChanges |> List.exists(fun p -> match p with | :? UpdateAddHTLCMsg -> true | _ -> false)

        member internal this.RemoteHasUnsignedOutgoingHTLCs() =
            this.ProposedRemoteChanges |> List.exists(fun p -> match p with | :? UpdateAddHTLCMsg -> true | _ -> false)


        static member RemoteCommitAmount (isLocalFunder: bool)
                                         (remoteParams: RemoteParams)
                                         (remoteCommit: RemoteCommit)
                                             : Amounts =
            let commitFee = Transactions.commitTxFee
                                remoteParams.DustLimitSatoshis
                                remoteCommit.Spec
            
            let (toLocalAmount, toRemoteAmount) =
                if isLocalFunder then
                    (remoteCommit.Spec.ToLocal.Satoshi
                     |> Money.Satoshis),
                    (remoteCommit.Spec.ToRemote.Satoshi
                     |> Money.Satoshis) - commitFee
                else
                    (remoteCommit.Spec.ToLocal.Satoshi
                     |> Money.Satoshis) - commitFee,
                    (remoteCommit.Spec.ToRemote.Satoshi
                     |> Money.Satoshis)

            {Amounts.ToLocal = toLocalAmount; ToRemote = toRemoteAmount}

        static member LocalCommitAmount (isLocalFunder: bool)
                                        (localParams: LocalParams)
                                        (localCommit: LocalCommit)
                                            : Amounts =
            let commitFee = Transactions.commitTxFee
                                localParams.DustLimitSatoshis
                                localCommit.Spec
            
            let (toLocalAmount, toRemoteAmount) =
                if isLocalFunder then
                    (localCommit.Spec.ToLocal.Satoshi
                     |> Money.Satoshis) - commitFee,
                    (localCommit.Spec.ToRemote.Satoshi
                     |> Money.Satoshis)
                else
                    (localCommit.Spec.ToLocal.Satoshi
                     |> Money.Satoshis),
                    (localCommit.Spec.ToRemote.Satoshi
                     |> Money.Satoshis) - commitFee

            {Amounts.ToLocal = toLocalAmount; ToRemote = toRemoteAmount}
