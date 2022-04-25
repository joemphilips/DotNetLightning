namespace DotNetLightning.Channel

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Crypto
open DotNetLightning.Transactions
open DotNetLightning.Serialization.Msgs

open ResultUtils
open ResultUtils.Portability
open DotNetLightning.Transactions

type LocalChanges =
    {
        Signed: list<IUpdateMsg>
        ACKed: list<IUpdateMsg>
    }

    static member Zero =
        {
            Signed = []
            ACKed = []
        }

type RemoteChanges =
    {
        Signed: list<IUpdateMsg>
        ACKed: list<IUpdateMsg>
    }

    static member Zero =
        {
            Signed = []
            ACKed = []
        }

type PublishableTxs =
    {
        CommitTx: FinalizedTx
        HTLCTxs: list<FinalizedTx>
    }

type LocalCommit =
    {
        Index: CommitmentNumber
        Spec: CommitmentSpec
        PublishableTxs: PublishableTxs
        /// These are not redeemable on-chain until we get a corresponding preimage.
        PendingHTLCSuccessTxs: list<HTLCSuccessTx>
    }

type RemoteCommit =
    {
        Index: CommitmentNumber
        Spec: CommitmentSpec
        TxId: TxId
        RemotePerCommitmentPoint: PerCommitmentPoint
    }

type RemoteNextCommitInfo =
    | Waiting of RemoteCommit
    | Revoked of PerCommitmentPoint

    member this.PerCommitmentPoint() : PerCommitmentPoint =
        match this with
        | Waiting remoteCommit -> remoteCommit.RemotePerCommitmentPoint
        | Revoked perCommitmentPoint -> perCommitmentPoint

type Amounts =
    {
        ToLocal: Money
        ToRemote: Money
    }

type Commitments =
    {
        ProposedLocalChanges: list<IUpdateMsg>
        ProposedRemoteChanges: list<IUpdateMsg>
        LocalNextHTLCId: HTLCId
        RemoteNextHTLCId: HTLCId
        OriginChannels: Map<HTLCId, HTLCSource>
    }

    member this.AddLocalProposal(proposal: IUpdateMsg) =
        { this with
            ProposedLocalChanges = proposal :: this.ProposedLocalChanges
        }

    member this.AddRemoteProposal(proposal: IUpdateMsg) =
        { this with
            ProposedRemoteChanges = proposal :: this.ProposedRemoteChanges
        }

    member this.IncrLocalHTLCId() =
        { this with
            LocalNextHTLCId = this.LocalNextHTLCId + 1UL
        }

    member this.IncrRemoteHTLCId() =
        { this with
            RemoteNextHTLCId = this.RemoteNextHTLCId + 1UL
        }

    member internal this.LocalHasUnsignedOutgoingHTLCs() =
        this.ProposedLocalChanges
        |> List.exists(fun p ->
            match p with
            | :? UpdateAddHTLCMsg -> true
            | _ -> false
        )

    member internal this.RemoteHasUnsignedOutgoingHTLCs() =
        this.ProposedRemoteChanges
        |> List.exists(fun p ->
            match p with
            | :? UpdateAddHTLCMsg -> true
            | _ -> false
        )


    static member RemoteCommitAmount
        (isLocalFunder: bool)
        (remoteParams: RemoteParams)
        (remoteCommit: RemoteCommit)
        : Amounts =
        let commitFee =
            Transactions.commitTxFee
                remoteParams.DustLimitSatoshis
                remoteCommit.Spec

        let (toLocalAmount, toRemoteAmount) =
            if isLocalFunder then
                (remoteCommit.Spec.ToLocal.Satoshi |> Money.Satoshis),
                (remoteCommit.Spec.ToRemote.Satoshi |> Money.Satoshis)
                - commitFee
            else
                (remoteCommit.Spec.ToLocal.Satoshi |> Money.Satoshis)
                - commitFee,
                (remoteCommit.Spec.ToRemote.Satoshi |> Money.Satoshis)

        {
            Amounts.ToLocal = toLocalAmount
            ToRemote = toRemoteAmount
        }

    static member LocalCommitAmount
        (isLocalFunder: bool)
        (localParams: LocalParams)
        (localCommit: LocalCommit)
        : Amounts =
        let commitFee =
            Transactions.commitTxFee
                localParams.DustLimitSatoshis
                localCommit.Spec

        let (toLocalAmount, toRemoteAmount) =
            if isLocalFunder then
                (localCommit.Spec.ToLocal.Satoshi |> Money.Satoshis) - commitFee,
                (localCommit.Spec.ToRemote.Satoshi |> Money.Satoshis)
            else
                (localCommit.Spec.ToLocal.Satoshi |> Money.Satoshis),
                (localCommit.Spec.ToRemote.Satoshi |> Money.Satoshis)
                - commitFee

        {
            Amounts.ToLocal = toLocalAmount
            ToRemote = toRemoteAmount
        }
