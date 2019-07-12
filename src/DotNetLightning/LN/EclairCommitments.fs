namespace DotNetLightning.LN

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Utils.Aether
open DotNetLightning.Utils.Aether.Operators
open DotNetLightning.Crypto
open DotNetLightning.Chain
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
    Index: uint32
    Spec: CommitmentSpec
    PublishableTxs: PublishableTxs
}
type RemoteCommit = {
    Index: uint32
    Spec: CommitmentSpec
    TxId: TxId
    RemotePerCommitmentPoint: PubKey
}

type WaitingForRevocation = {
    NextRemoteCommit: RemoteCommit
    Sent: CommitmentSigned
    SentAfterLocalCommitmentIndex: uint32
    ReAsignASAP: bool
}


type LocalParams = {
    NodeId: NodeId
    ChannelKeys: ChannelKeys
    DustLimitSatoshis: Money
    MaxHTLCValueInFlightMSat: LNMoney
    ChannelReserveSatoshis: Money
    HTLCMinimumMSat: LNMoney
    ToSelfDelay: BlockHeightOffset
    MaxAcceptedHTLCs: uint16
    IsFunder: bool
    DefaultFinalScriptPubKey: Script
    GlobalFeatuers: GlobalFeatures
    LocalFeatures: LocalFeatures
}

type RemoteParams = {
    NodeId: NodeId
    DustLimitSatoshis: Money
    MaxHTLCValueInFlightMSat: LNMoney
    ChannelReserveSatoshis: Money
    HTLCMinimumMSat: LNMoney
    ToSelfDelay: BlockHeightOffset
    MaxAcceptedHTLCs: uint16
    PaymentBasePoint: PubKey
    FundingPubKey: PubKey
    RevocationBasePoint: PubKey
    DelayedPaymentBasePoint: PubKey
    HTLCBasePoint: PubKey
    GlobalFeatures: GlobalFeatures
    LocalFeatures: LocalFeatures
}
    with
        static member FromAcceptChannel nodeId (remoteInit: Init) (msg: AcceptChannel) =
            {
                NodeId = nodeId
                DustLimitSatoshis = msg.DustLimitSatoshis
                MaxHTLCValueInFlightMSat = msg.MaxHTLCValueInFlightMsat
                ChannelReserveSatoshis = msg.ChannelReserveSatoshis
                HTLCMinimumMSat = msg.HTLCMinimumMSat
                ToSelfDelay = msg.ToSelfDelay
                MaxAcceptedHTLCs = msg.MaxAcceptedHTLCs
                PaymentBasePoint = msg.PaymentBasepoint
                FundingPubKey = msg.FundingPubKey
                RevocationBasePoint = msg.RevocationBasepoint
                DelayedPaymentBasePoint = msg.DelayedPaymentBasepoint
                HTLCBasePoint = msg.HTLCBasepoint
                GlobalFeatures = remoteInit.GlobalFeatures
                LocalFeatures = remoteInit.LocalFeatures
            }

        static member FromOpenChannel (nodeId) (remoteInit: Init) (msg: OpenChannel) =
            {
                NodeId = nodeId
                DustLimitSatoshis = msg.DustLimitSatoshis
                MaxHTLCValueInFlightMSat = msg.MaxHTLCValueInFlightMsat
                ChannelReserveSatoshis = msg.ChannelReserveSatoshis
                HTLCMinimumMSat = msg.HTLCMinimumMsat
                ToSelfDelay = msg.ToSelfDelay
                MaxAcceptedHTLCs = msg.MaxAcceptedHTLCs
                PaymentBasePoint = msg.PaymentBasepoint
                FundingPubKey = msg.FundingPubKey
                RevocationBasePoint = msg.RevocationBasepoint
                DelayedPaymentBasePoint = msg.DelayedPaymentBasepoint
                HTLCBasePoint = msg.HTLCBasepoint
                GlobalFeatures = remoteInit.GlobalFeatures
                LocalFeatures = remoteInit.LocalFeatures
            }
type Commitments = {
    LocalParams: LocalParams
    RemoteParams: RemoteParams
    ChannelFlags: uint8
    FundingTxOutIndex: TxOutIndex
    LocalCommit: LocalCommit
    RemoteCommit: RemoteCommit
    LocalChanges: LocalChanges
    RemoteChanges: RemoteChanges
    LocalNextHTLCId: HTLCId
    RemoteNextHTLCId: HTLCId
    OriginChannels: Map<HTLCId, HTLCSource>
    RemoteNextCommitInfo: Choice<WaitingForRevocation, PubKey>
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

        member this.AddLocalProposal(proposal: IUpdateMsg) =
            let lens = Commitments.LocalChanges_ >-> LocalChanges.Proposed_
            Optic.map lens (fun proposalList -> proposal :: proposalList) this

        member this.AddRemoteProposal(proposal: IUpdateMsg) =
            let lens = Commitments.RemoteChanges_ >-> RemoteChanges.Proposed_
            Optic.map lens (fun proposalList -> proposal :: proposalList) this

        member this.IncrLocalHTLCId = { this with LocalNextHTLCId = this.LocalNextHTLCId + 1UL }
        member this.IncrRemoteHTLCId = { this with RemoteNextHTLCId = this.RemoteNextHTLCId + 1UL }