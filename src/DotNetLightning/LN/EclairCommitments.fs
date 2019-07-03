namespace DotNetLightning.LN

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Crypto
open DotNetLightning.Transactions
open DotNetLightning.Serialize.Msgs

type LocalChanges = {
    Proposed: IUpdateMsg list
    Signed: IUpdateMsg list
    ACKed: IUpdateMsg list
}
    with
        member this.All =
            this.Proposed @ this.Signed @ this.ACKed

type RemoteChanges = { 
    Proposed: IUpdateMsg list
    Signed: IUpdateMsg list
    ACKed: IUpdateMsg list
}

type Changes =
    | Local of LocalChanges
    | Remote of RemoteChanges

type PublishableTxs = {
    CommitTx: CommitTx
    HTLCTxsAndSigs: ILightningTx
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

type Commitments = {
    LocalParams: LocalParams
    RemoteParams: RemoteParams
    ChannelFlags: byte
    LocalCommit: LocalCommit
    RemoteCommit: RemoteCommit
    LocalChanges: LocalChanges
    RemoteChanges: RemoteChanges
    LocalNextHTLCId: uint64
    RemoteNextHTLCId: uint64
    OriginChannels: Map<uint64, HTLCSource>
    RemoteNextCommitInfo: Choice<WaitingForRevocation, PubKey>
    CommitInput: InputInfo
    RemotePerCommitmentSecrets: ShaChain
    ChannelId: ChannelId
}