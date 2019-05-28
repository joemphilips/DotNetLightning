namespace DotNetLightning.LN
open NBitcoin
open DotNetLightning.Utils.Primitives
open Microsoft.Extensions.Logging
open NBitcoin
open System

/// An error enum representing a failure to persist a channel monitor update.
type ChannelMonitorUpdateErr =
    /// Used to to indicate a temporary fialure (eg connection to a watchtower or remote bakcup of
    /// our state failed, but is expected to succeed at some point on the future).
    /// Such a failure will "freeze" a channel, preventing us from revoking old states or
    /// submitting new commitment tx too the remote party.
    /// `ChannelManager.TestRestoreChannelMonitor` can  be used to retry the update(s) and restore
    /// the channel to an operational state.
    /// 
    /// Note that continuig to operate when no copy of the updated ChannelMonitor could be persisted
    /// is unsafe - if you failed to store the update on your own local disk you should
    /// instead return PermanentFailure to force closure of the channel ASAP.
    /// 
    /// Even when a channel has been "frozen", updates to the ChannelMonitor can continue to occur
    /// (eg if an inbound HTLC which we forwarded was claimed upstream resulting in us attempting
    /// to claim it on this channel) and those updates must be applied wherever they can be. At
    /// least one such updated ChannelMonitor must be persisted otherwise `PermanentFailure` should
    /// be returned to get things on-chain ASAP using only the in-memory copy. Obviously updates to
    /// the channel which would invalidate previous `ChannelMonitors` are not made when a channel
    /// has been "frozen".
    /// 
    /// Note that even if updates made after TemporaryFailure succeed you must still call
    /// test_restore_channel_monitor to ensure you have the latest monitor and re-enable  normal
    /// channel operation.
    /// 
    /// For deployments where a copy of `ChannelMonitors` and other local state are backed up in a
    /// remote location (with local copies persisted immediately), it is anticipated that all
    /// updates will return TemporaryFailure until the remote copies could be updated.
    | TemporaryFailure
    /// Used to indicate no further channel monitor updates will be allowed (e.g. we've moved on to a
    /// different watchtower and cannot update with all watchtowers that were previously informed
    /// of this channel). This will force-close the channel in question.
    /// 
    /// Should also be used to indicate a failure to update the localc copy of the channel monitor.
    | PermanentFailure

type HTLCUpdate = {
    PaymentHash: PaymentHash
    PaymentPreimage: PaymentPreimage option
    Source: HTLCSource
}

type LocalSignedTx = {
    TxId: TxId
    Tx: Transaction
    RevocationKey: PubKey
    AHTLCKey: PubKey
    BHTLCKey: PubKey
    DelayedPaymentKey: PubKey
    FeeRatePerKw: uint64
    HTLCOutputs: (HTLCOutputInCommitment * (TransactionSignature * TransactionSignature) option * HTLCSource option)  list
}

type LocalStorage = {
    RevocationBaaseKey: Key
    HTLCBaseKey: Key
    DelayedPaymentBaseKey: Key
    PaymentBaseKey: Key
    ShutDownPubKey: PubKey
    PreviousLatestPerCommitmentPoint: PubKey option
    LatestPerCommitmentPoint: PubKey option
    FundingInfo: (OutPoint * Script) option
    CurrentRemoteCommitmentTxId: TxId option
    PreviousRemoteCommitmentTxId: TxId option
}

type WatchTowerStorage = {
    RevocationBaseKey: PubKey
    HTLCBaseKey: PubKey
}

type Storage =
    | Local of LocalStorage
    | WatchTower of WatchTowerStorage

/// A ChannelMonitor handles chain events (block connected and disconnected) and generates
/// on-chain transactions to ensure no loss of funds occurs.
/// 
/// You MUST ensure that no ChannelMonitors for a given channel anywhere contains out-of-date
/// information and are actively monitoring the chain.
type ChannelMonitor = {
    CommitmentTxNumberObscureFactor: uint64
    KeyStorage: Storage
    TheirHTLCBaseKey: PubKey option
    TheirDelayedPaymentBaseKey: PubKey option
    /// First is the index of the first of the two reocation points.
    TheirCurrentRevocationPoints: (uint64 * PubKey * PubKey option) option

    OurToSelfDelay: DateTime
    TheirToSelfDelay: uint16 option
    RemoteClaimableOutpoints: Map<TxId, (HTLCOutputInCommitment * HTLCSource option) list>
    /// We cannot identify HTLC-Sucess or HTLC-Timeout Transactoins by themselves on the chain.
    /// Nor can we figure out their commitment numbers without the commitment tx they are spending.
    /// Thus, in order to claim them via revocation key, we track all the remote commitment txs
    /// which we find on-chain, mapping them to the commitment number which can be used to derive
    /// the revocation key nad claim the transacctions.
    RemoteCommitmentTxnOnChain: Map<TxId, (uint64 * Script list)>
    /// We store two local commitment txs to avoid any race conditions where we may update
    /// Some monitors (potentially on watchtowers) but then fail to update others, resulting
    /// in the various monitors for one channel being out of sync,and us braodcasting a local tx
    /// for which we have deleted claim information on some watchtowers.
    RemoteHashCommitmentNumber: Map<PaymentHash, uint64>

    PreviousLocalSignedCommitmentTx: LocalSignedTx option
    CurrentLocalSignedCommitmentTx: LocalSignedTx option

    /// Used just for ChannelManager to make sure it has the latest channel data during
    /// Deserialization
    CurrentRemoteCommitmentNumber: uint64

    PaymentPreimages: Map<PaymentHash, PaymentPreimage>
    DestinationScript: Script

    /// We simply modify LastBlockHash in Channel's `BlockConnected` so that serialization is
    /// consistent but hopefully the users' copy handles `BlockConnected` in a consistent way.
    /// (we do *not*, however, update them in InsertCombine to ensure any local user copies keep
    /// their LastBlockHash from its state and not based on updated copies that didn't run through
    /// the full BlockConnected).
    LastBlockHash: uint256
    Logger: ILogger
}

module ChannelMonitor =
    let create (revocationBaseKey: Key,
                htlcBaseKey: Key,
                delayedPaymentBaseKey: Key,
                paymentBaseKey: Key,
                shutDownPubKey: PubKey,
                ourToSelfDelay: DateTime,
                destinationScript: Script,
                logger: ILogger) =
        {
            CommitmentTxNumberObscureFactor = 0UL
            KeyStorage = Local {
                RevocationBaaseKey = revocationBaseKey
                HTLCBaseKey = htlcBaseKey
                DelayedPaymentBaseKey = delayedPaymentBaseKey
                PaymentBaseKey = paymentBaseKey
                ShutDownPubKey = shutDownPubKey
                PreviousLatestPerCommitmentPoint = None
                LatestPerCommitmentPoint = None
                FundingInfo = None
                CurrentRemoteCommitmentTxId = None
                PreviousRemoteCommitmentTxId = None
            }
            TheirHTLCBaseKey = None
            TheirDelayedPaymentBaseKey = None
            TheirCurrentRevocationPoints = None
            OurToSelfDelay = ourToSelfDelay
            TheirToSelfDelay = None
            RemoteClaimableOutpoints = Map.empty
            RemoteCommitmentTxnOnChain = Map.empty
            RemoteHashCommitmentNumber = Map.empty
            PreviousLocalSignedCommitmentTx = None
            CurrentLocalSignedCommitmentTx = None
            CurrentRemoteCommitmentNumber = 1UL <<< 48
            PaymentPreimages = Map.empty
            DestinationScript = destinationScript
            LastBlockHash = uint256()
            Logger = logger
        }

type IManyChannelMonitor =
    abstract member AddUpdateMonitor: (OutPoint * ChannelMonitor) -> Result<unit, ChannelMonitorUpdateErr>
    abstract member FetchPendingHTLCUpdated: unit -> HTLCUpdate list