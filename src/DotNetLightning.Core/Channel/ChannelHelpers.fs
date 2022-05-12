namespace DotNetLightning.Channel

open System

open DotNetLightning.Chain
open DotNetLightning.Utils
open DotNetLightning.Channel
open DotNetLightning.Crypto
open DotNetLightning.Transactions

open ResultUtils
open ResultUtils.Portability

open NBitcoin

/// cousin of `ChannelHelpers` module which only includes very primitive function.
module internal ChannelConstantHelpers =
    let deriveOurDustLimitSatoshis(feeEstimator: IFeeEstimator) : Money =
        let (FeeRatePerKw atOpenBackGroundFee) =
            feeEstimator.GetEstSatPer1000Weight(ConfirmationTarget.Background)

        (Money.Satoshis(
            (uint64 atOpenBackGroundFee) * B_OUTPUT_PLUS_SPENDING_INPUT_WEIGHT
            / 1000UL
         ),
         Money.Satoshis(546UL))
        |> Money.Max

    let getOurChannelReserve(channelValue: Money) =
        let q = channelValue / 100L
        Money.Min(channelValue, Money.Max(q, Money.Satoshis(1L)))

module ClosingHelpers =
    let TxVersionNumberOfCommitmentTxs = 2u

    type ValidateCommitmentTxError =
        | InvalidTxVersionForCommitmentTx of uint32
        | TxHasNoInputs
        | TxHasMultipleInputs of int
        | DoesNotSpendChannelFunds of OutPoint
        | InvalidLockTimeAndSequenceForCommitmentTx of LockTime * Sequence

        member this.Message: string =
            match this with
            | InvalidTxVersionForCommitmentTx version ->
                sprintf "invalid tx version for commitment tx (%i)" version
            | TxHasNoInputs -> "tx has no inputs"
            | TxHasMultipleInputs n -> sprintf "tx has multiple inputs (%i)" n
            | DoesNotSpendChannelFunds outPoint ->
                sprintf
                    "tx does not spend from the channel funds but spends from a different \
                    outpoint (%s)"
                    (outPoint.ToString())
            | InvalidLockTimeAndSequenceForCommitmentTx(lockTime, sequence) ->
                sprintf
                    "invalid lock time and sequence for commitment tx \
                    (locktime = %s, sequence = %s)"
                    (lockTime.ToString())
                    (sequence.ToString())

    type OutputClaimError =
        | BalanceBelowDustLimit
        | UnknownClosingTx

    type ClosingResult =
        {
            MainOutput: Result<TransactionBuilder, OutputClaimError>
        }

    let tryGetObscuredCommitmentNumber
        (fundingOutPoint: OutPoint)
        (transaction: Transaction)
        : Result<ObscuredCommitmentNumber, ValidateCommitmentTxError> =
        result {
            if transaction.Version <> TxVersionNumberOfCommitmentTxs then
                return!
                    Error <| InvalidTxVersionForCommitmentTx transaction.Version

            if transaction.Inputs.Count = 0 then
                return! Error <| TxHasNoInputs

            if transaction.Inputs.Count > 1 then
                return! Error <| TxHasMultipleInputs transaction.Inputs.Count

            let txIn = Seq.exactlyOne transaction.Inputs

            if fundingOutPoint <> txIn.PrevOut then
                return! Error <| DoesNotSpendChannelFunds txIn.PrevOut

            match
                ObscuredCommitmentNumber.TryFromLockTimeAndSequence
                    transaction.LockTime
                    txIn.Sequence
                with
            | None ->
                return!
                    Error
                    <| InvalidLockTimeAndSequenceForCommitmentTx(
                        transaction.LockTime,
                        txIn.Sequence
                    )
            | Some obscuredCommitmentNumber -> return obscuredCommitmentNumber
        }

    module RemoteClose =
        let private ClaimMainOutput
            (commitTx: Transaction)
            (staticChannelConfig: StaticChannelConfig)
            (localChannelPrivKeys: ChannelPrivKeys)
            (remotePerCommitmentPoint: PerCommitmentPoint)
            =
            result {
                let localChannelPubKeys =
                    localChannelPrivKeys.ToChannelPubKeys()

                let localPaymentPrivKey =
                    remotePerCommitmentPoint.DerivePaymentPrivKey
                        localChannelPrivKeys.PaymentBasepointSecret

                let localCommitmentPubKeys =
                    remotePerCommitmentPoint.DeriveCommitmentPubKeys
                        localChannelPubKeys

                let toRemoteScriptPubKey =
                    localCommitmentPubKeys
                        .PaymentPubKey
                        .RawPubKey()
                        .WitHash
                        .ScriptPubKey

                let toRemoteIndexOpt =
                    Seq.tryFindIndex
                        (fun (txOut: TxOut) ->
                            txOut.ScriptPubKey = toRemoteScriptPubKey
                        )
                        commitTx.Outputs

                let! toRemoteIndex =
                    match toRemoteIndexOpt with
                    | Some toRemoteIndex -> Ok toRemoteIndex
                    | None -> Error <| BalanceBelowDustLimit

                let transactionBuilder =
                    staticChannelConfig
                        .Network
                        .CreateTransactionBuilder()
                        .SetVersion(TxVersionNumberOfCommitmentTxs)
                        .AddKeys(localPaymentPrivKey.RawKey())

                return
                    transactionBuilder.AddCoin(
                        Coin(commitTx, uint32 toRemoteIndex)
                    )
            }

        let ClaimCommitTxOutputs
            (closingTx: Transaction)
            (staticChannelConfig: StaticChannelConfig)
            (channelPrivKeys: ChannelPrivKeys)
            (remoteCommit: RemoteCommit)
            =
            assert (remoteCommit.TxId = closingTx.GetTxId())

            {
                MainOutput =
                    ClaimMainOutput
                        closingTx
                        staticChannelConfig
                        channelPrivKeys
                        remoteCommit.RemotePerCommitmentPoint
            }

    module LocalClose =
        let private ClaimMainOutput
            (commitTx: Transaction)
            (commitmentNumber: CommitmentNumber)
            (staticChannelConfig: StaticChannelConfig)
            (localChannelPrivKeys: ChannelPrivKeys)
            =
            result {
                let localChannelPubKeys =
                    localChannelPrivKeys.ToChannelPubKeys()

                let remoteChannelPubKeys =
                    staticChannelConfig.RemoteChannelPubKeys

                let perCommitmentPoint =
                    localChannelPrivKeys.CommitmentSeed.DerivePerCommitmentPoint
                        commitmentNumber

                let localCommitmentPubKeys =
                    perCommitmentPoint.DeriveCommitmentPubKeys
                        localChannelPubKeys

                let remoteCommitmentPubKeys =
                    perCommitmentPoint.DeriveCommitmentPubKeys
                        remoteChannelPubKeys

                let transactionBuilder =
                    staticChannelConfig.Network.CreateTransactionBuilder()

                let toLocalScriptPubKey =
                    Scripts.toLocalDelayed
                        remoteCommitmentPubKeys.RevocationPubKey
                        staticChannelConfig.RemoteParams.ToSelfDelay
                        localCommitmentPubKeys.DelayedPaymentPubKey

                let toLocalIndexOpt =
                    let toLocalWitScriptPubKey =
                        toLocalScriptPubKey.WitHash.ScriptPubKey

                    Seq.tryFindIndex
                        (fun (txOut: TxOut) ->
                            txOut.ScriptPubKey = toLocalWitScriptPubKey
                        )
                        commitTx.Outputs

                let! toLocalIndex =
                    match toLocalIndexOpt with
                    | Some toLocalIndex -> Ok toLocalIndex
                    | None -> Error BalanceBelowDustLimit

                let delayedPaymentPrivKey =
                    perCommitmentPoint.DeriveDelayedPaymentPrivKey
                        localChannelPrivKeys.DelayedPaymentBasepointSecret

                transactionBuilder
                    .SetVersion(TxVersionNumberOfCommitmentTxs)
                    .Extensions.Add(CommitmentToLocalExtension())

                return
                    transactionBuilder
                        .AddKeys(delayedPaymentPrivKey.RawKey())
                        .AddCoin(
                            ScriptCoin(
                                commitTx,
                                uint32 toLocalIndex,
                                toLocalScriptPubKey
                            ),
                            CoinOptions(
                                Sequence =
                                    (Nullable
                                     <| Sequence(
                                         uint32
                                             staticChannelConfig.RemoteParams.ToSelfDelay.Value
                                     ))
                            )
                        )
            }

        let ClaimCommitTxOutputs
            (closingTx: Transaction)
            (staticChannelConfig: StaticChannelConfig)
            (channelPrivKeys: ChannelPrivKeys)
            =
            let obscuredCommitmentNumberRes =
                tryGetObscuredCommitmentNumber
                    staticChannelConfig.FundingScriptCoin.Outpoint
                    closingTx

            match obscuredCommitmentNumberRes with
            | Ok obscuredCommitmentNumber ->
                let localChannelPubKeys = channelPrivKeys.ToChannelPubKeys()

                let remoteChannelPubKeys =
                    staticChannelConfig.RemoteChannelPubKeys

                let commitmentNumber =
                    obscuredCommitmentNumber.Unobscure
                        staticChannelConfig.IsFunder
                        localChannelPubKeys.PaymentBasepoint
                        remoteChannelPubKeys.PaymentBasepoint

                {
                    MainOutput =
                        ClaimMainOutput
                            closingTx
                            commitmentNumber
                            staticChannelConfig
                            channelPrivKeys
                }
            | _ ->
                {
                    MainOutput = Error UnknownClosingTx
                }

    module RevokedClose =
        let private ClaimMainOutput
            (closingTx: Transaction)
            (commitmentNumber: CommitmentNumber)
            (staticChannelConfig: StaticChannelConfig)
            (remotePerCommitmentSecret: Choice<PerCommitmentSecret, PerCommitmentSecretStore>)
            (localChannelPrivKeys: ChannelPrivKeys)
            : Result<TransactionBuilder, OutputClaimError> =
            result {
                let localChannelPubKeys =
                    localChannelPrivKeys.ToChannelPubKeys()

                let remoteChannelPubKeys =
                    staticChannelConfig.RemoteChannelPubKeys

                let! perCommitmentSecret =
                    match remotePerCommitmentSecret with
                    | Choice1Of2 remotePerCommitmentSecret ->
                        Ok remotePerCommitmentSecret
                    | Choice2Of2 remotePerCommitmentSecretStore ->
                        let commitmentSecretOpt =
                            remotePerCommitmentSecretStore.GetPerCommitmentSecret
                                commitmentNumber

                        match commitmentSecretOpt with
                        | Some commitmentSecret -> Ok commitmentSecret
                        | None -> Error OutputClaimError.UnknownClosingTx

                let perCommitmentPoint =
                    perCommitmentSecret.PerCommitmentPoint()

                let localCommitmentPubKeys =
                    perCommitmentPoint.DeriveCommitmentPubKeys
                        localChannelPubKeys

                let remoteCommitmentPubKeys =
                    perCommitmentPoint.DeriveCommitmentPubKeys
                        remoteChannelPubKeys

                let transactionBuilder =
                    staticChannelConfig.Network.CreateTransactionBuilder()

                let toRemoteScriptPubKey =
                    localCommitmentPubKeys
                        .PaymentPubKey
                        .RawPubKey()
                        .WitHash
                        .ScriptPubKey

                let toLocalScriptPubKey =
                    Scripts.toLocalDelayed
                        localCommitmentPubKeys.RevocationPubKey
                        staticChannelConfig.LocalParams.ToSelfDelay
                        remoteCommitmentPubKeys.DelayedPaymentPubKey

                let toLocalWitScriptPubKey =
                    toLocalScriptPubKey.WitHash.ScriptPubKey

                let toRemoteIndexOpt =
                    closingTx.Outputs
                    |> Seq.tryFindIndex(fun out ->
                        out.ScriptPubKey = toRemoteScriptPubKey
                    )

                toRemoteIndexOpt
                |> Option.iter(fun toRemoteIndex ->
                    let localPaymentPrivKey =
                        perCommitmentPoint.DerivePaymentPrivKey
                            localChannelPrivKeys.PaymentBasepointSecret

                    transactionBuilder.SetVersion TxVersionNumberOfCommitmentTxs
                    |> ignore<TransactionBuilder>

                    transactionBuilder.AddKeys(localPaymentPrivKey.RawKey())
                    |> ignore<TransactionBuilder>

                    transactionBuilder.AddCoin(
                        Coin(closingTx, toRemoteIndex |> uint32)
                    )
                    |> ignore
                )

                let toLocalIndexOpt =
                    closingTx.Outputs
                    |> Seq.tryFindIndex(fun out ->
                        out.ScriptPubKey = toLocalWitScriptPubKey
                    )

                toLocalIndexOpt
                |> Option.iter(fun toLocalIndex ->
                    let revocationPrivKey =
                        perCommitmentSecret.DeriveRevocationPrivKey
                            localChannelPrivKeys.RevocationBasepointSecret

                    transactionBuilder.Extensions.Add(
                        CommitmentToLocalExtension()
                    )

                    transactionBuilder
                        .AddKeys(revocationPrivKey.RawKey())
                        .AddCoin(
                            ScriptCoin(
                                closingTx,
                                toLocalIndex |> uint32,
                                toLocalScriptPubKey
                            )
                        )
                    |> ignore
                )

                // We should've retuned BalanceBelowDustLimit here
                // but because it's possible for old local commitment TXs to
                // get used with HandleFundingTxSpent (which calls
                // RevokedClose.ClaimMainOutput for all non-last
                // commitment txs) we can't return BalanceBelowDustLimit error
                // and we instead use UnknownClosingTx error.
                // There's a small edge case: all the commitment tx money
                // to be in HTLCs (which we should return BalanceBelowDustLimit
                // for) but we return an invalid error msg.
                if toLocalIndexOpt.IsNone && toRemoteIndexOpt.IsNone then
                    return! Error UnknownClosingTx
                else
                    return transactionBuilder
            }

        let createPenaltyTx
            (localChannelPrivKeys: ChannelPrivKeys)
            (staticChannelConfig: StaticChannelConfig)
            (remoteCommit: RemoteCommit)
            (remotePerCommitmentSecret: PerCommitmentSecret)
            =
            let localChannelPubKeys = localChannelPrivKeys.ToChannelPubKeys()

            let remotePerCommitmentPoint =
                remotePerCommitmentSecret.PerCommitmentPoint()

            let localCommitmentPubKeys =
                remotePerCommitmentPoint.DeriveCommitmentPubKeys
                    localChannelPubKeys

            let remoteCommitmentPubKeys =
                remotePerCommitmentPoint.DeriveCommitmentPubKeys
                    staticChannelConfig.RemoteChannelPubKeys

            //Reconstruct the remote commitment tx for the given remoteCommit
            let commitTx =
                Transactions.makeCommitTx
                    staticChannelConfig.FundingScriptCoin
                    remoteCommit.Index
                    staticChannelConfig.RemoteChannelPubKeys.PaymentBasepoint
                    localChannelPubKeys.PaymentBasepoint
                    (not staticChannelConfig.IsFunder)
                    (staticChannelConfig.RemoteParams.DustLimitSatoshis)
                    localCommitmentPubKeys.RevocationPubKey
                    staticChannelConfig.LocalParams.ToSelfDelay
                    remoteCommitmentPubKeys.DelayedPaymentPubKey
                    localCommitmentPubKeys.PaymentPubKey
                    remoteCommitmentPubKeys.HtlcPubKey
                    localCommitmentPubKeys.HtlcPubKey
                    remoteCommit.Spec
                    staticChannelConfig.Network

            ClaimMainOutput
                (commitTx.Value.GetGlobalTransaction())
                remoteCommit.Index
                staticChannelConfig
                (Choice1Of2 remotePerCommitmentSecret)
                localChannelPrivKeys

        let ClaimCommitTxOutputs
            (closingTx: Transaction)
            (staticChannelConfig: StaticChannelConfig)
            (remotePerCommitmentSecrets: PerCommitmentSecretStore)
            (channelPrivKeys: ChannelPrivKeys)
            =
            let obscuredCommitmentNumberRes =
                tryGetObscuredCommitmentNumber
                    staticChannelConfig.FundingScriptCoin.Outpoint
                    closingTx

            match obscuredCommitmentNumberRes with
            | Ok obscuredCommitmentNumber ->
                let localChannelPubKeys = channelPrivKeys.ToChannelPubKeys()

                let remoteChannelPubKeys =
                    staticChannelConfig.RemoteChannelPubKeys

                let commitmentNumber =
                    obscuredCommitmentNumber.Unobscure
                        staticChannelConfig.IsFunder
                        localChannelPubKeys.PaymentBasepoint
                        remoteChannelPubKeys.PaymentBasepoint

                let claimMainOutputsRes =
                    ClaimMainOutput
                        closingTx
                        commitmentNumber
                        staticChannelConfig
                        (Choice2Of2 remotePerCommitmentSecrets)
                        channelPrivKeys

                match claimMainOutputsRes with
                | Error OutputClaimError.UnknownClosingTx ->
                    {
                        MainOutput = Error OutputClaimError.UnknownClosingTx
                    }
                | claimMainOutput ->
                    {
                        MainOutput = claimMainOutput
                    }
            | _ ->
                {
                    MainOutput = Error UnknownClosingTx
                }

    let HandleFundingTxSpent
        (savedChannelState: SavedChannelState)
        (remoteNextCommitInfoOpt: Option<RemoteNextCommitInfo>)
        (channelPrivKeys: ChannelPrivKeys)
        (closingTx: Transaction)
        : ClosingResult =
        let closingTxId = closingTx.GetTxId()

        if closingTxId = savedChannelState.LocalCommit.PublishableTxs.CommitTx.Value.GetTxId
                             () then
            LocalClose.ClaimCommitTxOutputs
                closingTx
                savedChannelState.StaticChannelConfig
                channelPrivKeys
        elif closingTxId = savedChannelState.RemoteCommit.TxId then
            RemoteClose.ClaimCommitTxOutputs
                closingTx
                savedChannelState.StaticChannelConfig
                channelPrivKeys
                savedChannelState.RemoteCommit
        else
            match remoteNextCommitInfoOpt with
            | Some(Waiting remoteNextCommit) when
                closingTxId = remoteNextCommit.TxId
                ->
                RemoteClose.ClaimCommitTxOutputs
                    closingTx
                    savedChannelState.StaticChannelConfig
                    channelPrivKeys
                    remoteNextCommit
            | _ ->
                RevokedClose.ClaimCommitTxOutputs
                    closingTx
                    savedChannelState.StaticChannelConfig
                    savedChannelState.RemotePerCommitmentSecrets
                    channelPrivKeys
