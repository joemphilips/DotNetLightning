namespace DotNetLightning.Channel

open System
open NBitcoin
open NBitcoin.BuilderExtensions
open DotNetLightning.Utils
open DotNetLightning.Utils.SeqConsumer
open DotNetLightning.Crypto

open ResultUtils
open ResultUtils.Portability

type CommitmentToLocalParameters = {
    ToSelfDelay: BlockHeightOffset16
    LocalDelayedPubKey: DelayedPaymentPubKey
}
    with
    static member TryExtractParameters (scriptPubKey: Script): Option<CommitmentToLocalParameters> =
        let ops =
            scriptPubKey.ToOps()
            // we have to collect it into a list and convert back to a seq
            // because the IEnumerable that NBitcoin gives us is internally
            // mutable.
            |> List.ofSeq
            |> Seq.ofList
        let checkOpCode(opcodeType: OpcodeType) = seqConsumer<Op> {
            let! op = NextInSeq()
            if op.Code = opcodeType then
                return ()
            else
                return! AbortSeqConsumer()
        }
        let consumeAllResult =
            SeqConsumer.ConsumeAll ops <| seqConsumer {
                do! checkOpCode OpcodeType.OP_IF
                let! opRevocationPubKey = NextInSeq()
                let! _revocationPubKey = seqConsumer {
                    match opRevocationPubKey.PushData with
                    | null -> return! AbortSeqConsumer()
                    | bytes ->
                        try
                            return RevocationPubKey.FromBytes bytes
                        with
                        | :? FormatException -> return! AbortSeqConsumer()
                }
                do! checkOpCode OpcodeType.OP_ELSE
                let! opToSelfDelay = NextInSeq()
                let! toSelfDelay = seqConsumer {
                    let nullableToSelfDelay = opToSelfDelay.GetLong()
                    if nullableToSelfDelay.HasValue then
                        try
                            return BlockHeightOffset16 (Convert.ToUInt16 nullableToSelfDelay.Value)
                        with
                        | :? OverflowException -> return! AbortSeqConsumer()
                    else
                        return! AbortSeqConsumer()
                }
                do! checkOpCode OpcodeType.OP_CHECKSEQUENCEVERIFY
                do! checkOpCode OpcodeType.OP_DROP
                let! opLocalDelayedPubKey = NextInSeq()
                let! localDelayedPubKey = seqConsumer {
                    match opLocalDelayedPubKey.PushData with
                    | null -> return! AbortSeqConsumer()
                    | bytes ->
                        try
                            return DelayedPaymentPubKey.FromBytes bytes
                        with
                        | :? FormatException -> return! AbortSeqConsumer()
                }
                do! checkOpCode OpcodeType.OP_ENDIF
                do! checkOpCode OpcodeType.OP_CHECKSIG
                return {
                    ToSelfDelay = toSelfDelay
                    LocalDelayedPubKey = localDelayedPubKey
                }
            }
        match consumeAllResult with
        | Ok data -> Some data
        | Error _consumeAllError -> None

type internal CommitmentToLocalExtension() =
    inherit BuilderExtension()
        override self.CanGenerateScriptSig (scriptPubKey: Script): bool =
            (CommitmentToLocalParameters.TryExtractParameters scriptPubKey).IsSome

        override self.GenerateScriptSig(scriptPubKey: Script, keyRepo: IKeyRepository, signer: ISigner): Script =
            let parameters =
                match (CommitmentToLocalParameters.TryExtractParameters scriptPubKey) with
                | Some parameters -> parameters
                | None ->
                    failwith
                        "NBitcoin should not call this unless CanGenerateScriptSig returns true"
            let pubKey = keyRepo.FindKey scriptPubKey
            // FindKey will return null if it can't find a key for
            // scriptPubKey. If we can't find a valid key then this method
            // should return null, indicating to NBitcoin that the sigScript
            // could not be generated.
            match pubKey with
            | null -> null
            | _ when pubKey = parameters.LocalDelayedPubKey.RawPubKey() ->
                let localDelayedSig = signer.Sign (parameters.LocalDelayedPubKey.RawPubKey())
                Script [
                    Op.GetPushOp (localDelayedSig.ToBytes())
                    Op.op_Implicit OpcodeType.OP_FALSE
                ]
            | _ -> null

        override self.CanDeduceScriptPubKey(_scriptSig: Script): bool =
            false

        override self.DeduceScriptPubKey(_scriptSig: Script): Script =
            raise <| System.NotSupportedException()

        override self.CanEstimateScriptSigSize(_scriptPubKey: Script): bool =
            false

        override self.EstimateScriptSigSize(_scriptPubKey: Script): int =
            raise <| System.NotSupportedException()

        override self.CanCombineScriptSig(_scriptPubKey: Script, _a: Script, _b: Script): bool = 
            false

        override self.CombineScriptSig(_scriptPubKey: Script, _a: Script, _b: Script): Script =
            raise <| System.NotSupportedException()

        override self.IsCompatibleKey(pubKey: PubKey, scriptPubKey: Script): bool =
            match CommitmentToLocalParameters.TryExtractParameters scriptPubKey with
            | None -> false
            | Some parameters -> parameters.LocalDelayedPubKey.RawPubKey() = pubKey


