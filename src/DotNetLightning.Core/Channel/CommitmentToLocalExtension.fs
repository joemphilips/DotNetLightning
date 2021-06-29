namespace DotNetLightning.Channel

open System
open NBitcoin
open NBitcoin.BuilderExtensions
open DotNetLightning.Utils
open DotNetLightning.Crypto

open ResultUtils
open ResultUtils.Portability

type CommitmentToLocalParameters = {
    RevocationPubKey: RevocationPubKey
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
        let checkOpCode(opcodeType: OpcodeType) = seqParser<Op> {
            let! op = SeqParser.next()
            if op.Code = opcodeType then
                return ()
            else
                return! SeqParser.abort()
        }
        let parseToCompletionResult =
            SeqParser.parseToCompletion ops <| seqParser {
                do! checkOpCode OpcodeType.OP_IF
                let! opRevocationPubKey = SeqParser.next()
                let! revocationPubKey = seqParser {
                    match opRevocationPubKey.PushData with
                    | null -> return! SeqParser.abort()
                    | bytes ->
                        try
                            return RevocationPubKey.FromBytes bytes
                        with
                        | :? FormatException -> return! SeqParser.abort()
                }
                do! checkOpCode OpcodeType.OP_ELSE
                let! opToSelfDelay = SeqParser.next()
                let! toSelfDelay = seqParser {
                    let nullableToSelfDelay = opToSelfDelay.GetLong()
                    if nullableToSelfDelay.HasValue then
                        try
                            return BlockHeightOffset16 (Convert.ToUInt16 nullableToSelfDelay.Value)
                        with
                        | :? OverflowException -> return! SeqParser.abort()
                    else
                        return! SeqParser.abort()
                }
                do! checkOpCode OpcodeType.OP_CHECKSEQUENCEVERIFY
                do! checkOpCode OpcodeType.OP_DROP
                let! opLocalDelayedPubKey = SeqParser.next()
                let! localDelayedPubKey = seqParser {
                    match opLocalDelayedPubKey.PushData with
                    | null -> return! SeqParser.abort()
                    | bytes ->
                        try
                            return DelayedPaymentPubKey.FromBytes bytes
                        with
                        | :? FormatException -> return! SeqParser.abort()
                }
                do! checkOpCode OpcodeType.OP_ENDIF
                do! checkOpCode OpcodeType.OP_CHECKSIG
                return {
                    RevocationPubKey = revocationPubKey
                    ToSelfDelay = toSelfDelay
                    LocalDelayedPubKey = localDelayedPubKey
                }
            }
        match parseToCompletionResult with
        | Ok data -> Some data
        | Error _consumeAllError -> None

type internal CommitmentToLocalExtension() =
    inherit BuilderExtension()
        override __.CanGenerateScriptSig (scriptPubKey: Script): bool =
            (CommitmentToLocalParameters.TryExtractParameters scriptPubKey).IsSome

        override __.GenerateScriptSig(scriptPubKey: Script, keyRepo: IKeyRepository, signer: ISigner): Script =
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
            | _ when pubKey = parameters.RevocationPubKey.RawPubKey() ->
                let revocationSig = signer.Sign (parameters.RevocationPubKey.RawPubKey())
                Script [
                    Op.GetPushOp (revocationSig.ToBytes())
                    Op.op_Implicit OpcodeType.OP_TRUE
                ]
            | _ when pubKey = parameters.LocalDelayedPubKey.RawPubKey() ->
                let localDelayedSig = signer.Sign (parameters.LocalDelayedPubKey.RawPubKey())
                Script [
                    Op.GetPushOp (localDelayedSig.ToBytes())
                    Op.op_Implicit OpcodeType.OP_FALSE
                ]
            | _ -> null

        override __.CanDeduceScriptPubKey(_scriptSig: Script): bool =
            false

        override __.DeduceScriptPubKey(_scriptSig: Script): Script =
            raise <| NotSupportedException()

        override __.CanEstimateScriptSigSize(_scriptPubKey: Script): bool =
            false

        override __.EstimateScriptSigSize(_scriptPubKey: Script): int =
            raise <| NotSupportedException()

        override __.CanCombineScriptSig(_scriptPubKey: Script, _a: Script, _b: Script): bool = 
            false

        override __.CombineScriptSig(_scriptPubKey: Script, _a: Script, _b: Script): Script =
            raise <| NotSupportedException()

        override __.IsCompatibleKey(pubKey: PubKey, scriptPubKey: Script): bool =
            match CommitmentToLocalParameters.TryExtractParameters scriptPubKey with
            | None -> false
            | Some parameters ->
                parameters.RevocationPubKey.RawPubKey() = pubKey
                || parameters.LocalDelayedPubKey.RawPubKey() = pubKey


