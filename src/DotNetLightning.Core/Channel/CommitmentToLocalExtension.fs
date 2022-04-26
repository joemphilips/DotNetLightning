namespace DotNetLightning.Channel

open System
open NBitcoin
open NBitcoin.BuilderExtensions
open DotNetLightning.Utils
open DotNetLightning.Crypto

open ResultUtils
open ResultUtils.Portability

type CommitmentToLocalParameters =
    {
        RevocationPubKey: RevocationPubKey
        ToSelfDelay: BlockHeightOffset16
        LocalDelayedPubKey: DelayedPaymentPubKey
    }

    static member TryExtractParameters
        (scriptPubKey: Script)
        : Option<CommitmentToLocalParameters> =
        let ops =
            scriptPubKey.ToOps()
            // we have to collect it into a list and convert back to a seq
            // because the IEnumerable that NBitcoin gives us is internally
            // mutable.
            |> List.ofSeq
            |> Seq.ofList

        let checkOpCode(opcodeType: OpcodeType) =
            seqParser<Op> { let! op = SeqParser.next()

            if op.Code = opcodeType then
                return ()
            else
                return! SeqParser.abort()
            }

        let parseToCompletionResult =
            SeqParser.parseToCompletion ops
            <| seqParser {
                do! checkOpCode OpcodeType.OP_IF
                let! opRevocationPubKey = SeqParser.next()

                let! revocationPubKey =
                    seqParser {
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

                let! toSelfDelay =
                    seqParser {
                        let nullableToSelfDelay = opToSelfDelay.GetLong()

                        if nullableToSelfDelay.HasValue then
                            try
                                return
                                    BlockHeightOffset16(
                                        Convert.ToUInt16
                                            nullableToSelfDelay.Value
                                    )
                            with
                            | :? OverflowException -> return! SeqParser.abort()
                        else
                            return! SeqParser.abort()
                    }

                do! checkOpCode OpcodeType.OP_CHECKSEQUENCEVERIFY
                do! checkOpCode OpcodeType.OP_DROP
                let! opLocalDelayedPubKey = SeqParser.next()

                let! localDelayedPubKey =
                    seqParser {
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

                return
                    {
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

    override __.Match(coin: ICoin, _input: PSBTInput) : bool =
        (CommitmentToLocalParameters.TryExtractParameters(coin.GetScriptCode()))
            .IsSome

    override __.Sign
        (
            inputSigningContext: InputSigningContext,
            keyRepo: IKeyRepository,
            signer: ISigner
        ) =
        let scriptPubKey = inputSigningContext.Coin.GetScriptCode()

        match keyRepo.FindKey scriptPubKey with
        | :? PubKey as pubKey when pubKey |> isNull |> not ->
            match signer.Sign pubKey with
            | :? TransactionSignature as signature when
                signature |> isNull |> not
                ->
                inputSigningContext.Input.PartialSigs.AddOrReplace(
                    pubKey,
                    signature
                )
            | _ -> ()
        | _ -> ()

    override __.CanDeduceScriptPubKey(_scriptSig: Script) : bool =
        false

    override __.DeduceScriptPubKey(_scriptSig: Script) : Script =
        raise <| NotSupportedException()

    override __.CanEstimateScriptSigSize(coin: ICoin) : bool =
        (CommitmentToLocalParameters.TryExtractParameters(coin.GetScriptCode()))
            .IsSome

    override __.EstimateScriptSigSize(_coin: ICoin) : int =
        (*
               Max script signature size = max signature size + op_true/op_false (1 byte)
               According to BIP 137: "Signatures are either 73, 72, or 71 bytes long,
               with probabilities approximately 25%, 50% and 25% respectively, although
               sizes even smaller than that are possible with exponentially decreasing probability"
               Reference: https://github.com/bitcoin/bips/blob/master/bip-0137.mediawiki#background-on-ecdsa-signatures
            *)
        73 + 1

    override __.IsCompatibleKey(pubKey: IPubKey, scriptPubKey: Script) : bool =
        match CommitmentToLocalParameters.TryExtractParameters scriptPubKey with
        | None -> false
        | Some parameters ->
            parameters.RevocationPubKey.RawPubKey() :> IPubKey = pubKey
            || parameters.LocalDelayedPubKey.RawPubKey() :> IPubKey = pubKey


    override __.Finalize(inputSigningContext: InputSigningContext) =
        let scriptPubKey = inputSigningContext.Coin.GetScriptCode()

        let parameters =
            match
                (CommitmentToLocalParameters.TryExtractParameters scriptPubKey)
                with
            | Some parameters -> parameters
            | None ->
                failwith
                    "NBitcoin should not call this unless Match returns true"

        let txIn = inputSigningContext.Input

        if txIn.PartialSigs.Count <> 0 then
            let keyAndSignatureOpt = txIn.PartialSigs |> Seq.tryExactlyOne

            match keyAndSignatureOpt with
            | Some keyAndSignature when
                keyAndSignature.Key = parameters.RevocationPubKey.RawPubKey()
                ->
                inputSigningContext.Input.FinalScriptSig <-
                    Script
                        [
                            Op.GetPushOp(keyAndSignature.Value.ToBytes())
                            Op.op_Implicit OpcodeType.OP_TRUE
                        ]
            | Some keyAndSignature when
                keyAndSignature.Key = parameters.LocalDelayedPubKey.RawPubKey()
                ->
                inputSigningContext.Input.FinalScriptSig <-
                    Script
                        [
                            Op.GetPushOp(keyAndSignature.Value.ToBytes())
                            Op.op_Implicit OpcodeType.OP_FALSE
                        ]
            | _ -> ()
