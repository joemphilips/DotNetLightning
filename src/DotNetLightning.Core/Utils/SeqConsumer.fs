namespace DotNetLightning.Utils

open ResultUtils
open ResultUtils.Portability

[<AutoOpen>]
module SeqConsumerCE =
    type SeqConsumer<'SeqElement, 'T> = {
        Consume: seq<'SeqElement> -> Option<seq<'SeqElement> * 'T>
    }

    type SeqConsumerBuilder<'SeqElement>() =
        member __.Bind<'Arg, 'Return>(seqConsumer0: SeqConsumer<'SeqElement, 'Arg>,
                                      func: 'Arg -> SeqConsumer<'SeqElement, 'Return>
                                     ): SeqConsumer<'SeqElement, 'Return> = {
            Consume = fun (sequence0: seq<'SeqElement>) ->
                match seqConsumer0.Consume sequence0 with
                | None -> None
                | Some (sequence1, value0) ->
                    let seqConsumer1 = func value0
                    seqConsumer1.Consume sequence1
        }

        member __.Return<'T>(value: 'T)
                                : SeqConsumer<'SeqElement, 'T> = {
            Consume = fun (sequence: seq<'SeqElement>) -> Some (sequence, value)
        }

        member __.ReturnFrom<'T>(seqConsumer: SeqConsumer<'SeqElement, 'T>): SeqConsumer<'SeqElement, 'T> =
            seqConsumer

        member __.Zero(): SeqConsumer<'SeqElement, unit> = {
            Consume = fun (sequence: seq<'SeqElement>) -> Some (sequence, ())
        }

        member __.Delay<'T>(delayedSeqConsumer: unit -> SeqConsumer<'SeqElement, 'T>)
                               : SeqConsumer<'SeqElement, 'T> = {
            Consume = fun (sequence: seq<'SeqElement>) ->
                (delayedSeqConsumer ()).Consume sequence
        }

        member __.TryWith<'T>(seqConsumer: SeqConsumer<'SeqElement, 'T>,
                              onException: exn -> SeqConsumer<'SeqElement, 'T>
                             ): SeqConsumer<'SeqElement, 'T> = {
            Consume = fun (sequence: seq<'SeqElement>) ->
                try
                    seqConsumer.Consume sequence
                with
                | ex ->
                    let subSeqConsumer = onException ex
                    subSeqConsumer.Consume sequence
        }


module SeqConsumer =
    let seqConsumer<'SeqElement> = SeqConsumerBuilder<'SeqElement>()

    let NextInSeq<'SeqElement>(): SeqConsumer<'SeqElement, 'SeqElement> = {
        Consume = fun (sequence: seq<'SeqElement>) ->
            match Seq.tryHead sequence with
            | None -> None
            | Some value -> Some (Seq.tail sequence, value)
    }

    let AbortSeqConsumer<'SeqElement, 'T>(): SeqConsumer<'SeqElement, 'T> = {
        Consume = fun (_sequence: seq<'SeqElement>) -> None
    }

    type ConsumeAllError =
        | SequenceEndedTooEarly
        | SequenceNotReadToEnd

    let ConsumeAll<'SeqElement, 'T>(sequence: seq<'SeqElement>) (seqConsumer: SeqConsumer<'SeqElement, 'T>)
                              : Result<'T, ConsumeAllError> =
        match seqConsumer.Consume sequence with
        | None -> Error SequenceEndedTooEarly
        | Some (consumedSequence, value) ->
            if Seq.isEmpty consumedSequence then
                Ok value
            else
                Error SequenceNotReadToEnd


