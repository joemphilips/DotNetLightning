namespace DotNetLightning.Utils

open ResultUtils
open ResultUtils.Portability

/// A `SeqParser` is a parser for sequences. It wraps a function which takes a
/// sequence and optionally returns a value successfully-parsed from the start of
/// the sequence along with the rest of the sequence. If parsing fails it returns
/// None.
///
/// You can construct a `SeqParser` using the `seqParser` computation expression.
/// The bind operation of the computation expression will return the value parsed
/// from the sequence and advance the sequence to the position where the next value
/// can be parsed from. For example, given a parser parseValue, you can construct a
/// parser which parses three values like this:
///
/// ```fsharp
/// seqParser {
///    let! value0 = parseValue()
///    let! value1 = parseValue()
///    let! value2 = parseValue()
///    return (value0, value1, value2)
/// }
/// ```
///
/// You can also call `SeqParser.next` and `SeqParser.abort` from within a
/// seqParser to pop the next element of the sequence or to abort parsing
/// respectively.
///
/// The function `SeqParser.parseToCompletion` takes a sequence and a `SeqParser`
/// and will attempt to parse the sequence to completion.

[<AutoOpen>]
module SeqParserCE =
    type SeqParser<'SeqElement, 'Value> =
        {
            Parse: seq<'SeqElement> -> Option<seq<'SeqElement> * 'Value>
        }

    type SeqParserBuilder<'SeqElement>() =
        member __.Bind<'Arg, 'Return>
            (
                seqParser0: SeqParser<'SeqElement, 'Arg>,
                func: 'Arg -> SeqParser<'SeqElement, 'Return>
            ) : SeqParser<'SeqElement, 'Return> =
            {
                Parse =
                    fun (sequence0: seq<'SeqElement>) ->
                        match seqParser0.Parse sequence0 with
                        | None -> None
                        | Some(sequence1, value0) ->
                            let seqParser1 = func value0
                            seqParser1.Parse sequence1
            }

        member __.Return<'Value>
            (value: 'Value)
            : SeqParser<'SeqElement, 'Value> =
            {
                Parse =
                    fun (sequence: seq<'SeqElement>) -> Some(sequence, value)
            }

        member __.ReturnFrom<'Value>
            (seqParser: SeqParser<'SeqElement, 'Value>)
            : SeqParser<'SeqElement, 'Value> =
            seqParser

        member __.Zero() : SeqParser<'SeqElement, unit> =
            {
                Parse = fun (sequence: seq<'SeqElement>) -> Some(sequence, ())
            }

        member __.Delay<'Value>
            (delayedSeqParser: unit -> SeqParser<'SeqElement, 'Value>)
            : SeqParser<'SeqElement, 'Value> =
            {
                Parse =
                    fun (sequence: seq<'SeqElement>) ->
                        (delayedSeqParser()).Parse sequence
            }

        member __.TryWith<'Value>
            (
                seqParser: SeqParser<'SeqElement, 'Value>,
                onException: exn -> SeqParser<'SeqElement, 'Value>
            ) : SeqParser<'SeqElement, 'Value> =
            {
                Parse =
                    fun (sequence: seq<'SeqElement>) ->
                        try
                            seqParser.Parse sequence
                        with
                        | ex ->
                            let subSeqParser = onException ex
                            subSeqParser.Parse sequence
            }

    let seqParser<'SeqElement> = SeqParserBuilder<'SeqElement>()

[<RequireQualifiedAccess>]
module SeqParser =
    let next<'SeqElement>() : SeqParser<'SeqElement, 'SeqElement> =
        {
            Parse =
                fun (sequence: seq<'SeqElement>) ->
                    Seq.tryHead sequence
                    |> Option.map(fun value -> (Seq.tail sequence, value))
        }

    let abort<'SeqElement, 'Value>() : SeqParser<'SeqElement, 'Value> =
        {
            Parse = fun (_sequence: seq<'SeqElement>) -> None
        }

    type ParseToCompletionError =
        | SequenceEndedTooEarly
        | SequenceNotReadToEnd

    let parseToCompletion<'SeqElement, 'Value>
        (sequence: seq<'SeqElement>)
        (seqParser: SeqParser<'SeqElement, 'Value>)
        : Result<'Value, ParseToCompletionError> =
        match seqParser.Parse sequence with
        | None -> Error SequenceEndedTooEarly
        | Some(consumedSequence, value) ->
            if Seq.isEmpty consumedSequence then
                Ok value
            else
                Error SequenceNotReadToEnd
