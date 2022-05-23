namespace DotNetLightning.ClnRpc.NewtonsoftJsonConverters

open System
open System.IO
open System.Runtime.CompilerServices
open System.Text.Json
open System.Text.Json.Nodes
open DotNetLightning.ClnRpc
open DotNetLightning.Utils
open Microsoft.FSharp.Reflection
open NBitcoin
open NBitcoin.JsonConverters
open NBitcoin.Scripting
open Newtonsoft.Json

[<Extension; AbstractClass; Sealed>]
type JsonConverterUtils() =
    [<Extension>]
    static member AssertJsonType(this: JsonReader, expectedType: JsonToken) =
        if this.TokenType <> expectedType then
            raise
            <| JsonObjectException(
                $"Unexpected json token type, expected is {expectedType} and actual is {this.TokenType}",
                this
            )

    [<Extension>]
    static member AssertJsonType
        (
            this: JsonReader,
            expectedTypes: JsonToken seq
        ) =
        if expectedTypes |> Seq.contains this.TokenType then
            let tStr = String.Join(", ", expectedTypes)

            raise
            <| JsonObjectException(
                $"Unexpected json token type, expected are {tStr} and actual is {this.TokenType}",
                this
            )

type MSatJsonConverter() =
    inherit JsonConverter<int64<msat>>()

    override this.WriteJson
        (
            writer: JsonWriter,
            value: int64<msat>,
            _serializer: JsonSerializer
        ) : unit =
        writer.WriteValue(value.ToString() + "msat")

    override this.ReadJson
        (
            reader,
            _objectType,
            _existingValue,
            _hasExistingValue,
            _serializer
        ) =
        reader.AssertJsonType(JsonToken.String)
        let mutable r: string = null

        try
            r <- reader.Value :?> string
            r |> parseClnAmount
        with
        | :? EndOfStreamException
        | :? FormatException ->
            raise <| JsonObjectException($"invalid msat value {r}", reader)

type PubKeyJsonConverter() =
    inherit JsonConverter<PubKey>()

    override this.WriteJson
        (
            writer: JsonWriter,
            value: PubKey,
            _serializer: JsonSerializer
        ) : unit =
        writer.WriteValue(value.ToHex())

    override this.ReadJson
        (
            reader,
            _objectType,
            _existingValue,
            _hasExistingValue,
            _serializer
        ) =
        if reader.TokenType = JsonToken.Null then
            null
        else
            reader.AssertJsonType(JsonToken.String)

            try
                let b = reader.Value |> string |> Convert.FromHexString

                if b.Length <> 33 then
                    raise
                    <| JsonObjectException(
                        $"Invalid length for pubkey: {b.Length}, it must be 33",
                        reader
                    )
                else
                    b |> PubKey
            with
            | :? EndOfStreamException
            | :? FormatException ->
                raise
                <| JsonObjectException($"invalid pubkey {reader.Value}", reader)

type ShortChannelIdJsonConverter() =
    inherit JsonConverter<ShortChannelId>()

    override this.WriteJson
        (
            writer: JsonWriter,
            value: ShortChannelId,
            _serializer: JsonSerializer
        ) : unit =
        writer.WriteValue(value.ToString())

    override this.ReadJson
        (
            reader,
            _objectType,
            _existingValue,
            _hasExistingValue,
            _serializer
        ) =
        reader.AssertJsonType(JsonToken.String)

        try
            let s = reader.Value |> string

            match
                ShortChannelId.TryParse s
                |> ResultUtils.Result.ToFSharpCoreResult
                with
            | Ok c -> c
            | Error e -> raise <| JsonObjectException(e, reader)
        with
        | :? EndOfStreamException
        | :? FormatException ->
            raise
            <| JsonObjectException($"invalid pubkey {reader.Value}", reader)

type KeyJsonConverter() =
    inherit JsonConverter<Key>()

    override this.WriteJson
        (
            writer: JsonWriter,
            value: Key,
            _serializer: JsonSerializer
        ) : unit =
        writer.WriteValue(value.ToHex())

    override this.ReadJson
        (
            reader,
            _objectType,
            _existingValue,
            _hasExistingValue,
            _serializer
        ) =
        if reader.TokenType = JsonToken.Null then
            null
        else
            reader.AssertJsonType(JsonToken.String)

            try
                let b = reader.Value |> string |> Convert.FromHexString
                new Key(b)
            with
            | :? EndOfStreamException
            | :? FormatException ->
                raise
                <| JsonObjectException($"invalid key {reader.Value}", reader)

type UInt256JsonConverter() =
    inherit JsonConverter<uint256>()

    override this.WriteJson
        (
            writer: JsonWriter,
            value: uint256,
            _serializer: JsonSerializer
        ) : unit =
        writer.WriteValue(value.ToString())

    override this.ReadJson
        (
            reader,
            _objectType,
            _existingValue,
            _hasExistingValue,
            _serializer
        ) =
        if reader.TokenType = JsonToken.Null then
            null
        else
            reader.AssertJsonType(JsonToken.String)

            try
                reader.Value |> string |> Convert.FromHexString |> uint256
            with
            | :? EndOfStreamException
            | :? FormatException ->
                raise
                <| JsonObjectException(
                    $"invalid uint256 {reader.Value}",
                    reader
                )

type AmountOrAnyJsonConverter() =
    inherit JsonConverter<AmountOrAny>()

    override this.WriteJson
        (
            writer: JsonWriter,
            value: AmountOrAny,
            _serializer: JsonSerializer
        ) : unit =
        match value with
        | AmountOrAny.Any -> writer.WriteValue "any"
        | AmountOrAny.Amount a -> writer.WriteValue(a.ToString() + "msat")

    override this.ReadJson
        (
            reader,
            _objectType,
            _existingValue,
            _hasExistingValue,
            _serializer
        ) =
        reader.AssertJsonType(JsonToken.String)

        try
            match reader.Value |> string with
            | "any" -> AmountOrAny.Any
            | x -> parseClnAmount x |> AmountOrAny.Amount
        with
        | :? EndOfStreamException
        | :? FormatException ->
            raise
            <| JsonObjectException(
                $"invalid AmountOrAny {reader.Value}",
                reader
            )


type AmountOrAllJsonConverter() =
    inherit JsonConverter<AmountOrAll>()

    override this.WriteJson
        (
            writer: JsonWriter,
            value: AmountOrAll,
            _serializer: JsonSerializer
        ) : unit =
        match value with
        | AmountOrAll.All -> writer.WriteValue "all"
        | AmountOrAll.Amount a -> writer.WriteValue(a.ToString() + "msat")

    override this.ReadJson
        (
            reader,
            _objectType,
            _existingValue,
            _hasExistingValue,
            _serializer
        ) =
        reader.AssertJsonType(JsonToken.String)

        try
            match reader.Value |> string with
            | "all" -> AmountOrAll.All
            | x -> parseClnAmount x |> AmountOrAll.Amount
        with
        | :? EndOfStreamException
        | :? FormatException ->
            raise
            <| JsonObjectException(
                $"invalid AmountOrAll {reader.Value}",
                reader
            )

type OutPointJsonConverter() =
    inherit JsonConverter<OutPoint>()

    override this.WriteJson
        (
            writer: JsonWriter,
            value: OutPoint,
            _serializer: JsonSerializer
        ) : unit =
        writer.WriteValue(value.ToString())

    override this.ReadJson
        (
            reader,
            _objectType,
            _existingValue,
            _hasExistingValue,
            _serializer
        ) =
        if reader.TokenType = JsonToken.Null then
            null
        else
            reader.AssertJsonType(JsonToken.String)

            try
                let splits = (reader.Value |> string).Split ":"

                if splits.Length <> 2 then
                    raise
                    <| JsonObjectException(
                        $"not a valid txid:output tuple. {reader.Value}",
                        reader
                    )
                else
                    let o = OutPoint()
                    o.Hash <- splits.[0] |> uint256.Parse
                    o.N <- splits.[1] |> uint32
                    o
            with
            | :? EndOfStreamException
            | :? FormatException ->
                raise
                <| JsonObjectException(
                    $"invalid uint256 {reader.Value}",
                    reader
                )

type FeerateJsonConverter() =
    inherit JsonConverter<Feerate>()

    override this.WriteJson
        (
            writer: JsonWriter,
            value: Feerate,
            _serializer: JsonSerializer
        ) : unit =
        value.ToString() |> writer.WriteValue

    override this.ReadJson
        (
            reader,
            _objectType,
            _existingValue,
            _hasExistingValue,
            _serializer
        ) =
        reader.AssertJsonType(JsonToken.String)

        try
            let s = reader.Value |> string

            let number =
                s
                |> Seq.choose(fun c ->
                    match UInt32.TryParse($"{c}") with
                    | true, v -> Some v
                    | _ -> None
                )
                |> Seq.fold (fun acc d -> acc * 10u + d) 0u

            let s = s.ToLowerInvariant()

            if s.EndsWith "perkw" then
                Feerate.PerKw(number)
            else if s.EndsWith "perkb" then
                Feerate.PerKb number
            else if s = "slow" then
                Feerate.Slow
            else if s = "normal" then
                Feerate.Normal
            else if s = "urgent" then
                Feerate.Urgent
            else
                raise
                <| JsonObjectException(
                    $"Unable to parse feerate from string {s}",
                    reader
                )
        with
        | :? EndOfStreamException
        | :? FormatException ->
            raise
            <| JsonObjectException($"invalid feerate {reader.Value}", reader)

type OutputDescriptorJsonConverter(network: Network) =
    inherit JsonConverter<OutputDescriptor>()

    override this.WriteJson
        (
            writer: JsonWriter,
            value: OutputDescriptor,
            _serializer: JsonSerializer
        ) : unit =
        value.ToString() |> writer.WriteValue

    override this.ReadJson
        (
            reader,
            _objectType,
            _existingValue,
            _hasExistingValue,
            _serializer
        ) =
        if reader.TokenType = JsonToken.Null then
            null
        else
            reader.AssertJsonType(JsonToken.String)

            try
                reader.Value
                |> string
                |> fun s -> OutputDescriptor.Parse(s, network)
            with
            | :? EndOfStreamException
            | :? FormatException ->
                raise
                <| JsonObjectException(
                    $"invalid output descriptor {reader.Value}",
                    reader
                )


open DotNetLightning.Serialization

type HexFeatureBitsJsonConverter() =
    inherit JsonConverter<FeatureBits>()

    override this.WriteJson
        (
            writer: JsonWriter,
            value: FeatureBits,
            _serializer: JsonSerializer
        ) : unit =
        value.ToHex() |> writer.WriteValue

    override this.ReadJson
        (
            reader,
            objectType,
            _existingValue,
            _hasExistingValue,
            _serializer
        ) =
        reader.AssertJsonType(JsonToken.String)

        try
            reader.Value |> string |> FeatureBits.ParseHexUnsafe
        with
        | :? EndOfStreamException
        | :? FormatException ->
            raise
            <| JsonObjectException(
                $"Invalid Feature bits {objectType.Name}",
                reader
            )

[<AutoOpen>]
module private NewtonsoftJsonHelpersCore =
    type JsonConverterCollection with

        member this._AddDNLJsonConverters() =
            this.Add(MSatJsonConverter())
            this.Add(PubKeyJsonConverter())
            this.Add(ShortChannelIdJsonConverter())
            this.Add(KeyJsonConverter())
            this.Add(UInt256JsonConverter())
            this.Add(AmountOrAnyJsonConverter())
            this.Add(AmountOrAllJsonConverter())
            this.Add(OutPointJsonConverter())
            this.Add(FeerateJsonConverter())
            this.Add(HexFeatureBitsJsonConverter())

/// F# options-converter
type OptionConverter() =
    inherit JsonConverter()

    override this.CanConvert t =
        t.IsGenericType
        && typedefof<option<_>>.Equals (t.GetGenericTypeDefinition())

    override _.WriteJson(writer, value, serializer) =
        let value =
            if value |> isNull then
                null
            else
                let _, fields =
                    FSharpValue.GetUnionFields(value, value.GetType())

                fields.[0]

        serializer.Serialize(writer, value)

    override _.ReadJson(reader, t, _existingValue, serializer) =
        let innerType = t.GetGenericArguments().[0]
        let cases = FSharpType.GetUnionCases t
        if reader.TokenType = JsonToken.Null then
            FSharpValue.MakeUnion(cases.[0], [||])
        else
            serializer.Converters._AddDNLJsonConverters()
            let value = serializer.Deserialize(reader, innerType)
            FSharpValue.MakeUnion(cases.[1], [| value |])

[<AutoOpen>]
module NewtonsoftJsonHelpers =
    type JsonConverterCollection with

        member this.AddDNLJsonConverters(n: Network) =
            this._AddDNLJsonConverters()
            this.Add(OutputDescriptorJsonConverter(n))
            this.Add(OptionConverter())

    type Newtonsoft.Json.JsonSerializerSettings with

        member this.AddDNLJsonConverters(n) =
            this.Converters.Add(MSatJsonConverter())
            this.Converters.Add(PubKeyJsonConverter())
            this.Converters.Add(ShortChannelIdJsonConverter())
            this.Converters.Add(KeyJsonConverter())
            this.Converters.Add(UInt256JsonConverter())
            this.Converters.Add(AmountOrAnyJsonConverter())
            this.Converters.Add(AmountOrAllJsonConverter())
            this.Converters.Add(OutPointJsonConverter())
            this.Converters.Add(FeerateJsonConverter())
            this.Converters.Add(OutputDescriptorJsonConverter(n))
            this.Converters.Add(HexFeatureBitsJsonConverter())
            this.Converters.Add(OptionConverter())
