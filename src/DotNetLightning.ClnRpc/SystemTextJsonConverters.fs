namespace DotNetLightning.ClnRpc.SystemTextJsonConverters

open System
open System.Text.Json
open System.Text.Json.Serialization
open DotNetLightning.ClnRpc
open DotNetLightning.Utils
open NBitcoin
open NBitcoin.Scripting

type MSatJsonConverter() =
    inherit JsonConverter<int64<msat>>()

    override this.Write(writer, value, _options) =
        writer.WriteStringValue(value.ToString() + "msat")

    override this.Read(reader, _typeToConvert, _options) =
        reader.GetString() |> parseClnAmount

type PubKeyJsonConverter() =
    inherit JsonConverter<PubKey>()

    override this.Write(writer, value, _options) =
        value.ToHex() |> writer.WriteStringValue

    override this.Read(reader, _typeToConvert, _options) =
        let b = reader.GetString() |> Convert.FromHexString

        if b.Length <> 33 then
            raise
            <| JsonException(
                $"Invalid length for pubkey: {b.Length}, it must be 33"
            )
        else
            b |> PubKey

type ShortChannelIdJsonConverter() =
    inherit JsonConverter<ShortChannelId>()

    override this.Write(writer, value, _options) =
        value.ToString() |> writer.WriteStringValue

    override this.Read(reader, _typeToConvert, _options) =
        let s = reader.GetString()

        match ShortChannelId.TryParse s |> ResultUtils.Result.ToFSharpCoreResult
            with
        | Ok c -> c
        | Error e -> raise <| JsonException e

type KeyJsonConverter() =
    inherit JsonConverter<Key>()

    override this.Write(writer, value, _options) =
        value.ToHex() |> writer.WriteStringValue

    override this.Read(reader, _typeToConvert, _options) =
        let b = reader.GetString() |> Convert.FromHexString
        new Key(b)

type UInt256JsonConverter() =
    inherit JsonConverter<uint256>()

    override this.Write(writer, value, _options) =
        value.ToString() |> writer.WriteStringValue

    override this.Read(reader, _typeToConvert, _options) =
        reader.GetString() |> uint256

type AmountOrAnyJsonConverter() =
    inherit JsonConverter<AmountOrAny>()

    override this.Write(writer, value, _options) =
        match value with
        | AmountOrAny.Any -> writer.WriteStringValue "any"
        | AmountOrAny.Amount a -> writer.WriteStringValue(a.ToString() + "msat")

    override this.Read(reader, _typeToConvert, _options) =
        match reader.GetString() with
        | "any" -> AmountOrAny.Any
        | x -> parseClnAmount x |> AmountOrAny.Amount

type AmountOrAllJsonConverter() =
    inherit JsonConverter<AmountOrAll>()

    override this.Write(writer, value, _options) =
        match value with
        | AmountOrAll.All -> writer.WriteStringValue "all"
        | AmountOrAll.Amount a -> writer.WriteStringValue(a.ToString() + "msat")

    override this.Read(reader, _typeToConvert, _options) =
        match reader.GetString() with
        | "all" -> AmountOrAll.All
        | x -> parseClnAmount x |> AmountOrAll.Amount

type OutPointJsonConverter() =
    inherit JsonConverter<OutPoint>()

    override this.Write(writer, value, _options) =
        writer.WriteStringValue($"{value.Hash}:{value.N}")

    override this.Read(reader, _typeToConvert, _options) =
        let splits = reader.GetString().Split ":"

        if splits.Length <> 2 then
            raise <| JsonException("not a valid txid:output tuple")
        else
            let o = OutPoint()
            o.Hash <- splits.[0] |> uint256.Parse
            o.N <- splits.[1] |> uint32
            o

type FeerateJsonConverter() =
    inherit JsonConverter<Feerate>()

    override this.Write(writer, value, _options) =
        value.ToString() |> writer.WriteStringValue

    override this.Read(reader, _typeToConvert, _options) =
        let s = reader.GetString()

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
            raise <| JsonException $"Unable to parse feerate from string {s}"

type OutputDescriptorJsonConverter(network: Network) =
    inherit JsonConverter<OutputDescriptor>()

    override this.Write(writer, value, _options) =
        value.ToString() |> writer.WriteStringValue

    override this.Read(reader, _typeToConvert, _options) =
        reader.GetString() |> fun s -> OutputDescriptor.Parse(s, network)


[<AutoOpen>]
module ClnSharpClientHelpers =
    type JsonSerializerOptions with

        member this.AddDNLJsonConverters(n: Network) =
            this.Converters.Add(MSatJsonConverter())
            this.Converters.Add(PubKeyJsonConverter())
            this.Converters.Add(ShortChannelIdJsonConverter())
            this.Converters.Add(KeyJsonConverter())
            this.Converters.Add(UInt256JsonConverter())
            this.Converters.Add(AmountOrAnyJsonConverter())
            this.Converters.Add(OutPointJsonConverter())
            this.Converters.Add(FeerateJsonConverter())
            this.Converters.Add(AmountOrAllJsonConverter())
            this.Converters.Add(OutputDescriptorJsonConverter(n))
