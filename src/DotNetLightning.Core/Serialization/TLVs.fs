namespace DotNetLightning.Serialization


open DotNetLightning.Utils
open System
open NBitcoin

open ResultUtils
open ResultUtils.Portability

type InitTLV =
    /// genesis chain hash that the node is interested in
    | Networks of array<uint256>
    | Unknown of GenericTLV

    static member FromGenericTLV(tlv: GenericTLV) =
        match tlv.Type with
        | 1UL ->
            let n, rem = Math.DivRem(tlv.Value.Length, 32)

            if rem <> 0 then
                raise
                <| FormatException(
                    sprintf
                        "Bogus length for TLV in init message (%i), remainder was (%i)"
                        tlv.Value.Length
                        rem
                )
            else
                let result = Array.zeroCreate n

                for i in 0 .. n - 1 do
                    result.[i] <-
                        tlv.Value.[(i * 32) .. ((i * 32) + 31)]
                        |> fun x -> uint256(x, true)

                result |> Networks
        | _ -> Unknown(tlv)

    member this.ToGenericTLV() =
        match this with
        | Networks networks ->
            let v =
                networks |> Array.map(fun x -> x.ToBytes(true)) |> Array.concat

            {
                GenericTLV.Type = 1UL
                Value = v
            }
        | Unknown tlv -> tlv

type OpenChannelTLV =
    | UpfrontShutdownScript of Option<ShutdownScriptPubKey>
    | Unknown of GenericTLV

    static member FromGenericTLV(tlv: GenericTLV) =
        match tlv.Type with
        | 0UL ->
            let script = Script tlv.Value

            let shutdownScript =
                if script = Script.Empty then
                    None
                else
                    match ShutdownScriptPubKey.TryFromScript script with
                    | Ok shutdownScript -> Some shutdownScript
                    | Error err ->
                        raise
                        <| FormatException(
                            sprintf "invalid script for shutdown script: %s" err
                        )

            UpfrontShutdownScript shutdownScript
        | _ -> Unknown tlv

    member this.ToGenericTLV() =
        match this with
        | UpfrontShutdownScript script ->
            {
                Type = 0UL
                Value =
                    match script with
                    | None -> Array.empty
                    | Some script -> script.ToBytes()
            }
        | Unknown tlv -> tlv

type AcceptChannelTLV =
    | UpfrontShutdownScript of Option<ShutdownScriptPubKey>
    | Unknown of GenericTLV

    static member FromGenericTLV(tlv: GenericTLV) =
        match tlv.Type with
        | 0UL ->
            let script = Script tlv.Value

            let shutdownScript =
                if script = Script.Empty then
                    None
                else
                    match ShutdownScriptPubKey.TryFromScript script with
                    | Ok shutdownScript -> Some shutdownScript
                    | Error err ->
                        raise
                        <| FormatException(
                            sprintf "invalid script for shutdown script: %s" err
                        )

            UpfrontShutdownScript shutdownScript
        | _ -> Unknown tlv

    member this.ToGenericTLV() =
        match this with
        | UpfrontShutdownScript script ->
            {
                Type = 0UL
                Value =
                    match script with
                    | None -> Array.empty
                    | Some script -> script.ToBytes()
            }
        | Unknown tlv -> tlv

type QueryShortChannelIdsTLV =
    | QueryFlags of
        encodingType: EncodingType *
        encodedQueryFlags: array<QueryFlags>
    | Unknown of GenericTLV

    static member FromGenericTLV(tlv: GenericTLV) =
        match tlv.Type with
        | 1UL ->
            let encodingType =
                LanguagePrimitives.EnumOfValue<byte, EncodingType>(
                    tlv.Value.[0]
                )

            let data = tlv.Value.[1..]
            let flags = Decoder.decodeQueryFlags encodingType data
            QueryFlags(encodingType, flags)
        | _ -> QueryShortChannelIdsTLV.Unknown(tlv)

    member this.ToGenericTLV() =
        match this with
        | QueryFlags(t, flags) ->
            let encodedFlags: array<byte> = flags |> Encoder.encodeQueryFlags t

            let v =
                Array.concat(
                    seq {
                        yield [| (uint8) t |]
                        yield encodedFlags
                    }
                )

            {
                Type = 1UL
                Value = v
            }
        | Unknown tlv -> tlv

type QueryChannelRangeTLV =
    | Opt of QueryOption
    | Unknown of GenericTLV

    static member FromGenericTLV(tlv: GenericTLV) =
        match tlv.Type with
        | 1UL ->
            let op = tlv.Value.[0] |> QueryOption.Create
            Opt op
        | _ -> Unknown tlv

    member this.ToGenericTLV() =
        match this with
        | Opt opt ->
            {
                Type = 1UL
                Value = opt.ToBytes()
            }
        | Unknown tlv -> tlv

type ReplyChannelRangeTLV =
    | Timestamp of
        encodingType: EncodingType *
        encodedTimestamps: array<TwoTimestamps>
    | Checksums of array<TwoChecksums>
    | Unknown of GenericTLV

    static member FromGenericTLV(tlv: GenericTLV) =
        match tlv.Type with
        | 1UL ->
            let encodingType =
                LanguagePrimitives.EnumOfValue<byte, EncodingType>(
                    tlv.Value.[0]
                )

            let data = tlv.Value.[1..]
            let timestamps = Decoder.decodeTimestampPairs encodingType data
            Timestamp(encodingType, timestamps)
        | 3UL ->
            let checksums = Decoder.bytesToChecksumPair tlv.Value
            Checksums(checksums)
        | _ -> Unknown tlv

    member this.ToGenericTLV() =
        match this with
        | Timestamp(e, ts) ->
            let bytes = Encoder.encodeTimestampsPairs e ts
            let value = Array.concat [| [| (byte) e |]; bytes |]

            {
                Type = 1UL
                Value = value
            }
        | Checksums checksums ->
            let b = checksums |> Array.map(fun i -> i.ToBytes()) |> Array.concat

            {
                Type = 3UL
                Value = b
            }
        | Unknown x -> x

type HopPayloadTLV =
    | AmountToForward of LNMoney
    | OutgoingCLTV of uint32
    | ShortChannelId of ShortChannelId
    | PaymentData of paymentSecret: PaymentPreimage * totalMsat: LNMoney
    | Unknown of GenericTLV

    static member FromGenericTLV(tlv: GenericTLV) =
        match tlv.Type with
        | 2UL ->
            NBitcoin.Utils.ToUInt64(tlv.Value, false)
            |> LNMoney.MilliSatoshis
            |> AmountToForward
        | 4UL -> NBitcoin.Utils.ToUInt32(tlv.Value, false) |> OutgoingCLTV
        | 6UL -> ShortChannelId.From8Bytes(tlv.Value) |> ShortChannelId
        | 8UL ->
            let secret =
                tlv.Value.[0 .. PaymentPreimage.LENGTH - 1]
                |> PaymentPreimage.Create

            let totalMSat =
                NBitcoin.Utils.ToUInt64(
                    tlv.Value.[PaymentPreimage.LENGTH ..],
                    false
                )
                |> LNMoney.MilliSatoshis

            (secret, totalMSat) |> PaymentData
        | _ -> Unknown tlv

    member this.ToGenericTLV() =
        match this with
        | AmountToForward x ->
            {
                Type = 2UL
                Value = Utils.ToBytes(uint64 x.MilliSatoshi, false)
            }
        | OutgoingCLTV x ->
            {
                Type = 4UL
                Value = Utils.ToBytes(x, false)
            }
        | ShortChannelId x ->
            {
                Type = 6UL
                Value = x.ToBytes()
            }
        | PaymentData(secret, amount) ->
            let value =
                Array.concat
                    [
                        secret.ToByteArray()
                        Utils.ToBytes(uint64 amount.MilliSatoshi, false)
                    ]

            {
                Type = 8UL
                Value = value
            }
        | Unknown x -> x
