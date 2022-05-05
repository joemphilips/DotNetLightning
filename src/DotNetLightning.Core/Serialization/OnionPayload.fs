namespace DotNetLightning.Serialization

open System
open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Core.Utils.Extensions

open ResultUtils
open ResultUtils.Portability

/// legacy `hop_data` payload format described in [bolt04](https://github.com/lightning/bolts/blob/master/04-onion-routing.md)
type OnionRealm0HopData =
    {
        ShortChannelId: ShortChannelId
        AmtToForward: LNMoney
        OutgoingCLTVValue: uint32
    }

    static member FromBytes(bytes: array<byte>) =
        if bytes.Length < 20 then
            Error(sprintf "%A is not valid legacy payload format " bytes)
        else
            let schId = bytes.[0..7] |> ShortChannelId.From8Bytes

            let amountToForward =
                bytes.[8..15]
                |> fun x -> Utils.ToUInt64(x, false) |> LNMoney.MilliSatoshis

            let outgoingCLTV =
                bytes.[16..19] |> fun x -> Utils.ToUInt32(x, false)

            {
                ShortChannelId = schId
                AmtToForward = amountToForward
                OutgoingCLTVValue = outgoingCLTV
            }
            |> Ok

    member this.ToBytes() =
        let schid = this.ShortChannelId.ToBytes()

        let amt =
            this.AmtToForward.MilliSatoshi
            |> uint64
            |> fun x -> Utils.ToBytes(x, false)

        let outgoingCLTV =
            this.OutgoingCLTVValue |> fun x -> Utils.ToBytes(x, false)

        let pad = Array.zeroCreate 12

        Array.concat
            [
                [| 0uy |]
                schid
                amt
                outgoingCLTV
                pad
            ]

/// data foreach `hop_payloads` in `onion_packet`
/// described in [bolt04](https://github.com/lightning/bolts/blob/master/04-onion-routing.md)
type OnionPayload =
    | Legacy of OnionRealm0HopData
    | TLVPayload of tlvs: array<HopPayloadTLV> * hmac: uint256

    static member FromBytes(bytes: array<byte>) =
        match bytes.[0] with
        | 0uy -> OnionRealm0HopData.FromBytes(bytes.[1..]) |> Result.map(Legacy)
        | _ ->
            result {
                let! l, bytes = bytes.TryPopVarInt()

                if (l > (uint64 Int32.MaxValue)) then
                    return!
                        Error(
                            sprintf "length for onion paylaod is too long %A" l
                        )
                else
                    let l = int32 l

                    let! tlvs =
                        GenericTLV.TryCreateManyFromBytes(bytes.[0 .. (l - 1)])
                        |> Result.map(Array.map(HopPayloadTLV.FromGenericTLV))

                    let hmac = uint256(bytes.[l .. (l + 31)], false)
                    return (tlvs, hmac) |> TLVPayload
            }

    member this.ToBytes() =
        match this with
        | Legacy o -> o.ToBytes()
        | TLVPayload(tlvs, hmac) ->
            let payloads =
                tlvs
                |> Seq.map(fun tlv -> tlv.ToGenericTLV().ToBytes())
                |> Seq.concat
                |> Seq.toArray

            let length = payloads.LongLength.ToVarInt()

            Array.concat
                [
                    length
                    payloads
                    hmac.ToBytes(false)
                ]
