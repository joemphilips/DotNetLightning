namespace DotNetLightning.Payment

open System

open ResultUtils

open DotNetLightning.Serialize.Msgs
open DotNetLightning.Utils


type TaggedField =
    | UnknownTaggedField of byte[]
    | PaymentHash of PaymentHash
    | PaymentSecret of PaymentPreimage
    | Description of string
    /// Hash that will be included in the payment request, and can be checked against the hash of a
    /// long description, an invoice,
    | DescriptionHash of PaymentHash
    | FallbackAddress of version: uint8 * data: byte[]
    | RoutingInfo of ExtraHop list
    | Expiry of DateTimeOffset
    | MinFinalCltvExpiry of DateTimeOffset
    | Features of LocalFeatures
and ExtraHop = {
    NodeId: NodeId
    ShortChannelId: ShortChannelId
    FeeBase: LNMoney
    FeeProportionalMillionths: int64
    CLTVExpiryDelta: BlockHeightOffset
}

type PaymentRequest = private {
    Prefix: string
    Amount: LNMoney option
    Timestamp: DateTimeOffset
    NodeId: NodeId
    Tags:TaggedField list
    Signature: LNECDSASignature
}
    with
    static member Create (prefix: string, amount: LNMoney option, timestamp, nodeId, tags, signature) =
        result {
            do! amount |> function None -> Ok() | Some a -> Result.requireTrue "amount must be larger than 0" (a > LNMoney.Zero)
            do! tags
                |> List.filter(function PaymentHash ph -> true | _ -> false)
                |> List.length
                |> Result.requireEqualTo (1) "There must be exactly one payment hash tag"
            do! tags
                |> List.filter(function Description _ | DescriptionHash _-> true | _ -> false)
                |> List.length
                |> Result.requireEqualTo (1) "There must be exactly one payment secret tag when feature bit is set"
            return {
                Prefix = prefix
                Amount = amount
                Timestamp = timestamp
                NodeId = nodeId
                Tags = tags
                Signature = signature
            }
        }
