namespace DotNetLightning.Serialization

open System
open DotNetLightning.Core.Utils.Extensions

open ResultUtils
open ResultUtils.Portability

/// <namespacedoc>
///     <summary>
///         "DotNetLightning.Serialization" contains a low-level primitives for
///         Working with P2P network data serialization.
///         Such as general use TLV, lightning-rfc-defined message types,
///         bit-level serializers, etc.
///     </summary>
/// </namespacedoc>
/// <exclude />
module NamespaceDocDummy =
    ()

/// <summary>
///     TLV with unknown types.
///     TLV represents Type-Length-Value format. and it is designed to be
///     ignorable if the semantics is not clear to the node.
///     So that P2P messages can be forward compatible.
///     See [bolt01](https://github.com/lightning/bolts/blob/master/01-messaging.md)
///     For the detail.
///
///     All other concrete TLV types (e.g. `InitTLV`) has methods
///     To convert to/from this type.
/// </summary>
/// <seealso cref="InitTLV" />
type GenericTLV =
    {
        Type: uint64
        Value: array<byte>
    }

    static member TryCreateFromBytes(b: array<byte>) =
        result {
            let! ty, b = b.TryPopVarInt()
            let! l, b = b.TryPopVarInt()

            if (l > (uint64 Int32.MaxValue)) then
                return! Error(sprintf "length for tlv is too long %A" l)
            else
                let l = l |> int32

                if b.Length < l then
                    return!
                        Error(
                            sprintf
                                "malformed Generic TLV! bytes (%A) are shorter than specified length (%A)"
                                b
                                l
                        )
                else
                    let value = b.[0 .. (l - 1)]

                    return
                        {
                            Type = ty
                            Value = value
                        },
                        b.[l..]
        }

    /// consumes all bytes.
    static member TryCreateManyFromBytes(bytes: array<byte>) =
        result {
            let result = ResizeArray()
            let mutable b = bytes
            let mutable cont = true

            while cont do
                let! tlv, b2 = GenericTLV.TryCreateFromBytes(b)
                result.Add(tlv)
                b <- b2
                cont <- b.Length > 1

            return result.ToArray()
        }

    member this.ToBytes() =
        let ty = this.Type.ToVarInt()

        Array.concat
            [
                ty
                this.Value.LongLength.ToVarInt()
                this.Value
            ]
