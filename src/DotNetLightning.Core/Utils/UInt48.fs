namespace DotNetLightning.Utils

open System
open DotNetLightning.Core.Utils.Extensions

/// Integral type to represent the commitment number.
/// The commitment number is a number set to commitment tx locktime
/// (in obscured form), see [bolt03-transactions](https://github.com/lightning/bolts/blob/master/03-transactions.md)
/// for more detail.
#if !NoDUsAsStructs
[<Struct>]
#endif
type UInt48 =
    {
        UInt64: uint64
    }

    static member private BitMask: uint64 = 0x0000ffffffffffffUL

    static member Zero: UInt48 =
        {
            UInt64 = 0UL
        }

    static member One: UInt48 =
        {
            UInt64 = 1UL
        }

    static member MaxValue: UInt48 =
        {
            UInt64 = UInt48.BitMask
        }

    static member FromUInt64(x: uint64) : UInt48 =
        if x > UInt48.BitMask then
            raise
            <| ArgumentOutOfRangeException(
                "x",
                "value is out of range for a UInt48"
            )
        else
            {
                UInt64 = x
            }

    override this.ToString() =
        this.UInt64.ToString()

    member this.GetBytesBigEndian() : array<byte> =
        this.UInt64.GetBytesBigEndian().[2..]

    static member FromBytesBigEndian(bytes6: array<byte>) =
        if bytes6.Length <> 6 then
            failwith "UInt48.FromBytesBigEndian expects a 6 byte array"
        else
            let bytes8 = Array.concat [| [| 0uy; 0uy |]; bytes6 |]

            {
                UInt64 = System.UInt64.FromBytesBigEndian bytes8
            }

    static member (+)(a: UInt48, b: UInt48) : UInt48 =
        {
            UInt64 = ((a.UInt64 <<< 8) + (b.UInt64 <<< 8)) >>> 8
        }

    static member (+)(a: UInt48, b: uint32) : UInt48 =
        {
            UInt64 = ((a.UInt64 <<< 8) + ((uint64 b) <<< 8)) >>> 8
        }

    static member (-)(a: UInt48, b: UInt48) : UInt48 =
        {
            UInt64 = ((a.UInt64 <<< 8) - (b.UInt64 <<< 8)) >>> 8
        }

    static member (-)(a: UInt48, b: uint32) : UInt48 =
        {
            UInt64 = ((a.UInt64 <<< 8) - ((uint64 b) <<< 8)) >>> 8
        }

    static member (*)(a: UInt48, b: UInt48) : UInt48 =
        {
            UInt64 = ((a.UInt64 <<< 4) * (b.UInt64 <<< 4)) >>> 8
        }

    static member (*)(a: UInt48, b: uint32) : UInt48 =
        {
            UInt64 = ((a.UInt64 <<< 4) * ((uint64 b) <<< 4)) >>> 8
        }

    static member (&&&)(a: UInt48, b: UInt48) : UInt48 =
        {
            UInt64 = a.UInt64 &&& b.UInt64
        }

    static member (^^^)(a: UInt48, b: UInt48) : UInt48 =
        {
            UInt64 = a.UInt64 ^^^ b.UInt64
        }

    static member (>>>)(a: UInt48, b: int) : UInt48 =
        {
            UInt64 = (a.UInt64 >>> b) &&& UInt48.BitMask
        }

    member this.TrailingZeros() : int =
        let rec count (acc: int) (x: uint64) : int =
            if acc = 48 || x &&& 1UL = 1UL then
                acc
            else
                count (acc + 1) (x >>> 1)

        count 0 this.UInt64
