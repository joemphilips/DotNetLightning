namespace DotNetLightning.Payment.LSAT

open System
open DotNetLightning.Utils.Primitives
open NBitcoin
open DotNetLightning.Core.Utils.Extensions
open DotNetLightning.Utils
open NBitcoin.Crypto

open ResultUtils
open ResultUtils.Portability

module private Helpers =
    let hex = DataEncoders.HexEncoder()   
    
type MacaroonIdentifierV0 = {
    PaymentHash: PaymentHash
    TokenId: uint256
}
    with
    static member Create(p: PaymentHash) =
        {
            PaymentHash = p
            TokenId = RandomUtils.GetUInt256()
        }

/// see ref: https://github.com/lightninglabs/LSAT/blob/master/macaroons.md#macaroon-identifier
type MacaroonIdentifier =
    | V0 of MacaroonIdentifierV0
    | UnknownVersion of version: uint16 * contents: byte[]
    with
    static member CreateV0 hash =
        MacaroonIdentifierV0.Create hash |> V0
    static member TryCreateFromBytes(b: byte[]) =
        let e = Error(sprintf "Invalid bytes for macaroon identifier %A" b) 
        if (b.Length < 2) then e else
        match UInt16.FromBytesBigEndian(b.[0..1]) with
        | 0us ->
            if (b.Length <> 2 + 32 + 32) then e else
            {
                PaymentHash = PaymentHash.PaymentHash(uint256(b.[2..33], false))
                TokenId = uint256(b.[34..], false)
            }
            |> V0
            |> Ok
        | x ->
            UnknownVersion(x, b.[2..])
            |> Ok
            
    member this.ToBytes() =
        match this with
        | V0 i ->
            let r = Array.zeroCreate 66
            Array.blit (0us.GetBytesBigEndian()) 0 r 0 2
            Array.blit (i.PaymentHash.ToBytes(false)) 0 r 2 32
            Array.blit (i.TokenId.ToBytes(false)) 0 r 34 32
            r
        | UnknownVersion (v, bytes) ->
            Array.concat (seq { yield v.GetBytesBigEndian(); yield bytes } )
            
    static member TryParse(str: string) =
        str |> Helpers.hex.DecodeData |> MacaroonIdentifier.TryCreateFromBytes
    member this.ToHex() =
        this.ToBytes() |> Helpers.hex.EncodeData
        
    member this.Hash =
        this.ToBytes() |> Hashes.SHA256 |> fun x -> uint256(x, false)
    override this.ToString() = this.ToHex()
    
