namespace DotNetLightning.Payment

open System

open ResultUtils

open DotNetLightning.Chain
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Utils
open NBitcoin
open NBitcoin
open NBitcoin.DataEncoders
open NBitcoin.DataEncoders
open NBitcoin.DataEncoders


module private Helpers =
    let base58check = Base58CheckEncoder()
    
    let base58CheckDecode data =
        try
            base58check.DecodeData data |> Ok
        with
        | :? FormatException as fex ->
            fex.ToString() |> Error
    let parseBitcoinAddress addr n =
        try
            BitcoinAddress.Create(addr, n) |> Ok
        with
        | :? FormatException as fex ->
            fex.ToString() |> Error
    
    let tryGetP2WPKHAddressEncoder (n: Network) =
        let maybeEncoder = n.GetBech32Encoder(Bech32Type.WITNESS_PUBKEY_ADDRESS, false)
        if isNull maybeEncoder then Error("Failed to get p2wpkh encoder") else Ok maybeEncoder
        
    let tryGetP2WSHAddressEncoder (n: Network) =
        let maybeEncoder = n.GetBech32Encoder(Bech32Type.WITNESS_SCRIPT_ADDRESS, false)
        if isNull maybeEncoder then Error("Failed to get p2wsh encoder") else Ok maybeEncoder
            
    // ----- base58check prefixes -----
    // ref:https://en.bitcoin.it/wiki/List_of_address_prefixes 
    // (TODO: We might be able to get rid of this by using NBitcoin correctly)
    [<Literal>]
    let PREFIX_ADDRESS_PUBKEYHASH = 0uy
    
    [<Literal>]
    let PREFIX_ADDRESS_PUBKEYHASH_TESTNET = 111uy
    
    [<Literal>]
    let PREFIX_ADDRESS_SCRIPTHASH = 5uy
    
    [<Literal>]
    let PREFIX_ADDRESS_SCRIPTHASH_TESTNET = 196uy
        
type FallbackAddress = {
    Version: uint8
    Data: byte[]
}
    with
    static member FromBase58Address(addr: string) =
        result {
            let! d = Helpers.base58CheckDecode(addr)
            let (prefix, addressHash) = d |> Array.splitAt(1)
            match prefix.[0] with
            | Helpers.PREFIX_ADDRESS_PUBKEYHASH | Helpers.PREFIX_ADDRESS_PUBKEYHASH_TESTNET ->
                return { Version = 17uy; Data = addressHash }
            | Helpers.PREFIX_ADDRESS_SCRIPTHASH | Helpers.PREFIX_ADDRESS_SCRIPTHASH_TESTNET ->
                return { Version = 18uy; Data = addressHash }
            | x ->
                return! Error(sprintf "Unknown address prefix %d" x)
        }
        
    static member FromBech32Address(addrStr: string, n: Network) =
            match Helpers.tryGetP2WPKHAddressEncoder(n) with
            | Ok encoder ->
                match encoder.Decode(addrStr) with
                | decoded, witVersion ->
                    { Version = witVersion; Data = decoded } |> Ok
            | Error e ->
                match Helpers.tryGetP2WSHAddressEncoder(n) with
                | Ok encoder ->
                    match encoder.Decode(addrStr) with
                    | decoded, witVersion -> { Version = witVersion; Data = decoded } |> Ok
                | Error e2 ->
                    e + e2 |> Error
    member this.ToAddress(prefix: string) =
        match this.Version with
        | 17uy when prefix = "lnbc" ->
            let data = Array.concat (seq { [| Helpers.PREFIX_ADDRESS_PUBKEYHASH |]; this.Data })
            Helpers.base58check.EncodeData(data)
        | 18uy when prefix = "lnbc" ->
            let data = Array.concat (seq { [| Helpers.PREFIX_ADDRESS_SCRIPTHASH |]; this.Data })
            Helpers.base58check.EncodeData(data)
        | 17uy when prefix = "lntb" || prefix = "lnbcrt" ->
            let data = Array.concat (seq { [| Helpers.PREFIX_ADDRESS_PUBKEYHASH_TESTNET |]; this.Data })
            Helpers.base58check.EncodeData(data)
        | 18uy when prefix = "lnbc" || prefix = "lnbcrt" ->
            let data = Array.concat (seq { [| Helpers.PREFIX_ADDRESS_SCRIPTHASH_TESTNET |]; this.Data })
            Helpers.base58check.EncodeData(data)
        | v when prefix = "lnbc" ->
            let encoder = Bech32Encoder(Encoders.ASCII.DecodeData("bc"))
            encoder.Encode(v, this.Data)
        | v when prefix = "lntb" ->
            let encoder = Bech32Encoder(Encoders.ASCII.DecodeData("tb"))
            encoder.Encode(v, this.Data)
        | v when prefix = "lnbcrt" ->
            let encoder = Bech32Encoder(Encoders.ASCII.DecodeData("bcrt"))
            encoder.Encode(v, this.Data)
        | v ->
            failwithf "Unreachable! Unexpected version %A" v
            
type TaggedField =
    | UnknownTaggedField of byte[]
    | PaymentHash of PaymentHash
    | PaymentSecret of PaymentPreimage
    | Description of string
    /// Hash that will be included in the payment request, and can be checked against the hash of a
    /// long description, an invoice,
    | DescriptionHash of PaymentHash
    | FallbackAddress of FallbackAddress
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
    member this.PaymentHash =
        this.Tags |> Seq.choose(function TaggedField.PaymentHash p -> Some p | _ -> None) |> Seq.tryExactlyOne
        
    member this.PaymentSecret =
        this.Tags |> Seq.choose(function TaggedField.PaymentSecret ps -> Some ps | _ -> None) |> Seq.tryExactlyOne
        
    member this.Description =
        this.Tags
            |> Seq.choose(function
                          | Description d -> Some(Choice1Of2 d)
                          | DescriptionHash d -> Some(Choice2Of2 d)
                          | _ -> None)
            |> Seq.tryExactlyOne

    member this.FallbackAddress() =
        this.Tags
        |> Seq.choose(function FallbackAddress f -> Some f | _ -> None)
        |> Seq.map(fun fallbackAddr -> fallbackAddr.ToAddress(this.Prefix))
        |> Seq.tryExactlyOne
        
    member this.RoutingInfo =
        this.Tags
        |> List.choose(function RoutingInfo r -> Some r | _ -> None)
        
    member this.Expiry =
        this.Tags
        |> Seq.choose(function Expiry e -> Some (e) | _ -> None)
        |> Seq.tryExactlyOne
        
    member this.MinFinalCLTVExpiryDelta =
        this.Tags
        |> Seq.choose(function MinFinalCltvExpiry cltvE -> Some (cltvE) | _ -> None)
        |> Seq.tryExactlyOne

    member this.Features =
        this.Tags
        |> Seq.choose(function Features f -> Some f | _ -> None)
        |> Seq.tryExactlyOne
        
    member this.IsExpired =
        match this.Expiry with
        | Some e ->
            failwith ""
            // this.Timestamp + e <= DateTimeOffset.UtcNow
        | None ->
            // this.Timestamp + DEFAULT_EXPIRY_SECONDS <= DateTimeOffset.UtcNow
            failwith ""
            
    static member Create (chainhash: BlockId,
                          amount: LNMoney option,
                          paymentHash: PaymentHash,
                          privKey: Key,
                          description: string,
                          ?fallbackAddr: string option,
                          ?expirySeconds: DateTimeOffset option,
                          ?extraHops: ExtraHop list list,
                          ?timeStamp: DateTimeOffset option,
                          ?features: LocalFeatures) =
        let fallbackAddr = defaultArg fallbackAddr None
        let expirySeconds = defaultArg expirySeconds None
        let extraHops = defaultArg extraHops []
        let timeStamp = defaultArg timeStamp None
        let features = defaultArg features (LocalFeatures.Flags([||]))
        failwith ""
