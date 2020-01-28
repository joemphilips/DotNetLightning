namespace DotNetLightning.Payment

open System

open ResultUtils

open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs

open DotNetLightning.Serialize
open System.Text
open NBitcoin
open NBitcoin.Crypto
open NBitcoin.DataEncoders


module private Helpers =
    let base58check = Base58CheckEncoder()
    let utf8 = UTF8Encoding()
    
    let ascii = ASCIIEncoder()
    
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
            
    let decodeBech32 s =
        try
            InternalBech32Encoder.Instance.DecodeData(s).ToTuple()
            |> Ok
        with
        | :? FormatException as fex ->
            fex.ToString() |> Error
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
    
    let prefixes =
        Map.empty
        |> Map.add (Network.RegTest.GenesisHash) ("lnbcrt")
        |> Map.add (Network.TestNet.GenesisHash) ("lntb")
        |> Map.add (Network.Main.GenesisHash) ("lnbc")
        
    let prefixValues = prefixes |> Map.toList |> List.map(fun (k, v) -> v)
        
    let checkAndGetPrefixFromHrp (hrp: string) =
        let maybePrefix = prefixValues |> List.filter(fun p -> hrp.StartsWith(p)) |> List.tryExactlyOne
        match maybePrefix with
        | None ->
            Error(sprintf "Unknown prefix type %s! hrp must be either of %A" hrp prefixValues)
        | Some(prefix) ->
            Ok(prefix)
        
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
    | PaymentHashTaggedField of PaymentHash
    | PaymentSecretTaggedField of PaymentPreimage
    | NodeIdTaggedField of NodeId
    | DescriptionTaggedField of string
    /// Hash that will be included in the payment request, and can be checked against the hash of a
    /// long description, an invoice,
    | DescriptionHashTaggedField of uint256
    | FallbackAddressTaggedField of FallbackAddress
    | RoutingInfoTaggedField of ExtraHop list
    | ExpiryTaggedField of TimeSpan
    | MinFinalCltvExpiryTaggedField of DateTimeOffset
    | FeaturesTaggedField of LocalFeatures
    with
    static member Deserialize(br: BitReader) =
        let t = br.ReadBitsBEAsUInt8(5)
        let readForExpectedLength (br: BitReader) (expectedLength) =
            let bitLength = br.ReadBitsBEAsUInt16(10) |> int |> (*) 5
            if (bitLength <> expectedLength) then
                sprintf "Unexpected length. actual: %d; expected %d" bitLength expectedLength
                |> Error
            else
                br.ReadBytes(bitLength) |> Ok
        match t with
        // 'p' payment-hash
        | 1uy ->
            readForExpectedLength br 256
            |> Result.map(uint256 >> PaymentHash.PaymentHash >> PaymentHashTaggedField)
        // 's' payment-secret
        | 16uy ->
            readForExpectedLength br 256
            |> Result.map(PaymentPreimage.PaymentPreimage >> PaymentSecretTaggedField)
        // 'd' description
        | 13uy ->
            let bitLength = br.ReadBitsBEAsUInt16(10) |> int |> (*) 5
            let desc = br.ReadBytes(bitLength) |> Helpers.utf8.GetString
            DescriptionTaggedField(desc) |> Ok
        // 'n' node id
        | 19uy ->
            readForExpectedLength br 264
            |> Result.map(fun b -> PubKey(b, true) |> NodeId |> NodeIdTaggedField)
        // 'h' description hash
        | 23uy ->
            readForExpectedLength br 256
            |> Result.map(uint256 >> DescriptionHashTaggedField)
        // 'x' expiry time
        | 6uy ->
            let bitLength = br.ReadBitsBEAsUInt16(10) |> int |> (*) 5
            let t = br.ReadBytes(bitLength)
            failwith "TODO"
        // 'c' min-final-cltv-expiry
        | 24uy ->
            let bitLength = br.ReadBitsBEAsUInt16(10) |> int |> (*) 5
            let data = br.ReadBytes(bitLength)
            failwith "TODO"
        // 'f' fallback address
        | 9uy ->
            let bitLength = br.ReadBitsBEAsUInt16(10) |> int |> (*) 5
            let data = br.ReadBytes(bitLength)
            failwith "TODO"
        // 'r' 
        | 3uy ->
            failwith "TODO"
            
and ExtraHop = {
    NodeId: NodeId
    ShortChannelId: ShortChannelId
    FeeBase: LNMoney
    FeeProportionalMillionths: int64
    CLTVExpiryDelta: BlockHeightOffset
}

[<CLIMutable>]
type Bolt11Data = {
    mutable Timestamp: DateTimeOffset
    mutable TaggedFields: TaggedField list
    mutable Signature: byte[]
}
    with
    member this.Deserialize(ls) =
        use br = new BitReader(ls)
        this.Timestamp <- br.ReadBit35BEAsDateTime()
        // this.TaggedField <- List.iter()
        failwith ""
    member this.Serialize(ls) =
        use bw = new BitWriter(ls)
        // bw.Write(this.Timestamp.ToUnixTimeSeconds)
        failwith ""
        
    static member FromBytes(b: byte[]) =
        try
            ILightningSerializable.fromBytes<Bolt11Data>(b) |> Ok
        with
        | ex ->
            Error(ex.ToString())
            
        
    interface ILightningSerializable<Bolt11Data> with
        member this.Deserialize(ls) = this.Deserialize(ls)
        member this.Serialize(ls) = this.Serialize(ls)
        

[<CLIMutable>]
type PaymentRequest = private {
    mutable Prefix: string
    mutable Amount: LNMoney option
    mutable Timestamp: DateTimeOffset
    mutable NodeId: NodeId
    mutable Tags:TaggedField list
    mutable Signature: LNECDSASignature
}
    with
    static member TryCreate (prefix: string, amount: LNMoney option, timestamp, nodeId, tags, signature) =
        result {
            do! amount |> function None -> Ok() | Some a -> Result.requireTrue "amount must be larger than 0" (a > LNMoney.Zero)
            do! tags
                |> List.filter(function PaymentHashTaggedField ph -> true | _ -> false)
                |> List.length
                |> Result.requireEqualTo (1) "There must be exactly one payment hash tag"
            do! tags
                |> List.filter(function DescriptionTaggedField _ | DescriptionHashTaggedField _-> true | _ -> false)
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
    member this.PrefixValue = this.Prefix
    member this.AmountValue = this.Amount
    member this.TimestampValue = this.Timestamp
    member this.NodeIdValue = this.NodeId
    member this.TagsValue = this.Tags
    member this.SignatureValue = this.Signature
    
    member this.PaymentHash =
        this.Tags |> Seq.choose(function TaggedField.PaymentHashTaggedField p -> Some p | _ -> None) |> Seq.tryExactlyOne
        
    member this.PaymentSecret =
        this.Tags |> Seq.choose(function TaggedField.PaymentSecretTaggedField ps -> Some ps | _ -> None) |> Seq.tryExactlyOne
        
    member this.Description =
        this.Tags
            |> Seq.choose(function
                          | DescriptionTaggedField d -> Some(Choice1Of2 d)
                          | DescriptionHashTaggedField d -> Some(Choice2Of2 d)
                          | _ -> None)
            |> Seq.tryExactlyOne

    member this.FallbackAddress() =
        this.Tags
        |> Seq.choose(function FallbackAddressTaggedField f -> Some f | _ -> None)
        |> Seq.map(fun fallbackAddr -> fallbackAddr.ToAddress(this.Prefix))
        |> Seq.tryExactlyOne
        
    member this.RoutingInfo =
        this.Tags
        |> List.choose(function RoutingInfoTaggedField r -> Some r | _ -> None)
        
    member this.Expiry =
        this.Tags
        |> Seq.choose(function ExpiryTaggedField e -> Some (e) | _ -> None)
        |> Seq.tryExactlyOne
        
    member this.MinFinalCLTVExpiryDelta =
        this.Tags
        |> Seq.choose(function MinFinalCltvExpiryTaggedField cltvE -> Some (cltvE) | _ -> None)
        |> Seq.tryExactlyOne

    member this.Features =
        this.Tags
        |> Seq.choose(function FeaturesTaggedField f -> Some f | _ -> None)
        |> Seq.tryExactlyOne
        
    member this.IsExpired =
        match this.Expiry with
        | Some e ->
             (this.Timestamp + e) <= DateTimeOffset.UtcNow
        | None ->
            this.Timestamp + PaymentConstants.DEFAULT_EXPIRY_SECONDS <= DateTimeOffset.UtcNow
            
    /// the hash of this payment request
    member this.Hash =
        let hrp =
            (sprintf "%s%s" (this.Prefix) (Amount.encode(this.Amount))) |> Helpers.utf8.GetBytes
        let data = { Bolt11Data.Timestamp = this.Timestamp
                     TaggedFields = this.Tags
                     Signature = Array.zeroCreate(65) }
        let bin = data.ToBytes()
        let msg = Array.concat(seq { hrp; bin.[bin.Length - (521)..bin.Length - 1] }) // 520 bits are for signature
        Hashes.SHA256(msg)
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
    member this.ToString(signature: ECDSASignature) =
        failwith ""
        
    member this.Sign(privKey: Key): ECDSASignature =
        failwith ""
        
    static member Parse(str: string): Result<PaymentRequest, string> =
        let mutable s = (str.Clone() :?> string).ToLowerInvariant()
        if (s.StartsWith("lightning:", StringComparison.OrdinalIgnoreCase)) then
            s <- s.Substring("lightning:".Length)
        result {
            let! (hrp, data) = Helpers.decodeBech32(s)
            let! prefix = Helpers.checkAndGetPrefixFromHrp hrp
            let! bolt11Data = Bolt11Data.FromBytes(data)
            let signature = LNECDSASignature.FromBytesCompact(bolt11Data.Signature)
            let nodeId =
                let msg = hrp |> Helpers.ascii.DecodeData
                PubKey.RecoverFromMessage(msg, bolt11Data.Signature)
                |> NodeId
            
            let maybeAmount = Amount.decode(hrp.Substring(prefix.Length)) |> function Ok s -> Some s | Error _ -> None
            return {
                PaymentRequest.Amount = maybeAmount
                Prefix = hrp
                Timestamp = bolt11Data.Timestamp
                NodeId = nodeId
                Tags = bolt11Data.TaggedFields
                Signature = signature
            }
        }
