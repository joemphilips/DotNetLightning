namespace DotNetLightning.Payment

open System

open ResultUtils

open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs

open System.Collections
open System.IO
open DotNetLightning.Serialize
open System.Text
open NBitcoin
open NBitcoin.Crypto
open NBitcoin.DataEncoders


module private Helpers =
    let base58check = Base58CheckEncoder()
    let utf8 = UTF8Encoding(false)
    
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
        
    /// maxInvoiceLength is the maximum total length an invoice can have.
    /// This is chosen to be the maximum number of bytes that can fit into a
    /// single QR code: https://en.wikipedia.org/wiki/QR_code#Storage
    [<Literal>]
    let maxInvoiceLength = 7089
    
    let checkMaxInvoiceLength (invoice: string) =
        if invoice.Length <= maxInvoiceLength then
            Ok()
        else
            Error(sprintf "Invoice length too large! max size is %d but it was %d" maxInvoiceLength invoice.Length)
            
/// To make it network-agnostic, it holds data directly in bytes rather than `NBitcoin.BitcoinAddress`
type FallbackAddress = private {
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
    static member TryCreate(version, data: byte[]) =
        match version with
        // p2pkh
        | 17uy when data.Length = 20 ->
            { Version = version; Data = data } |> Ok
        // p2sh
        | 18uy when data.Length = 20 ->
            { Version = version; Data = data } |> Ok
        // p2wpkh or p2wsh
        | v when (data.Length = 20 || data.Length = 32) ->
            { Version = version; Data = data } |> Ok
        | v ->
            sprintf "Unknown combination of bitcoin address version and length! version %d. length: %d" v data.Length
            |> Error
            
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
    | ExpiryTaggedField of DateTimeOffset
    | MinFinalCltvExpiryTaggedField of BlockHeight
    | FeaturesTaggedField of LocalFeatures
    with
    static member Deserialize(br: BitReader) =
        failwith ""
            
and ExtraHop = {
    NodeId: NodeId
    ShortChannelId: ShortChannelId
    FeeBase: LNMoney
    FeeProportionalMillionths: int64
    CLTVExpiryDelta: BlockHeightOffset
}
    with
    static member Size = 264 + 64 + 32 + 32 + 16

type TaggedFields = {
    Fields: TaggedField list
}
    with
    static member Zero = { Fields = [] }
    member this.FallbackAddresses =
        this.Fields |> List.choose(function FallbackAddressTaggedField a -> Some(a) | _ -> None)
        
    member this.CheckSanity() =
        let pHashes = this.Fields |> List.choose(function PaymentHashTaggedField x -> Some x | _ -> None)
        if (pHashes.Length > 1) then "Invalid BOLT11! duplicate 'p' field" |> Error else
        let secrets = this.Fields |> List.choose(function PaymentSecretTaggedField x -> Some (x) | _ -> None)
        if (secrets.Length > 1) then "Invalid BOLT11! duplicate 's' field" |> Error else
        let descriptions = this.Fields |> List.choose(function DescriptionTaggedField d -> Some d | _ -> None)
        if (descriptions.Length > 1) then Error("Invalid BOLT11! duplicate 'd' field") else
        let dHashes = this.Fields |> List.choose(function DescriptionHashTaggedField x -> Some x | _ -> None)
        if (dHashes.Length > 1) then Error ("Invalid BOLT11! duplicate 'h' field") else
        if (descriptions.Length = 1 && dHashes.Length = 1) then Error("Invalid BOLT11! both 'h' and 'd' field exists") else
        () |> Ok
type Bolt11Data = {
    Timestamp: DateTimeOffset
    TaggedFields: TaggedFields
    Signature: LNECDSASignature option
}
    with
    member this.Serialize(ls) =
        // use bw = new BitWriter(ls)
        // bw.Write(this.Timestamp.ToUnixTimeSeconds)
        failwith ""
        
    static member FromBytes(b: byte[]): Result<Bolt11Data, _> =
        result {
            let bitArray = BitArray(b.Length * 5)
            for di in 0..(b.Length - 1) do
                bitArray.Set(di * 5 + 0, ((b.[di] >>> 4) &&& 0x01uy) = 1uy)
                bitArray.Set(di * 5 + 1, ((b.[di] >>> 3) &&& 0x01uy) = 1uy)
                bitArray.Set(di * 5 + 2, ((b.[di] >>> 2) &&& 0x01uy) = 1uy)
                bitArray.Set(di * 5 + 3, ((b.[di] >>> 1) &&& 0x01uy) = 1uy)
                bitArray.Set(di * 5 + 4, ((b.[di] >>> 0) &&& 0x01uy) = 1uy)
            let reader = BitReader(bitArray)
            reader.Position <- reader.Count - 520 - 30
            if (reader.Position < 0) then
                return! "Invalid BOLT11: Invalid size" |> Error
            else if (not <| reader.CanConsume(65)) then
                return! "Invalid BOLT11: Invalid size" |> Error
            else
                let rs = reader.ReadBytes(65)
                let signature = LNECDSASignature.FromBytesCompact(rs, true)
                
                reader.Position <- 0
                let timestamp = Utils.UnixTimeToDateTime(reader.ReadULongBE(35))
                let checkSize (r: BitReader) c =
                    if (not <| r.CanConsume(c)) then
                        Error("Invalid BOLT11: Invalid size")
                    else
                        Ok()
                let expiryDate = timestamp + TimeSpan.FromHours(1.0)
                let minimalCLTVExpiry = 9
                let rec loop (r: BitReader) (acc: TaggedFields) =
                    result {
                        match (r.Position <> r.Position - 52- 30) with
                        | false ->
                            return acc
                        | true ->
                            do! checkSize r (5 + 10)
                            let tag = r.ReadULongBE(5)
                            let mutable size = (r.ReadULongBE(10) * 5UL) |> int
                            do! checkSize r (size)
                            match tag with
                            |  1UL -> // payment hash
                                if (size <> 52 * 5) then
                                    return! loop r acc // we must omit instead of returning an error (according to the BOLT11)
                                else
                                    let ph = r.ReadBytes(32) |> fun x -> uint256(x, false) |> PaymentHash |> PaymentHashTaggedField
                                    return! loop r ({ acc  with Fields = ph :: acc.Fields})
                            | 16UL -> // payment secret
                                if (size <> 52 * 5) then
                                    return! loop r acc // we must omit instead of returning an error (according to the BOLT11)
                                else
                                    let ps = r.ReadBytes(32) |> PaymentPreimage.Create |> PaymentSecretTaggedField
                                    return! loop r { acc with Fields = ps :: acc.Fields }
                            | 6UL -> // expiry
                                let expiryDate = timestamp + TimeSpan.FromSeconds(r.ReadULongBE(size) |> float) |> ExpiryTaggedField
                                return! loop r ({ acc with Fields = expiryDate :: acc.Fields })
                            | 13UL -> // description
                                let bytesCount = size / 8
                                let bytes = r.ReadBytes(bytesCount)
                                try
                                    let shortDesc = Helpers.utf8.GetString(bytes, 0 , bytesCount) |> DescriptionTaggedField
                                    return! loop r ({ acc with Fields = shortDesc :: acc.Fields })
                                with
                                | exp ->
                                    return! loop r acc
                            | 19UL -> // pubkey for node id
                                if (size <> 53 * 5) then
                                    return! loop r acc // we must omit instead of returning an error (according to the BOLT11)
                                else
                                    let pk = r.ReadBytes(33) |> PubKey |> NodeId |> NodeIdTaggedField
                                    return! loop r ({ acc with Fields = pk :: acc.Fields })
                            | 24UL -> // min_final_cltv_expiry
                                let v = r.ReadULongBE(size)
                                if (v > (UInt32.MaxValue |> uint64)) then
                                    return! loop r acc
                                else
                                    let minFinalCltvExpiry = v |> uint32 |> BlockHeight |> MinFinalCltvExpiryTaggedField
                                    return! loop r ({ acc with Fields = minFinalCltvExpiry :: acc.Fields })
                            | 9UL -> // fallback address
                                if (size < 5) then
                                    return! loop r acc
                                else
                                    let version = r.ReadULongBE(5)
                                    match version with
                                    | 0UL ->
                                        if (size = 5 + (20 * 8)) then
                                            let! addr = FallbackAddress.TryCreate(0uy, r.ReadBytes(20))
                                            return! loop r { acc with Fields = (FallbackAddressTaggedField addr) :: acc.Fields }
                                        else if (size = (5 + (32 * 8) + 4)) then
                                            let! addr = FallbackAddress.TryCreate(0uy, r.ReadBytes(32))
                                            return! loop r { acc with Fields = (FallbackAddressTaggedField addr) :: acc.Fields }
                                        else
                                            return! loop r acc
                                    | 17UL when (size <> 5 + (20 * 8)) ->
                                        let! addr = FallbackAddress.TryCreate(17uy, r.ReadBytes(20))
                                        return! loop r { acc with Fields = (FallbackAddressTaggedField addr) :: acc.Fields }
                                    | 18UL when (size <> 5 + (20 * 8)) ->
                                        let! addr = FallbackAddress.TryCreate(18uy, r.ReadBytes(20))
                                        return! loop r { acc with Fields = (FallbackAddressTaggedField addr) :: acc.Fields }
                                    | x ->
                                        // in case of unknown version, we should just ignore for future compatibility
                                        return! loop r acc
                            | 23UL -> // description hash
                                if (size <> 52 * 5) then
                                    return! loop r acc // we must omit instead of returning an error (according to the BOLT11)
                                else
                                    let dHash = r.ReadBytes(32) |> fun x -> uint256(x, true) |> DescriptionHashTaggedField
                                    return! loop r ({ acc with Fields = dHash :: acc.Fields })
                            | 3UL -> // routing info
                                if (size < ExtraHop.Size) then
                                    return! sprintf "Unexpected length for routing info (%d)" size |> Error
                                else
                                    let positionBefore = r.Position
                                    let hopInfos = ResizeArray()
                                    while (size >= ExtraHop.Size) do
                                        let nodeId = r.ReadBytes(264 / 8) |> PubKey |> NodeId
                                        let schId = r.ReadULongBE(64) |> ShortChannelId.FromUInt64
                                        let feeBase = r.ReadULongBE(32) |> LNMoney.MilliSatoshis
                                        let feeProportional = r.ReadULongBE(32) |> int64
                                        let cltvExpiryDelta =  r.ReadULongBE(16) |> uint16 |> BlockHeightOffset
                                        let hopInfo = { NodeId = nodeId
                                                        ShortChannelId = schId
                                                        FeeBase = feeBase
                                                        CLTVExpiryDelta = cltvExpiryDelta
                                                        FeeProportionalMillionths = feeProportional }
                                        hopInfos.Add(hopInfo)
                                        size <- size - ExtraHop.Size
                                    let hopInfoList = hopInfos |> Seq.toList |> RoutingInfoTaggedField
                                    return! loop r { acc with Fields = hopInfoList :: acc.Fields }
                            | 5UL -> // feature bits
                                if (size < 5) then
                                    return! loop r acc
                                else
                                    let feature = r.ReadULongBE(size)
                                    return failwith ""
                            | x -> // we must skip unknown field
                                return! loop r acc
                                    
                    }
                let! taggedField = loop reader TaggedFields.Zero
                    
                do! taggedField.CheckSanity()
                return {
                    Timestamp = timestamp
                    TaggedFields = taggedField
                    Signature = signature |> Some
                }
            }
    
    member this.ToBytes() =
        use ms = new MemoryStream()
        use stream = new LightningWriterStream(ms)
        this.Serialize(stream)
        ms.ToArray()
            

[<CLIMutable>]
type PaymentRequest = private {
    mutable Prefix: string
    mutable Amount: LNMoney option
    mutable Timestamp: DateTimeOffset
    mutable NodeId: NodeId
    mutable Tags:TaggedFields
    mutable Signature: LNECDSASignature
}
    with
    static member TryCreate (prefix: string, amount: LNMoney option, timestamp, nodeId, tags, signature) =
        result {
            do! amount |> function None -> Ok() | Some a -> Result.requireTrue "amount must be larger than 0" (a > LNMoney.Zero)
            do! tags.Tags.CheckSanity()
            return {
                Prefix = prefix
                Amount = amount
                Timestamp = timestamp
                NodeId = nodeId
                Tags = tags.Tags
                Signature = signature
            }
        }
    static member TryCreate (chainhash: BlockId,
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
    member this.PrefixValue = this.Prefix
    member this.AmountValue = this.Amount
    member this.TimestampValue = this.Timestamp
    member this.NodeIdValue = this.NodeId
    member this.SignatureValue = this.Signature
    member this.TagsValue = this.Tags
    
    member this.PaymentHash =
        this.Tags.Fields
        |> Seq.choose(function TaggedField.PaymentHashTaggedField p -> Some p | _ -> None)
        |> Seq.exactlyOne // we assured in constructor that it has only one
        
    member this.PaymentSecret =
        this.Tags.Fields
        |> Seq.choose(function TaggedField.PaymentSecretTaggedField ps -> Some ps | _ -> None)
        |> Seq.tryExactlyOne
        
    member this.Description =
        this.Tags.Fields
            |> Seq.choose(function
                          | DescriptionTaggedField d -> Some(Choice1Of2 d)
                          | DescriptionHashTaggedField d -> Some(Choice2Of2 d)
                          | _ -> None)
            |> Seq.tryExactlyOne

    member this.FallbackAddresses() =
        this.Tags.Fields
        |> List.choose(function FallbackAddressTaggedField f -> Some f | _ -> None)
        |> List.map(fun fallbackAddr -> fallbackAddr.ToAddress(this.Prefix))
        
    member this.RoutingInfo =
        this.Tags.Fields
        |> List.choose(function RoutingInfoTaggedField r -> Some r | _ -> None)
        |> List.tryExactlyOne
        
    /// absolute expiry date.
    member this.Expiry =
        this.Tags.Fields
        |> Seq.choose(function ExpiryTaggedField e -> Some (e) | _ -> None)
        |> Seq.tryExactlyOne
        
    member this.MinFinalCLTVExpiryDelta =
        this.Tags.Fields
        |> Seq.choose(function MinFinalCltvExpiryTaggedField cltvE -> Some (cltvE) | _ -> None)
        |> Seq.tryExactlyOne

    member this.Features =
        this.Tags.Fields
        |> Seq.choose(function FeaturesTaggedField f -> Some f | _ -> None)
        |> Seq.tryExactlyOne
        
    member this.IsExpired =
        match this.Expiry with
        | Some e ->
             e <= DateTimeOffset.UtcNow
        | None ->
            this.Timestamp + PaymentConstants.DEFAULT_EXPIRY_SECONDS <= DateTimeOffset.UtcNow
            
    member private this.Hash =
        let hrp =
            (sprintf "%s%s" (this.Prefix) (Amount.encode(this.Amount))) |> Helpers.utf8.GetBytes
        let data = { Bolt11Data.Timestamp = this.Timestamp
                     TaggedFields = this.Tags
                     Signature = None }
        let bin = data.ToBytes()
        let msg = Array.concat(seq { hrp; bin })
        Hashes.SHA256(msg) |> uint256
    member this.Sign(privKey: Key) =
        let sig64 = privKey.Sign(this.Hash) |> LNECDSASignature
        { this with Signature = sig64 }
        
    member this.ToString(signature: LNECDSASignature) =
        failwith "TODO"
        
    member this.ToString(privKey: Key) =
        let sign = this.Sign(privKey).SignatureValue
        this.ToString(sign)
        
        
    static member Parse(str: string): Result<PaymentRequest, string> =
        result {
            do! Helpers.checkMaxInvoiceLength (str) // for DoS protection
            let mutable s = (str.Clone() :?> string).ToLowerInvariant() // assure reference transparency
            if (s.StartsWith("lightning:", StringComparison.OrdinalIgnoreCase)) then
                s <- s.Substring("lightning:".Length)
            let! (hrp, data) = Helpers.decodeBech32(s)
            let! prefix = Helpers.checkAndGetPrefixFromHrp hrp
            let maybeAmount = Amount.decode(hrp.Substring(prefix.Length)) |> function Ok s -> Some s | Error _ -> None
            
            let! bolt11Data = Bolt11Data.FromBytes(data)
            let nodeId =
                let msg = hrp |> Helpers.ascii.DecodeData
                PubKey.RecoverFromMessage(msg, bolt11Data.Signature.Value.ToDER())
                |> NodeId
            
            return {
                PaymentRequest.Amount = maybeAmount
                Prefix = hrp
                Timestamp = bolt11Data.Timestamp
                NodeId = nodeId
                Tags = bolt11Data.TaggedFields
                Signature = bolt11Data.Signature.Value
            }
        }
