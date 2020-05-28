namespace DotNetLightning.Payment

open System
open System.IO
open System.Text
open System.Collections
open System.Diagnostics

open ResultUtils

open DotNetLightning.Utils
open DotNetLightning.Core.Utils.Extensions
open DotNetLightning.Serialize

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
            
    let encodeBech32 hrp s =
        InternalBech32Encoder.Instance.EncodeData(hrp, s, 0, s.Length)
        
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
        
    /// The values for prefix are sorted by its length, in this way we assure that we try to match the longest
    /// value first, so that e.g. when we have "lnbcrt" it will not match "lnbc"
    let sortedPrefixValues = prefixes |> Map.toSeq |> Seq.map(fun (_, v) -> v) |> Seq.sortByDescending(fun x -> x.Length)
    let checkAndGetPrefixFromHrp (hrp: string) =
        let maybePrefix = sortedPrefixValues |> Seq.filter(fun p -> hrp.StartsWith(p)) |> Seq.tryHead
        match maybePrefix with
        | None ->
            Error(sprintf "Unknown prefix type %s! hrp must be either of %A" hrp sortedPrefixValues)
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
            
    let uint64ToBase32(num: uint64):byte[] =
        if num = 0UL then [||] else
            
        let mutable numMutable = num
        // To fit an uint64, we need at most is ceil (64 / 5) = 13 groups.
        let arr = Array.zeroCreate 13
        let mutable i = 13
        while numMutable > 0UL do
            i <- i - 1
            arr.[i] <- byte(numMutable &&& 0b11111UL)
            numMutable <- numMutable >>> 5
        arr.[i..] // we only return non-zero leading groups
        
    let convertBits(data, fromBits, toBits, pad: bool) =
        InternalBech32Encoder.Instance.ConvertBits(data, fromBits, toBits, pad)
        
    let convert8BitsTo5 data =
        convertBits(data, 8, 5, true)
        
    let convert5BitsTo8 data =
        convertBits(data, 5, 8, true)
            
type IMessageSigner =
    /// take serialized msg hash and returns 65 bytes signature for it.(1byte header + 32 bytes r + 32 bytes s)
    /// The header byte must be (recovery id + 27uy + 4uy).
    abstract member SignMessage: uint256 -> byte[]
    
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
            { Version = v; Data = data } |> Ok
        | v ->
            sprintf "Unknown combination of bitcoin address version and length! version %d. length: %d" v data.Length
            |> Error
            
    member this.ToAddress(prefix: string) =
        match this.Version with
        | 17uy when prefix = "lnbc" ->
            let data = Array.concat (seq { yield [| Helpers.PREFIX_ADDRESS_PUBKEYHASH |]; yield this.Data })
            Helpers.base58check.EncodeData(data)
        | 18uy when prefix = "lnbc" ->
            let data = Array.concat (seq { yield [| Helpers.PREFIX_ADDRESS_SCRIPTHASH |]; yield this.Data })
            Helpers.base58check.EncodeData(data)
        | 17uy when prefix = "lntb" || prefix = "lnbcrt" ->
            let data = Array.concat (seq { yield [| Helpers.PREFIX_ADDRESS_PUBKEYHASH_TESTNET |]; yield this.Data })
            Helpers.base58check.EncodeData(data)
        | 18uy when prefix = "lnbc" || prefix = "lnbcrt" ->
            let data = Array.concat (seq { yield [| Helpers.PREFIX_ADDRESS_SCRIPTHASH_TESTNET |]; yield this.Data })
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
            
type ExtraHop = {
    NodeId: NodeId
    ShortChannelId: ShortChannelId
    FeeBase: LNMoney
    FeeProportionalMillionths: uint32
    CLTVExpiryDelta: BlockHeightOffset16
}
    with
    static member Size = 264 + 64 + 32 + 32 + 16
type TaggedField =
    | PaymentHashTaggedField of PaymentHash
    | PaymentSecretTaggedField of uint256
    | NodeIdTaggedField of NodeId
    | DescriptionTaggedField of string
    /// Hash that will be included in the payment request, and can be checked against the hash of a
    /// long description, an invoice,
    | DescriptionHashTaggedField of uint256
    | FallbackAddressTaggedField of FallbackAddress
    | RoutingInfoTaggedField of ExtraHop list
    | ExpiryTaggedField of DateTimeOffset
    | MinFinalCltvExpiryTaggedField of BlockHeightOffset32
    | FeaturesTaggedField of FeatureBit
    with
    member this.Type =
        match this with
        | PaymentHashTaggedField _ -> 1uy
        | PaymentSecretTaggedField _ -> 16uy
        | DescriptionTaggedField _ -> 13uy
        | NodeIdTaggedField _ -> 19uy
        | DescriptionHashTaggedField _ -> 23uy
        | ExpiryTaggedField _ -> 6uy
        | MinFinalCltvExpiryTaggedField _ -> 24uy
        | FallbackAddressTaggedField _ -> 9uy
        | RoutingInfoTaggedField _ -> 3uy
        | FeaturesTaggedField _ -> 5uy
        
    member private this.WriteField(writer: BinaryWriter, data: byte[]) =
        let mutable dataLengthInBase32 = Helpers.uint64ToBase32(data.Length |> uint64)
        // data length must be exactly 10 bits
        if (dataLengthInBase32.Length < 2) then
            dataLengthInBase32 <- Array.append ([|0uy|]) dataLengthInBase32
        Debug.Assert(dataLengthInBase32.Length = 2)
        
        writer.Write([|this.Type|])
        writer.Write(dataLengthInBase32)
        writer.Write(data)
        
    member internal this.WriteTo(writer: BinaryWriter, timestamp) =
        match this with
        | PaymentHashTaggedField p ->
            this.WriteField(writer, Helpers.convert8BitsTo5(p.ToBytes()))
        | PaymentSecretTaggedField p  ->
            this.WriteField(writer, Helpers.convert8BitsTo5(p.ToBytes()))
        | DescriptionTaggedField d ->
            let dBase32 = d |> Helpers.utf8.GetBytes |> Helpers.convert8BitsTo5
            this.WriteField(writer, dBase32)
        | DescriptionHashTaggedField h ->
            let dBase32 = h.ToBytes(false) |> Helpers.convert8BitsTo5
            this.WriteField(writer, dBase32)
        | NodeIdTaggedField(NodeId pk) ->
            let dBase32 = pk.ToBytes() |> Helpers.convert8BitsTo5
            this.WriteField(writer, dBase32)
        | MinFinalCltvExpiryTaggedField (BlockHeightOffset32 c) ->
            let dBase32 = c |> uint64 |> Helpers.uint64ToBase32
            this.WriteField(writer, dBase32)
        | ExpiryTaggedField x ->
            let dBase32 = ((x.ToUnixTimeSeconds() |> uint64) - timestamp) |> Helpers.uint64ToBase32
            this.WriteField(writer, dBase32)
        | FallbackAddressTaggedField f ->
            let d =
                let dBase32 = f.Data |> Helpers.convert8BitsTo5
                Array.append [|f.Version|] dBase32
            this.WriteField(writer, d)
        | RoutingInfoTaggedField r ->
            let routeInfoBase256  = ResizeArray()
            for hopInfo in r do
                let hopInfoBase256 = Array.zeroCreate (51) // 51 is the number of bytes needed to encode the single route
                Array.blit (hopInfo.NodeId.Value.ToBytes()) 0 hopInfoBase256 0 33
                Array.blit (hopInfo.ShortChannelId.ToBytes()) 0 hopInfoBase256 33 8
                Array.blit ((hopInfo.FeeBase.MilliSatoshi |> uint32).GetBytesBigEndian()) 0 hopInfoBase256 41 4
                Array.blit ((hopInfo.FeeProportionalMillionths |> uint32).GetBytesBigEndian()) 0 hopInfoBase256 45 4
                Array.blit (hopInfo.CLTVExpiryDelta.Value.GetBytesBigEndian()) 0 hopInfoBase256 49 2
                routeInfoBase256.Add(hopInfoBase256)
            let routeInfoBase32 = routeInfoBase256 |> Array.concat |>  Helpers.convert8BitsTo5
            this.WriteField(writer, routeInfoBase32)
        | FeaturesTaggedField f ->
            let dBase32 =
                let mutable byteArray = f.BitArray.ToByteArray()
                while (byteArray.Length * 8) % 5 <> 0 do
                    byteArray <- Array.concat [ [| 0uy |]; byteArray ]
                byteArray |> Helpers.convert8BitsTo5 |> Array.skipWhile((=)0uy)
            this.WriteField(writer, dBase32)
            

type TaggedFields = {
    Fields: TaggedField list
}
    with
    static member Zero =
        { Fields = [] }
    member this.FallbackAddresses =
        this.Fields |> List.choose(function FallbackAddressTaggedField a -> Some(a) | _ -> None)
        
    member this.ExplicitNodeId =
        this.Fields |> Seq.choose(function NodeIdTaggedField x -> Some x | _ -> None) |> Seq.tryExactlyOne
        
    member this.FeatureBit =
        this.Fields |> Seq.choose(function FeaturesTaggedField fb -> Some fb | _ -> None) |> Seq.tryExactlyOne
        
    member this.CheckSanity() =
        let pHashes = this.Fields |> List.choose(function PaymentHashTaggedField x -> Some x | _ -> None)
        if (pHashes.Length > 1) then "duplicate 'p' field" |> Error else
        if (pHashes.Length < 1) then "no payment hash" |> Error else
        let secrets = this.Fields |> List.choose(function PaymentSecretTaggedField x -> Some (x) | _ -> None)
        if (secrets.Length > 1) then "duplicate 's' field" |> Error else
        if (secrets.Length = 1 && this.FeatureBit.IsNone) then
            sprintf "secret (%A) is set but there were no feature bits" secrets.[0] |> Error
        else if (secrets.Length = 1 && not <| this.FeatureBit.Value.HasFeature(Feature.PaymentSecret)) then
            let fb = this.FeatureBit.Value
            sprintf "secret (%A) is set but feature bit (%s) is not set (%A)" (secrets.[0]) (fb.ToString()) (fb)
            |> Error else
        if (secrets.Length = 0 && this.FeatureBit.IsSome && (this.FeatureBit.Value.HasFeature(Feature.PaymentSecret, Mandatory))) then
            Error "feature bit for payment secret is set but payment secret is not set" else
        let descriptions = this.Fields |> List.choose(function DescriptionTaggedField d -> Some d | _ -> None)
        if (descriptions.Length > 1) then Error("duplicate 'd' field") else
        let dHashes = this.Fields |> List.choose(function DescriptionHashTaggedField x -> Some x | _ -> None)
        if (dHashes.Length > 1) then Error ("duplicate 'h' field") else
        if (descriptions.Length = 1 && dHashes.Length = 1) then Error("both 'h' and 'd' field exists") else
        if (descriptions.Length <> 1 && dHashes.Length <> 1) then Error("must have either description hash or description") else
        () |> Ok
        |> Result.mapError(fun s -> "Invalid BOLT11! " + s)
type private Bolt11Data = {
    Timestamp: DateTimeOffset
    TaggedFields: TaggedFields
    // byte is recovery id
    Signature: (LNECDSASignature * byte) option
}
    with
    static member FromBytes(b: byte[]): Result<Bolt11Data, _> =
        result {
            let bitArray = BitArray.From5BitEncoding(b)
            let reader = BitReader(bitArray)
            reader.Position <- reader.Count - 520 - 30
            if (reader.Position < 0) then
                return! sprintf "Invalid BOLT11: Invalid size. reader.Position was (%d)" reader.Position |> Error
            else if (not <| reader.CanConsume(65)) then
                return! "Invalid BOLT11: Invalid size. could not consume 65" |> Error
            else
                let rs = reader.ReadBytes(65)
                let signature = LNECDSASignature.FromBytesCompact(rs.[0..rs.Length - 2])
                let recvId = rs.[rs.Length - 1]
                
                reader.Position <- 0
                let timestamp = Utils.UnixTimeToDateTime(reader.ReadULongBE(35))
                let checkSize (r: BitReader) c =
                    if (not <| r.CanConsume(c)) then
                        Error(sprintf "Invalid BOLT11: Invalid size. could not consume %d" c)
                    else
                        Ok()
                let rec loop (r: BitReader) (acc: TaggedFields) (skipTo: int) =
                    result {
                        do! r.SkipTo(skipTo)
                        match (r.Position <> r.Count - 520 - 30) with
                        | false ->
                            return acc
                        | true ->
                            do! checkSize r (5 + 10)
                            let tag = r.ReadULongBE(5)
                            let mutable size = (r.ReadULongBE(10) * 5UL) |> int
                            do! checkSize r (size)
                            let afterReadPosition = r.Position + size
                            match tag with
                            |  1UL -> // payment hash
                                if (size <> 52 * 5) then
                                    return! loop r acc afterReadPosition // we must omit instead of returning an error (according to the BOLT11)
                                else
                                    let ph = r.ReadBytes(32) |> fun x -> uint256(x, false) |> PaymentHash |> PaymentHashTaggedField
                                    return! loop r ({ acc  with Fields = ph :: acc.Fields}) afterReadPosition 
                            | 16UL -> // payment secret
                                if (size <> 52 * 5) then
                                    return! loop r acc afterReadPosition  // we must omit instead of returning an error (according to the BOLT11)
                                else
                                    let ps = r.ReadBytes(32) |> fun x -> uint256(x, false) |> PaymentSecretTaggedField
                                    return! loop r { acc with Fields = ps :: acc.Fields } afterReadPosition
                            | 6UL -> // expiry
                                let expiryDate = timestamp + TimeSpan.FromSeconds(r.ReadULongBE(size) |> float) |> ExpiryTaggedField
                                    
                                return! loop r { acc with Fields = expiryDate :: acc.Fields } afterReadPosition
                            | 13UL -> // description
                                let bytesCount = size / 8
                                let bytes = r.ReadBytes(bytesCount)
                                try
                                    let shortDesc = Helpers.utf8.GetString(bytes, 0 , bytesCount) |> DescriptionTaggedField
                                    return! loop r ({ acc with Fields = shortDesc :: acc.Fields }) afterReadPosition
                                with
                                | _ ->
                                    return! loop r acc afterReadPosition
                            | 19UL -> // pubkey for node id
                                if (size <> 53 * 5) then
                                    return! loop r acc afterReadPosition // we must omit instead of returning an error (according to the BOLT11)
                                else
                                    let pk = r.ReadBytes(33) |> PubKey |> NodeId |> NodeIdTaggedField
                                    return! loop r ({ acc with Fields = pk :: acc.Fields }) afterReadPosition
                            | 24UL -> // min_final_cltv_expiry
                                let v = r.ReadULongBE(size)
                                if (v > (UInt32.MaxValue |> uint64)) then
                                    return! loop r acc afterReadPosition
                                else
                                    let minFinalCltvExpiry = v |> uint32 |> BlockHeightOffset32 |> MinFinalCltvExpiryTaggedField
                                    return! loop r { acc with Fields = minFinalCltvExpiry :: acc.Fields } afterReadPosition
                            | 9UL -> // fallback address
                                if (size < 5) then
                                    return! loop r acc afterReadPosition
                                else
                                    let version = r.ReadULongBE(5)
                                    match version with
                                    | 0UL ->
                                        if (size = 5 + (20 * 8)) then
                                            let! addr = FallbackAddress.TryCreate(0uy, r.ReadBytes(20))
                                            return! loop r { acc with Fields = (FallbackAddressTaggedField addr) :: acc.Fields } afterReadPosition
                                        else if (size = (5 + (32 * 8) + 4)) then
                                            let! addr = FallbackAddress.TryCreate(0uy, r.ReadBytes(32))
                                            return! loop r { acc with Fields = (FallbackAddressTaggedField addr) :: acc.Fields } afterReadPosition
                                        else
                                            return! loop r acc afterReadPosition
                                    | 17UL when (size = 5 + (20 * 8)) ->
                                        let! addr = FallbackAddress.TryCreate(17uy, r.ReadBytes(20))
                                        return! loop r { acc with Fields = (FallbackAddressTaggedField addr) :: acc.Fields } afterReadPosition
                                    | 18UL when (size = 5 + (20 * 8)) ->
                                        let! addr = FallbackAddress.TryCreate(18uy, r.ReadBytes(20))
                                        return! loop r { acc with Fields = (FallbackAddressTaggedField addr) :: acc.Fields } afterReadPosition
                                    | _ ->
                                        // in case of unknown version, we should just ignore for future compatibility
                                        return! loop r acc afterReadPosition
                            | 23UL -> // description hash
                                if (size <> 52 * 5) then
                                    return! loop r acc afterReadPosition // we must omit instead of returning an error (according to the BOLT11)
                                else
                                    let dHash = r.ReadBytes(32) |> fun x -> uint256(x, false) |> DescriptionHashTaggedField
                                    return! loop r ({ acc with Fields = dHash :: acc.Fields }) afterReadPosition
                            | 3UL -> // routing info
                                if (size < ExtraHop.Size) then
                                    return! sprintf "Unexpected length for routing info (%d)" size |> Error
                                else
                                    let hopInfos = ResizeArray()
                                    while (size >= ExtraHop.Size) do
                                        let nodeId = r.ReadBytes(264 / 8) |> PubKey |> NodeId
                                        let schId = r.ReadULongBE(64) |> ShortChannelId.FromUInt64
                                        let feeBase = r.ReadULongBE(32) |> LNMoney.MilliSatoshis
                                        let feeProportional = r.ReadULongBE(32) |> uint32
                                        let cltvExpiryDelta =  r.ReadULongBE(16) |> uint16 |> BlockHeightOffset16
                                        let hopInfo = { NodeId = nodeId
                                                        ShortChannelId = schId
                                                        FeeBase = feeBase
                                                        CLTVExpiryDelta = cltvExpiryDelta
                                                        FeeProportionalMillionths = feeProportional }
                                        hopInfos.Add(hopInfo)
                                        size <- size - ExtraHop.Size
                                    let hopInfoList = hopInfos |> Seq.toList |> RoutingInfoTaggedField
                                    return! loop r { acc with Fields = hopInfoList :: acc.Fields } afterReadPosition
                            | 5UL -> // feature bits
                                let bits = r.ReadBits(size)
                                let! fb =
                                    bits
                                    |> FeatureBit.TryCreate
                                    |> Result.mapError(fun x ->
                                        "Invalid BOLT11! Feature field is bogus " + x.ToString())
                                let features = fb |> FeaturesTaggedField
                                return! loop r { acc with Fields = features :: acc.Fields } afterReadPosition
                            | _ -> // we must skip unknown field
                                return! loop r acc afterReadPosition
                                    
                    }
                let! taggedField = loop reader (TaggedFields.Zero) reader.Position
                do! taggedField.CheckSanity()
                return {
                    Timestamp = timestamp
                    TaggedFields = taggedField
                    Signature = (signature, recvId) |> Some
                }
            }
    
    member this.ToBytesBase32() =
        use ms = new MemoryStream()
        use writer = new BinaryWriter(ms)
        let timestamp = 
            Utils.DateTimeToUnixTime(this.Timestamp)
            |> uint64
        let timestampBase32 =
            timestamp
            |> Helpers.uint64ToBase32
        Debug.Assert(timestampBase32.Length <= (35 / 5), sprintf "Timestamp too big: %d" timestampBase32.Length)
        let padding: byte[] = Array.zeroCreate(7 - timestampBase32.Length)
        writer.Write(padding)
        writer.Write(timestampBase32)
        
        this.TaggedFields.Fields
        |> List.rev
        |> List.iter(fun f ->
            f.WriteTo(writer, timestamp)
            )
        
        this.Signature
        |> Option.iter(fun (s, recv) ->
            let sigBase32 = Array.concat [ s.ToBytesCompact(); [|recv|]] |> Helpers.convert8BitsTo5
            writer.Write(sigBase32)
            )
        ms.ToArray()
        
    /// Returns binary representation for signing.
    member this.ToBytes() =
        this.ToBytesBase32()
        |> Helpers.convert5BitsTo8
            

/// a.k.a bolt11-invoice, lightning-invoice
type PaymentRequest = private {
    Prefix: string
    Amount: LNMoney option
    Timestamp: DateTimeOffset
    NodeId: NodeId
    Tags: TaggedFields
    Signature: (LNECDSASignature * byte) option
}
    with
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
            |> Seq.exactlyOne

    member this.FallbackAddresses =
        this.Tags.Fields
        |> List.choose(function FallbackAddressTaggedField f -> Some f | _ -> None)
        |> List.map(fun fallbackAddr -> fallbackAddr.ToAddress(this.Prefix))
        
    member this.RoutingInfo =
        this.Tags.Fields
        |> List.choose(function RoutingInfoTaggedField r -> Some r | _ -> None)
        
    /// absolute expiry date.
    member this.Expiry =
        this.Tags.Fields
        |> Seq.choose(function ExpiryTaggedField e -> Some (e) | _ -> None)
        |> Seq.tryExactlyOne
        |> Option.defaultValue (this.Timestamp + PaymentConstants.DEFAULT_EXPIRY_SECONDS)
        
    member this.MinFinalCLTVExpiryDelta =
        this.Tags.Fields
        |> Seq.choose(function MinFinalCltvExpiryTaggedField cltvE -> Some (cltvE) | _ -> None)
        |> Seq.tryExactlyOne
        |> Option.defaultValue (PaymentConstants.DEFAULT_MINIMUM_CLTVEXPIRY)

    member this.Features =
        this.Tags.Fields
        |> Seq.choose(function FeaturesTaggedField f -> Some f | _ -> None)
        |> Seq.tryExactlyOne
        
    member this.IsExpired =
        this.Expiry <= DateTimeOffset.UtcNow
            
    member this.HumanReadablePart =
        (sprintf "%s%s" (this.Prefix) (Amount.encode(this.Amount)))
    member private this.Hash =
        let hrp =
             this.HumanReadablePart |> Helpers.utf8.GetBytes
             
        let data = { Bolt11Data.Timestamp = this.Timestamp
                     TaggedFields = this.Tags
                     Signature = None }
        let bin = data.ToBytes()
        let msg = Array.concat(seq { yield hrp; yield bin })
        Hashes.SHA256(msg) |> uint256
        
    member this.Sign(privKey: Key, ?forceLowR: bool) =
        let forceLowR = Option.defaultValue true forceLowR
        let ecdsaSig = privKey.SignCompact(this.Hash, forceLowR)
        let recvId = ecdsaSig.[0] - 27uy - 4uy
        let sig64 = ecdsaSig |> fun s -> LNECDSASignature.FromBytesCompact(s, true)
        { this with Signature = (sig64, recvId) |> Some }
        
    override this.ToString() =
        let hrp = this.HumanReadablePart
        let data =
            { Bolt11Data.TaggedFields = this.Tags
              Timestamp = this.TimestampValue
              Signature = this.Signature }
        data.ToBytesBase32() |> Helpers.encodeBech32 hrp
        
    member private this.ToString(signature65bytes: byte[]) =
        let hrp = this.HumanReadablePart
        let data =
            let recvId = signature65bytes.[0] - 27uy - 4uy
            let signature = signature65bytes |> fun s -> LNECDSASignature.FromBytesCompact(s, true)
            { Bolt11Data.TaggedFields = this.Tags
              Timestamp = this.Timestamp
              Signature = (signature, recvId) |> Some }
        data.ToBytesBase32() |> Helpers.encodeBech32 hrp
        
    member this.ToString(signer: IMessageSigner) =
        let sign = signer.SignMessage(this.Hash)
        this.ToString(sign)
        
    static member Parse(str: string): Result<PaymentRequest, string> =
        result {
            do! Helpers.checkMaxInvoiceLength (str) // for DoS protection
            let mutable s = str.ToLowerInvariant() // assure reference transparency
            if (s.StartsWith("lightning:", StringComparison.OrdinalIgnoreCase)) then
                s <- s.Substring("lightning:".Length)
            let! (hrp, data) = Helpers.decodeBech32(s)
            let! prefix = Helpers.checkAndGetPrefixFromHrp hrp
            let maybeAmount = Amount.decode(hrp.Substring(prefix.Length)) |> function Ok s -> Some s | Error _ -> None
            
            let! bolt11Data = Bolt11Data.FromBytes(data)
            let (sigCompact, recv) = bolt11Data.Signature.Value
            let nodeId =
                match (bolt11Data.TaggedFields.ExplicitNodeId) with
                | Some n -> n
                | None ->
                    let msg =
                        let bitArray = BitArray.From5BitEncoding(data)
                        let reader = BitReader(bitArray, bitArray.Count - 520 - 30)
                        let remainder = ref 0;
                        let mutable byteCount = Math.DivRem(reader.Count, 8, remainder)
                        if (!remainder <> 0) then
                            byteCount <- byteCount + 1
                    
                        seq { yield (hrp |> Helpers.utf8.GetBytes); yield (reader.ReadBytes(byteCount)) }
                        |> Array.concat
                    let signatureInNBitcoinFormat = Array.zeroCreate(65)
                    Array.blit (sigCompact.ToBytesCompact()) 0 signatureInNBitcoinFormat 1 64
                    signatureInNBitcoinFormat.[0] <- (recv + 27uy + 4uy)
                    let r = Hashes.SHA256(msg) |> uint256
                    PubKey.RecoverCompact(r, signatureInNBitcoinFormat).Compress()
                    |> NodeId
            
            return {
                PaymentRequest.Amount = maybeAmount
                Prefix = prefix
                Timestamp = bolt11Data.Timestamp
                NodeId = nodeId
                Tags = bolt11Data.TaggedFields
                Signature = (sigCompact, recv) |> Some
            }
        }
    static member TryCreate(prefix: string, amount: LNMoney option, timestamp, nodeId, tags: TaggedFields, nodeSecret: Key) =
        let msgSigner  = { new IMessageSigner
                           with
                               member this.SignMessage(data) = nodeSecret.SignCompact(data, false) }
        PaymentRequest.TryCreate(prefix, amount, timestamp, nodeId, tags, msgSigner)
        
    /// signer must sign by node_secret which corresponds to node_id
    static member TryCreate (prefix: string, amount: LNMoney option, timestamp, nodeId, tags: TaggedFields, signer: IMessageSigner) =
        result {
            do! amount |> function None -> Ok() | Some a -> Result.requireTrue "amount must be larger than 0" (a > LNMoney.Zero)
            do! tags.CheckSanity()
            let r = {
                Prefix = prefix
                Amount = amount
                Timestamp = timestamp
                NodeId = nodeId
                Tags = tags
                Signature = None
            }
            let signature65bytes = signer.SignMessage (r.Hash)
            let recvId = signature65bytes.[0] - 27uy - 4uy
            let signature = signature65bytes |> fun s -> LNECDSASignature.FromBytesCompact(s, true)
            return { r with Signature = Some(signature, recvId) }
        }
