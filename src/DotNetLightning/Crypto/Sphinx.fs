namespace DotNetLightning.Crypto
open System
open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Utils.NBitcoinExtensions
open DotNetLightning.Serialize.Msgs

type SharedSecret = uint256

module internal Sphinx =
    open NBitcoin.Crypto

    [<Literal>]
    let VERSION = 0uy

    [<Literal>]
    let PayloadLength = 33

    [<Literal>]
    let MacLength = 32

    [<Literal>]
    let MaxHops = 20

    [<Literal>]
    let PACKET_LENGTH =  1366 // 1 + 33 + MacLength + MaxHops * (PayloadLength + MacLength)

    let chacha20 = NSec.Cryptography.ChaCha20Poly1305.ChaCha20Poly1305
    let hex = NBitcoin.DataEncoders.HexEncoder()
    let ascii = NBitcoin.DataEncoders.ASCIIEncoder()

    let mac (key, msg) = Hashes.HMACSHA256(key, msg).[0..MacLength - 1] |> uint256

    let xor (a: byte[], b: byte[]) =
        Array.zip a b
        |> Array.map(fun (abyte, bbyte) -> (abyte ^^^ bbyte))

    let generateKey (key, secret) =
        let kb = key |> ascii.DecodeData
        Hashes.HMACSHA256 (kb, secret)

    let zeros (l) = Array.zeroCreate l

    let generateStream (key, l) =
        CryptoUtils.encryptWithoutADAndMac(0UL, key, ReadOnlySpan(Array.zeroCreate l))

    let computeSharedSecret = CryptoUtils.SharedSecret.FromKeyPair

    let computeBlindingFactor(pk: PubKey) (secret: Key) =
        [| pk.ToBytes(); secret.ToBytes() |]
        |> Array.concat
        |> Crypto.Hashes.SHA256
        |> Key

    let blind (pk: PubKey) (secret: Key) =
        pk.GetSharedPubkey(secret)

    let blindMulti (pk: PubKey) (secrets: Key seq) =
        Seq.fold (blind) pk secrets

    // computes ephemeral public keys and shared secretes for all nodes on the route
    let rec computeEphemeralPublicKeysAndSharedSecretsCore
        (sessionKey: Key)
        (pubKeys: PubKey list)
        (ephemeralPubKeys: PubKey list)
        (blindingFactors: Key list)
        (sharedSecrets: Key list) =
        if (pubKeys.Length = 0) then
            (ephemeralPubKeys, sharedSecrets)
        else
            // printfn "inside loop ----------\n\n"
            let ephemeralPubKey = blind (ephemeralPubKeys |> List.last) (blindingFactors |> List.last)
            let secret = computeSharedSecret (blindMulti (pubKeys.[0]) (blindingFactors), sessionKey) |> Key
            let blindingFactor = computeBlindingFactor(ephemeralPubKey) (secret)
            // printfn "ephemeral pubkey is %A" (ephemeralPubKey)
            // printfn "secret is %O" (secret.ToBytes() |> hex.EncodeData)
            // printfn "blinding factor is %O" (blindingFactor.ToBytes() |> hex.EncodeData)
            // printfn "finish ----------\n\n"
            computeEphemeralPublicKeysAndSharedSecretsCore
                sessionKey (pubKeys |> List.tail)
                           (ephemeralPubKeys @ [ephemeralPubKey])
                           (blindingFactors @ [blindingFactor])
                           (sharedSecrets @ [secret])

    let rec computeEphemeralPublicKeysAndSharedSecrets(sessionKey: Key) (pubKeys: PubKey list) =
        let ephemeralPK0 = sessionKey.PubKey
        let secret0 = computeSharedSecret(pubKeys.[0], sessionKey) |> Key
        let blindingFactor0 = computeBlindingFactor(ephemeralPK0) (secret0)
        // printfn "first ephemeral pubkey is %A" (ephemeralPK0)
        // printfn "first secret is %O" (secret0.ToBytes() |> hex.EncodeData)
        // printfn "first blinding factor is %O" (blindingFactor0.ToBytes() |> hex.EncodeData)
        computeEphemeralPublicKeysAndSharedSecretsCore
            (sessionKey) (pubKeys |> List.tail) ([ephemeralPK0]) ([blindingFactor0]) ([secret0])

    let rec generateFiller (keyType: string) (sharedSecrets: Key list) (hopSize: int) (maxNumberOfHops: int option) =
        let maxHopN = defaultArg maxNumberOfHops MaxHops
        sharedSecrets
        |> List.fold (fun (padding: byte[]) (secret: Key) ->
            let key = generateKey(keyType, secret.ToBytes())
            let padding1 = Array.append padding (zeros hopSize)
            let stream =
                let s = generateStream(key, hopSize * (maxHopN + 1))
                s.[s.Length - padding1.Length .. s.Length - 1] // take padding1 from tale
            assert (stream.Length = padding1.Length)
            xor(padding1, stream)
            ) [||]

    type ParsedPacket = {
        Payload: byte[]
        NextPacket: OnionPacket
        SharedSecret: byte[]
    }
    let parsePacket (nodePrivateKey: Key) (ad: byte[]) (rawPacket: byte[]): RResult<ParsedPacket> =
        if (rawPacket.Length <> PACKET_LENGTH) then
            sprintf "onion packet length is %d. but it must be %d" rawPacket.Length PACKET_LENGTH
            |> RResult.rmsg
        else
            let packet = OnionPacket.Init().FromBytes(rawPacket)
            if not (PubKey.Check(packet.PublicKey, true)) then
                RResult.rmsg "Invalid Public Key from the node"
            else
                let pk = packet.PublicKey |> PubKey
                let ss = computeSharedSecret(pk, nodePrivateKey)
                let mu = generateKey("mu", ss)
                let check =
                    let msg = Array.concat (seq [ packet.HopData; ad ])
                    mac(mu, msg)
                if check <> packet.HMAC then
                    "Invalid header MAC"
                    |> RResult.rmsg
                else
                    let rho = generateKey("rho", ss)
                    let bin =
                        let d = Array.concat (seq [packet.HopData; zeros(PayloadLength + MacLength)])
                        let dataLength = PayloadLength + MacLength + MaxHops * (PayloadLength + MacLength)
                        xor(d, generateStream(rho, dataLength))

                    let payload = bin.[0..PayloadLength - 1]
                    let hmac = bin.[PayloadLength .. PayloadLength + MacLength - 1] |> uint256
                    let nextRouteInfo = bin.[PayloadLength + MacLength..]
                    let nextPubKey = blind(pk) (computeBlindingFactor(pk) (Key ss))
                    { ParsedPacket.Payload = payload
                      NextPacket = { Version = VERSION; PublicKey = nextPubKey.ToBytes(); HMAC= hmac; HopData = nextRouteInfo }
                      SharedSecret = ss } |> Good

    /// Compute the next packet from the current packet and node parameters.
    /// Packets are constructed in reverse order:
    /// - you first build the last packet
    /// - then you call makeNextPacket(...) until you've build the final onion packet
    ///   that will be sent to the first node
    let makeNextPacket
        (payload: byte[],
         ad: byte[],
         ephemeralPubKey: PubKey,
         sharedSecret: byte[],
         packet: OnionPacket,
         routingInfoFiller: byte[] option) =
        if (payload.Length <> PayloadLength) then
            failwithf "Payload length is not %A" PayloadLength
        else
            let filler = defaultArg routingInfoFiller ([||])
            // printfn "makeNextPacket----\n\n"
            let nextRoutingInfo =
                let routingInfo1 = seq [ payload; packet.HMAC.ToBytes(); (packet.HopData |> Array.skipBack(PayloadLength + MacLength)) ]
                                   |> Array.concat
                let routingInfo2 =
                    let rho = generateKey("rho", sharedSecret)
                    let numHops = MaxHops * (PayloadLength + MacLength)
                    xor(routingInfo1, generateStream(rho, numHops))

                // printfn "extRoutig ifo 1 is %A" routingInfo1 
                // printfn "extRoutig ifo 2 is %A" routingInfo2 

                Array.append (routingInfo2 |> Array.skipBack filler.Length) filler
            
            let nextHmac = mac(generateKey("mu", sharedSecret), (Array.append nextRoutingInfo ad))
            // printfn "nextRoutingInfo is %A" nextRoutingInfo
            // printfn "nextHMAC is is %A" nextHmac
            // printfn "end----\n\n"
            let nextPacket ={ OnionPacket.Version = VERSION
                              PublicKey = ephemeralPubKey.ToBytes()
                              HopData = nextRoutingInfo
                              HMAC = nextHmac }
            nextPacket

    type PacketAndSecrets = {
        Packet: OnionPacket
        /// Shared secrets (one per node in the route). Known (and needed) only if you're creating the
        /// packet. Empty if you're just forwarding the packet to the next node
        SharedSecrets: (Key * PubKey) list
    }
        with
            static member Create (log: Logger) (sessionKey: Key, pubKeys: PubKey list, payloads: byte[] list, ad: byte[]) =
                let (ephemeralPubKeys, sharedSecrets) = computeEphemeralPublicKeysAndSharedSecrets (sessionKey) (pubKeys)
                let debug = log LogLevel.Debug
                (sprintf "ephemeral Pubkeys are %A" ephemeralPubKeys) |> debug
                (sprintf "shared secrets are %A"  (sharedSecrets |> Seq.map(fun s -> s.ToBytes() |> hex.EncodeData))) |> debug
                let filler = generateFiller "rho" sharedSecrets.[0..sharedSecrets.Length - 2] (PayloadLength + MacLength) (Some MaxHops)
                (sprintf "filler is %A" filler) |> debug

                let lastPacket = makeNextPacket(payloads |> List.last,
                                                ad,
                                                ephemeralPubKeys |> List.last,
                                                (sharedSecrets |> List.last |> fun ss -> ss.ToBytes()),
                                                OnionPacket.LastPacket,
                                                Some(filler))
                let rec loop (hopPayloads: byte[] list, ephKeys: PubKey list, ss: Key list, packet: OnionPacket) =
                    // printfn "loop: packet is %A" packet
                    // printfn "loop: hoppayloads are is %A" hopPayloads
                    if (hopPayloads.IsEmpty) then
                        packet
                    else
                        let nextPacket = makeNextPacket(hopPayloads |> List.last,
                                                        ad,
                                                        ephKeys |> List.last,
                                                        (ss |> List.last |> fun (s: Key) -> s.ToBytes()),
                                                        packet,
                                                        None)
                        loop (hopPayloads.[0..hopPayloads.Length - 2], ephKeys.[0..ephKeys.Length - 2], ss.[0..ss.Length - 2], nextPacket)
                let p = loop (payloads.[0..payloads.Length - 2], ephemeralPubKeys.[0..ephemeralPubKeys.Length - 2], sharedSecrets.[0..sharedSecrets.Length - 2], lastPacket)
                { PacketAndSecrets.Packet = p; SharedSecrets = List.zip sharedSecrets pubKeys }


type ErrorPacket = {
    OriginNode: NodeId
    FailureMsg: string
}