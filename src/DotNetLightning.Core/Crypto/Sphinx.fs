namespace DotNetLightning.Crypto
open System
open NBitcoin

open ResultUtils

open DotNetLightning.Utils
open DotNetLightning.Serialize
open DotNetLightning.Serialize.Msgs

module Sphinx =
    open NBitcoin.Crypto

    let private crypto = CryptoUtils.impl

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

    let private hex = NBitcoin.DataEncoders.HexEncoder()
    let private ascii = NBitcoin.DataEncoders.ASCIIEncoder()

    let private mac (key, msg) = Hashes.HMACSHA256(key, msg) |> uint256

    let private xor (a: byte[], b: byte[]) =
        Array.zip a b
        |> Array.map(fun (abyte, bbyte) -> (abyte ^^^ bbyte))

    let private generateKey (key, secret) =
        let kb = key |> ascii.DecodeData
        Hashes.HMACSHA256 (kb, secret)

    let private zeros (l) = Array.zeroCreate l

    let private generateStream (key, l) : byte[] =
        crypto.encryptWithoutAD(0UL, key, ReadOnlySpan(Array.zeroCreate l))

    let private computeSharedSecret = Secret.FromKeyPair

    let private computeBlindingFactor(pk: PubKey) (secret: Key) =
        [| pk.ToBytes(); secret.ToBytes() |]
        |> Array.concat
        |> Crypto.Hashes.SHA256
        |> Key

    let private blind (pk: PubKey) (secret: Key) =
        pk.GetSharedPubkey(secret)

    let private blindMulti (pk: PubKey) (secrets: Key seq) =
        Seq.fold (blind) pk secrets

    // computes ephemeral public keys and shared secretes for all nodes on the route
    let rec private computeEphemeralPublicKeysAndSharedSecretsCore
        (sessionKey: Key)
        (pubKeys: PubKey list)
        (ephemeralPubKeys: PubKey list)
        (blindingFactors: Key list)
        (sharedSecrets: Key list) =
        if (pubKeys.Length = 0) then
            (ephemeralPubKeys, sharedSecrets)
        else
            let ephemeralPubKey = blind (ephemeralPubKeys |> List.last) (blindingFactors |> List.last)
            let secret = computeSharedSecret (blindMulti (pubKeys.[0]) (blindingFactors), sessionKey) |> Key
            let blindingFactor = computeBlindingFactor(ephemeralPubKey) (secret)
            computeEphemeralPublicKeysAndSharedSecretsCore
                sessionKey (pubKeys |> List.tail)
                           (ephemeralPubKeys @ [ephemeralPubKey])
                           (blindingFactors @ [blindingFactor])
                           (sharedSecrets @ [secret])

    let rec internal computeEphemeralPublicKeysAndSharedSecrets(sessionKey: Key) (pubKeys: PubKey list) =
        let ephemeralPK0 = sessionKey.PubKey
        let secret0 = computeSharedSecret(pubKeys.[0], sessionKey) |> Key
        let blindingFactor0 = computeBlindingFactor(ephemeralPK0) (secret0)
        computeEphemeralPublicKeysAndSharedSecretsCore
            (sessionKey) (pubKeys |> List.tail) ([ephemeralPK0]) ([blindingFactor0]) ([secret0])

    let rec internal generateFiller (keyType: string) (sharedSecrets: Key list) (hopSize: int) (maxNumberOfHops: int option) =
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
    let parsePacket (nodePrivateKey: Key) (ad: byte[]) (rawPacket: byte[]): Result<ParsedPacket, CryptoError> =
        if (rawPacket.Length <> PACKET_LENGTH) then
             CryptoError.InvalidErrorPacketLength (PACKET_LENGTH, rawPacket.Length)
            |> Error
        else
            let packet = ILightningSerializable.fromBytes<OnionPacket>(rawPacket)
            if not (PubKey.Check(packet.PublicKey, true)) then
                InvalidPublicKey(packet.PublicKey) |> Error
            else
                let pk = packet.PublicKey |> PubKey
                let ss = computeSharedSecret(pk, nodePrivateKey)
                let mu = generateKey("mu", ss)
                let check =
                    let msg = Array.concat (seq [ packet.HopData; ad ])
                    mac(mu, msg)
                if check <> packet.HMAC then
                    CryptoError.BadMac |> Error
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
                      SharedSecret = ss } |> Ok

    /// Compute the next packet from the current packet and node parameters.
    /// Packets are constructed in reverse order:
    /// - you first build the last packet
    /// - then you call makeNextPacket(...) until you've build the final onion packet
    ///   that will be sent to the first node
    let internal makeNextPacket
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
            let nextRoutingInfo =
                let routingInfo1 = seq [ payload; packet.HMAC.ToBytes(); (packet.HopData |> Array.skipBack(PayloadLength + MacLength)) ]
                                   |> Array.concat
                let routingInfo2 =
                    let rho = generateKey("rho", sharedSecret)
                    let numHops = MaxHops * (PayloadLength + MacLength)
                    xor(routingInfo1, generateStream(rho, numHops))

                Array.append (routingInfo2 |> Array.skipBack filler.Length) filler
            
            let nextHmac = 
                let macKey = generateKey("mu", sharedSecret)
                let macMsg = (Array.append nextRoutingInfo ad)
                mac(macKey, macMsg)
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
            static member Create (sessionKey: Key, pubKeys: PubKey list, payloads: byte[] list, ad: byte[]) =
                let (ephemeralPubKeys, sharedSecrets) = computeEphemeralPublicKeysAndSharedSecrets (sessionKey) (pubKeys)
                let filler = generateFiller "rho" sharedSecrets.[0..sharedSecrets.Length - 2] (PayloadLength + MacLength) (Some MaxHops)

                let lastPacket = makeNextPacket(payloads |> List.last,
                                                ad,
                                                ephemeralPubKeys |> List.last,
                                                (sharedSecrets |> List.last |> fun ss -> ss.ToBytes()),
                                                OnionPacket.LastPacket,
                                                Some(filler))
                let rec loop (hopPayloads: byte[] list, ephKeys: PubKey list, ss: Key list, packet: OnionPacket) =
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

    let [<Literal>] MAX_ERROR_PAYLOAD_LENGTH = 256
    let [<Literal>] ERROR_PACKET_LENGTH = 292 // MacLength + MAX_ERROR_PAYLOAD_LENGTH + 2 + 2

    let forwardErrorPacket (packet: byte[], ss: byte[]) =
        assert(packet.Length = ERROR_PACKET_LENGTH)
        let k = generateKey("ammag", ss)
        let s = generateStream(k, ERROR_PACKET_LENGTH)
        xor(packet, s)

    let private checkMac(ss: byte[], packet: byte[]): bool =
        let (macV, payload) = packet |> Array.splitAt(MacLength)
        let um = generateKey("um", ss)
        (macV |> uint256) = mac(um, payload)

    let private extractFailureMessage (packet: byte[]) =
        if (packet.Length <> ERROR_PACKET_LENGTH) then
            InvalidErrorPacketLength(ERROR_PACKET_LENGTH, packet.Length)
            |> Error
        else
            let (mac, payload) = packet |> Array.splitAt(MacLength)
            let len = Utils.ToUInt16(payload.[0..1], false) |> int
            if (len < 0 || (len > MAX_ERROR_PAYLOAD_LENGTH)) then
                InvalidMessageLength len
                |> Error
            else
                let msg = payload.[2..2 + len - 1]
                ILightningSerializable.fromBytes<FailureMsg>(msg) |> Ok
    type ErrorPacket = {
        OriginNode: NodeId
        FailureMsg: FailureMsg
    }
        with
            static member Create (ss: byte[], msg: FailureMsg) =
                let msgB = msg.ToBytes()
                assert (msgB.Length <= MAX_ERROR_PAYLOAD_LENGTH)
                let um = generateKey("um", ss)
                let padLen = MAX_ERROR_PAYLOAD_LENGTH - msgB.Length
                let payload =
                    use ms = new System.IO.MemoryStream()
                    use st = new LightningWriterStream(ms)
                    st.Write(uint16 msgB.Length, false)
                    st.Write(msgB)
                    st.Write(uint16 padLen, false)
                    st.Write(zeros padLen)
                    ms.ToArray()
                forwardErrorPacket(Array.append (mac(um, payload).ToBytes()) payload, ss)

            static member Parse(packet: byte[], ss: (Key * PubKey) list) =
                let ssB = ss |> List.map(fun (k, pk) -> (k.ToBytes(), pk))
                ErrorPacket.Parse(packet, ssB)

            static member Parse(packet: byte[], ss: (byte[] * PubKey) list): Result<ErrorPacket, CryptoError> =
                if (packet.Length <> ERROR_PACKET_LENGTH) then
                    InvalidErrorPacketLength (ERROR_PACKET_LENGTH, packet.Length) |> Error
                else
                    let rec loop (packet: byte[], ss: (byte[] * PubKey) list) =
                        match ss with
                        | [] ->
                            FailedToParseErrorPacket (packet, ss)
                            |> Error
                        | (secret, pk)::tail ->
                            let packet1 = forwardErrorPacket(packet, secret)
                            if ((checkMac(secret, packet1))) then
                                extractFailureMessage packet1
                                >>= fun msg ->
                                        { OriginNode = pk |> NodeId
                                          FailureMsg = msg }
                                        |> Ok
                            else
                                loop (packet1, tail)
                    loop(packet, ss)
