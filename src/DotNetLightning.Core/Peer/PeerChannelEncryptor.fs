namespace DotNetLightning.Peer

open System

open NBitcoin
open NBitcoin.Crypto

open DotNetLightning.Utils
open DotNetLightning.Crypto

open ResultUtils
open ResultUtils.Portability

[<AutoOpen>]
module PeerChannelEncryptor =
    let crypto = CryptoUtils.impl

    type BitConverter with

        static member GetBytesBE(data: uint16) =
            let buf = Array.zeroCreate 2
            buf.[0] <- (byte(data >>> 8))
            buf.[1] <- byte data
            buf

        static member ToUint16BE(data: array<byte>) =
            ((uint16 data.[0]) <<< 8 ||| uint16(data.[1]))

    // Sha256("Noise_XK_secp256k1_ChaChaPoly_SHA256")
    let NOISE_CK =
        [|
            0x26uy
            0x40uy
            0xf5uy
            0x2euy
            0xebuy
            0xcduy
            0x9euy
            0x88uy
            0x29uy
            0x58uy
            0x95uy
            0x1cuy
            0x79uy
            0x42uy
            0x50uy
            0xeeuy
            0xdbuy
            0x28uy
            0x00uy
            0x2cuy
            0x05uy
            0xd7uy
            0xdcuy
            0x2euy
            0xa0uy
            0xf1uy
            0x95uy
            0x40uy
            0x60uy
            0x42uy
            0xcauy
            0xf1uy
        |]

    // Sha256(NOISE_CK || "lightning")
    let NOISE_H =
        [|
            0xd1uy
            0xfbuy
            0xf6uy
            0xdeuy
            0xe4uy
            0xf6uy
            0x86uy
            0xf1uy
            0x32uy
            0xfduy
            0x70uy
            0x2cuy
            0x4auy
            0xbfuy
            0x8fuy
            0xbauy
            0x4buy
            0xb4uy
            0x20uy
            0xd8uy
            0x9duy
            0x2auy
            0x04uy
            0x8auy
            0x3cuy
            0x4fuy
            0x4cuy
            0x09uy
            0x2euy
            0x37uy
            0xb6uy
            0x76uy
        |]

    type NextNoiseStep =
        | ActOne
        | ActTwo
        | ActThree
        | NoiseComplete

    type NoiseStep =
        | PreActOne
        | PostActOne
        | PostActTwo

    type BidirectionalNoiseState =
        {
            /// The handshake hash. This value is the accumulated hash of all handhsake data that has been sent
            /// and received so far during the handshake process.
            H: uint256
            /// The chaining key. This value is the accumulated hash of all previous ECDH outputs.
            /// At the end of the handshake, it is used for deriving the encryption keys for Lightning Messages.
            CK: uint256
        }

    type DirectionalNoisestate =
        | OutBound of OutBound
        | InBound of InBound

    and OutBound =
        {
            IE: Key
        }

    and InBound =
        {
            IE: option<PubKey> // Some if state >= PostActOne
            RE: option<Key> // Some if state >= PostActTwo
            TempK2: option<uint256> // Some if state >= PostActTwo
        }

    type NoiseState =
        | InProgress of InProgressNoiseState
        | Finished of FinishedNoiseState

    and InProgressNoiseState =
        {
            State: NoiseStep
            DirectionalState: DirectionalNoisestate
            BidirectionalState: BidirectionalNoiseState
        }

    and FinishedNoiseState =
        {
            /// Sending key
            SK: uint256
            /// Sending nonce
            SN: uint64
            SCK: uint256
            /// Reading key
            RK: uint256
            /// Reading nonce
            RN: uint64
            RCK: uint256
        }

    type PeerChannelEncryptor =
        internal
            {
                TheirNodeId: option<NodeId>
                NoiseState: NoiseState
            }

        member this.IsReadyForEncryption() =
            match this.NoiseState with
            | Finished _ -> true
            | _ -> false

        member this.GetNoiseStep() =
            match this.NoiseState with
            | InProgress d ->
                match d.State with
                | NoiseStep.PreActOne -> NextNoiseStep.ActOne
                | NoiseStep.PostActOne -> NextNoiseStep.ActTwo
                | NoiseStep.PostActTwo -> NextNoiseStep.ActThree
            | Finished _ -> NextNoiseStep.NoiseComplete


    module PeerChannelEncryptor =
        let newOutBound(NodeId theirNodeId, ourNodeSecret: Key) =
            let hashInput = Array.concat [| NOISE_H; theirNodeId.ToBytes() |]
            let h = uint256(Hashes.SHA256(hashInput))

            {
                TheirNodeId = Some(NodeId theirNodeId)
                NoiseState =
                    InProgress
                        {
                            State = PreActOne
                            DirectionalState =
                                OutBound(
                                    {
                                        IE = ourNodeSecret
                                    }
                                )
                            BidirectionalState =
                                {
                                    H = h
                                    CK = uint256(NOISE_CK)
                                }
                        }
            }

        let newInBound(ourNodeSecret: Key) =
            let hashInput =
                Array.concat
                    [|
                        NOISE_H
                        ourNodeSecret.PubKey.ToBytes()
                    |]

            let h = uint256(Hashes.SHA256(hashInput))

            {
                TheirNodeId = None
                NoiseState =
                    InProgress
                        {
                            State = PreActOne
                            DirectionalState =
                                InBound
                                    {
                                        IE = None
                                        RE = None
                                        TempK2 = None
                                    }
                            BidirectionalState =
                                {
                                    H = h
                                    CK = uint256(NOISE_CK)
                                }
                        }
            }

        let internal decryptWithAD
            (
                n: uint64,
                key: uint256,
                ad: array<byte>,
                cipherText: ReadOnlySpan<byte>
            ) : Result<array<byte>, _> =
            crypto.decryptWithAD(n, key, ad, cipherText)
            |> Result.mapError(PeerError.CryptoError)

        let internal hkdfExtractExpand(salt: array<byte>, ikm: array<byte>) =
            let prk = Hashes.HMACSHA256(salt, ikm)
            let t1 = Hashes.HMACSHA256(prk, [| 1uy |])
            let t2 = Hashes.HMACSHA256(prk, Array.append t1 [| 2uy |])
            (t1, t2)

        let private hkdf
            (sharedSecret: array<byte>)
            (state: PeerChannelEncryptor)
            =
            match state.NoiseState with
            | InProgress inProgressState ->
                let (t1, t2) =
                    let ck = inProgressState.BidirectionalState.CK
                    hkdfExtractExpand(ck.ToBytes(), sharedSecret)

                uint256 t2,
                { state with
                    NoiseState =
                        InProgress
                            { inProgressState with
                                BidirectionalState =
                                    { inProgressState.BidirectionalState with
                                        CK = uint256 t1
                                    }
                            }
                }
            | _ -> failwith "Invalid state when calculating hdkf"

        let private updateHWith
            (state: PeerChannelEncryptor)
            (item: array<byte>)
            =
            match state.NoiseState with
            | InProgress inProgressNoiseState ->
                { state with
                    NoiseState =
                        InProgress
                            { inProgressNoiseState with
                                BidirectionalState =
                                    { inProgressNoiseState.BidirectionalState with
                                        H =
                                            [|
                                                inProgressNoiseState.BidirectionalState.H.ToBytes
                                                    ()
                                                item
                                            |]
                                            |> Array.concat
                                            |> Hashes.SHA256
                                            |> uint256
                                    }
                            }
                }
            | _ -> failwith "Invalid state when updating H"

        let private outBoundNoiseAct
            (
                state: PeerChannelEncryptor,
                ourKey: Key,
                theirKey: PubKey
            ) =
            let ourPub = ourKey.PubKey

            let tempK, s3 =
                let s2 = updateHWith state (ourPub.ToBytes())
                let ss = Secret.FromKeyPair(theirKey, ourKey)
                hkdf ss s2

            let c =
                let h =
                    match s3.NoiseState with
                    | InProgress {
                                     BidirectionalState = bState
                                 } -> Some bState.H
                    | _ -> None

                crypto.encryptWithAD(
                    0UL,
                    tempK,
                    ReadOnlySpan(h.Value.ToBytes()),
                    ReadOnlySpan([||])
                )

            let resultToSend =
                Array.concat(seq [ [| 0uy |]; ourPub.ToBytes(); c ])

            let newState = updateHWith s3 c
            (resultToSend, tempK), newState

        let private inBoundNoiseAct
            (
                state: PeerChannelEncryptor,
                act: array<byte>,
                ourKey: Key
            ) : Result<(PubKey * uint256) * PeerChannelEncryptor, PeerError> =
            if (act.Length <> 50) then
                raise
                <| ArgumentException(
                    sprintf "Invalid act length: %i" (act.Length)
                )

            if (act.[0] <> 0uy) then
                Error(UnknownHandshakeVersionNumber(act.[0]))
            else
                let publicKeyBytes = act.[1..33]

                match PubKey.TryCreatePubKey publicKeyBytes with
                | false, _ ->
                    Error(
                        PeerError.CryptoError(InvalidPublicKey(publicKeyBytes))
                    )
                | true, theirPub ->
                    let tempK, s3 =
                        let ss = Secret.FromKeyPair(theirPub, ourKey)
                        let s2 = updateHWith state (theirPub.ToBytes())
                        hkdf ss s2

                    let currentH =
                        match s3.NoiseState with
                        | InProgress {
                                         BidirectionalState = bState
                                     } -> Some bState.H
                        | _ -> None

                    result {
                        let! _ =
                            decryptWithAD(
                                0UL,
                                tempK,
                                currentH.Value.ToBytes(),
                                ReadOnlySpan(act.[34..])
                            )

                        let s4 = updateHWith s3 act.[34..]
                        return ((theirPub, tempK), s4)
                    }

        let getActOne
            (pce: PeerChannelEncryptor)
            : array<byte> * PeerChannelEncryptor =
            match pce.NoiseState with
            | InProgress {
                             State = state
                             DirectionalState = dState
                         } ->
                match dState with
                | OutBound {
                               IE = ie
                           } ->
                    if state <> PreActOne then
                        failwith "Requested act at wrong step"
                    else
                        outBoundNoiseAct(pce, ie, pce.TheirNodeId.Value.Value)
                        |> fun ((res, _), pce2) ->
                            match pce2.NoiseState with
                            | InProgress inProgressNoiseState ->
                                res,
                                { pce2 with
                                    NoiseState =
                                        InProgress
                                            { inProgressNoiseState with
                                                State = NoiseStep.PostActOne
                                            }

                                }
                            | _ -> failwith "Invalid state for updating state"
                | _ -> failwith "Wrong Direction for Act"
            | _ ->
                failwith "Cannot get act one after noise  handshake completes"

        let processActOneWithEphemeralKey
            (actOne: array<byte>)
            (ourNodeSecret: Key)
            (ourEphemeral: Key)
            (pce: PeerChannelEncryptor)
            : Result<array<byte> * _, PeerError> =
            match pce.NoiseState with
            | InProgress {
                             State = state
                             DirectionalState = dState
                         } ->
                match dState with
                | InBound _ ->
                    if state <> PreActOne then
                        failwith "Requested act at wrong step"

                    inBoundNoiseAct(pce, actOne, ourNodeSecret)
                    >>= fun res ->
                            let (theirPub, _), pce2 = res

                            let pce3 =
                                match pce2.NoiseState with
                                | InProgress inProgressState ->
                                    match inProgressState.DirectionalState with
                                    | InBound inBoundState ->
                                        { pce2 with
                                            NoiseState =
                                                InProgress
                                                    { inProgressState with
                                                        DirectionalState =
                                                            InBound
                                                                { inBoundState with
                                                                    IE =
                                                                        Some
                                                                            theirPub
                                                                    RE =
                                                                        Some
                                                                            ourEphemeral
                                                                }
                                                    }
                                        }
                                    | _ -> failwith "Invalid directional state"
                                | _ -> failwith "Invalid noise state"

                            let (res, tempK), pce4 =
                                outBoundNoiseAct(pce3, ourEphemeral, theirPub)

                            let pce5 =
                                match pce4.NoiseState with
                                | InProgress inProgressState ->
                                    match inProgressState.DirectionalState with
                                    | InBound inBoundState ->
                                        { pce4 with
                                            NoiseState =
                                                InProgress
                                                    { inProgressState with
                                                        DirectionalState =
                                                            InBound
                                                                { inBoundState with
                                                                    TempK2 =
                                                                        Some
                                                                            tempK
                                                                }
                                                        State =
                                                            NoiseStep.PostActTwo
                                                    }
                                        }
                                    | _ -> failwith "Invalid directional state"
                                | _ -> failwith "Invalid noise state"

                            Ok(res, pce5)
                | _ -> failwith "Requested act at wrong step"
            | _ -> failwith "Cannot get acg one after noise handshake completes"

        let processActOneWithKey
            (actOne: array<byte>)
            (ourNodeSecret: Key)
            (pce: PeerChannelEncryptor)
            : Result<array<byte> * _, PeerError> =
            if (actOne.Length <> 50) then
                raise
                <| ArgumentException(
                    sprintf "invalid actOne length: %i" (actOne.Length)
                )

            let ephemeralKey = new Key()
            processActOneWithEphemeralKey actOne ourNodeSecret ephemeralKey pce

        let processActTwo
            (actTwo: array<byte>)
            (ourNodeSecret: Key)
            (pce: PeerChannelEncryptor)
            : Result<(array<byte> * NodeId) * PeerChannelEncryptor, PeerError> =
            match pce.NoiseState with
            | InProgress {
                             State = state
                             DirectionalState = dState
                         } ->
                match dState with
                | OutBound {
                               IE = ie
                           } ->
                    if state <> NoiseStep.PostActOne then
                        failwith "Requested act at wrong step"

                    inBoundNoiseAct(pce, actTwo, ie)
                    >>= fun ((re, tempK2), pce2) ->
                            let ourNodeId = ourNodeSecret.PubKey

                            let encryptedRes1 =
                                let currentH =
                                    match pce2.NoiseState with
                                    | InProgress {
                                                     BidirectionalState = bState
                                                 } -> Some bState.H
                                    | _ -> None

                                crypto.encryptWithAD(
                                    1UL,
                                    tempK2,
                                    ReadOnlySpan(currentH.Value.ToBytes()),
                                    ReadOnlySpan(ourNodeId.ToBytes())
                                )

                            let pce3 = updateHWith pce2 encryptedRes1

                            let (tempK, pce4) =
                                let ss = Secret.FromKeyPair(re, ourNodeSecret)
                                hkdf ss pce3

                            let encryptedRes2 =
                                let currentH =
                                    match pce4.NoiseState with
                                    | InProgress {
                                                     BidirectionalState = bState
                                                 } -> Some bState.H
                                    | _ -> None

                                crypto.encryptWithAD(
                                    0UL,
                                    tempK,
                                    ReadOnlySpan(currentH.Value.ToBytes()),
                                    ReadOnlySpan([||])
                                )

                            let (sk, rk) =
                                let currentCK =
                                    match pce4.NoiseState with
                                    | InProgress {
                                                     BidirectionalState = bState
                                                 } -> Some bState.CK
                                    | _ -> None

                                hkdfExtractExpand(
                                    currentCK.Value.ToBytes(),
                                    [||]
                                )

                            let newPce =
                                let ck =
                                    match pce4.NoiseState with
                                    | InProgress {
                                                     BidirectionalState = bState
                                                 } -> Some bState.CK
                                    | _ -> None

                                { pce4 with
                                    NoiseState =
                                        Finished
                                            {
                                                FinishedNoiseState.SK =
                                                    uint256 sk
                                                SN = 0UL
                                                SCK = ck.Value
                                                RK = uint256 rk
                                                RN = 0UL
                                                RCK = ck.Value
                                            }
                                }

                            let resultToSend =
                                Array.concat(
                                    seq
                                        [
                                            [| 0uy |]
                                            encryptedRes1
                                            encryptedRes2
                                        ]
                                )

                            Ok((resultToSend, newPce.TheirNodeId.Value), newPce)
                | _ -> failwith "Wrong direction with act"
            | _ -> failwith "Cannot get act one after noise handshake completes"

        let processActThree
            (actThree: array<byte>)
            (pce: PeerChannelEncryptor)
            : Result<NodeId * PeerChannelEncryptor, PeerError> =
            if (actThree.Length <> 66) then
                raise
                <| ArgumentException(
                    sprintf
                        "actThree must be 66 bytes, but it was %i"
                        (actThree.Length)
                )

            match pce.NoiseState with
            | InProgress {
                             State = state
                             DirectionalState = dState
                         } ->
                match dState with
                | InBound {
                              IE = _
                              RE = re
                              TempK2 = tempk2
                          } ->
                    if (state <> PostActTwo) then
                        failwith "Requested act at wrong step"
                    else if (actThree.[0] <> 0uy) then
                        Error(UnknownHandshakeVersionNumber(actThree.[0]))
                    else
                        let h =
                            match pce.NoiseState with
                            | InProgress {
                                             BidirectionalState = bState
                                         } -> Some bState.H
                            | _ -> None

                        decryptWithAD(
                            1UL,
                            tempk2.Value,
                            h.Value.ToBytes(),
                            ReadOnlySpan(actThree.[1..49])
                        )
                        >>= fun theirNodeId ->
                                match PubKey.TryCreatePubKey theirNodeId with
                                | false, _ ->
                                    Error(
                                        PeerError.CryptoError(
                                            CryptoError.InvalidPublicKey(
                                                theirNodeId
                                            )
                                        )
                                    )
                                | true, theirNodeIdPubKey ->
                                    let tempK, pce3 =
                                        let pce2 =
                                            let pceWithUpdatedH =
                                                pce
                                                |> fun p ->
                                                    updateHWith
                                                        p
                                                        (actThree.[1..49])

                                            { pceWithUpdatedH with
                                                TheirNodeId =
                                                    theirNodeIdPubKey
                                                    |> NodeId
                                                    |> Some
                                            }

                                        let ss =
                                            Secret.FromKeyPair(
                                                pce2.TheirNodeId.Value.Value,
                                                re.Value
                                            )

                                        hkdf ss pce2

                                    let h =
                                        match pce3.NoiseState with
                                        | InProgress {
                                                         BidirectionalState = bState
                                                     } -> Some bState.H
                                        | _ -> None

                                    decryptWithAD(
                                        0UL,
                                        tempK,
                                        h.Value.ToBytes(),
                                        ReadOnlySpan(actThree.[50..])
                                    )
                                    >>= fun _ ->
                                            let ck =
                                                match pce3.NoiseState with
                                                | InProgress {
                                                                 BidirectionalState = bState
                                                             } -> bState.CK
                                                | _ ->
                                                    failwith
                                                        "Invalid state when reading CK"

                                            let (rk, sk) =
                                                hkdfExtractExpand(
                                                    ck.ToBytes(),
                                                    [||]
                                                )

                                            let newPce =
                                                { pce3 with
                                                    NoiseState =
                                                        Finished
                                                            {
                                                                FinishedNoiseState.SK =
                                                                    sk
                                                                    |> uint256
                                                                SN = 0UL
                                                                SCK = ck
                                                                RK =
                                                                    rk
                                                                    |> uint256
                                                                RN = 0UL
                                                                RCK = ck
                                                            }
                                                }

                                            Ok(newPce.TheirNodeId.Value, newPce)
                | _ -> failwith "wrong direction for act"
            | _ -> failwith "Cannot get act one after noise handshake completes"

        let private encryptCore (msg: array<byte>) pceLocal =
            let sn, sk =
                match pceLocal.NoiseState with
                | Finished {
                               SN = sn
                               SK = sk
                           } -> sn, sk
                | _ -> failwith "Invalid state when reading SK"

            let lengthBytes = BitConverter.GetBytesBE(uint16 msg.Length)
            // 2 byte length + 16 bytes MAC
            let cipherLength =
                crypto.encryptWithAD(
                    sn,
                    sk,
                    ReadOnlySpan([||]),
                    ReadOnlySpan(lengthBytes)
                )

            let newSN = (sn + 1UL)

            let pce3 =
                match pceLocal.NoiseState with
                | Finished finishedNoiseState ->
                    { pceLocal with
                        NoiseState =
                            Finished
                                { finishedNoiseState with
                                    SN = newSN
                                }
                    }
                | _ -> failwith "Invalid state when incrementing SN"

            let cipherText =
                crypto.encryptWithAD(
                    newSN,
                    sk,
                    ReadOnlySpan [||],
                    ReadOnlySpan(msg)
                )

            let pce4 =
                match pce3.NoiseState with
                | Finished finishedNoiseState ->
                    { pce3 with
                        NoiseState =
                            Finished
                                { finishedNoiseState with
                                    SN = newSN + 1UL
                                }
                    }
                | _ -> failwith "Invalid state when incrementing SN"

            Array.concat(seq [ cipherLength; cipherText ]), pce4

        let encryptMessage
            (msg: array<byte>)
            (pce: PeerChannelEncryptor)
            : array<byte> * PeerChannelEncryptor =
            if msg.Length > 65535 then
                failwith "Attempted encrypt message longer thaan 65535 bytes"

            match pce.NoiseState with
            | Finished {
                           SK = sk
                           SN = sn
                           SCK = sck
                       } ->
                let refreshState(pceLocal: PeerChannelEncryptor) =
                    let (newSCK, newSK) =
                        hkdfExtractExpand(sck.ToBytes(), sk.ToBytes())

                    match pceLocal.NoiseState with
                    | Finished finishedNoiseState ->
                        { pceLocal with
                            NoiseState =
                                Finished
                                    { finishedNoiseState with
                                        SCK = uint256 newSCK
                                        SK = uint256 newSK
                                        SN = 0UL
                                    }
                        }
                    | _ -> failwith "Invalid state when encrypting"

                if sn >= 1000UL then
                    let newState = refreshState(pce)
                    encryptCore (msg) newState
                else
                    encryptCore (msg) pce
            | _ ->
                failwith
                    "Tried to encrypt a message prior to noise handshake completion"

        let private decryptCore msg localPce =
            let newRK =
                match localPce.NoiseState with
                | Finished {
                               RK = rk
                           } -> rk
                | _ -> failwith "Invalid state when reading RK"

            let newRN =
                match localPce.NoiseState with
                | Finished {
                               RN = rn
                           } -> rn
                | _ -> failwith "Invalid state when reading RN"

            decryptWithAD(newRN, newRK, [||], ReadOnlySpan(msg))
            >>= fun plainText ->
                    let newPCE =
                        match localPce.NoiseState with
                        | Finished finishedNoiseState ->
                            { localPce with
                                NoiseState =
                                    Finished
                                        { finishedNoiseState with
                                            RN = finishedNoiseState.RN + 1UL
                                        }
                            }
                        | _ -> failwith "Invalid state when incrementing RN"

                    (BitConverter.ToUint16BE(plainText), newPCE) |> Ok

        let decryptLengthHeader
            (msg: array<byte>)
            pce
            : Result<uint16 * PeerChannelEncryptor, PeerError> =
            if (msg.Length <> 16 + 2) then
                raise
                <| ArgumentException(
                    sprintf "Invalid length of message %i" msg.Length
                )

            match pce.NoiseState with
            | Finished {
                           RK = rk
                           RN = rn
                           RCK = rck
                       } ->
                let refreshState localPce =
                    let (newRCK, newRK) =
                        hkdfExtractExpand(rck.ToBytes(), rk.ToBytes())

                    match localPce.NoiseState with
                    | Finished finishedNoiseState ->
                        { localPce with
                            NoiseState =
                                Finished
                                    { finishedNoiseState with
                                        RCK = uint256 newRCK
                                        RK = uint256 newRK
                                        RN = 0UL
                                    }
                        }
                    | _ -> failwith "Invalid state when decrypting"

                if (rn >= 1000UL) then
                    let newState = refreshState pce
                    decryptCore msg (newState)
                else
                    decryptCore msg (pce)

            | _ ->
                failwith
                    "Tried to encrypt a message prior to noies handshake completion"

        let decryptMessage (msg: array<byte>) pce =
            if (msg.Length > 16 + 65535) then
                raise
                <| ArgumentException(
                    sprintf "Invalid length of message %i" msg.Length
                )

            match pce.NoiseState with
            | Finished {
                           RK = rk
                           RN = rn
                       } ->
                decryptWithAD(rn, rk, [||], ReadOnlySpan(msg))
                >>= fun plainText ->
                        let newPCE =
                            match pce.NoiseState with
                            | Finished finishedNoiseState ->
                                { pce with
                                    NoiseState =
                                        Finished
                                            { finishedNoiseState with
                                                RN = finishedNoiseState.RN + 1UL
                                            }
                                }
                            | _ -> failwith "Invalid state when incrementing RN"

                        Ok(plainText, newPCE)
            | _ ->
                failwith
                    "Tried to encyrpt a message prior to noise handshake completion"


/// Might remove in the future
[<AutoOpen>]
module PeerChannelEncryptorMonad =
    type PeerChannelEncryptorComputation<'a> =
        | PeerChannelEncryptorComputation of
            (PeerChannelEncryptor -> Result<'a * PeerChannelEncryptor, PeerError>)

    let runP pcec initialState =
        let (PeerChannelEncryptorComputation innerFn) = pcec
        innerFn initialState

    let returnP x =
        let innerFn state =
            Ok(x, state)

        PeerChannelEncryptorComputation innerFn

    let bindP
        (f: 'a -> PeerChannelEncryptorComputation<'b>)
        (xT: PeerChannelEncryptorComputation<'a>)
        : PeerChannelEncryptorComputation<'b> =
        let innerFn state =
            runP xT state
            >>= fun (res, state2) ->
                    let h = runP (f res) state2
                    h

        PeerChannelEncryptorComputation innerFn

    let mapP f =
        bindP(f >> returnP)

    /// Lift non-failable function to monadic world
    let fromPlainFunction
        (f: PeerChannelEncryptor -> 'a * PeerChannelEncryptor)
        =
        let innerFn state =
            let (result, newState) = f state
            Ok(result, newState)

        PeerChannelEncryptorComputation innerFn

    /// Lift non-failable reader function to monadic world
    let fromReaderFunction(f: PeerChannelEncryptor -> 'a) =
        let innerFn state =
            let result = f state
            result, state

        fromPlainFunction innerFn

    /// Lift non-failable writer function to monadic world
    let fromWriterFunction f =
        let f2 state =
            (), f state

        fromPlainFunction f2

    let fromFailableFunction
        (f: PeerChannelEncryptor -> Result<'a * PeerChannelEncryptor, _>)
        =
        PeerChannelEncryptorComputation f

    let fromFailableReaderFunction(f: PeerChannelEncryptor -> Result<'a, _>) =
        let innerFn state =
            f state >>= fun result -> Ok(result, state)

        PeerChannelEncryptorComputation innerFn


    let processActOneWithKey actOne key =
        fromFailableFunction(
            PeerChannelEncryptor.processActOneWithKey actOne key
        )

    let processActOneWithEphemeralKey actOne key ourEphemeral =
        fromFailableFunction(
            PeerChannelEncryptor.processActOneWithEphemeralKey
                actOne
                key
                ourEphemeral
        )

    let processActTwo actTwo ourNodeSecret =
        fromFailableFunction(
            PeerChannelEncryptor.processActTwo actTwo ourNodeSecret
        )

    let processActThree actThree =
        fromFailableFunction(PeerChannelEncryptor.processActThree actThree)

    let encryptMessage msg =
        fromPlainFunction(PeerChannelEncryptor.encryptMessage msg)

    let decryptMessage msg =
        fromFailableFunction(PeerChannelEncryptor.decryptMessage msg)

    let decryptLengthHeader msg =
        fromFailableFunction(PeerChannelEncryptor.decryptLengthHeader msg)

    type PeerChannelEncryptorComputationBuilder() =
        member this.Return x =
            returnP x

        member this.ReturnFrom x =
            x

        member this.Bind(x, f) =
            bindP f x

        member this.Zero() =
            returnP()

    let noise = PeerChannelEncryptorComputationBuilder()
