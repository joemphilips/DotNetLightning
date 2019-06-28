namespace DotNetLightning.LN
open NBitcoin
open NBitcoin.Crypto
open System.IO
open DotNetLightning.Utils.Primitives
open DotNetLightning.Utils.Aether
open DotNetLightning.Utils.Aether.Operators
open DotNetLightning.Serialize.Msgs
open System
open System.Diagnostics


[<AutoOpen>]
module PeerChannelEncryptor =
    // Sha256("Noise_XK_secp256k1_ChaChaPoly_SHA256")
    let NOISE_CK = [|0x26uy; 0x40uy; 0xf5uy; 0x2euy; 0xebuy; 0xcduy; 0x9euy; 0x88uy; 0x29uy; 0x58uy; 0x95uy; 0x1cuy; 0x79uy; 0x42uy; 0x50uy; 0xeeuy; 0xdbuy; 0x28uy; 0x00uy; 0x2cuy; 0x05uy; 0xd7uy; 0xdcuy; 0x2euy; 0xa0uy; 0xf1uy; 0x95uy; 0x40uy; 0x60uy; 0x42uy; 0xcauy; 0xf1uy|]

    // Sha256(NOISE_CK || "lightning")
    let NOISE_H = [|0xd1uy; 0xfbuy; 0xf6uy; 0xdeuy; 0xe4uy; 0xf6uy; 0x86uy; 0xf1uy; 0x32uy; 0xfduy; 0x70uy; 0x2cuy; 0x4auy; 0xbfuy; 0x8fuy; 0xbauy; 0x4buy; 0xb4uy; 0x20uy; 0xd8uy; 0x9duy; 0x2auy; 0x04uy; 0x8auy; 0x3cuy; 0x4fuy; 0x4cuy; 0x09uy; 0x2euy; 0x37uy; 0xb6uy; 0x76uy|]

    let chacha20 = NSec.Cryptography.ChaCha20Poly1305.ChaCha20Poly1305
    type SharedSecret = NSec.Cryptography.SharedSecret

    module internal SharedSecret =
        let FromKeyPair(pub: PubKey, priv: Key) =
            Hashes.SHA256 (pub.GetSharedPubkey(priv).ToBytes())

    type NextNoiseStep =
        | ActOne
        | ActTwo
        | ActThree
        | NoiseComplete

    type NoiseStep =
        | PreActOne
        | PostActOne
        | PostActTwo

    type BidirectionalNoiseState = {
        /// The handshake hash. This value is the accumulated hash of all handhsake data that has been sent
        /// and received so far during the handshake process.
        H: uint256
        /// The chaining key. This value is the accumulated hash of all previous ECDH outputs.
        /// At the end of the handshake, it is used for deriving the encryption keys for Lightning Messages.
        CK: uint256
    }
        with
            static member H_: Lens<_, _> =
                (fun state -> state.H),
                (fun h state -> { state with BidirectionalNoiseState.H = h })
            static member CK_: Lens<_ ,_> =
                (fun state -> state.CK),
                (fun ck state -> { state with BidirectionalNoiseState.CK = ck })

    type DirectionalNoisestate =
        | OutBound of OutBound
        | InBound of InBound
        with
            static member OutBound_: Prism<DirectionalNoisestate, _> =
                (fun state ->
                    match state with
                    | OutBound o -> Some o
                    | InBound i -> None),
                (fun o state ->
                    match state with
                    | OutBound _ -> OutBound o
                    | _ -> state)

            static member InBound_: Prism<DirectionalNoisestate, _> =
                (fun state ->
                    match state with
                    | OutBound _ -> None
                    | InBound i -> Some i),
                (fun i state ->
                    match state with
                    | InBound _ -> InBound i
                    | _ -> state)

    and OutBound = { IE: Key }
        with
            static member IE_: Lens<_, _> =
                (fun ob -> ob.IE),
                (fun ie ob -> { ob with OutBound.IE = ie})
    and InBound = {
        IE: PubKey option // Some if state >= PostActOne
        RE: Key option // Some if state >= PostActTwo
        TempK2: uint256 option // Some if state >= PostActTwo
    }
        with
            static member IE_: Prism<InBound, _> =
                (fun ib -> ib.IE),
                (fun ie ib -> { ib with InBound.IE = Some ie})

            static member RE_: Prism<_, _>  =
                (fun ib -> ib.RE),
                (fun re ib -> { ib with InBound.RE = Some re })

            static member TempK2_: Prism<_, _> =
                (fun ib -> ib.TempK2),
                (fun tempk2 ib -> {ib with InBound.TempK2 = Some tempk2})

    type NoiseState =
        | InProgress of InProgressNoiseState
        | Finished of FinishedNoiseState
        with
            static member InProgress_: Prism<_, _> =
                (fun ns ->
                    match ns with
                    | InProgress i -> Some i
                    | _ -> None),
                (fun ipns ns ->
                    match ns with
                    | InProgress _ -> InProgress ipns
                    | _ -> ns)
            static member Finished_: Prism<_, _> =
                (fun ns ->
                    match ns with
                    | Finished state -> Some state
                    | _ -> None),
                (fun finishedNS ns ->
                    match ns with
                    | Finished _ -> Finished finishedNS
                    | _ -> ns)
    and InProgressNoiseState = {
        State: NoiseStep
        DirectionalState: DirectionalNoisestate
        BidirectionalState: BidirectionalNoiseState
    }
        with
            static member State_: Lens<_, _>  =
                (fun ipns -> ipns.State),
                (fun noiseStep ipns -> { ipns with InProgressNoiseState.State = noiseStep })
            static member DirectionalState_: Lens<_, _>  =
                (fun ipns -> ipns.DirectionalState),
                (fun ds ipns -> { ipns with InProgressNoiseState.DirectionalState = ds })
            static member BiDirectionalState_: Lens<_, _>  =
                (fun ipns -> ipns.BidirectionalState),
                (fun bidirectionalState ipns -> { ipns with InProgressNoiseState.BidirectionalState = bidirectionalState })

    and FinishedNoiseState = {
        SK: uint256
        SN: uint64
        SCK: uint256
        RK: uint256
        RN: uint64
        RCK: uint256
    }

        with
            static member SK_: Lens<_, _> =
                (fun fns -> fns.SK),
                (fun sk fns -> { fns with FinishedNoiseState.SK = sk })
            static member SN_: Lens<_, _>  =
                (fun fns -> fns.SN),
                (fun sn fns -> { fns with FinishedNoiseState.SN = sn })
            static member SCK_: Lens<_, _>  =
                (fun fns -> fns.SCK),
                (fun sck fns -> { fns with FinishedNoiseState.SCK = sck })
            static member RK_: Lens<_, _>  =
                (fun fns -> fns.RK),
                (fun rk fns -> { fns with FinishedNoiseState.SCK = rk })
            static member RN_: Lens<_, _>  =
                (fun fns -> fns.RN),
                (fun rn fns -> { fns with FinishedNoiseState.RN = rn })
            static member RCK_: Lens<_, _>  =
                (fun fns -> fns.RCK),
                (fun rck fns -> { fns with FinishedNoiseState.RCK = rck })

    type PeerChannelEncryptor = internal {
        TheirNodeId: NodeId option
        NoiseState: NoiseState
    }
        with
            static member internal TheirNodeId_: Prism<_, _> =
                (fun pce -> pce.TheirNodeId),
                (fun nodeid pce -> { pce with TheirNodeId = Some nodeid })
            static member internal NoiseState_: Lens<_, _> =
                (fun pce -> pce.NoiseState),
                (fun ns pce -> { pce with NoiseState = ns })

            static member internal Finished_: Prism<_, _> =
                PeerChannelEncryptor.NoiseState_ >-> NoiseState.Finished_

            static member internal InProgress_: Prism<_,_> =
                PeerChannelEncryptor.NoiseState_ >-> NoiseState.InProgress_

            static member internal State_ :Prism<_, _> =
                PeerChannelEncryptor.NoiseState_ >-> NoiseState.InProgress_ >?> InProgressNoiseState.State_

            static member internal BState_ :Prism<_, _> =
                PeerChannelEncryptor.NoiseState_ >-> NoiseState.InProgress_ >?> InProgressNoiseState.BiDirectionalState_

            static member internal H_ :Prism<_, _> =
                PeerChannelEncryptor.BState_ >?> BidirectionalNoiseState.H_

            static member internal CK_ :Prism<_, _> =
                PeerChannelEncryptor.BState_ >?> BidirectionalNoiseState.CK_

            static member internal DState_ :Prism<_, _> =
                PeerChannelEncryptor.NoiseState_ >-> NoiseState.InProgress_ >?> InProgressNoiseState.DirectionalState_

            static member internal OutBound_: Prism<_,_> =
                PeerChannelEncryptor.DState_ >?> DirectionalNoisestate.OutBound_

            static member internal OutBoundIE_: Prism<_,_> =
                PeerChannelEncryptor.OutBound_ >?> OutBound.IE_

            static member internal InBound_: Prism<_,_> =
                PeerChannelEncryptor.DState_ >?> DirectionalNoisestate.InBound_

            static member internal InBoundIE_: Prism<_,_> =
                PeerChannelEncryptor.InBound_ >?> InBound.IE_

            static member internal RE_: Prism<_,_> =
                PeerChannelEncryptor.InBound_ >?> InBound.RE_

            static member internal TempK2_: Prism<_,_> =
                PeerChannelEncryptor.InBound_ >?> InBound.TempK2_

    module PeerChannelEncryptor =
        open DotNetLightning.Utils
        let newOutBound (NodeId theirNodeId) =
            let hashInput = Array.concat[| NOISE_H; theirNodeId.ToBytes()|]
            let h = uint256(Hashes.SHA256(hashInput))
            {
                TheirNodeId = Some(NodeId theirNodeId)
                NoiseState = InProgress {
                        State = PreActOne
                        DirectionalState = OutBound ({ IE = Key() })
                        BidirectionalState = {H = h; CK = uint256(NOISE_CK)}
                    }
            }

        let newInBound (ourNodeSecret: Key) =
            let hashInput = Array.concat[|NOISE_H; ourNodeSecret.PubKey.ToBytes()|]
            let h = uint256(Hashes.SHA256(hashInput))

            {
                TheirNodeId = None
                NoiseState = InProgress {
                        State = PreActOne
                        DirectionalState = InBound { IE = None; RE = None; TempK2 = None}
                        BidirectionalState = {H = h; CK = uint256(NOISE_CK)}
                    }
            }

        let private getNonce (n: uint64) =
            let nonceBytes = ReadOnlySpan(Array.concat[| Array.zeroCreate 4; BitConverter.GetBytes(n) |]) // little endian
            NSec.Cryptography.Nonce(nonceBytes, 0)

        let decryptWithAD(n: uint64, key: uint256, h: uint256, plaintext: ReadOnlySpan<byte>): RResult<byte[]> =
            let nonce = getNonce n
            let keySpan = ReadOnlySpan(key.ToBytes())
            let hSpan = ReadOnlySpan(h.ToBytes())
            let blobF = NSec.Cryptography.KeyBlobFormat.RawSymmetricKey
            let chachaKey = NSec.Cryptography.Key.Import(chacha20, keySpan, blobF)
            match chacha20.Decrypt(chachaKey, &nonce, hSpan, plaintext) with
            | true, i -> Good i
            | false, _ -> RResult.rbad(RBad.Object({ HandleError.Error = "Bad MAC"; Action = Some(DisconnectPeer(None))} ))

        let private encryptWithAD(n: uint64, key: uint256, h: ReadOnlySpan<byte>, plainText: ReadOnlySpan<byte>) =
            let nonce = getNonce n
            let keySpan = ReadOnlySpan(key.ToBytes())
            let blobF = NSec.Cryptography.KeyBlobFormat.RawSymmetricKey
            use chachaKey = NSec.Cryptography.Key.Import(chacha20, keySpan, blobF)
            chacha20.Encrypt(chachaKey, &nonce, h, plainText)

        let hkdfExtractExpand(salt: byte[], ikm: byte[]) =
            let prk = Hashes.HMACSHA256(salt, ikm)
            let t1 = Hashes.HMACSHA256(prk, [|1uy|])
            let t2 = Hashes.HMACSHA256(prk, Array.append t1 [|2uy|])
            (t1, t2)

        let private hkdf(state: BidirectionalNoiseState, sharedSecret: byte[]) =
            let (t1, t2) = hkdfExtractExpand(state.CK.ToBytes(), sharedSecret)
            uint256(t2), { state with CK = t1 |> uint256 }


        let private outBoundNoiseAct (state: BidirectionalNoiseState, ourKey: Key, theirKey: PubKey) =
            let ourPub = ourKey.PubKey
            let tempK, s3 =
                let h = Hashes.SHA256(Array.concat ([| state.H.ToBytes(); ourPub.ToBytes() |])) |> uint256
                let s2 = Optic.set (BidirectionalNoiseState.H_) h state
                let ss = SharedSecret.FromKeyPair(theirKey, ourKey)
                hkdf(s2, ss)

            let c = encryptWithAD (0UL, tempK, ReadOnlySpan(s3.H.ToBytes()), ReadOnlySpan([||]))
            let resultToSend = Array.concat (seq [ [|0uy|]; ourPub.ToBytes(); c ])
            (resultToSend, tempK), { s3 with H = Hashes.SHA256(Array.concat [| s3.H.ToBytes(); c |]) |> uint256 }

        let private inBoundNoiseAct (state: BidirectionalNoiseState, act: byte[], ourKey: Key): RResult<(PubKey *uint256) * BidirectionalNoiseState> =
            assert (act.Length = 50)
            if (act.[0] <> 0uy) then
                RResult.rbad(RBad.Object { HandleError.Error = ("Unknown handshake version number"); Action = Some (DisconnectPeer(None)) } )
            else if not (PubKey.Check(act.[1..33], true)) then
                RResult.rbad(RBad.Object({ HandleError.Error = "Invalid Public Key"; Action = Some(DisconnectPeer(None)) }))
            else
                let theirPub = PubKey(act.[1..33])
                let s2 = { state with H = Hashes.SHA256(Array.concat [| state.H.ToBytes(); theirPub.ToBytes() |]) |> uint256 }
                let ss = SharedSecret.FromKeyPair(theirPub, ourKey)
                let tempK, _ = hkdf (state, ss)
                let dec = decryptWithAD (0UL, tempK, s2.H, ReadOnlySpan(act.[34..]))
                let s3 = { s2 with H = Hashes.SHA256(Array.concat [| state.H.ToBytes(); act.[34..] |]) |> uint256 }
                Good ((theirPub, tempK), s3)

        let getActOne (pce: PeerChannelEncryptor) : byte[] * PeerChannelEncryptor =
            match pce.NoiseState with
            | InProgress { State = state; DirectionalState = dState; BidirectionalState = bState} ->
                match dState with
                | OutBound { IE = ie } ->
                    if state <> PreActOne then
                        failwith "Requested act at wrong step"
                    else
                        let (res, _), newBState = outBoundNoiseAct (bState, ie, pce.TheirNodeId.Value.Value)
                        let newPce = pce
                                     |> Optic.set (PeerChannelEncryptor.State_)
                                                  NoiseStep.PostActOne
                                     |> Optic.set (PeerChannelEncryptor.BState_)
                                                  newBState
                        res, newPce
                | _ -> failwith "Wrong Directiona for Act"
            | _ -> failwith "Cannot get act one after noise  handshake completes"

        let processActOneWithEphemeralKey(actOne: byte[], ourNodeSecret: Key, ourEphemeral: Key) (pce: PeerChannelEncryptor): RResult<byte[] * _> =
            match pce.NoiseState with
            | InProgress { State = state; DirectionalState = dState; BidirectionalState = bState1} ->
                match dState with
                | InBound { IE = ie; RE = re; TempK2 = tempk2 } ->
                    if state <> PreActOne then 
                        failwith "Requested act at wrong step"

                    inBoundNoiseAct(bState1, actOne, ourNodeSecret)
                    |>> fun res ->
                        let (theirPub, _), bState2 = res
                        let newDState =
                            Optic.set (DirectionalNoisestate.InBound_ >?> InBound.IE_) theirPub dState
                            |> Optic.set (DirectionalNoisestate.InBound_ >?> InBound.RE_) ourEphemeral
                        let (res, tempK), bState3 = outBoundNoiseAct (bState2, ourEphemeral, theirPub)
                        let newPCE =
                            pce
                            |> Optic.set PeerChannelEncryptor.State_ NoiseStep.PostActTwo
                            |> Optic.set PeerChannelEncryptor.DState_ newDState
                            |> Optic.set PeerChannelEncryptor.BState_ bState3
                            |> Optic.set PeerChannelEncryptor.TempK2_ tempK

                        (res, newPCE)
                | _ -> failwith "Requested act at wrong step"
            | _ ->
                failwith "Cannot get acg one after noise handshake completes"

        let processActOneWithKey (actTwo: byte[]) (ourNodeSecret: Key) (pce: PeerChannelEncryptor): RResult<byte[] * _> =
            assert (actTwo.Length = 50)

            let ephemeralKey = Key()
            processActOneWithEphemeralKey (actTwo, ourNodeSecret, ephemeralKey) pce

        let processActTwo (actTwo: byte[], ourNodeSecret: Key) (pce: PeerChannelEncryptor): RResult<(byte[] * NodeId) * PeerChannelEncryptor> = 
            match pce.NoiseState with
            | InProgress {State = state; DirectionalState = dState; BidirectionalState = bState1 } -> 
                match dState with
                | OutBound {IE = ie} ->
                    if state <> NoiseStep.PostActOne then
                        failwith "Requested act at wrong step"
                    inBoundNoiseAct (bState1, actTwo, ie)
                    |>> fun (res, bState2) ->
                        let re, tempK2 = res
                        let ourNodeId = ourNodeSecret.PubKey
                        let encryptedRes1 = encryptWithAD (1UL, tempK2, ReadOnlySpan(bState2.H.ToBytes()), ReadOnlySpan(ourNodeId.ToBytes()))
                        let bState2 = { bState1 with H = Hashes.SHA256(Array.concat[| bState2.H.ToBytes(); encryptedRes1 |]) |> uint256 }
                        let ss = SharedSecret.FromKeyPair(re, ourNodeSecret)
                        let (tempK, bState3) = hkdf(bState2, ss)
                        let encryptedRes2 = encryptWithAD (0UL, tempK, ReadOnlySpan(bState3.H.ToBytes()), ReadOnlySpan([||]))
                        let (sk, rk) = hkdfExtractExpand(bState3.CK.ToBytes(), [||])
                        let newPce =
                            pce
                            |> Optic.set (PeerChannelEncryptor.BState_) bState3
                            |> Optic.set (PeerChannelEncryptor.Finished_)
                                         {
                                             FinishedNoiseState.SK = sk |> uint256
                                             SN = 0UL
                                             SCK = bState3.CK
                                             RK = rk |> uint256
                                             RN = 0UL
                                             RCK = bState3.CK
                                         }
                        (encryptedRes2, newPce.TheirNodeId.Value), newPce
                | _ -> failwith "Wrong direction with act"
            | _ -> failwith "Cannot get act one after noise handshake completes"

        let processActThree (actThree: byte[]) (pce: PeerChannelEncryptor): RResult<NodeId * PeerChannelEncryptor> =
            assert (actThree.Length = 66)
            match pce.NoiseState with
            | InProgress { State = state; DirectionalState = dState; BidirectionalState = bState } ->
                match dState with
                | InBound { IE = _; RE = re; TempK2 = tempk2 } ->
                    if (state <> PostActTwo) then
                        failwith "Requested act at wrong step"
                    else if (actThree.[0] <> 0uy) then
                        RResult.rbad (RBad.Object({ HandleError.Error = "Unknown handshake version number"; Action = Some (DisconnectPeer(None))}))
                    else
                        decryptWithAD (1UL, tempk2.Value, bState.H, ReadOnlySpan(actThree.[1..50]))
                        >>= fun theirNodeId ->
                            if PubKey.Check (theirNodeId, true) then
                                RResult.rbad(RBad.Object({ HandleError.Error = "Bad Nodeid from Peer"; Action = Some (DisconnectPeer(None))}))
                            else
                                let pce2 =
                                    pce
                                    |> Optic.set (PeerChannelEncryptor.H_) (Array.concat[| bState.H.ToBytes(); actThree.[1..50] |] |> uint256)
                                    |> Optic.set (PeerChannelEncryptor.TheirNodeId_) (PubKey(theirNodeId) |> NodeId)
                                let ss = SharedSecret.FromKeyPair(pce2.TheirNodeId.Value.Value, re.Value)
                                let tempK, bState2 = hkdf(bState, ss)
                                decryptWithAD(0UL, tempK, bState2.H, ReadOnlySpan(actThree.[50..]))
                                |>> fun _ ->
                                    let (rk, sk) = hkdfExtractExpand(bState2.CK.ToBytes(), [||])
                                    let pce3 = Optic.set (PeerChannelEncryptor.Finished_)
                                                         {
                                                            FinishedNoiseState.SK = sk |> uint256
                                                            SN = 0UL
                                                            SCK = bState.CK
                                                            RK = rk |> uint256
                                                            RN= 0UL
                                                            RCK = bState2.CK
                                                         } 
                                                         pce2
                                    pce3.TheirNodeId.Value, pce3
                | _ -> failwith "wrong direction for act"
            | _ -> failwith "Cannot get act one after noise handshake completes"


    type PeerChannelEncryptor with
        static member EncryptMessage (msg: Span<byte>): Span<byte> =
            msg

        static member DecryptLengthHeader (msg: Span<byte>): Span<byte> =
            msg

        member this.IsReadyForEncryption(): bool =
            match this.NoiseState with
            | InProgress _ -> false
            | Finished _ -> true

    (*
    type PeerChannelEncryptor(theirNodeId: NodeId option, noiseState: NoiseState) =
        member this.TheirNodeId = theirNodeId
        member val NoiseState = noiseState with get, set

        static member NewOutbound(theirNodeId: NodeId): PeerChannelEncryptor =
            let hashInput = Array.concat[| NOISE_H; theirNodeId.Value.ToBytes()|]
            let h = uint256(Hashes.SHA256(hashInput))
            PeerChannelEncryptor(
                Some(theirNodeId),
                InProgress {
                        State = PreActOne
                        DirectionalState = OutBound ({ IE = Key() })
                        BidirectionalState = {H = h; CK = uint256(NOISE_CK)}
                    }
                )

        static member NewInbound (ourNodeSecret: Key) =
            let hashInput = Array.concat[|NOISE_H; ourNodeSecret.PubKey.ToBytes()|]
            let h = uint256(Hashes.SHA256(hashInput))

            PeerChannelEncryptor(
                None,
                InProgress {
                        State = PreActOne
                        DirectionalState = InBound { IE = None; RE = None; TempK2 = None}
                        BidirectionalState = {H = h; CK = uint256(NOISE_CK)}
                    }
            )

        static member EncryptWithAd(n: uint64, key: uint256, h: ReadOnlySpan<byte>, plainText: ReadOnlySpan<byte>) =
            let nonce = getNonce n
            let keySpan = ReadOnlySpan(key.ToBytes())
            let blobF = NSec.Cryptography.KeyBlobFormat.RawPrivateKey
            use chachaKey = NSec.Cryptography.Key.Import(chacha20, keySpan, blobF)
            chacha20.Encrypt(chachaKey, &nonce, h, plainText)

        static member DecryptWithAd(n: uint64, key: uint256, h: ReadOnlySpan<byte>, cypherText: ReadOnlySpan<byte>): Result<byte[], HandleError> =
            let nonce = getNonce n
            let blobF = NSec.Cryptography.KeyBlobFormat.RawPrivateKey
            let keySpan = ReadOnlySpan(key.ToBytes())
            use chachaKey = NSec.Cryptography.Key.Import(chacha20, keySpan, blobF)
            match chacha20.Decrypt(chachaKey, &nonce, h, cypherText) with
            | true, data -> Ok(data)
            | false, _ -> Result.Error {Error = "Bad Mac"; Action = Some( ErrorAction.DisconnectPeer None )}

        static member HKDF(state: BidirectionalNoiseState, ss: SharedSecret): uint256 =
            let hkdf = NSec.Cryptography.HkdfSha256()
            failwith "Not impl"

        static member OutBoundNoiseAct(state: BidirectionalNoiseState, ourKey: Key, theirKey: PubKey): (byte[] * uint256) =
            let ourPub = ourKey.PubKey
            let h = Hashes.SHA256(Array.concat ([| state.H.ToBytes(); ourPub.ToBytes() |]))
            failwith "needs update"

        static member InBoundNoiseAct(state: BidirectionalNoiseState, act: byte[], ourKey: Key): Result<(PubKey * uint256), HandleError> =
            failwith "Not impl"

        member this.ProessActOneWithEphemeralKey(actTwo: byte[], ourNodeSecret: Key): Result<byte[], HandleError> = 
            Debug.Assert(actTwo.Length = 50)

            match this.NoiseState with
            | InProgress { State = state; DirectionalState = dState; BidirectionalState = bState } ->
                match dState with
                | OutBound {IE = ie} ->
                    if state = PostActone then
                        match PeerChannelEncryptor.InBoundNoiseAct(bState, actTwo, ie) with
                        | Ok (re, tempK2) ->
                            let ourNodeId = ourNodeSecret.PubKey
                            let res = PeerChannelEncryptor.EncryptWithAd(
                                            1UL,
                                            tempK2,
                                            ReadOnlySpan(bState.H.ToBytes()),
                                            ReadOnlySpan(ourNodeId.ToBytes())
                                        )
                            let h = Hashes.SHA256(Array.concat[| bState.H.ToBytes(); res |])
                            let newbState = { bState with H = uint256(h) }

                            let ss = SharedSecret.FromKeyPair(re, ourNodeSecret)
                            failwith ""
                        | Result.Error _ -> 
                            failwith "Failed to process InboundNoiseAct"
                    else
                        failwith "Requested act at wrong step"
                | _ -> failwith "Wrong direction for act"
            | _ -> failwith "Cannot get act one after noise handshake completes."


        /// Panics if `msg.Length > 65535` (Out of Noise protocol support)
        member this.EncryptMessage(msg: byte[]) =
            failwith "Not impl"
        
        /// Decrypts a message  length header from the remote peer.
        /// panics if noise handshake has not yet finished or `msg.Length != 18`
        member this.DecryptLengthHeader(msg: byte[]): Result<uint16, HandleError> =
            failwith "Not impl"

        /// Decrypts the given message.
        /// panics if `msg.Length > 65535 + 16`
        member this.DecryptMessage(msg: byte[]): Result<byte[], HandleError> =
            failwith "Not impl"

        member this.GetNoiseStep(): NextNoiseStep =
            match this.NoiseState with
            | NoiseState.InProgress s ->
                match s.State with
                | NoiseStep.PreActOne -> NextNoiseStep.ActOne
                | PostActone -> ActTwo
                | PostActTwo -> ActThree
            | Finished _ -> NoiseComplete

        *)
