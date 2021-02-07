namespace DotNetLightning.Peer

open System

open NBitcoin
open NBitcoin.Crypto

open DotNetLightning.Utils
open DotNetLightning.Utils.Aether
open DotNetLightning.Utils.Aether.Operators
open DotNetLightning.Crypto

open ResultUtils
open ResultUtils.Portability

[<AutoOpen>]
module PeerChannelEncryptor =
    let crypto = CryptoUtils.impl

    type BitConverter with
        static member GetBytesBE(data: uint16) =
            let buf = Array.zeroCreate 2
            buf.[0] <- (byte (data >>> 8))
            buf.[1] <- byte data
            buf
        static member ToUint16BE(data: byte[]) =
            ((uint16 data.[0]) <<< 8 ||| uint16 (data.[1]))

    // Sha256("Noise_XK_secp256k1_ChaChaPoly_SHA256")
    let NOISE_CK = [|0x26uy; 0x40uy; 0xf5uy; 0x2euy; 0xebuy; 0xcduy; 0x9euy; 0x88uy; 0x29uy; 0x58uy; 0x95uy; 0x1cuy; 0x79uy; 0x42uy; 0x50uy; 0xeeuy; 0xdbuy; 0x28uy; 0x00uy; 0x2cuy; 0x05uy; 0xd7uy; 0xdcuy; 0x2euy; 0xa0uy; 0xf1uy; 0x95uy; 0x40uy; 0x60uy; 0x42uy; 0xcauy; 0xf1uy|]

    // Sha256(NOISE_CK || "lightning")
    let NOISE_H = [|0xd1uy; 0xfbuy; 0xf6uy; 0xdeuy; 0xe4uy; 0xf6uy; 0x86uy; 0xf1uy; 0x32uy; 0xfduy; 0x70uy; 0x2cuy; 0x4auy; 0xbfuy; 0x8fuy; 0xbauy; 0x4buy; 0xb4uy; 0x20uy; 0xd8uy; 0x9duy; 0x2auy; 0x04uy; 0x8auy; 0x3cuy; 0x4fuy; 0x4cuy; 0x09uy; 0x2euy; 0x37uy; 0xb6uy; 0x76uy|]

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
                    | InBound _i -> None),
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

            static member internal SK_: Prism<_, _> =
                NoiseState.Finished_ >?> FinishedNoiseState.SK_

            static member internal SN_: Prism<_, _> =
                NoiseState.Finished_ >?> FinishedNoiseState.SN_

            static member internal SCK_: Prism<_, _> =
                NoiseState.Finished_ >?> FinishedNoiseState.SCK_

            static member internal RK_: Prism<_, _> =
                NoiseState.Finished_ >?> FinishedNoiseState.RK_

            static member internal RN_: Prism<_, _> =
                NoiseState.Finished_ >?> FinishedNoiseState.RN_

            static member internal RCK_: Prism<_, _> =
                NoiseState.Finished_ >?> FinishedNoiseState.RCK_

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
                (fun rk fns -> { fns with FinishedNoiseState.RK = rk })
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

            // finished noise state lenses
            static member SK_: Prism<_, _> =
                PeerChannelEncryptor.Finished_  >?> FinishedNoiseState.SK_
            static member SN_: Prism<_, _> =
                PeerChannelEncryptor.Finished_  >?> FinishedNoiseState.SN_
            static member SCK_: Prism<_, _> =
                PeerChannelEncryptor.Finished_  >?> FinishedNoiseState.SCK_
            static member RK_: Prism<_, _> =
                PeerChannelEncryptor.Finished_  >?> FinishedNoiseState.RK_
            static member RN_: Prism<_, _> =
                PeerChannelEncryptor.Finished_  >?> FinishedNoiseState.RN_
            static member RCK_: Prism<_, _> =
                PeerChannelEncryptor.Finished_  >?> FinishedNoiseState.RCK_


            // inprogress noise state lenses
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

            static member OutBoundIE_: Prism<_,_> =
                PeerChannelEncryptor.OutBound_ >?> OutBound.IE_

            static member internal InBound_: Prism<_,_> =
                PeerChannelEncryptor.DState_ >?> DirectionalNoisestate.InBound_

            static member internal InBoundIE_: Prism<_,_> =
                PeerChannelEncryptor.InBound_ >?> InBound.IE_

            static member internal RE_: Prism<_,_> =
                PeerChannelEncryptor.InBound_ >?> InBound.RE_

            static member internal TempK2_: Prism<_,_> =
                PeerChannelEncryptor.InBound_ >?> InBound.TempK2_

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
        let newOutBound (NodeId theirNodeId, ourNodeSecret: Key) =
            let hashInput = Array.concat[| NOISE_H; theirNodeId.ToBytes()|]
            let h = uint256(Hashes.SHA256(hashInput))
            {
                TheirNodeId = Some(NodeId theirNodeId)
                NoiseState = InProgress {
                        State = PreActOne
                        DirectionalState = OutBound ({ IE = ourNodeSecret })
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
        let internal decryptWithAD(n: uint64, key: uint256, ad: byte[], cipherText: ReadOnlySpan<byte>): Result<byte[], _> =
            crypto.decryptWithAD(n, key, ad, cipherText)
            |> Result.mapError(PeerError.CryptoError)

        let internal hkdfExtractExpand(salt: byte[], ikm: byte[]) =
            let prk = Hashes.HMACSHA256(salt, ikm)
            let t1 = Hashes.HMACSHA256(prk, [|1uy|])
            let t2 = Hashes.HMACSHA256(prk, Array.append t1 [|2uy|])
            (t1, t2)

        let private hkdf (sharedSecret: byte[]) (state: PeerChannelEncryptor) =
            let (t1, t2) =
                let ck = Optic.get (PeerChannelEncryptor.CK_) state
                hkdfExtractExpand(ck.Value.ToBytes(), sharedSecret)
            uint256(t2), (state |> Optic.set (PeerChannelEncryptor.CK_) (t1 |> uint256 ))

        let private updateHWith (state: PeerChannelEncryptor) (item: byte[]) =
             Optic.map (PeerChannelEncryptor.H_) (fun h -> Array.concat[| h.ToBytes(); item |] |> Hashes.SHA256 |> uint256) state

        let private outBoundNoiseAct (state: PeerChannelEncryptor, ourKey: Key, theirKey: PubKey) =
            let ourPub = ourKey.PubKey
            let tempK, s3 =
                let s2 = updateHWith state (ourPub.ToBytes())
                let ss = Secret.FromKeyPair(theirKey, ourKey)
                hkdf ss s2

            let c =
                let h = Optic.get (PeerChannelEncryptor.H_) s3
                crypto.encryptWithAD (0UL, tempK, ReadOnlySpan(h.Value.ToBytes()), ReadOnlySpan([||]))
            let resultToSend = Array.concat (seq [ [|0uy|]; ourPub.ToBytes(); c ])
            let newState = updateHWith s3 c
            (resultToSend, tempK), newState

        let private inBoundNoiseAct (state: PeerChannelEncryptor, act: byte[], ourKey: Key): Result<(PubKey * uint256) * PeerChannelEncryptor, PeerError> =
            if (act.Length <> 50) then raise <| ArgumentException(sprintf "Invalid act length: %d" (act.Length))
            if (act.[0] <> 0uy) then
                Error(UnknownHandshakeVersionNumber (act.[0]))
            else if not (PubKey.Check(act.[1..33], true)) then
                Error(PeerError.CryptoError(InvalidPublicKey(act.[1..33])))
            else
                let theirPub = PubKey(act.[1..33])
                let tempK, s3 =
                    let ss = Secret.FromKeyPair(theirPub, ourKey)
                    let s2 = updateHWith state (theirPub.ToBytes())
                    hkdf ss s2
                let currentH = Optic.get (PeerChannelEncryptor.H_) s3
                result {
                    let! _ = decryptWithAD (0UL, tempK, currentH.Value.ToBytes(), ReadOnlySpan(act.[34..]))
                    let s4 =  updateHWith s3 act.[34..]
                    return ((theirPub, tempK), s4)
                }

        let getActOne (pce: PeerChannelEncryptor) : byte[] * PeerChannelEncryptor =
            match pce.NoiseState with
            | InProgress { State = state; DirectionalState = dState} ->
                match dState with
                | OutBound { IE = ie } ->
                    if state <> PreActOne then
                        failwith "Requested act at wrong step"
                    else
                        outBoundNoiseAct (pce, ie, pce.TheirNodeId.Value.Value)
                        |> fun ((res, _), pce2) ->
                            res, (pce2 |> Optic.set (PeerChannelEncryptor.State_) NoiseStep.PostActOne)
                | _ -> failwith "Wrong Direction for Act"
            | _ -> failwith "Cannot get act one after noise  handshake completes"

        let processActOneWithEphemeralKey (actOne: byte[]) (ourNodeSecret: Key) (ourEphemeral: Key) (pce: PeerChannelEncryptor): Result<byte[] * _, PeerError> =
            match pce.NoiseState with
            | InProgress { State = state; DirectionalState = dState; } ->
                match dState with
                | InBound _ ->
                    if state <> PreActOne then 
                        failwith "Requested act at wrong step"

                    inBoundNoiseAct(pce, actOne, ourNodeSecret)
                    >>= fun res ->
                        let (theirPub, _), pce2 = res
                        let pce3 =
                            pce2
                            |> Optic.set (PeerChannelEncryptor.InBoundIE_) theirPub
                            |> Optic.set (PeerChannelEncryptor.RE_) ourEphemeral
                        let (res, tempK), pce4 = outBoundNoiseAct (pce3, ourEphemeral, theirPub)
                        let pce5 =
                            pce4
                            |> Optic.set PeerChannelEncryptor.State_ NoiseStep.PostActTwo
                            |> Optic.set PeerChannelEncryptor.TempK2_ tempK

                        Ok (res, pce5)
                | _ -> failwith "Requested act at wrong step"
            | _ ->
                failwith "Cannot get acg one after noise handshake completes"

        let processActOneWithKey (actOne: byte[]) (ourNodeSecret: Key) (pce: PeerChannelEncryptor): Result<byte[] * _, PeerError> =
            if (actOne.Length <> 50) then raise <| ArgumentException(sprintf "invalid actOne length: %d" (actOne.Length))

            let ephemeralKey = new Key()
            processActOneWithEphemeralKey actOne ourNodeSecret ephemeralKey pce

        let processActTwo (actTwo: byte[]) (ourNodeSecret: Key) (pce: PeerChannelEncryptor): Result<(byte[] * NodeId) * PeerChannelEncryptor, PeerError> = 
            match pce.NoiseState with
            | InProgress {State = state; DirectionalState = dState } -> 
                match dState with
                | OutBound {IE = ie} ->
                    if state <> NoiseStep.PostActOne then
                        failwith "Requested act at wrong step"
                    inBoundNoiseAct (pce, actTwo, ie)
                    >>= fun ((re, tempK2), pce2) ->
                        let ourNodeId = ourNodeSecret.PubKey
                        let encryptedRes1 =
                            let currentH = Optic.get (PeerChannelEncryptor.H_) pce2
                            crypto.encryptWithAD (1UL, tempK2, ReadOnlySpan(currentH.Value.ToBytes()), ReadOnlySpan(ourNodeId.ToBytes()))
                        let pce3 = updateHWith pce2 encryptedRes1 
                        let (tempK, pce4) =
                            let ss = Secret.FromKeyPair(re, ourNodeSecret)
                            hkdf ss pce3

                        let encryptedRes2 =
                            let currentH = Optic.get (PeerChannelEncryptor.H_) pce4
                            crypto.encryptWithAD (0UL, tempK, ReadOnlySpan(currentH.Value.ToBytes()), ReadOnlySpan([||]))

                        let (sk, rk) =
                            let currentCK = Optic.get (PeerChannelEncryptor.CK_) pce4
                            hkdfExtractExpand(currentCK.Value.ToBytes(), [||])

                        let newPce =
                            let ck = Optic.get (PeerChannelEncryptor.CK_) (pce4)
                            pce4
                            |> Optic.set (PeerChannelEncryptor.NoiseState_)
                                         (Finished {
                                             FinishedNoiseState.SK = sk |> uint256
                                             SN = 0UL
                                             SCK = ck.Value
                                             RK = rk |> uint256
                                             RN = 0UL
                                             RCK = ck.Value
                                         })
                        let resultToSend = Array.concat (seq [ [|0uy|]; encryptedRes1; encryptedRes2 ])
                        Ok ((resultToSend, newPce.TheirNodeId.Value), newPce)
                | _ -> failwith "Wrong direction with act"
            | _ -> failwith "Cannot get act one after noise handshake completes"

        let processActThree (actThree: byte[]) (pce: PeerChannelEncryptor): Result<NodeId * PeerChannelEncryptor, PeerError> =
            if (actThree.Length <> 66) then raise <| ArgumentException (sprintf "actThree must be 66 bytes, but it was %d" (actThree.Length))
            match pce.NoiseState with
            | InProgress { State = state; DirectionalState = dState;} ->
                match dState with
                | InBound { IE = _; RE = re; TempK2 = tempk2 } ->
                    if (state <> PostActTwo) then
                        failwith "Requested act at wrong step"
                    else if (actThree.[0] <> 0uy) then
                        Error(UnknownHandshakeVersionNumber(actThree.[0]))
                    else
                        let h = Optic.get (PeerChannelEncryptor.H_) pce
                        decryptWithAD (1UL, tempk2.Value, h.Value.ToBytes(), ReadOnlySpan(actThree.[1..49]))
                        >>= fun theirNodeId ->
                            if not (PubKey.Check (theirNodeId, true)) then
                                Error(PeerError.CryptoError(CryptoError.InvalidPublicKey(theirNodeId)))
                            else
                                let tempK, pce3 =
                                    let pce2 =
                                        pce
                                        |> fun p -> updateHWith p (actThree.[1..49])
                                        |> Optic.set (PeerChannelEncryptor.TheirNodeId_) (PubKey(theirNodeId) |> NodeId)
                                    let ss = Secret.FromKeyPair(pce2.TheirNodeId.Value.Value, re.Value)
                                    hkdf ss pce2

                                let h = Optic.get (PeerChannelEncryptor.H_) pce3
                                decryptWithAD(0UL, tempK, h.Value.ToBytes(), ReadOnlySpan(actThree.[50..]))
                                >>= fun _ ->
                                    let ck = (Optic.get (PeerChannelEncryptor.CK_) pce3).Value
                                    let (rk, sk) = hkdfExtractExpand(ck.ToBytes(), [||])
                                    let newPce = Optic.set (PeerChannelEncryptor.NoiseState_)
                                                         (Finished({
                                                            FinishedNoiseState.SK = sk |> uint256
                                                            SN = 0UL
                                                            SCK = ck
                                                            RK = rk |> uint256
                                                            RN= 0UL
                                                            RCK = ck
                                                         }))
                                                         pce3
                                    Ok(newPce.TheirNodeId.Value, newPce)
                | _ -> failwith "wrong direction for act"
            | _ -> failwith "Cannot get act one after noise handshake completes"

        let private encryptCore (msg: byte[]) (pceLocal) =
            let sn = (Optic.get (PeerChannelEncryptor.SN_) pceLocal).Value
            let sk = (Optic.get (PeerChannelEncryptor.SK_) pceLocal).Value
            let lengthBytes = BitConverter.GetBytesBE(uint16 msg.Length)
            // 2 byte length + 16 bytes MAC
            let cipherLength = crypto.encryptWithAD (sn, sk, ReadOnlySpan([||]) , ReadOnlySpan(lengthBytes))
            let newSN = (sn + 1UL)
            let pce3 = pceLocal |> Optic.set (PeerChannelEncryptor.SN_) (newSN)
            let cipherText = crypto.encryptWithAD (newSN, sk, ReadOnlySpan[||], ReadOnlySpan(msg))
            let pce4 = pce3 |> Optic.set (PeerChannelEncryptor.SN_) (newSN + 1UL)
            Array.concat (seq [ cipherLength; cipherText ]), pce4

        let encryptMessage (msg: byte[]) (pce: PeerChannelEncryptor): byte[] * PeerChannelEncryptor =
            if msg.Length > 65535 then failwith "Attempted encrypt message longer thaan 65535 bytes"
            match pce.NoiseState with
            | Finished { SK = sk; SN = sn; SCK = sck } ->
                let refreshState(pceLocal) = 
                    let (newSCK, newSK) = hkdfExtractExpand(sck.ToBytes(), sk.ToBytes())
                    pceLocal
                    |> Optic.set (PeerChannelEncryptor.SCK_)
                                 (uint256 newSCK)
                    |> Optic.set (PeerChannelEncryptor.SK_)
                                 (uint256 newSK)
                    |> Optic.set (PeerChannelEncryptor.SN_)
                                 (0UL)

                if sn >= 1000UL then
                    let newState = refreshState(pce)
                    encryptCore(msg) newState
                else
                    encryptCore (msg) pce
            | _ -> failwith "Tried to encrypt a message prior to noise handshake completion"

        let private decryptCore (msg) (localPce) =
            let newRK = (Optic.get (PeerChannelEncryptor.RK_) localPce).Value
            let newRN = (Optic.get (PeerChannelEncryptor.RN_) localPce).Value
            decryptWithAD(newRN, newRK, [||], ReadOnlySpan(msg))
            >>= fun plainText ->
                let newPCE =
                    localPce
                    |> Optic.set (PeerChannelEncryptor.RN_) (newRN + 1UL)
                (BitConverter.ToUint16BE(plainText), newPCE) |> Ok
        let decryptLengthHeader (msg: byte[]) (pce): Result<uint16 * PeerChannelEncryptor, PeerError> =
            if (msg.Length <> 16 + 2) then raise <| ArgumentException(sprintf "Invalid length of message %d" msg.Length)
            match pce.NoiseState with
            | Finished { RK = rk; RN = rn; RCK = rck } ->
                let refreshState (localPce) =
                    let (newRCK, newRK) = hkdfExtractExpand(rck.ToBytes(), rk.ToBytes())
                    localPce
                    |> Optic.set (PeerChannelEncryptor.RCK_)
                                 (uint256 newRCK)
                    |> Optic.set (PeerChannelEncryptor.RK_)
                                 (uint256 newRK)
                    |> Optic.set (PeerChannelEncryptor.RN_)
                                 (0UL)

                if (rn >= 1000UL) then
                    let newState = refreshState pce
                    decryptCore msg (newState)
                else
                    decryptCore msg (pce)

            | _ -> failwith "Tried to encrypt a message prior to noies handshake completion"

        let decryptMessage (msg: byte[]) (pce) = 
            if (msg.Length > 16 + 65535) then raise <| ArgumentException(sprintf "Invalid length of message %d" msg.Length)
            match pce.NoiseState with
            | Finished { RK = rk; RN  = rn;} ->
                decryptWithAD(rn, rk, [||], ReadOnlySpan(msg))
                >>= fun plainText ->
                    let newState =
                        pce |> Optic.set (PeerChannelEncryptor.RN_) (rn + 1UL)
                    Ok(plainText, newState)
            | _ -> failwith "Tried to encyrpt a message prior to noise handshake completion"


/// Might remove in the future
[<AutoOpen>]
module PeerChannelEncryptorMonad =
    type PeerChannelEncryptorComputation<'a> =
        PeerChannelEncryptorComputation of (PeerChannelEncryptor -> Result<'a * PeerChannelEncryptor, PeerError>)

    let runP pcec initialState =
        let (PeerChannelEncryptorComputation innerFn)= pcec
        innerFn initialState

    let returnP x =
        let innerFn state = 
            Ok(x, state)
        PeerChannelEncryptorComputation innerFn

    let bindP (f: 'a -> PeerChannelEncryptorComputation<'b>) (xT: PeerChannelEncryptorComputation<'a>): PeerChannelEncryptorComputation<'b> =
        let innerFn state =
            runP xT state
            >>= fun (res, state2) ->
                let h = runP (f res) state2
                h
        PeerChannelEncryptorComputation innerFn

    let mapP f =
        bindP (f >> returnP)

    /// Lift non-failable function to monadic world
    let fromPlainFunction (f: PeerChannelEncryptor -> 'a * PeerChannelEncryptor) =
        let innerFn state =
            let (result, newState) = f state
            Ok(result, newState)
        PeerChannelEncryptorComputation innerFn

    /// Lift non-failable reader function to monadic world
    let fromReaderFunction (f: PeerChannelEncryptor -> 'a) =
        let innerFn state =
            let result = f state
            result, state
        fromPlainFunction innerFn

    /// Lift non-failable writer function to monadic world
    let fromWriterFunction f =
        let f2 state =
            (), f state
        fromPlainFunction f2

    let fromFailableFunction (f: PeerChannelEncryptor -> Result<'a * PeerChannelEncryptor, _>) =
        PeerChannelEncryptorComputation f

    let fromFailableReaderFunction (f: PeerChannelEncryptor -> Result<'a, _>) =
        let innerFn state =
            f state
            >>= fun result ->
                Ok (result, state)
        PeerChannelEncryptorComputation innerFn


    let processActOneWithKey (actOne) (key) =
        fromFailableFunction(PeerChannelEncryptor.processActOneWithKey actOne key)

    let processActOneWithEphemeralKey (actOne) (key) (ourEphemeral) =
        fromFailableFunction(PeerChannelEncryptor.processActOneWithEphemeralKey actOne key ourEphemeral)

    let processActTwo (actTwo) (ourNodeSecret) =
        fromFailableFunction (PeerChannelEncryptor.processActTwo actTwo ourNodeSecret)

    let processActThree (actThree) =
        fromFailableFunction (PeerChannelEncryptor.processActThree actThree)

    let encryptMessage (msg) =
        fromPlainFunction(PeerChannelEncryptor.encryptMessage msg)

    let decryptMessage (msg) =
        fromFailableFunction(PeerChannelEncryptor.decryptMessage msg)

    let decryptLengthHeader (msg) =
        fromFailableFunction(PeerChannelEncryptor.decryptLengthHeader msg)

    type PeerChannelEncryptorComputationBuilder() =
        member this.Return(x) = returnP x
        member this.ReturnFrom(x) = x
        member this.Bind(x, f) = bindP f x
        member this.Zero() = returnP ()

    let noise = PeerChannelEncryptorComputationBuilder()
