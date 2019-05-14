module DotNetLightning.Serialize.PeerChannelEncryptor
open NBitcoin
open System.IO
open DotNetLightning.Utils.Primitives

// Sha256("Noise_XK_secp256k1_ChaChaPoly_SHA256")
let NOISE_CK = [0x26uy, 0x40, 0xf5, 0x2e, 0xeb, 0xcd, 0x9e, 0x88, 0x29, 0x58, 0x95, 0x1c, 0x79, 0x42, 0x50, 0xee, 0xdb, 0x28, 0x00, 0x2c, 0x05, 0xd7, 0xdc, 0x2e, 0xa0, 0xf1, 0x95, 0x40, 0x60, 0x42, 0xca, 0xf1] |> Seq.cast<byte>

// Sha256(NOISE_CK || "lightning")
let NOISE_H = [0xd1, 0xfb, 0xf6, 0xde, 0xe4, 0xf6, 0x86, 0xf1, 0x32, 0xfd, 0x70, 0x2c, 0x4a, 0xbf, 0x8f, 0xba, 0x4b, 0xb4, 0x20, 0xd8, 0x9d, 0x2a, 0x04, 0x8a, 0x3c, 0x4f, 0x4c, 0x09, 0x2e, 0x37, 0xb6, 0x76]

type NextNoiseStep =
    | ActOne
    | ActTwo
    | ActThree
    | NoiseComplete

type NoiseStep =
    | PreActOne
    | PostActone
    | PostActTwo

type BidirectinalNoiseState = {
    H: uint256
    /// The chaining key. This value is the accumulated hash of all previous ECDH outputs.
    /// At the end of the handshake, it is used for deriving the encryption keys for Lightning Messages.
    CK: uint256
}

type DirectionalNoisestate = {
        OutBound: OutBound
    }
and OutBound = { IE: Key }
and InBound = {
    IE: PubKey option
    RE: Key option
    TempK2: uint256 option
}

type NoiseState = {
    InProgress: InProgressNoiseState
    Finished: FinishedNoiseState
}
and InProgressNoiseState = {
    State: NoiseStep
    DirectionalState: DirectionalNoisestate
    BidirectinalState: BidirectinalNoiseState
}
and FinishedNoiseState = {
    SK: uint256
    SN: uint64
    SCK: uint256
    RK: uint256
    RN: uint64
    RCK: uint256
}

type PeerChannelEncryptorStream(theirNodeId: NodeId option, NoiseState: NoiseState, inner: Stream) =
    do failwith "not impl"

