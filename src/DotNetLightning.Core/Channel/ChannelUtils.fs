namespace DotNetLightning.Channel

open NBitcoin
open NBitcoin.Crypto

module ChannelUtils =
    
    /// Various functions for key derivation and tx creation for use within channels. Primarily used in Channel
    let buildCommitmentSecret (commitmentSeed: uint256, index: uint64): Key =
        let mutable res = commitmentSeed.ToBytes()
        for i in 0..47 do
            let bitpos = 47 - i
            if index &&& (1UL <<< bitpos) = (1UL <<< bitpos) then
                res.[bitpos / 8] <- (res.[bitpos / 8] ^^^ (1uy <<< (7 &&& bitpos)))
                res <- Hashes.SHA256(res)
        res |> Key
    
    let buildCommitmentPoint(seed: uint256, index: uint64) =
        buildCommitmentSecret(seed, index) |> fun k -> k.PubKey

