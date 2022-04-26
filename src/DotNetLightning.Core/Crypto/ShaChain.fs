namespace DotNetLightning.Crypto

type Node =
    {
        Value: array<byte>
        Height: int32
        Parent: option<Node>
    }

type ShaChain =
    {
        KnownHashes: Map<array<bool>, array<byte>>
        LastIndex: option<uint64>
    }

    static member Zero =
        {
            KnownHashes = Map.empty
            LastIndex = None
        }

module ShaChain =
    let flip (_input: array<byte>) (_index: uint64) : array<byte> =
        failwith "Not implemented: ShaChain::flip"

    let addHash (_receiver: ShaChain) (_hash: array<byte>) (_index: uint64) =
        failwith "Not implemented: ShaChain::addHash"

    let getHash (_receiver: ShaChain) (_index: uint64) =
        failwith "Not implemented: ShaChain::getHash"

type ShaChain with

    member this.AddHash(hash: array<byte>, index: uint64) : ShaChain =
        ShaChain.addHash this hash index

    member this.GetHash(index: uint64) : ShaChain =
        ShaChain.getHash this index
