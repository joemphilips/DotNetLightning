namespace DotNetLightning.Crypto

open System

type Node = {
    Value: byte[]
    Height: int32
    Parent: Node option
}

type ShaChain = {
    KnownHashes: Map<bool[], byte[]>
    LastIndex: uint64 option
}
    with    
        static member Zero = { KnownHashes = Map.empty; LastIndex = None }
module ShaChain =
    let flip (_input: byte[]) (_index: uint64): byte[] =
        failwith "Not implemented: ShaChain::flip"

    let addHash (receiver: ShaChain) (_hash: byte[]) (_index: uint64) =
        Console.WriteLine("WARNING: Not implemented: ShaChain::addHash")
        receiver

    let getHash (_receiver: ShaChain)(_index: uint64) =
        failwith "Not implemented: ShaChain::getHash"

type ShaChain with
    member this.AddHash(hash: byte[], index: uint64): ShaChain =
        ShaChain.addHash this hash index

    member this.GetHash(index: uint64): ShaChain =
        ShaChain.getHash this index
