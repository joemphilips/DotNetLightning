namespace DotNetLightning.Crypto

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
    let flip (input: byte[]) (index: uint64): byte[] =
        failwith ""

    let addHash (receiver: ShaChain) (hash: byte[]) (index: uint64) =
        failwith ""

    let getHash (receiver: ShaChain)(index: uint64) =
        failwith  ""

type ShaChain with
    member this.AddHash(hash: byte[], index: uint64): ShaChain =
        ShaChain.addHash this hash index

    member this.GetHash(index: uint64): ShaChain =
        ShaChain.getHash this index
