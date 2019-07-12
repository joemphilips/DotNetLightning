namespace DotNetLightning.Crypto

type Node = {
    Value: byte[]
    Height: int32
    Parent: Node option
}

type ShaChain = {
    KnownHashes: Map<bool[], byte[]>
    LastIndex: int32 option
}
    with    
        static member Zero = { KnownHashes = Map.empty; LastIndex = None }
module ShaChain =
    let flip (input: byte[]) (index: int32): byte[] =
        failwith ""

    let addHash (receiver: ShaChain) (hash: byte[]) (index: int32) =
        failwith ""

    let getHash (receiver: ShaChain)(index: int32) =
        failwith  ""

type ShaChain with
    member this.AddHash(hash: byte[], index: int32): ShaChain =
        ShaChain.addHash this hash index
    member this.GetHash(index: int32): ShaChain =
        ShaChain.getHash this index
