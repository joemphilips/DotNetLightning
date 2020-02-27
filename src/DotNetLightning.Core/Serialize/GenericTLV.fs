namespace DotNetLightning.Serialize

type EncodingType =
    | Uncompressed
    | ZLibCompressed

type GenericTLV = {
    Type: uint64
    Value: byte[]
}
