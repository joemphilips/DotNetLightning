namespace DotNetLightning.Infrastructure.Services

open System.IO
open DotNetLightning.Serialization
open DotNetLightning.Infrastructure

type FlatFileDB(path: string, codec: SupportedCodec) =
    let _path = path

    member this.Write() =
        use fs = new FileStream(_path, FileMode.Open)
        use stream = new LightningWriterStream(fs)
        failwith "Not implemented: FlatFileDB::Write"


