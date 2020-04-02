namespace DotNetLightning.Infrastructure.Services

open DotNetLightning.Infrastructure.Interfaces

type RepositoryProvider() =
    do failwith "Not implemented: RepositoryProvider::ctor"
    interface IRepositoryProvider with
        member this.TryGetRepository(network) =
            failwith "Not implemented: RepositoryProvider::TryGetRepository"
