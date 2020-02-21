namespace DotNetLightning.Infrastructure.Services

open DotNetLightning.Infrastructure.Interfaces

type RepositoryProvider() =
    do failwith ""
    interface IRepositoryProvider with
        member this.TryGetRepository(network) =
            failwith ""
