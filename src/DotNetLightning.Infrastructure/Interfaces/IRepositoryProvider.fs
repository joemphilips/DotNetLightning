namespace DotNetLightning.Infrastructure.Interfaces

open DotNetLightning.Infrastructure

type IRepositoryProvider =
    abstract member TryGetRepository: DotNetLightningNetwork -> IRepository option


