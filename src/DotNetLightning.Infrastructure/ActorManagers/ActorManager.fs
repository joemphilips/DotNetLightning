namespace DotNetLightning.Infrastructure.ActorManagers

open System.Threading.Tasks


type IActorManager<'TContextCommand> =
    abstract member AcceptCommandAsync: 'TContextCommand -> ValueTask