namespace DotNetLightning.Infrastructure.Interfaces

type IRepository =
    abstract member Insert: key: string -> unit
