namespace DotNetLightning.Client.CLightning.DTO


type GetInfoAddress =  {
    Type: string
    Address: string
    Port: int
}
type GetInfoResponse = {
    Id: string
    Address: GetInfoAddress list
    Version: string
    BlockHeight: int
    Network: int
}