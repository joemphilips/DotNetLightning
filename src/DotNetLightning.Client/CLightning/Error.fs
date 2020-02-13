namespace DotNetLightning.Client.CLightning

/// ref: https://github.com/ElementsProject/lightning/blob/a47fd8cf3e1c04fb4f3a6b7a7d0c5d8160c2cd69/common/jsonrpc_errors.h
type LightningRPCErrorType =
    // --- Standard errors defined by JSON-RPC 2.0 standard ---
    | JsonRPC2_InvalidRequest
    | JsonRPC2_MethodNotFound
    | JsonRPC2_InvalidParams
    // ------
    | PluginError
    | PayInProgress
    | PayRHashAlreadyUsed
    | PayUnParsableOnion
    | PayDestinationPermFail
    | PayTryOtherRoute
    | Unknown of code: int
    
    with
        
    static member FromCode errorCode =
        match errorCode with
        | -32600 -> JsonRPC2_InvalidParams
        | -32601 -> JsonRPC2_MethodNotFound
        | -32602 -> JsonRPC2_InvalidParams
        | -3 -> PluginError
        | x ->  Unknown x
        
type LightningRPCError =  {
    Msg: string
    Code: LightningRPCErrorType
}
exception LightningRPCException of LightningRPCError
    with
    static member Create msg code =
        {
            Msg = msg
            Code = LightningRPCErrorType.FromCode code
        } |> LightningRPCException

