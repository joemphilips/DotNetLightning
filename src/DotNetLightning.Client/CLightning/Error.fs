namespace DotNetLightning.Client.CLightning

/// Errors from `pay`, `sendpay` or `waitsendpay` commands
type PaymentRPCError =
    | PayInProgress
    | PayRHashAlreadyUsed
    | PayUnParsableOnion
    | PayDestinationPermFail
    | PayTryOtherRoute
    | PayRouteNotFound
    | PayInvoiceExpired
    | PayNoSuchPayment
    | PayUnSpecifiedError
    | PayStoppedRetrying
    
type FundingRPCError =
    | FundMaxExceeded
    | FundCannotAfford
    | FundOutputIsDust
    | FundingBroadcastFailed
    | FundingStillSyncingBitcoin
    | FundingPeerNotConnected
    | FundingUnknownPeer
    
type ConnectRPCError =
    | ConnectNoKnownAddress
    | ConnectAllAddressFailed
    
type InvoiceRPCError =
    | InvoiceLabelAlreadyExists
    | InvoicePreimageAlreadyExists
    | InvoiceHintsGaveNoRoutes
    | InvoiceExpiredDuringWait
    | InvoiceWaitTimedOut
/// Standard errors defined by JSON-RPC 2.0 standard
type JsonRPCError =
    | JsonRPC2_InvalidRequest
    | JsonRPC2_MethodNotFound
    | JsonRPC2_InvalidParams
/// ref: https://github.com/ElementsProject/lightning/blob/a47fd8cf3e1c04fb4f3a6b7a7d0c5d8160c2cd69/common/jsonrpc_errors.h
type LightningRPCErrorType =
    | JsonError of JsonRPCError
    | PluginError
    | PaymentError of PaymentRPCError
    | FundingError of FundingRPCError
    | ConnectError of ConnectRPCError
    | InvoiceError of InvoiceRPCError
    // ------
    | Unknown of code: int
    
    with
        
    static member FromCode errorCode =
        match errorCode with
        | -32600 -> JsonRPC2_InvalidParams |> JsonError
        | -32601 -> JsonRPC2_MethodNotFound |> JsonError
        | -32602 -> JsonRPC2_InvalidParams |> JsonError
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

