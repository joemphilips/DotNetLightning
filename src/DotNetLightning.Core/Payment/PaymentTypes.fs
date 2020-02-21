namespace DotNetLightning.Payment
open System

type PaymentId = private PaymentId of string
    with
        static member Create() =
            PaymentId (Guid.NewGuid().ToString())
            
[<AutoOpen>]
module PaymentType =
    let (|PaymentId|) (PaymentId x) = x
        
