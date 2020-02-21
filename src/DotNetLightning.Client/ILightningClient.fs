namespace DotNetLightning.Client

open System
open DotNetLightning.Payment
open DotNetLightning.Utils
open System.Threading
open System.Threading.Tasks

open DotNetLightning.Infrastructure.DTOs

type ILightningClient =
    abstract member GetInvoice: invoiceId: string * ct: CancellationToken -> Task<PaymentRequest>
    abstract member CreateInvoice: amount: LNMoney * description: string * expiry: TimeSpan * ct:CancellationToken -> Task<PaymentRequest>
    abstract member GetInfo: unit -> Task<GetInfoResponse>
    abstract member Pay: PaymentRequest * ct: CancellationToken -> Task<OurPaymentResult>
    abstract member OpenChannel: OpenChannelRequest * ct: CancellationToken -> Task<OpenChannelResponse>
