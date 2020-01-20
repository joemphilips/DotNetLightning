namespace DotNetLightning.Channel

open DotNetLightning.Crypto
open DotNetLightning.Transactions
open DotNetLightning.Utils
open DotNetLightning.Utils.Error
open NBitcoin

[<StructuredFormatDisplay("{ToString}")>]
type ChannelError =
    | CryptoError of CryptoError
    | TransactionRelatedErrors of TransactionError list
    
    | InvalidMessage of MessageValidationError
    | HTLCAlreadySent of HTLCId
    | InvalidPaymentPreimage of expectedHash: PaymentHash * actualPreimage: PaymentPreimage
    | UnknownHTLCId of HTLCId
    | HTLCOriginNotKnown of HTLCId
    | InvalidFailureCode of ErrorCode
    /// Consumer of the channel domain have given invalid command
    | APIMisuse of string
    | WeCannotAffordFee of localChannelReserve: Money * requiredFee: Money * missingAmount: Money
    | CanNotSignBeforeRevocation
    | ReceivedCommitmentSignedWhenWeHaveNoPendingChanges
    | SignatureCountMismatch of expected: int * actual: int
    /// When we create the first commitment txs as fundee,
    /// There might be a case that their initial funding amount is too low that it
    /// cannot afford fee
    | TheyCannotAffordFee of toRemote: LNMoney * fee: Money * channelReserve: Money
    
    override this.ToString() =
        match this with
        | HTLCAlreadySent htlcId ->
            sprintf "We have already sent a fail/fulfill for this htlc: %A" htlcId
        | InvalidPaymentPreimage(e, actual) ->
            sprintf "Invalid HTLC PreImage %A. Hash (%A) does not match the one expected %A"
                    actual
                    (actual.GetSha256())
                    e
        | InvalidFailureCode errorCode ->
            sprintf "invalid failure code %A" (errorCode.GetOnionErrorDescription())
        | WeCannotAffordFee (channelReserve, fees, missing) ->
            sprintf
                "Cannot afford fees. Missing Satoshis are: %A . Reserve Satoshis are %A . Fees are %A"
                channelReserve fees (missing)
        | SignatureCountMismatch(expected, actual) ->
            sprintf "Number of signatures went from the remote (%A) does not match the number expected (%A)" actual expected
        | TheyCannotAffordFee (toRemote, fee, channelReserve) ->
            sprintf "they are funder but cannot afford their fee. to_remote output is: %A; actual fee is %A; channel_reserve_satoshis is: %A" toRemote fee channelReserve
        | x -> sprintf "%A" x

/// Helpers to create channel error
[<AutoOpen>]
module internal ChannelError =
    let inline feeDeltaTooHigh x =
        x |> (FeeDeltaTooHigh >> InvalidMessage >> Error)
        
    let inline htlcAlreadySent htlcId =
        htlcId |> HTLCAlreadySent |> Error
        
    let inline invalidPaymentPreimage (e, a) =
        (e, a) |> InvalidPaymentPreimage |> Error
        
    let inline unknownHTLCId x =
        x |> UnknownHTLCId |> Error
        
    let inline htlcOriginNowKnown x =
        x |> HTLCOriginNotKnown |> Error
    let inline invalidFailureCode x =
        x |> InvalidFailureCode |> Error
        
    let inline apiMisuse x =
        x |> APIMisuse |> Error
        
    let inline cannotAffordFee x =
        x |> WeCannotAffordFee |> Error
    
    let inline signatureCountMismatch x =
        x |> SignatureCountMismatch |> Error
        
    let inline theyCannotAffordFee x =
        x |> TheyCannotAffordFee |> Error
    
    let expectTransactionError result =
        Result.mapError (List.singleton >> TransactionRelatedErrors) result
        
    let expectTransactionErrors result =
        Result.mapError (TransactionRelatedErrors) result
        
