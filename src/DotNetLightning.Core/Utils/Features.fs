namespace DotNetLightning.Utils

open System.Collections

type FeatureSupport =
    | Mandatory
    | Optional

type Feature =
    | OptionDataLossProtect
    | InitialRoutingSync
    | ChannelRangeQueries
    | VariableLengthOnion
    | PaymentSecret
    | BasicMultiPartPayment
    with
    member this.RFCName =
        match this with
        | OptionDataLossProtect -> "option_data_loss_protect"
        | InitialRoutingSync -> "initial_routing_sync"
        | ChannelRangeQueries -> "gossip_queries"
        | VariableLengthOnion -> "var_onion_optin"
        | PaymentSecret -> "payment_secret"
        | BasicMultiPartPayment -> "basic_mpp"
    member this.Mandatory =
        match this with
        | OptionDataLossProtect -> 0
        | InitialRoutingSync -> 2
        | ChannelRangeQueries -> 6
        | VariableLengthOnion -> 8
        | PaymentSecret -> 14
        | BasicMultiPartPayment -> 16
    member this.Optional =
        match this with
        | x -> x.Mandatory + 1

    override this.ToString() = this.RFCName
    
module Features =
    /// Features may depend on other features, as specified in BOLT 9
    let internal featuresDependency =
        Map.empty
        |> Map.add (ChannelRangeQueries) ([ChannelRangeQueries])
        |> Map.add (PaymentSecret) ([VariableLengthOnion])
        |> Map.add (BasicMultiPartPayment) ([PaymentSecret])
        
type LocalFeatures =
    Flags of BitArray
        member this.Value = let (Flags v) = this in v
        member private this.SizeGreaterThan(bit: int) = failwith ""
        member private this.HasFeature(bit: int) =
            this.SizeGreaterThan(bit) && (this.Value |> Array.rev |> fun x -> x.[bit] = 1uy)
        member private this.HasFeature(feature: Feature, ?support: FeatureSupport) =
            match support with
            | Some(Mandatory) -> this.HasFeature(feature.Mandatory)
            | Some(Optional) -> this.HasFeature(feature.Optional)
            | None ->this.HasFeature(feature.Optional) && this.HasFeature(feature.Mandatory)
        
        member this.ValidateFeaturesGraph() =
            Features.featuresDependency
            |> Map.filter (fun feature deps ->
                this.HasFeature(feature) &&
                (deps |> Seq.exists(fun d -> not <| this.HasFeature(d))))
                
type GlobalFeatures =
    Flags of BitArray
        member x.Value = let (Flags v) = x in v

        member x.RequiresUnknownBits() =
            x.Value
            |> Array.exists(fun b -> b &&& 0b01010101uy <> 0uy)
        
        member x.SupportsUnknownBits() =
            x.Value
            |> Array.exists(fun b -> b <> 0uy)

