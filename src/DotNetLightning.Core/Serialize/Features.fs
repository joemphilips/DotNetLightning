namespace DotNetLightning.Serialize

open System.Collections

open ResultUtils

open DotNetLightning.Core.Utils.Extensions

type FeaturesSupport =
    | Mandatory
    | Optional

/// Feature bits specified in BOLT 9.
/// It has no constructors, use its static members to instantiate
type Feature = private {
    RfcName: string
    Mandatory: int
}
    with
    member this.MandatoryBitPosition = this.Mandatory
    member this.OptionalBitPosition = this.Mandatory + 1
    override this.ToString() = this.RfcName

    static member OptionDataLossProtect = {
        RfcName = "option_data_loss_protect"
        Mandatory = 0
    }
        
    static member InitialRoutingSync = {
        RfcName = "initial_routing_sync"
        Mandatory = 2
    }
    
    static member ChannelRangeQueries = {
        RfcName = "gossip_queries"
        Mandatory = 6
    }
    
    static member VariableLengthOnion = {
        RfcName = "var_onion_optin"
        Mandatory = 8
    }
    
    static member ChannelRangeQueriesExtended = {
        RfcName = "gossip_queries_ex"
        Mandatory = 10
    }
    
    static member PaymentSecret = {
        RfcName = "payment_secret"
        Mandatory = 14
    }
    
    static member BasicMultiPartPayment = {
        RfcName = "basic_mpp"
        Mandatory = 16
    }
    
module internal Feature =
    /// Features may depend on other features, as specified in BOLT 9
    let private featuresDependency =
        Map.empty
        |> Map.add (Feature.ChannelRangeQueriesExtended) ([Feature.ChannelRangeQueries])
        |> Map.add (Feature.BasicMultiPartPayment) ([Feature.PaymentSecret])
        
    let private isFeatureOn(features: BitArray) (bit: int) =
        (features.Length > bit) && features.Reverse().[bit]
        
    let hasFeature(features: BitArray) (f: Feature) (support: FeaturesSupport option) =
        match support with
        | Some(Mandatory) ->
            isFeatureOn(features) (f.Mandatory)
        | Some(Optional) ->
            isFeatureOn(features) (f.OptionalBitPosition)
        | None ->
            isFeatureOn(features) (f.OptionalBitPosition) || isFeatureOn(features) (f.Mandatory)
        
    let private printDeps (deps: #seq<Feature>) (features) =
        deps
        |> Seq.filter(fun d -> not <| (hasFeature(features) (d) (None) ))
        |> Seq.map(fun d -> d.ToString())
        |> String.concat " and "
    let validateFeatureGraph(features: BitArray) =
        result {
            let mutable c = 0
            for kvp in featuresDependency do
                c <- c + 1
                let f = kvp.Key
                let deps = kvp.Value
                if hasFeature(features) (f) (None) && deps |> List.exists(fun d -> not <| hasFeature(features) (d) (None)) then
                    return!
                        sprintf
                            "%s sets %s but is missing a dependency %s "
                            (features.PrintBits())
                            (f.ToString())
                            (printDeps deps features)
                        |> Error
                else
                    return ()
        }
        
    /// Check that the features that we understand are correctly specified, and that there are no mandatory features
    /// we don't understand
    let areSupported(features: BitArray) =
        let supportedMandatoryFeatures =
            seq { Feature.OptionDataLossProtect
                  Feature.ChannelRangeQueries
                  Feature.VariableLengthOnion
                  Feature.ChannelRangeQueriesExtended
                  Feature.PaymentSecret
                  Feature.BasicMultiPartPayment }
            |> Seq.map(fun f -> f.Mandatory)
            |> Set
            
        let reversed = features.Reverse()
        seq {
            for i in 0..reversed.Length - 1 do
                if (i % 2 = 0) then
                    yield i
        }
        |> Seq.exists(fun i -> reversed.[i] && not <| supportedMandatoryFeatures.Contains(i))
        |> not
        
type FeatureBit private (v: byte[], bitArray) =
    member val BitArray = bitArray with get, set
    member val Value = v with get, set
    static member TryCreate(bytes:byte[]) =
        result {
            let ba = BitArray.FromBytes(bytes)
            do! Feature.validateFeatureGraph(ba)
            if not <| Feature.areSupported(ba) then
                return! Error(sprintf "feature bits (%s) contains a mandatory flag that we don't know!" (ba.PrintBits()))
            else
                return (FeatureBit(bytes, ba))
        }
