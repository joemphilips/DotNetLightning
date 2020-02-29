namespace DotNetLightning.Serialize

open System.Collections

open ResultUtils

open System
open System.Text
open DotNetLightning.Core.Utils.Extensions

type FeaturesSupport =
    | Mandatory
    | Optional

type FeatureError =
    | UnknownRequiredFeature of msg: string
    | BogusFeatureDependency of msg: string
    override this.ToString() =
        match this with
        | UnknownRequiredFeature msg
        | BogusFeatureDependency msg -> msg

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
    
    static member OptionUpfrontShutdownScript = {
        RfcName = "option_upfront_shutdown_script"
        Mandatory = 4
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
    
    static member OptionStaticRemoteKey = {
        RfcName = "option_static_remotekey"
        Mandatory = 12
    }
    
    static member PaymentSecret = {
        RfcName = "payment_secret"
        Mandatory = 14
    }
    
    static member BasicMultiPartPayment = {
        RfcName = "basic_mpp"
        Mandatory = 16
    }
    
    static member OptionSupportLargeChannel = {
        RfcName = "option_support_large_channel"
        Mandatory = 18
    }
    
module internal Feature =
    /// Features may depend on other features, as specified in BOLT 9
    let private featuresDependency =
        Map.empty
        |> Map.add (Feature.ChannelRangeQueriesExtended) ([Feature.ChannelRangeQueries])
        |> Map.add (Feature.BasicMultiPartPayment) ([Feature.PaymentSecret])
        |> Map.add (Feature.PaymentSecret) ([Feature.VariableLengthOnion])
        
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
            for kvp in featuresDependency do
                let f = kvp.Key
                let deps = kvp.Value
                if hasFeature(features) (f) (None) && deps |> List.exists(fun d -> not <| hasFeature(features) (d) (None)) then
                    return!
                        sprintf
                            "%s sets %s but is missing a dependency %s "
                            (features.PrintBits())
                            (f.ToString())
                            (printDeps deps features)
                        |> FeatureError.BogusFeatureDependency
                        |> Error
                else
                    return ()
        }
        
    let private supportedMandatoryFeatures =
        seq { Feature.OptionDataLossProtect
              Feature.InitialRoutingSync
              Feature.OptionUpfrontShutdownScript
              Feature.ChannelRangeQueries
              Feature.VariableLengthOnion
              // TODO: support this feature
              // Feature.ChannelRangeQueriesExtended
              Feature.OptionStaticRemoteKey
              Feature.PaymentSecret
              // TODO: support this feature
              // Feature.BasicMultiPartPayment
          }
        |> Seq.map(fun f -> f.Mandatory)
        |> Set
    /// Check that the features that we understand are correctly specified, and that there are no mandatory features
    /// we don't understand
    let areSupported(features: BitArray) =
            
        let reversed = features.Reverse()
        seq {
            for i in 0..reversed.Length - 1 do
                if (i % 2 = 0) then
                    yield i
        }
        |> Seq.exists(fun i ->
            reversed.[i] && not <| supportedMandatoryFeatures.Contains(i)
            )
        |> not
        
    let allFeatures =
        seq {
            Feature.OptionDataLossProtect
            Feature.InitialRoutingSync
            Feature.OptionUpfrontShutdownScript
            Feature.ChannelRangeQueries
            Feature.VariableLengthOnion
            Feature.ChannelRangeQueriesExtended
            Feature.OptionStaticRemoteKey
            Feature.PaymentSecret
            Feature.BasicMultiPartPayment
            Feature.OptionSupportLargeChannel
        }
        |> Set
        
        
/// Uses regular class instead of F# type for caching byte[] representation
[<StructuredFormatDisplay("{PrettyPrint}")>]
type FeatureBit private (bitArray) =
    let mutable bytes = null
    member val BitArray: BitArray = bitArray with get, set
    member this.ByteArray
        with get() =
            if isNull bytes then
                bytes <- this.BitArray.ToByteArray()
            bytes
        and set(v: byte[]) = bytes <- v
    static member TryCreate(ba: BitArray) =
        result {
            do! Feature.validateFeatureGraph(ba)
            if not <| Feature.areSupported(ba) then
                return!
                    sprintf "feature bits (%s) contains a mandatory flag that we don't know!" (ba.PrintBits())
                    |> FeatureError.UnknownRequiredFeature
                    |> Error
            else
                return (FeatureBit(ba))
        }
        
    static member TryCreate(bytes: byte[]) =
        result {
            let! fb = FeatureBit.TryCreate(BitArray.FromBytes(bytes))
            fb.ByteArray <- bytes
            return fb
        }
        
    static member TryCreate(v: int64) =
        BitArray.FromInt64(v) |> FeatureBit.TryCreate
        
    static member CreateUnsafe(v: int64) =
        BitArray.FromInt64(v) |> FeatureBit.CreateUnsafe
        
    static member private Unwrap(r: Result<FeatureBit, _>) =
        match r with
        | Error(FeatureError.UnknownRequiredFeature(e))
        | Error(FeatureError.BogusFeatureDependency(e)) -> raise <| FormatException(e)
        | Ok fb -> fb
    /// Throws FormatException
    /// TODO: ugliness of this method is caused by binary serialization throws error instead of returning Result
    /// We should refactor serialization altogether at some point
    static member CreateUnsafe(bytes: byte[]) =
        FeatureBit.TryCreate bytes |> FeatureBit.Unwrap
        
    static member CreateUnsafe(ba: BitArray) =
        FeatureBit.TryCreate ba |> FeatureBit.Unwrap
    static member TryParse(str: string) =
        result {
            let! ba = BitArray.TryParse str
            return! ba |> FeatureBit.TryCreate |> Result.mapError(fun fe -> fe.ToString())
        }
        
    override this.ToString() =
        this.BitArray.PrintBits()
        
    member this.HasFeature(f, ?featureType) =
        Feature.hasFeature this.BitArray (f) (featureType)
        
    member this.PrettyPrint =
        let sb = StringBuilder()
        let reversed = this.BitArray.Reverse()
        for f in Feature.allFeatures do
            if (reversed.[f.MandatoryBitPosition]) then
                sb.Append(sprintf "%s is mandatory. " f.RfcName) |> ignore
            else if (reversed.[f.OptionalBitPosition]) then
                sb.Append(sprintf "%s is optional. " f.RfcName) |> ignore
            else
                ()
        sb.ToString()
    
    member this.ToByteArray() = this.ByteArray
        
    member this.Equals(o: FeatureBit) =
        if this.BitArray.Length <> o.BitArray.Length then false else
        let mutable result = true
        for i in 0..this.BitArray.Length - 1 do
            if this.BitArray.[i] <> o.BitArray.[i] then
                result <- false
        result
    interface IEquatable<FeatureBit> with
        member this.Equals(o: FeatureBit) = this.Equals(o)
    override this.Equals(other: obj) =
        match other with
        | :? FeatureBit as o -> this.Equals(o)
        | _ -> false
        
    override this.GetHashCode() =
        let mutable num = 0
        for i in this.BitArray do
            num <- -1640531527 + i.GetHashCode() + ((num <<< 6) + (num >>> 2))
        num
