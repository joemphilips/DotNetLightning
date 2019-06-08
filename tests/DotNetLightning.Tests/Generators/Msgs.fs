module MsgGenerators

open DotNetLightning.Serialize.Msgs
open FsCheck

let private globalFeaturesGen =
    Arb.generate<uint8[]> |> Gen.map GlobalFeatures.Flags

let private localFeaturesGen =
    Arb.generate<uint8[]> |> Gen.map LocalFeatures.Flags

let initGen =
    Gen.map2 (fun g l -> { GlobalFeatures = g; LocalFeatures = l})
        globalFeaturesGen
        localFeaturesGen
