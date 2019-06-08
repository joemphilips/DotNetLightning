module Generators

open DotNetLightning.Serialize.Msgs
open FsCheck
open MsgGenerators


type P2PMsgGenerators =
    static member Init() : Arbitrary<Init> =
        Arb.fromGen(initGen)
