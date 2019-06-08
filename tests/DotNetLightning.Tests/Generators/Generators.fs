module Generators

open DotNetLightning.Serialize.Msgs
open FsCheck
open MsgGenerators


type P2PMsgGenerators =
    static member Init() : Arbitrary<Init> =
        Arb.fromGen(initGen)

    static member Ping() : Arbitrary<Ping> =
        Arb.fromGen(pingGen)

    static member Pong() : Arbitrary<Pong> =
        Arb.fromGen(pongGen)

    static member OpenChannel(): Arbitrary<OpenChannel> =
        Arb.fromGen(openChannelGen)