module DotNetLightning.Core.Utils.Extensions

open System.Collections.Generic
open System.Runtime.CompilerServices

module Dict =
    let tryGetValue key (dict: IDictionary<_,_>)=
       match dict.TryGetValue key with
       | true, v -> Some v
       | false, _ -> None
[<Extension>]
type DictionaryExtensions() =

    [<Extension>]
    static member TryGetValueOption(this: IDictionary<_, _>, key) =
        Dict.tryGetValue key this
