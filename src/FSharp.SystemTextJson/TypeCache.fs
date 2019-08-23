namespace System.Text.Json.Serialization

module TypeCache =
    open FSharp.Reflection

    type Dict<'a, 'b> = System.Collections.Concurrent.ConcurrentDictionary<'a, 'b>

    let isUnion =
        let cache = Dict<System.Type, bool>()
        fun (ty: System.Type) ->
            cache.GetOrAdd(ty, (fun ty -> FSharpType.IsUnion(ty, true)))

    let isRecord =
        let cache = Dict<System.Type,  bool>()
        fun (ty: System.Type) ->
            cache.GetOrAdd(ty, (fun ty -> FSharpType.IsRecord(ty, true)))