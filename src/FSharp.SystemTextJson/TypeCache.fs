namespace System.Text.Json.Serialization

open System

module TypeCache =
    open FSharp.Reflection

    type Dict<'a, 'b> = System.Collections.Concurrent.ConcurrentDictionary<'a, 'b>

    
    type TypeKind =
        | Record = 0
        | Union = 1
        | List = 2
        | Set = 3
        | Map = 4
        | Tuple = 5
        | Other = 100
        
    let getKind =
        let cache = Dict<Type, TypeKind>()
        let listTy = typedefof<_ list>
        let setTy = typedefof<Set<_>>
        let mapTy = typedefof<Map<_,_>>
        fun (ty: System.Type) ->
            cache.GetOrAdd(ty, fun ty ->
                if ty.IsGenericType && ty.GetGenericTypeDefinition() = listTy then TypeKind.List
                elif ty.IsGenericType && ty.GetGenericTypeDefinition() = setTy then TypeKind.Set
                elif ty.IsGenericType && ty.GetGenericTypeDefinition() = mapTy then TypeKind.Map
                elif FSharpType.IsTuple(ty) then TypeKind.Tuple
                elif FSharpType.IsUnion(ty, true) then TypeKind.Union
                elif FSharpType.IsRecord(ty, true) then TypeKind.Record
                else TypeKind.Other)
    let isUnion ty =
        getKind ty = TypeKind.Union
    let isRecord ty =
         getKind ty = TypeKind.Record
         
    let isList ty =
        getKind ty = TypeKind.List

    let isSet ty =
        getKind ty = TypeKind.Set

    let isMap ty =
        getKind ty = TypeKind.Map

    let isTuple ty =
        getKind ty = TypeKind.Tuple