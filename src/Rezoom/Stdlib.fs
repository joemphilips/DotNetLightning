module Rezoom.Stdlib

open System
open System

module TrivialErrand =
    let ofSynchronous category id f =
        let cacheInfo = {
                new CacheInfo() with
                    override __.Category = category
                    override __.Identity = id
                    override __.Cacheable = false
            }
            
        fun arg ->
            {
                new SynchronousErrand<_>() with
                    override __.CacheInfo = cacheInfo
                    override __.CacheArgument = box arg
                    override __.Prepare(_) = fun () -> f arg
            }

type private StdlibFunction = class end
let private category = typeof<StdlibFunction>
let private trivial name f =
    TrivialErrand.ofSynchronous category name f
let private trivialUnit name f =
    trivial name f () |> Plan.ofErrand
let private dateTimeUtcNow = trivialUnit "dtUtcNow" (fun _ -> DateTime.UtcNow)
let private dateTimeNow = trivialUnit "dtLocalNow" (fun _ -> DateTime.Now)
let private dateTimeOffsetUtcNow = trivialUnit "dtoUtcNow" (fun _ -> DateTimeOffset.UtcNow)
let private dateTimeOffsetNow = trivialUnit "dtoLocalNow" (fun _ -> DateTimeOffset.Now)

type DateTime with
    static member UtcNowPlan = dateTimeUtcNow
    static member NowPlan = dateTimeNow
    
type DateTimeOffset with
    static member UtcNowPlan = dateTimeOffsetUtcNow
    static member NowPlan = dateTimeOffsetNow