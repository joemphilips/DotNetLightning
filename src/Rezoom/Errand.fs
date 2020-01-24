namespace Rezoom

open System.Threading
open System.Threading.Tasks

[<AbstractClass>]
type Errand() =
    abstract member CacheInfo : CacheInfo
    abstract member CacheArgument : obj
    default __.CacheArgument = null
    abstract member SequenceGroup: obj
    default __.SequenceGroup = null
    abstract member PrepareUntyped: ServiceContext -> (CancellationToken -> obj Task)
    
[<AbstractClass>]
type Errand<'a>() =
    inherit Errand()
    
[<AbstractClass>]
type AsynchronousErrand<'a>() =
    inherit Errand<'a>()
    static member private BoxResult(task: 'a Task) =
        box task.Result
        
    abstract member Prepare : ServiceContext -> (CancellationToken -> 'a Task)
    override this.PrepareUntyped(cxt) : CancellationToken -> obj Task =
        let typed = this.Prepare(cxt)
        fun token ->
            (typed token).ContinueWith(AsynchronousErrand<'a>.BoxResult, TaskContinuationOptions.ExecuteSynchronously)
            
[<AbstractClass>]
type SynchronousErrand<'a>() =
    inherit Errand<'a>()
    abstract member Prepare : ServiceContext -> (unit -> 'a)
    override this.PrepareUntyped(cxt): CancellationToken -> obj Task =
        let sync = this.Prepare(cxt)
        fun _ -> Task.FromResult(box (sync()))
