namespace DotNetLightning.Server.P2PPipeline

open DotNetLightning.Server.P2P
open Microsoft.Extensions.Logging
open System.Threading.Tasks

/// These are based on Giraffe. But not for p2p context instead of HTTP Context
/// refs: https://github.com/giraffe-fsharp/Giraffe/blob/master/DOCUMENTATION.md
type P2PFuncResult = Task<P2PConnectionContext option>
type P2PFunc = P2PConnectionContext -> P2PFuncResult
type P2PHandler = P2PFunc -> P2PFunc
type ErrorHandler = exn -> ILogger -> P2PHandler

module Core =
    let inline warbler f (next: P2PHandler) (ctx: P2PConnectionContext) = f (next, ctx) next ctx
    let skipPipeline : P2PFuncResult = Task.FromResult None
    let earlyReturn: P2PFunc = Some >> Task.FromResult
    
    // ---------------------------
    // Default Combinators
    // ---------------------------

    let compose (handler1: P2PHandler) (handler2: P2PHandler): P2PHandler =
        fun (final : P2PFunc) ->
            let func = final |> handler2 |> handler1
            func
            
    let (>=>) = compose
    
