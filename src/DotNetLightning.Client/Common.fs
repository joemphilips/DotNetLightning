[<AutoOpen>]
module internal DotNetLightning.Client.Common

open System.Text.Json
open Microsoft.FSharp.Quotations

let nameof (q:Expr<_>) = 
  match q with 
  | Patterns.Let(_, _, DerivedPatterns.Lambdas(_, Patterns.Call(_, mi, _))) -> mi.Name
  | Patterns.PropertyGet(_, mi, _) -> mi.Name
  | DerivedPatterns.Lambdas(_, Patterns.Call(_, mi, _)) -> mi.Name
  | _ -> failwith "Unexpected format"

let any<'R> : 'R = failwith "!"

let inline checkNull<'T when 'T : null> (a: 'T) =
    if isNull a then nullArg (sprintf "%A" (nameof <@ any<'T> @> )) else ()
    
type JsonElement
    with
    member this.ToObject<'T>() =
        this.GetRawText() |> JsonSerializer.Deserialize<'T>
        
type JsonDocument
    with
    member this.ToObject<'T>() =
        this.RootElement.GetRawText() |> JsonSerializer.Deserialize<'T>
    
