[<AutoOpen>]
module internal DotNetLightning.TestFramework.Utils

open System.IO
open System.Reflection

let getAssemblyDirectory () =
    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location) |> Path.GetFullPath