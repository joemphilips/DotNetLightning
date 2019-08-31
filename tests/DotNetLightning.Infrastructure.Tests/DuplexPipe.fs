[<AutoOpen>]
module DuplexPipe
open System.IO.Pipelines

type DuplexPipePair = {
    Transport: IDuplexPipe
    Application: IDuplexPipe
}

and DuplexPipe = {
    Input: PipeReader
    Output: PipeWriter
}
    with
    static member CreatePair(?inputOption, ?outputOption) =
        let inputOption = defaultArg inputOption PipeOptions.Default
        let outputOption = defaultArg outputOption PipeOptions.Default
        let input = Pipe(inputOption)
        let output = Pipe(outputOption)
        let transportToApplication = { Input = output.Reader; Output = input.Writer }
        let applicationToTransport = { Input = input.Reader; Output = output.Writer }
        {
            Transport = applicationToTransport
            Application = transportToApplication
        }
    interface IDuplexPipe with
        member this.Input = this.Input
        member this.Output = this.Output

