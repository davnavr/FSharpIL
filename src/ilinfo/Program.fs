module ILInfo.Program

open Argu

open FSharpIL

[<RequireQualifiedAccess>]
type OutputKind =
    | Stdout
    //| Html

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Stdout -> "the assembly information is directed to standard output"

type Argument =
    | Coff_Header
    | Standard_Fields
    | NT_Specific_Fields
    | Data_Directories
    | Section_Headers
    | [<ExactlyOnce>] File of string
    | [<Unique>] Launch_Debugger
    | [<Unique>] Output of OutputKind

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Coff_Header -> "include the values of the COFF header fields in the output."
            | Standard_Fields -> "includes the values of the optional header standard fields in the output."
            | NT_Specific_Fields -> "includes the values of the optional header NT-specific fields in the output."
            | Section_Headers -> "includes the values of the section headers' fields in the output."
            | Data_Directories -> "includes the RVAs and sizes of each data directory in the output."
            | File _ -> "specifies the file containing the metadata to read."
            | Launch_Debugger -> "launches the debugger."
            | Output _ -> "specifies how the assembly information is outputted."

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<Argument>(programName = "ilinfo", helpTextMessage = "CLI metadata reader")
    let result = parser.ParseCommandLine(args, raiseOnUsage = false)

    // TODO: Show usage if no arguments/help argument is specified

    0
